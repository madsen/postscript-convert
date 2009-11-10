#---------------------------------------------------------------------
package PostScript::Convert;
#
# Copyright 2009 Christopher J. Madsen
#
# Author: Christopher J. Madsen <perl@cjmweb.net>
# Created: November 9, 2009
#
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either the
# GNU General Public License or the Artistic License for more details.
#
# ABSTRACT: Use Ghostscript to convert PostScript to other formats
#---------------------------------------------------------------------

use 5.008;
our $VERSION = '0.01';

use strict;
use warnings;
use Carp qw(croak verbose);
use File::Spec ();
use Scalar::Util qw(blessed reftype);

=head1 DEPENDENCIES

PostScript::Convert depends on L<Carp>, L<Exporter>, L<File::Spec>,
L<File::Temp>, and L<Scalar::Util>.  All of these are core modules,
but you may need to install a newer version of File::Temp.

=cut

use Exporter 'import';

our @EXPORT = qw(psconvert);

#=====================================================================
# Package PostScript::Convert:

our %default = (
  ghostscript => ($^O =~ 'MSWin32' ? 'gswin32c.exe' : 'gs'),
);

our %format = (
  png => {
    device    => 'png16m',
    extension => 'png',
    format_param => [qw(-dTextAlphaBits=4 -dGraphicsAlphaBits=4)],
  },
  pnggray => {
    device    => 'pnggray',
    extension => 'png',
    format_param => [qw(-dTextAlphaBits=4 -dGraphicsAlphaBits=4)],
  },
  pngmono => {
    device    => 'pngmono',
    extension => 'png',
  },
  pdf14 => {
    device    => 'pdfwrite',
    extension => 'pdf',
    format_param => [qw(-dCompatibilityLevel=1.4 -c .setpdfwrite)],
  },
  pdf13 => {
    device    => 'pdfwrite',
    extension => 'pdf',
    format_param => [qw(-dCompatibilityLevel=1.3 -c .setpdfwrite)],
  },
  pdf12 => {
    device    => 'pdfwrite',
    extension => 'pdf',
    format_param => [qw(-dCompatibilityLevel=1.2 -c .setpdfwrite)],
  },
);

$format{pdf} = $format{pdf14};

#---------------------------------------------------------------------
sub psconvert
{
  my $ps = shift;

  unshift @_, 'filename' if @_ % 2;
  my %opt = (%default, @_);

  return convert_object($ps, \%opt) if blessed $ps;
  return convert_ref(   $ps, \%opt) if ref $ps;
  convert_filename(     $ps, \%opt);
} # end psconvert

#---------------------------------------------------------------------
sub convert_object
{
  my ($obj, $opt) = @_;

  return convert_fh(    $obj, $opt) if $obj->isa('IO::Handle');
  return convert_psfile($obj, $opt) if $obj->isa('PostScript::File');

  return convert_psfile($obj->get__PostScript_File, $opt)
      if $obj->can('get__PostScript_File');

  croak "Don't know how to handle a " . blessed($obj);
} # end convert_object

#---------------------------------------------------------------------
sub convert_psfile
{
  my ($ps, $opt) = @_;

  # Check version of PostScript::File:
  my $v = PostScript::File->VERSION;
  croak "Must have PostScript::File 2.00 or later, this is only $v"
      unless $v >= 2;

  # Save old filename:
  my $oldFN  = $ps->get_filename;
  my $oldExt = $ps->get_file_ext;
  $opt->{input} ||= "$oldFN.ps" if defined $oldFN;

  # Compute output filename:
  apply_format($opt);
  my ($outVol, $outDir, $outFN) =
      File::Spec->splitpath( guess_output_filename($opt) );

  $outFN =~ s/(\.\w+)$// or croak "No extension in $outFN";
  my $ext = $1;

  require File::Temp;

  my $dir = File::Temp->newdir;

  $ps->set_filename($outFN, $dir);
  $ps->set_file_ext(undef);

  # Process the file(s):
  my @files = $ps->output;

  foreach my $fn (@files) {
    $outFN = (File::Spec->splitpath($fn))[2];
    $outFN =~ s/\.\w+$/$ext/ or die "Expected extension in $outFN";

    $opt->{filename} = File::Spec->catpath( $outVol, $outDir, $outFN );

    convert_filename($fn, $opt);
  } # end foreach $fn in @files

  # Restore settings:
  $ps->set_filename($oldFN);
  $ps->set_file_ext($oldExt);
} # end convert_psfile

#---------------------------------------------------------------------
sub convert_ref
{
  my ($ref, $opt) = @_;

  my $type = reftype $ref;

  convert_fh($ref, $opt) if $type eq 'GLOB';

  croak "Don't know how to handle a $type ref"
      unless $type eq 'SCALAR' or $type eq 'ARRAY';

  require File::Temp;

  my $fh = File::Temp->new;

  if ($type eq 'ARRAY') { print $fh @$ref }
  else                  { print $fh $$ref }

  seek($fh, 0,0) or croak "Can't seek temporary file: $!";

  convert_fh($fh, $opt);
} # end convert_ref

#---------------------------------------------------------------------
sub convert_filename
{
  my ($filename, $opt) = @_;

  $opt->{input} ||= $filename;
  open(my $in, '<:raw', $filename) or croak "Unable to open $filename: $!";

  convert_fh($in, $opt);
} # end convert_filename

#---------------------------------------------------------------------
sub check_options
{
  my ($opt) = @_;

  my @cmd = ($opt->{ghostscript} || croak "ghostscript not defined");

  foreach my $dir (@{ $opt->{include} || [] }) {
    push @cmd, "-I$dir";
  } # end foreach $dir

  push @cmd, qw(-q -sstdout=%stderr -dBATCH -dNOPAUSE);
  push @cmd, ($opt->{unsafe} ? '-dNOSAFER' : '-dSAFER');

  apply_format($opt);

  push @cmd, "-sOutputFile=" . guess_output_filename($opt);

  my $device = $opt->{device};
  croak "No output device supplied" unless defined $device and length $device;
  push @cmd, "-sDEVICE=$device";

  push @cmd, @{ $opt->{format_param} } if $opt->{format_param};
  push @cmd, @{ $opt->{gs_param} }     if $opt->{gs_param};

  print STDERR "@cmd\n";

  @cmd;
} # end check_options

#---------------------------------------------------------------------
sub apply_format
{
  my ($opt) = @_;

  unless ($opt->{format}) {
    my $outFN = $opt->{filename};

    croak "No output format or filename supplied"
        unless defined $outFN and length $outFN;

    $outFN =~ /\.([^.\s]+)$/ or croak "Unable to determine format from $outFN";
    $format{ $opt->{format} = lc $1 } or croak "Unknown extension .$1";
  }

  my $fmt = $format{ $opt->{format} } or croak "Unknown format $opt->{format}";

  while (my ($key, $val) = each %$fmt) {
    $opt->{$key} = $val unless defined $opt->{key};
  }
} # end apply_format

#---------------------------------------------------------------------
sub guess_output_filename
{
  my ($opt) = @_;

  my $fn = $opt->{filename};

 CHOICE: {
    last CHOICE if defined $fn;

    $fn = $opt->{input};
    last CHOICE unless defined $fn and length $fn;

    my $ext = $opt->{extension};
    croak "No extension defined for format $opt->{format}" unless $ext;

    $fn =~ s/(?:\.\w*)?$/.$ext/;
  }

  croak "No output filename supplied" unless defined $fn and length $fn;

  $fn;
} # end guess_output_filename

#---------------------------------------------------------------------
sub convert_fh
{
  my ($fh, $opt) = @_;

  my @cmd = (check_options($opt), '-_');

  open(my $oldin, '<&STDIN') or croak "Can't dup STDIN: $!";
  open(STDIN, '<&', $fh)     or croak "Can't redirect STDIN: $!";
  system @cmd;
  open(STDIN, '<&', $oldin)  or croak "Can't restore STDIN: $!";

  if ($?) {
    my $exit   = $? >> 8;
    my $signal = $? & 127;
    my $core   = $? & 128;

    my $err = "Ghostscript failed: exit status $exit";
    $err .= " (signal $signal)" if $signal;
    $err .= " (core dumped)"    if $core;

    croak $err;
  } # end if ghostscript failed
} # end convert_fh

#=====================================================================
# Package Return Value:

1;

__END__

=head1 SYNOPSIS

    use PostScript::Convert;

    psconvert($filename, $output_filename);

    # Base output filename on input filename:
    psconvert($filename, format => 'pdf');

    my $postscript = "%!PS-Adobe-3.0 ...";
    psconvert(\$postscript, $output_filename);

    my $ps = PostScript::File->new;
    $ps->add_to_page(...);
    psconvert($ps, $output_filename);

=head1 DESCRIPTION

PostScript::Convert uses Ghostscript to convert PostScript to other
formats.  You will need to have Ghostscript installed.

=head2 psconvert

  psconvert($input, [$output_filename], [options...])

This is the only function exported by PostScript::Convert.  The return
value is not meaningful.  It throws an exception if an error occurs.

C<$input> may be one of the following:

=over

=item A string

This is interpreted as a filename to open.

=item A scalar reference

This is must be a reference to a string containing a PostScript document.

=item An array reference

This must be a reference to an array of strings, which when joined
together form a PostScript document.  No newlines are added when joining.

=item A glob reference

This is interpreted as a filehandle to read from.

=item An IO::Handle object

This is interpreted as a filehandle to read from.

=item A PostScript::File object

Note: in C<eps> mode, this will generate multiple output files if the
document has multiple pages.

=item Any other object

The object must implement a C<get__PostScript_File> method that
returns a PostScript::File object.
(Note: there are 2 underscores after C<get>)

=back

=head1 DIAGNOSTICS

=for author to fill in:
    List every single error and warning message that the module can
    generate (even the ones that will "never happen"), with a full
    explanation of each problem, one or more likely causes, and any
    suggested remedies.

=over

=item C<< Error message here, perhaps with %s placeholders >>

[Description of error here]

=item C<< Another error message here >>

[Description of error here]

[Et cetera, et cetera]

=back


=head1 INCOMPATIBILITIES

PostScript::Convert is not compatible with versions of
PostScript::File older than 2.00.  (However, you could have an older
version of PostScript::File write the PostScript to a file, and then
pass that file to C<psconvert>.)
