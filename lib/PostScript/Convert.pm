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
our $VERSION = '0.01';          ## no critic

use strict;
use warnings;
use Carp qw(croak verbose);
use File::Spec ();
use Scalar::Util qw(blessed openhandle reftype);

=head1 DEPENDENCIES

PostScript::Convert depends on L<Carp>, L<Exporter>, L<File::Spec>,
L<File::Temp>, and L<Scalar::Util>.  All of these are core modules,
but you may need to install a newer version of File::Temp.

It also requires you to have Ghostscript
(L<http://pages.cs.wisc.edu/~ghost/>) installed somewhere on your
PATH, unless you use the C<ghostscript> option to specify its
location.

=cut

use Exporter 'import';

our @EXPORT = qw(psconvert);

#=====================================================================
# Package PostScript::Convert:

our $Debug;  # Set this to a true value for debugging output to STDERR

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

  return convert_fh( openhandle $ps, \%opt) if openhandle $ps;
  return convert_object($ps, \%opt) if blessed $ps;
  return convert_ref(   $ps, \%opt) if ref $ps;
  convert_filename(     $ps, \%opt);
} # end psconvert

#---------------------------------------------------------------------
sub convert_object
{
  my ($obj, $opt) = @_;

  return convert_psfile($obj, $opt) if $obj->isa('PostScript::File');

  return convert_psfile($obj->get__PostScript_File, $opt)
      if $obj->can('get__PostScript_File');

  croak "Don't know how to handle a " . blessed($obj);
} # end convert_object

=diag C<< Don't know how to handle a %s >>

You passed an object that psconvert doesn't accept as C<$input>.

=cut

#---------------------------------------------------------------------
sub convert_psfile
{
  my ($ps, $opt) = @_;

  # Check version of PostScript::File:
  my $v = PostScript::File->VERSION;
  croak "Must have PostScript::File 2.00 or later, this is only $v"
      unless $v >= 2;

=diag C<< Must have PostScript::File 2.00 or later, this is only %s >>

PostScript::Convert isn't directly compatible with versions of
PostScript::File older than 2.00.  (If you can't upgrade
PostScript::File, then you can write the PostScript to a file and pass
that file to psconvert.)

=cut

  # Save old filename:
  my $oldFN  = $ps->get_filename;
  $opt->{input} ||= "$oldFN.ps" if defined $oldFN;

  require File::Temp;

  if ($ps->get_eps and $ps->get_pagecount > 1) {
    # Compute output filename:
    apply_format($opt);
    my ($outVol, $outDir, $outFN) =
        File::Spec->splitpath( guess_output_filename($opt) );

    $outFN =~ s/(\.\w+)$// or croak "No extension in $outFN";
    my $ext = $1;

=diag C<< No extension in %s >>

The output filename must have a file extension.

=diag C<< Expected extension in %s >>

The temporary filename created by PostScript::File must have an
extension, but it didn't.

=diag C<< Can't seek temporary file: %s >>

A seek failed for the specified reason.

=cut

    my $dir = File::Temp->newdir;

    my $oldExt = $ps->get_file_ext;
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
  } # end if EPS with multiple pages
  else {
    # Only one file, we don't need a temporary directory:
    my $fh = File::Temp->new;

    $ps->output($fh);

    seek($fh, 0,0) or croak "Can't seek temporary file: $!";

    convert_fh($fh, $opt);
  } # end else only one PostScript file to process
} # end convert_psfile

#---------------------------------------------------------------------
sub convert_ref
{
  my ($ref, $opt) = @_;

  my $type = reftype $ref;

  croak "Don't know how to handle a $type ref"
      unless $type eq 'SCALAR' or $type eq 'ARRAY';

=diag C<< Don't know how to handle a %s ref >>

psconvert only accepts a scalar or array reference as C<$input>.

=cut

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

=diag C<< Unable to open %s: %s >>

Opening the specified file failed for the specified reason.

=cut

  convert_fh($in, $opt);
} # end convert_filename

#---------------------------------------------------------------------
sub check_options
{
  my ($opt) = @_;

  my @cmd = ($opt->{ghostscript} || croak "ghostscript not defined");

=diag C<< ghostscript not defined >>

The C<ghostscript> option was somehow unset.  This shouldn't happen,
since it has a default value.

=diag C<< No output device supplied >>

The C<device> option (which normally comes from the C<format>) was not set.

=cut

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

  print STDERR "@cmd\n" if $Debug;

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

=diag C<< No output format or filename supplied >>

You didn't specify the C<format> option, nor did you supply an output
filename from which to guess it.

=diag C<< Unable to determine format from %s >>

You didn't specify the C<format> option, and the output filename you
supplied doesn't match any known format.

=diag C<< Unknown format %s >>

The C<format> you specified is not valid.

=cut

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

=diag C<< No extension defined for format %s >>

The specified C<format> failed to define a file extension.

=diag C<< No output filename supplied >>

You didn't specify an output filename, nor did you provide an input
filename to guess it from.

=cut

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

=diag C<< Can't %s STDIN: %s >>

There was an error while redirecting STDIN in order to run Ghostscript.

=diag C<< Ghostscript failed: exit status %s >>

Ghostscript did not exit successfully.  The exit status is reported as
a decimal number (C<<< $? >> 8 >>>),
followed by " (signal %d)" if C<< $? & 127 >> is non-zero,
followed by " (core dumped)" if C<< $? & 128 >>.

=cut

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
    psconvert($ps, filename => $output_filename, format => 'pnggray');

=head1 DESCRIPTION

PostScript::Convert uses Ghostscript to convert PostScript to other
formats.  You will need to have Ghostscript installed.

It exports a single function:

=head2 psconvert

  psconvert($input, [$output_filename], [options...])

This takes the PostScript code pointed to by C<$input> and processes
it through Ghostscript.  The return value is not meaningful.  It
throws an exception if an error occurs.

=head3 Input specifications

C<$input> must be one of the following:

=over

=item A string

This is interpreted as a filename to open.

=item A scalar reference

This must be a reference to a string containing a PostScript document.

=item An array reference

This must be a reference to an array of strings, which when joined
together form a PostScript document.  No newlines are added when joining.

=item An open filehandle

Any argument accepted by L<Scalar::Util/openhandle> is interpreted as
a filehandle to read from.

=item A PostScript::File object

Note: in C<eps> mode, this will generate multiple output files if the
document has multiple pages.

=item Any other object

The object must implement a C<get__PostScript_File> method that
returns a PostScript::File object.
(Note: there are 2 underscores after C<get>)

=back

=head3 Output options

The remaining arguments after C<$input> are key-value pairs that
control the output.  If there are an odd number of arguments following
C<$input>, then the first one is the C<filename>.

=over

=item C<filename>

This is the output filename.  If omitted, it will be calculated from
C<input> and C<format>.

=item C<format>

This is the output format.  If omitted, it will be taken from the
extension of C<filename>.  Accepted formats are:

=over

=item C<png>

24-bit color PNG

=item C<pnggray>

8-bit grayscale PNG

=item C<pngmono>

1-bit monochrome PNG

=item C<pdf>

The preferred PDF version (currently 1.4, but subject to change).

=item C<pdf14>

PDF version 1.4 (Acrobat 5.0 - 2001)

=item C<pdf13>

PDF version 1.3 (Acrobat 4.0 - 1999)

=item C<pdf12>

PDF version 1.2 (Acrobat 3.0 - 1996)

=back

=item C<ghostscript>

This is the Ghostscript executable to use.  It defaults to C<gs>,
except on Microsoft Windows, where it is C<gswin32c.exe>.
(You may use a pathname here.)

=item C<include>

An arrayref of directories to add to Ghostscript's search path (for
advanced users only).

=item C<input>

This is the input filename.  (This is used only for calculating
C<filename> when necessary.  It does not mean to actually read from
this file, and it need not exist on disk.)  If omitted, it will be
taken from C<$input> (if that is a filename or a PostScript::File
object containing a filename).

=item C<device>

The Ghostscript device to use (for advanced users only).  This is
normally set automatically from the C<format>.

=item C<gs_param>

An arrayref of additional parameters to pass to Ghostscript (for
advanced users only).

=item C<unsafe>

Ghostscript is normally run with -dSAFER, which prevents the
PostScript code from accessing the filesystem.  Passing
S<< C<< unsafe => 1 >> >> will use -dNOSAFER instead.  Don't do this
unless you trust the PostScript code you are converting.

=back


=head1 INCOMPATIBILITIES

PostScript::Convert is not compatible with versions of
PostScript::File older than 2.00.  (However, you could have an older
version of PostScript::File write the PostScript to a file, and then
pass that file to C<psconvert>.)

=for Pod::Coverage
apply_format
check_options
^convert_
guess_output_filename
