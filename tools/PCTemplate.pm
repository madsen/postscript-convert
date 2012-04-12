#---------------------------------------------------------------------
package tools::PCTemplate;
#
# Copyright 2012 Christopher J. Madsen
#
# Author: Christopher J. Madsen <perl@cjmweb.net>
# Created:  12 Apr 2012
#
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either the
# GNU General Public License or the Artistic License for more details.
#
# ABSTRACT: Pod::Loom template for PostScript-Convert
#---------------------------------------------------------------------

our $VERSION = '0.02';

use 5.010;
use Moose;
extends 'Pod::Loom::Template::Default';
with 'Pod::Loom::Role::Extender';

use List::Util qw(max);

use namespace::autoclean;

sub remap_sections { {
  DESCRIPTION => [ DESCRIPTION => 'Paper Sizes' ],
} }

#---------------------------------------------------------------------
sub section_Paper_Sizes
{
  my ($self, $title) = @_;

  require 'lib/PostScript/Convert.pm';

  my @sizes = sort keys %PostScript::Convert::paper_size;

  my $aMax = max( map { /^a(\d+)$/ ? $1 : () } @sizes );
  my $bMax = max( map { /^b(\d+)$/ ? $1 : () } @sizes );

  @sizes = grep { not /^[ab]\d+$/ } @sizes;

  for (@sizes) {
    $_ = qq'"$_"' if /[- ]/;
  }

  local $" = ', ';

  return <<"END SIZES";
\=head2 $title

Paper sizes are not case sensitive.  These are the known sizes:
@sizes, A0 - A$aMax, B0 - B$bMax.
END SIZES
} # end section_Paper_Sizes

#=====================================================================
# Package Return Value:

no Moose;
__PACKAGE__->meta->make_immutable;
1;
