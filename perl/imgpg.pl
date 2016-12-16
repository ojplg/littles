#!/usr/bin/perl

use strict;
use warnings;

# imgpg -- create an index page with thumbnails for some images in a directory

# open files in directory
# use image magick to create thumbnails
# write a quick index.html which includes thumbnails and links to each image

# `convert $file -resize 100x100 thumb_$file`

my %thumbs = ();

opendir (my $dh, ".") || die "cannot open directory";

my @files = readdir ($dh);

foreach my $file (@files) {
    if ( ! ($file =~ /\.jpg$/ ) ){
       next;
    }
    my $thumb = "thumb_$file";
    print "$thumb -> $file\n";
    $thumbs{$thumb} = $file;
    `convert $file -resize 100x100 $thumb`;
}

closedir $dh;

open my $ifh, "> index.html";

print $ifh "<html>\n";
print $ifh "  <body>\n";

foreach my $thumb (sort keys %thumbs){
    print $ifh "    <a href=\"$thumbs{$thumb}\"><img src=\"$thumb\"></a>\n";
}

print $ifh "  </body>\n";
print $ifh "</html>\n";

close $ifh;
