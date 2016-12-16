#!/usr/bin/perl -w

use strict;

# script for doing ciphers that act on two character blocks

my $plain_file = "plain.txt";
my $encoded_file = "encoded.txt";
my $decoded_file = "decoded.txt";

my $decode_mode;
my $source_file;
my $destination_file;

my $seperator = "_";

sub make_encoding_map(){
    my $cnt = 1;
    my %map = ();
    foreach my $first (("A".."Z")){
        foreach my $second (("A".."Z")){
            my $key = $first . $second;
            $map{$key} = $cnt;
            $cnt++;
        }
    }
    foreach my $char (("A".."Z")){
        $map{$char . " "} = $cnt;
        $cnt++;
    }
    %map;
}

sub make_decoding_map(){
    my %encoding_map = make_encoding_map();
    my %decoding_map = ();
    foreach my $string (keys %encoding_map){
        $decoding_map{$encoding_map{$string}} = $string;
    }
    %decoding_map;
}

sub word_to_pairs($){
    my $word = shift;
    my $length = length $word;
    if ( $length % 2 == 1 ){
        $word = $word . " ";
    }
    my @pairs = ();
    for (my $i = 0; $i < $length ; $i+=2 ){
        push (@pairs, substr ($word, $i, 2))
    }
    @pairs;
}

my $cnt = 0;

if ( $ARGV[0] && $ARGV[0] eq "decode" ){
    $decode_mode = 1;
    $source_file = $encoded_file;
    $destination_file = $decoded_file;
    print "decoding contents of $source_file to $destination_file\n";
} else {
    $decode_mode = 0;
    $source_file = $plain_file;
    $destination_file = $encoded_file;
    print "encoding contents of $source_file to $destination_file\n";
}


sub encode_line($){
    my @words = split(/ /);
    my $encoded = "";
    my %encoder = make_encoding_map();
    foreach my $word (@words){
        print "word is |$word| broken into: \n";
        my @pairs = word_to_pairs($word);
        foreach my $pair (@pairs) {
            my $code = $encoder{$pair};
            print " |$pair| mapped to $code\n";
            $encoded = $encoded . $code . " ";
        }
        $encoded =~ s/\s$//;
        $encoded  = $encoded . $seperator;
    }
    $encoded;
}
 
sub decode_line($){
    my @code_words = split(/$seperator/, $_);
    my $decoded = "";
    my %decoder = make_decoding_map();
    foreach my $code_word (@code_words){
        print "code word |$code_word|\n";
        my @codes = split (/ /, $code_word );
        foreach my $code (@codes) {
            my $letters = $decoder{$code};
            print " $code -> $letters \n";
            $decoded = $decoded . $letters;
        }
        $decoded =~ s/\s$//;
        $decoded = $decoded . " ";
    }
    $decoded;
}

open FILE, "< $source_file";
open OUT, "> $destination_file";
while(<FILE>){
    chomp;
    if ( $decode_mode ){
        print OUT decode_line($_);
    } else {
        print OUT encode_line($_);
    }
    print OUT "\n";
}

close OUT;
close FILE;
