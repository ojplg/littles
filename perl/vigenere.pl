#!/usr/bin/perl -w

use strict;

# script for doing vigenere cyphers

my $key = "ZM";
my $plain_file = "plain.txt";
my $encoded_file = "encoded.txt";
my $decoded_file = "decoded.txt";

my $decode_mode;
my $source_file;
my $destination_file;

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

my @char_keys = split(//, $key);
my $key_length =  length $key;

sub map_char($$$){
    my $char = shift;
    my $key_char = shift;
    my $decode_flag = shift;
    my $offset = ord($key_char) - 64;
    if ( $decode_flag ){
        $offset = -$offset;
    }
    my $num = ord($char) - 64 + $offset;
    if ( $num > 26 ){
        $num = $num - 26;
    }
    if ( $num < 0 ) {
        $num = $num + 26;
    }
    $num = $num + 64;
    my $value = chr($num);
    $value;
}

my $cnt = 0;

open FILE, "< $source_file";
open OUT, "> $destination_file";
while(<FILE>){
    my @array = split(//);
    foreach my $char (@array){
        if ( $char eq " " ){
            print OUT " ";
        } elsif ($char eq "\n" ){
            print OUT "\n";
        } else {
            my $key_char = $char_keys[$cnt % $key_length];
            print OUT map_char($char, $key_char, $decode_mode);
            $cnt++;
        }
    }
}

close OUT;
close FILE;
