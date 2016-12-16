#!/usr/bin/perl -w

# An encrypt decrypt program that does a simple encoding 
# based on a secret key

use carp;
use strict;

sub main($$$) {
        my $command = shift;
        my $codeword = shift;
        my $file = shift;

        print "command is $command, codeword is $codeword, and file is $file\n";
       
        if($command eq "encode"){
                encode_file($codeword,$file);
        } elsif ($command eq "decode"){
                decode_file($codeword,$file);
        }
}

sub encode_file($$){
        my $codeword = shift;
        my $filename = shift;
        open(INFILE, "< $filename");
        open(OUTFILE, "> $filename.enc");
        while (<INFILE>) {
                chomp;
                print "line is |$_|\n"; 
                my @numbers = encode_line($codeword, $_);
                foreach my $num (@numbers){
                        print OUTFILE $num;
                        print OUTFILE " ";
                }
                print OUTFILE "\n";
        }
        close INFILE;
        close OUTFILE;
}

sub decode_file($$){
        my $codeword = shift;
        my $filename = shift;
        open(INFILE, "< $filename");
        open(OUTFILE, "> $filename.dec");
        while (<INFILE>) {
                chomp;
                print "decoding $_";
                my $decoded_line = decode_line($codeword,$_);
                #print "decoded to: $decoded_line \n";
                print OUTFILE $decoded_line;
                print OUTFILE "\n";
        }
        close INFILE;
        close OUTFILE;
}

sub decode_line($$){
        my $codeword = shift;
        my $line = shift;
        my @numbers = split / /, $line;
        my $cnt = 0;
        my $decoded_line = "";
        foreach my $num (@numbers){
                my $codechar = substr($codeword, $cnt % (length($codeword)), 1);
                my $pair = decode_pair($codechar, $num);
                $decoded_line .= $pair;
                $cnt++;
        }
        $decoded_line;
}

sub encode_line($$){
        my $codeword = shift;
        my $line = shift;
        my @numbers;
        for my $i (0 .. (length($line)-1)/2){
                my $pair = substr($line, $i*2, 2);
                my $codechar = substr($codeword, $i % (length($codeword)), 1);
                my $num = encode_pair($codechar, $pair);
                my $decoded_pair = decode_pair($codechar, $num);
                print "  The pair is |$pair| valued at $num by character $codechar decoded to $decoded_pair\n";
                push @numbers, $num;
        }
        @numbers;
}

sub encode_pair($$){
        my $codechar = shift;
        my $pair = shift;
        my $first = "true";
        my $num = 0;
        for my $c (split //, $pair){
               if($first) {
                        $num = 128 * (ord($c) + ord($codechar));
                } else {
                        $num += ord($c);
                }
                $first = "";
        }        
        $num;
}

sub decode_pair($$){
        my $codechar = shift;
        my $num = shift;
        if($num == 0){
                return "";
        }
        my $first_ord = $num / 128;
        my $second_ord = $num % 128;
        my $first_char = chr($first_ord - ord($codechar));
        if( $second_ord == 0){
                return $first_char;
        }
        my $second_char = chr($second_ord);
        "$first_char$second_char";
}

main($ARGV[0],$ARGV[1],$ARGV[2]);
