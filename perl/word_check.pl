#!/usr/bin/perl

#
# Script to find any five letter word pairs that have the
# same three letter prefix and end in either "nd" or "ze".
#

foreach $i ('a' .. 'z') {
    print "checking $i\n";
    foreach $j ('a' .. 'z') {
        foreach $k ('a' .. 'z') {
            $words = $i . $j . $k . "nd " . $i . $j . $k . "ze";
            $results = `echo $words | aspell list`;
            $length = length $results;
            if ( $length < 1 ) {
                print "$words\n";
            }
        }          
    }
}
