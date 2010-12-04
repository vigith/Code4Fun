#!/usr/bin/env perl

# Question
# http://www.topcoder.com/stat?c=problem_statement&pm=1259&rd=4493

# Author: Vigith Maurice <vigith@gmail.com>

use strict;
use Test::More tests => 5;


sub zigzag {
  my @arr = @_;
  my $flag    = +2; # 1 means +ve and -1 means -ve, +2 is for starter
  my $counter = 1;
  
  for (my $i = 0; $i < $#arr; $i++) { ## -1 because we always take with one element ahead
    # the XOR operation will make sure two flags won't be same
    if (($flag ^ (($arr[$i+1] - $arr[$i]) cmp 0)) != 0 and ($arr[$i+1] - $arr[$i] != 0)) {
      $counter++;
    }

    # set the flag for the current case
    $flag = (($arr[$i+1] - $arr[$i]) cmp 0) if ($arr[$i+1] - $arr[$i]);
  }

  return $counter;
}


# test array, 2D array, 1 element the list of sequence and 2nd element is the longest subsequence
my $test = [
	    [[70, 55, 13, 2, 99, 2, 80, 80, 80, 80, 100, 19, 7, 5, 5, 5, 1000, 32, 32], 8],
	    [[1, 17, 5, 10, 13, 15, 10, 5, 16, 8], 7],
	    [[1,7,4,9,2,5], 6],
	    [[1, 2, 3, 4, 5, 6, 7, 8, 9], 2],
	    [[1], 1]
	   ];

foreach (@$test) {
  ok(zigzag(@{$_->[0]} == $_->[1], $#{$_->[0]}))
}

#done_testing(scalar @$test);
