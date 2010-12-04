#!/usr/bin/env perl

# Question:
# http://www.topcoder.com/stat?c=problem_statement&pm=2402&rd=5009

# Author: Vigith Maurice <vigith@gmail.com>

use strict;
use Test::More tests => 4;

$\ = "\n";

sub bad_neighbors {
  my @arr = @_;
  my $dp  = [[0, 0]];  # initial state, 0 elements chosen and value is 0
  my $fst = [0, 0];    # is the first elemented taken or not

  # for each index there are two options
  # either take the index or leave it
  for (my $i=0; $i<=$#arr; $i++) {
    # $i+1 for $dp indexing because 0th element is [0, 0]
    $dp->[$i+1][0] = ($dp->[$i+1-1][1] >= $dp->[$i+1-1][0] ? $dp->[$i+1-1][1] : $dp->[$i+1-1][0]); # take the max of previous if previous is not included
    $dp->[$i+1][1] = $dp->[$i+1-1][0] + $arr[$i];
  }

#  use Data::Dumper; print Dumper $dp;
  return $dp->[$#arr+1][0] >= $dp->[$#arr+1][1] ? $dp->[$#arr+1][0] : $dp->[$#arr+1][1];
}

sub max_bad_neighbors {
  my @arr = @_;
  # leave first and last and compute. 
  # solution is whoever is larger
  my $with_1 = bad_neighbors(@arr[0..($#arr - 1)]);
  my $with_n = bad_neighbors(@arr[1..$#arr]);

  my $ans = $with_n >= $with_1 ? $with_n : $with_1;
  return $ans
}

my $test = [
	    [[10, 3, 2, 5, 7, 8], 19],
	    [[11, 15], 15],
	    [[7, 7, 7, 7, 7, 7, 7], 21],
	    [[1, 2, 3, 4, 5, 1, 2, 3, 4, 5], 16],
	    [[94, 40, 49, 65, 21, 21, 106, 80, 92, 81, 679, 4, 61, 6, 237, 12, 72, 74, 29, 95, 265, 35, 47, 1, 61, 397, 52, 72, 37, 51, 1, 81, 45, 435, 7, 36, 57, 86, 81, 72], 2926],
	    ];

foreach (@$test) {
  ok(max_bad_neighbors(@{$_->[0]}) == $_->[1])
}
