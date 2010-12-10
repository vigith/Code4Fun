%% P39 (*) A list of prime numbers.

%% Given a range of integers by its lower and upper limit, 
%% construct a list of all prime numbers in that range.

-module(p39).

-export([list_prime_numbers/2]).

list_prime_numbers(_, End) when End =< 1 ->
    error_prime_number;
list_prime_numbers(_, End) when End =:= 2 ->
    [2];
list_prime_numbers(Start, End) ->
    Range = p22:range(2, End),
    ListOfPrimes = primes(Range, 2),
    extract_range(ListOfPrimes, Start). %% no need of End limit as it is already been taken care of

extract_range(List = [H|_], Start) when H >= Start ->
    List;
extract_range([_|T], Start) ->
    extract_range(T, Start);
extract_range([], _) ->
    out_of_index.


primes(Range, Prime) ->
    Divisbles = get_divisible(Range, Prime, []),
    NewRange = lists:subtract(Range, lists:reverse(Divisbles)),
    NextPrime = next_prime(NewRange, Prime),
    SqRt = math:sqrt(lists:last(NewRange)),
    if 
	NextPrime > SqRt ->
	    NewRange;
	true ->
	    primes(NewRange, NextPrime)
    end.
    

next_prime([Prime|T], Prime) ->
    [NextPrime | _] = T,
    NextPrime;
next_prime([], _) ->
    out_of_index;
next_prime([_|T], Prime) ->
    next_prime(T, Prime).

get_divisible([H|T], Prime, Acc) ->
    SqRt = math:sqrt(H),
    if
	SqRt < Prime ->
	    get_divisible(T, Prime, Acc);
	true ->
	    if 
		H rem Prime =:= 0 ->
		    get_divisible(T, Prime, [H | Acc]);
		true ->
		    get_divisible(T, Prime, Acc)
	    end
    end;
get_divisible([], _, Acc) ->
    Acc.
   	      
