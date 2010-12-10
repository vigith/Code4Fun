%% P38 (*) Compare the two methods of calculating Euler's totient function.

%% Use the solutions of problems P34 and P37 to compare the algorithms. 
%% Take the number of logical inferences as a measure for efficiency. 
%% Try to calculate phi(10090) as an example.

-module(p38).

-export([compare_p34_p37/1]).

compare_p34_p37(Number) ->
    statistics(wall_clock),
    Ans1 = p34:totient_func(Number),
    {_, Time_34} = statistics(wall_clock),
    U_34 = Time_34 * 1000, 
    io:format("Ans by p34.erl is [~w] and took ~p miscroseconds~n", [Ans1, U_34]),
    statistics(wall_clock),
    Ans2 = p37:totient_func(Number),
    {_, Time_37} = statistics(wall_clock),
    U_37 = Time_37 * 1000, 
    io:format("Ans by p37.erl is [~w] and took ~p miscroseconds~n", [Ans2, U_37]).
