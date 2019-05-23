-module(day01a).
-export([init/0, test/0]).
% c(day01a), l(day01a), day01a:init().
assert(A, A) -> ok;
assert(A, B) -> throw(lists:flatten(io_lib:format("~p is not equal to ~p", [A, B]))).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

parse(FileName) ->
    lists:map(fun binary_to_integer/1, readlines(FileName)).

init() ->
    Input = parse("day01a.txt"),
    ResultPart1 = solve(Input),
    io:format("Part 1: The result is ~p~n", [ResultPart1]),
    ResultPart2 = solve_sets(Input),
    io:format("Part 2: The result is ~p~n", [ResultPart2]).


% +1, +1, +1 results in  3
% +1, +1, -2 results in  0
% -1, -2, -3 results in -6
solve([]) -> 0;
solve([H|T]) -> H + solve(T).

count([], _) -> 0;
count([H|T], H) -> 1 + count(T, H);
count([_|T], A) -> count(T, A).

solve_tail(A) -> solve_tail(A, 0, [0], A).
solve_tail([], Result, Results, Original) -> 
    solve_tail(Original, Result, Results, Original);
solve_tail([H|T], Result, Results, Original) ->
    case count(Results, Result) > 1 of
        true -> Result;
        false -> solve_tail(T, Result + H, [Result + H|Results], Original)
    end.

solve_gb([H|T] = A) -> 
    solve_gb(T, H, gb_sets:add(0, gb_sets:new()), A).

solve_gb([], Result, Results, Original) -> 
    solve_gb(Original, Result, Results, Original);
solve_gb([H|T], Result, Results, Original) ->
    case gb_sets:is_member(Result, Results) of
        true -> Result;
        false ->
            solve_gb(T, Result + H, gb_sets:add(Result, Results), Original)
    end. 

solve_ord([H|T] = A) -> 
    solve_ord(T, H, ordsets:add_element(0, ordsets:new()), A).

solve_ord([], Result, Results, Original) -> 
    solve_ord(Original, Result, Results, Original);
solve_ord([H|T], Result, Results, Original) ->
    case ordsets:is_element(Result, Results) of
        true -> Result;
        false ->
            solve_ord(T, Result + H, ordsets:add_element(Result, Results), Original)
    end. 

solve_sets([H|T] = A) -> 
    solve_sets(T, H, sets:add_element(0, sets:new()), A).

solve_sets([], Result, Results, Original) -> 
    solve_sets(Original, Result, Results, Original);
solve_sets([H|T], Result, Results, Original) ->
    case sets:is_element(Result, Results) of
        true -> Result;
        false ->
            solve_sets(T, Result + H, sets:add_element(Result, Results), Original)
    end. 


test() ->
    assert(count([0,0,0], 0), 3),
    assert(count([1,1,0], 0), 1),
    assert(count([0,1,1], 0), 1),
    assert(solve([1, 1, 1]), 3),
    assert(solve([-1, -2, -3 ]), -6),
    assert(solve([1, 1, -2]), 0),
    assert(solve_sets([1, -1]), 0),
    assert(solve_sets([3, 3, 4, -2, -4 ]), 10),
    assert(solve_sets([-6, +3, +8, +5, -6]), 5),
    assert(solve_sets([+7, +7, -2, -7, -4]), 14).
