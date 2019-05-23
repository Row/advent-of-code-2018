-module(day02a).
-export([init/0, test/0]).

assert(A, A) -> ok;
assert(A, B) -> throw(lists:flatten(io_lib:format("~p is not equal to ~p", [A, B]))).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

parse(FileName) ->
    lists:map(fun binary_to_list/1, readlines(FileName)).

init() ->
    Input = parse("day02a.txt"),
    ResultPart1 = solve(Input),
    io:format("Part 1: The result is ~p~n", [ResultPart1]),
    ResultPart2 = compare_all(Input),
    io:format("Part 2: The result is ~p~n", [ResultPart2]).

% abcdef contains no letters that appear exactly two or three times.
% bababc contains two a and three b, so it counts for both.
% abbcde contains two b, but no letter appears exactly three times.
% abcccd contains three c, but no letter appears exactly two times.
% aabcdd contains two a and two d, but it only counts once.
% abcdee contains two e.
% ababab contains three a and three b, but it only counts once.

to_int(false) -> 0;
to_int(true) -> 1.

summa([]) -> {0, 0};
summa([{A, B}|T]) -> 
    {R1, R2} = summa(T),
    {R1 + A, R2 + B}.

solve(L) -> 
    {A, B} = summa(lists:map(fun char_fr/1, L)),
    A * B.

char_fr(W) ->
    Results = sets:from_list(maps:values(char_frr(W))),
    {to_int(sets:is_element(2, Results)), to_int(sets:is_element(3, Results))}.

char_frr([]) -> maps:new();
char_frr([H|T]) -> 
    Results = char_frr(T),
    case maps:find(H, Results) of
        {ok, Value} -> maps:update(H, Value + 1, Results);
        error -> maps:put(H, 1, Results)
    end.

compare_word(S1, S2) -> 
    case compare_word(S1, S2, 0) of 
        true -> {S1, S2};
        false -> false
    end.

compare_word(_, _, I) when I > 1 -> false;
compare_word([H1|T1], [H1|T2], N) -> compare_word(T1, T2, N);
compare_word([_|T1], [_|T2], N) -> compare_word(T1, T2, N + 1);
compare_word([], [], 0) -> false;
compare_word([], [], 1) -> true.

equalize([H|T1], [H|T2], Acc) -> equalize(T1, T2, [H|Acc]);
equalize([_|T], _, Acc) -> lists:reverse(Acc)++T.

compare_all([H|T]) -> 
    case compare_to_rest(H,T) of
        false -> compare_all(T);
        {A, B} -> equalize(A, B, []) 
    end;
compare_all([]) -> false.

compare_to_rest(A, [H|T]) ->
     case compare_word(A,H) of
         false -> compare_to_rest(A, T);
         M -> M
    end;
compare_to_rest(_, []) -> false.

test() ->
    assert(char_fr("abcdef"), {0, 0}),
    assert(char_fr("bababc"), {1, 1}),
    assert(char_fr("abbcde"), {1, 0}),
    assert(char_fr("abcccd"), {0, 1}),
    assert(char_fr("aabcdd"), {1, 0}),
    assert(char_fr("abcdee"), {1, 0}),
    assert(char_fr("ababab"), {0, 1}),
    assert(summa([
        {1, 1},
        {1, 0},
        {0, 1}
    ]), {2, 2}),
    assert(solve([
        "abcdef",
        "bababc",
        "abcccd",
        "abbcde",
        "aabcdd",
        "abcdee",
        "ababab"
    ]), 12),
    assert(compare_word("abc", "fgh"), false),
    assert(compare_word("abc", "acf"), false),
    assert(compare_word("abc", "acc"), {"abc", "acc"}).