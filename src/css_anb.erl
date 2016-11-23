-module(css_anb).

-export([parse/1]).

%%% From CSS Syntax Module Level 3:
%%% > The An+B notation was originally defined using a slightly different
%%% > tokenizer than the rest of CSS, resulting in a somewhat odd definition
%%% > when expressed in terms of CSS tokens.

-define(is_n(N),(N =:= <<"n">> orelse N =:= <<"N">>)).
-define(is_n_i(N),(N =:= $n orelse N =:= $N)).
-define(is_sign(S),(S =:= $+ orelse S =:= $-)).
-define(is_digit(C), (C >= $0 andalso C =< $9)).

-spec parse(Tokens::list()) -> {'an+b', A::integer(), B::integer()} | undefined.
% Keywords
parse([{ident, <<"odd">>}]) -> {'an+b', 2, 1};
parse([{ident, <<"even">>}]) -> {'an+b', 2, 0};

% B only
parse([{{number, integer}, {Val, _Str}}]) -> {'an+b', 0, trunc(Val)};

% A only
parse([{{dimension, integer}, {Val, _Str, N}}]) when ?is_n(N) ->
  {'an+b', trunc(Val), 0};
parse([{ident, N}]) when ?is_n(N) -> {'an+b', 1, 0};
parse([{delim, $+}, {ident, N}]) when ?is_n(N) -> {'an+b', 1, 0};
parse([{ident, <<$-, N/binary>>}]) when ?is_n(N) -> {'an+b', -1, 0};

% No spaces (subtraction)
% Unit names break on $+, so we only have to parse out B in subtraction
parse([{{dimension, integer}, {A_Val, _Str, <<N/utf8, $-, B/binary>>}}]) when ?is_n_i(N) ->
  build(A_Val, $-, B);
parse([{ident, <<N/utf8, $-, B/binary>>}]) when ?is_n_i(N) ->
  build(1, $-, B);
parse([{delim, $+}, {ident, <<N/utf8, $-, B/binary>>}]) when ?is_n_i(N) ->
  build(1, $-, B);
parse([{ident, <<$-, N/utf8, $-, B/binary>>}]) when ?is_n_i(N) ->
  build(-1, $-, B);

% No spaces (addition) and
% Space after N, but not before B
parse([{{dimension, integer}, {A_Val, _Str, N}}, {{number, integer}, {B_Val, <<B_Start/utf8, _/binary>>}}])
  when ?is_n(N) andalso ?is_sign(B_Start) ->
  {'an+b', trunc(A_Val), trunc(B_Val)};
parse([{ident, N}, {{number, integer}, {B_Val, <<B_Start/utf8, _/binary>>}}])
  when ?is_n(N) andalso ?is_sign(B_Start) ->
  {'an+b', 1, trunc(B_Val)};
parse([{delim, $+}, {ident, N}, {{number, integer}, {B_Val, <<B_Start/utf8, _/binary>>}}])
  when ?is_n(N) andalso ?is_sign(B_Start) ->
  {'an+b', 1, trunc(B_Val)};
parse([{ident, <<$-, N/utf8>>}, {{number, integer}, {B_Val, <<B_Start/utf8, _/binary>>}}])
  when ?is_n_i(N) andalso ?is_sign(B_Start) ->
  {'an+b', -1, trunc(B_Val)};

% Space before B, but not after N (subtraction)
parse([{{dimension, integer}, {A_Val, _Str, <<N/utf8, $->>}}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n_i(N) andalso ?is_digit(B_Start) ->
  build(A_Val, $-, B);
parse([{ident, <<N/utf8, $->>}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n_i(N) andalso ?is_digit(B_Start) ->
  build(1, $-, B);
parse([{delim, $+}, {ident, <<N/utf8, $->>}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n_i(N) andalso ?is_digit(B_Start) ->
  build(1, $-, B);
parse([{ident, <<$-, N/utf8, $->>}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n_i(N) andalso ?is_digit(B_Start) ->
  build(-1, $-, B);

% Space before B, but not after N (addition) and
% Space after N and before B
parse([{{dimension, integer}, {A_Val, _Str, N}}, {delim, S}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n(N) andalso ?is_sign(S) andalso ?is_digit(B_Start) ->
  build(A_Val, S, B);
parse([{ident, N}, {delim, S}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n(N) andalso ?is_sign(S) andalso ?is_digit(B_Start) ->
  build(1, S, B);
parse([{delim, $+}, {ident, N}, {delim, S}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n(N) andalso ?is_sign(S) andalso ?is_digit(B_Start) ->
  build(1, S, B);
parse([{ident, <<$-, N/utf8>>}, {delim, S}, {{number, integer}, {B, <<B_Start/utf8, _/binary>>}}])
  when ?is_n_i(N) andalso ?is_sign(S) andalso ?is_digit(B_Start) ->
  build(-1, S, B);

parse(_Toks) -> undefined.

build(A_Val, B_Sign, B) when is_binary(B) ->
  S = case B_Sign of
    $+ -> 1;
    $- -> -1
  end,
  try binary_to_integer(B) of
    B_Val -> {'an+b', trunc(A_Val), S * trunc(B_Val)}
  catch
    error:badarg -> undefined
  end;
build(A_Val, B_Sign, B_Val) when is_number(B_Val) ->
  S = case B_Sign of
    $+ -> 1;
    $- -> -1
  end,
  {'an+b', trunc(A_Val), S * trunc(B_Val)}.
