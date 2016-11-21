-module(css_tokenizer).

-export([consume/2]).

-define(is_nl(C), (C =:= 16#0A orelse C =:= 16#0D orelse C =:= 16#0C)).
-define(is_ws(C), (?is_nl(C) orelse C =:= 16#09 orelse C =:= 16#20)).
-define(is_digit(C), (C >= $0 andalso C =< $9)).
-define(is_hex(C), (?is_digit(C) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).
-define(is_upper(C), (C >= $A andalso C =< $Z)).
-define(is_lower(C), (C >= $a andalso C =< $z)).
-define(is_letter(C), (?is_upper(C) orelse ?is_lower(C))).
-define(is_quot(C), (C =:= $" orelse C =:= $')).
-define(non_ascii(C), (C >= 16#80)).
-define(name_start(C), (?is_letter(C) orelse ?non_ascii(C) orelse C =:= $_)).
-define(is_name(C), (?name_start(C) orelse ?is_digit(C) orelse C =:= $-)).
-define(non_print(C), ((C > 0 andalso C < 16#08) orelse C =:= 16#0B orelse (C > 16#0E andalso C =< 16#1F) orelse C =:= 16#7F)).

-type css_token() :: atom() | tuple().

-spec consume(Input::binary(), Num_Tokens::non_neg_integer()) -> {Tokens::list(), Remaining::binary()}.
consume(Input, N) ->
  consume(Input, N, []).

-spec consume(Input::binary(), Num_Tokens::non_neg_integer(), Acc::list()) -> {Tokens::list(), Remaining::binary()}.
consume(Input, 0, Acc) ->
  {lists:reverse(Acc), Input};
consume(Input, N, Acc) ->
  case consume_token(Input) of
    {Tok, Rem} ->
      consume(Rem, N - 1, [Tok | Acc]);
    eof ->
      consume(Input, 0, Acc)
  end.

-spec consume_token(Input::binary()) -> {Token::css_token(), Remaining::binary()} | eof.
consume_token(<<>>) -> eof;

consume_token(<<C/utf8, Rem/binary>>) when ?is_quot(C) -> consume_string(C, Rem, <<>>);

consume_token(<<$#, C/utf8, Rem/binary>>) when ?is_name(C)->
  consume_hash(<<C/utf8, Rem/binary>>);
consume_token(<<$#, $\\, C/utf8, Rem/binary>>) when not ?is_nl(C)->
  consume_hash(<<$\\, C/utf8, Rem/binary>>);

consume_token(<<"$=", Rem/binary>>) -> {'$=', Rem};
consume_token(<<"*=", Rem/binary>>) -> {'*=', Rem};
consume_token(<<"^=", Rem/binary>>) -> {'^=', Rem};
consume_token(<<"~=", Rem/binary>>) -> {'~=', Rem};
consume_token(<<"|=", Rem/binary>>) -> {'|=', Rem};
consume_token(<<"||", Rem/binary>>) -> {'||', Rem};

consume_token(<<$(, Rem/binary>>) -> {'(', Rem};
consume_token(<<$), Rem/binary>>) -> {')', Rem};
consume_token(<<$[, Rem/binary>>) -> {'[', Rem};
consume_token(<<$], Rem/binary>>) -> {']', Rem};
consume_token(<<${, Rem/binary>>) -> {'{', Rem};
consume_token(<<$}, Rem/binary>>) -> {'}', Rem};
consume_token(<<$,, Rem/binary>>) -> {',', Rem};
consume_token(<<$:, Rem/binary>>) -> {':', Rem};
consume_token(<<$;, Rem/binary>>) -> {';', Rem};

consume_token(<<"<!--", Rem/binary>>) -> {'<!--', Rem};
consume_token(<<"-->", Rem/binary>>) -> {'-->', Rem};
consume_token(<<"/*", Rem/binary>>) ->
  Rem2 = skip_comment(Rem),
  consume_token(Rem2);

consume_token(<<C/utf8, $+, V/utf8, Rem/binary>>)
  when (C =:= $u orelse C =:= $U) andalso (?is_hex(V) orelse V =:= $?) ->
  consume_unicode_range(<<V/utf8, Rem/binary>>);

consume_token(<<$\\, C/utf8, _Rem/binary>> = B) when not ?is_nl(C) -> consume_identish(B);
consume_token(<<C/utf8, _Rem/binary>> = B) when ?name_start(C) -> consume_identish(B);
consume_token(<<C/utf8, _Rem/binary>> = B) when ?is_digit(C) -> consume_number(B);
% XXX Ignores whitespace. This goes against the spec, but a parser that does
% this is indistinguishable from one that doesn't.
% consume_token(<<C/utf8, Rem/binary>>) when ?is_ws(C) -> {ws, skip_ws(Rem)};
consume_token(<<C/utf8, Rem/binary>>) when ?is_ws(C) -> consume_token(skip_ws(Rem));

consume_token(<<C/utf8, Rem/binary>> = B) when C =:= $+ orelse C =:= $. ->
  case start_number(B) of
    true -> consume_number(B);
    false -> {{delim, C}, Rem}
  end;
consume_token(<<$-, Rem/binary>> = B) ->
  case start_number(B) of
    true -> consume_number(B);
    false ->
      case start_identifier(B) of
        true -> consume_identish(B);
        false -> {{delim, $-}, Rem}
      end
  end;
consume_token(<<$@, Rem/binary>>) ->
  case start_identifier(Rem) of
    true ->
      {Name, Rem2} = consume_name(Rem, <<>>),
      {{'@', Name}, Rem2};
    false -> {{delim, $@}, Rem}
  end;

consume_token(<<C/utf8, Rem/binary>>) ->
  {{delim, C}, Rem}.

-spec trim_ws(Input::binary()) -> Remaining::binary().
trim_ws(<<C/utf8, Rem/binary>>) when ?is_ws(C) -> Rem;
trim_ws(Rem) -> Rem.

-spec skip_ws(Input::binary()) -> Remaining::binary().
skip_ws(<<>>) -> <<>>;
skip_ws(<<C/utf8, Rem/binary>>) when ?is_ws(C) ->
  skip_ws(Rem);
skip_ws(Rem) -> Rem.

-spec skip_comment(Input::binary()) -> Remaining::binary().
skip_comment(<<>>) -> <<>>;
skip_comment(<<$*, $/, Rem/binary>>) -> Rem;
skip_comment(<<_C/utf8, Rem/binary>>) -> skip_comment(Rem).

-spec skip_bad_url(Input::binary()) -> Remaining::binary().
skip_bad_url(<<>>) -> <<>>;
skip_bad_url(<<$), Rem/binary>>) -> Rem;
skip_bad_url(<<$\\, C/utf8, Rem/binary>>) when not ?is_nl(C) ->
  {_Char, Rem2} = consume_escape(<<C/utf8, Rem/binary>>),
  skip_bad_url(Rem2);
skip_bad_url(<<_C/utf8, Rem/binary>>) -> skip_bad_url(Rem).

-spec start_identifier(Input::binary()) -> Starts_Identifier::boolean().
start_identifier(<<C/utf8, _Rem/binary>>) when ?name_start(C) -> true;
start_identifier(<<$-, C/utf8, _Rem/binary>>) when ?name_start(C) -> true;
start_identifier(<<$-, $\\, C/utf8, _Rem/binary>>) when not ?is_nl(C) -> true;
start_identifier(<<$\\, C/utf8, _Rem/binary>>) when not ?is_nl(C) -> true;
start_identifier(_) -> false.

-spec start_number(Input::binary()) -> Starts_Number::boolean().
start_number(<<$+, $., C/utf8, _Rem/binary>>) when ?is_digit(C) -> true;
start_number(<<$-, $., C/utf8, _Rem/binary>>) when ?is_digit(C) -> true;
start_number(<<$+, C/utf8, _Rem/binary>>) when ?is_digit(C) -> true;
start_number(<<$-, C/utf8, _Rem/binary>>) when ?is_digit(C) -> true;
start_number(<<$., C/utf8, _Rem/binary>>) when ?is_digit(C) -> true;
start_number(<<$., C/utf8, _Rem/binary>>) when ?is_digit(C) -> true;
start_number(<<C/utf8, _Rem/binary>>) when ?is_digit(C) -> true;
start_number(_) -> false.

-spec maybe_replace(Char::non_neg_integer()) -> Replacement::pos_integer().
maybe_replace(C) when C =:= 0 orelse (C > 16#D800 andalso C < 16#DFFF) orelse C > 16#10FFFF ->
  16#FFFD;
maybe_replace(C) -> C.

-spec count_n_xs(Remaining, X::non_neg_integer(), Count, 0) -> {Count, Remaining} when Count::non_neg_integer(), Remaining::binary();
                (Input::binary(), X::non_neg_integer(), Xs::non_neg_integer(), N::pos_integer()) -> {Counted::non_neg_integer(), Remaining::binary()}.
count_n_xs(<<>>, _X, Xs, _N) -> {Xs, <<>>};
count_n_xs(Rem, _X, Xs, 0) -> {Xs, Rem};
count_n_xs(<<X/utf8, Rem/binary>>, X, Xs, N) ->
  count_n_xs(Rem, X, Xs + 1, N - 1);
count_n_xs(Rem, _X, Xs, _N) -> {Xs, Rem}.

-spec consume_unicode_range(Input::binary()) -> {{unicode_range, non_neg_integer(), non_neg_integer()}, binary()}.
consume_unicode_range(Input) ->
  case consume_n_hex(Input, <<>>, 6) of
    {Digits, <<$?, Rem/binary>>} when byte_size(Digits) < 6 ->
      {Qs, Rem2} = count_n_xs(Rem, $?, 0, 6 - byte_size(Digits)),
      Start = binary_to_integer(<<Digits/binary, (binary:copy(<<$0>>, Qs))/binary>>, 16),
      End = binary_to_integer(<<Digits/binary, (binary:copy(<<$F>>, Qs))/binary>>, 16),
      {{unicode_range, Start, End}, Rem2};
    {Start, <<$-, C/utf8, Rem/binary>>} when ?is_hex(C) ->
      {End, Rem2} = consume_n_hex(Rem, <<>>, 6),
      {{unicode_range, binary_to_integer(Start, 16), binary_to_integer(End, 16)}, Rem2};
    {Digits, Rem} ->
      {{unicode_range, binary_to_integer(Digits, 16), binary_to_integer(Digits, 16)}, Rem}
  end.

-spec consume_n_hex(Remaining, Hex, 0) -> {Hex, Remaining} when Hex::binary(), Remaining::binary();
                 (Input::binary(), Hex::binary(), N::pos_integer()) -> {Consumed_Hex::binary(), Remaining::binary()}.
consume_n_hex(<<>>, Hex, _N) -> {Hex, <<>>};
consume_n_hex(Rem, Hex, 0) -> {Hex, Rem};
consume_n_hex(<<C/utf8, Rem/binary>>, Hex, N) when ?is_hex(C) ->
  consume_n_hex(Rem, <<Hex/binary, C/utf8>>, N - 1);
consume_n_hex(Rem, Hex, _N) -> {Hex, Rem}.

-spec consume_number(Input::binary()) ->
  {{'%', {Value::float(), Representation::binary()}}, Remaining::binary()} |
  {{{number, integer | number}, {Value::float(), Representation::binary()}}, Remaining::binary()} |
  {{{dimension, integer | number}, {Value::float(), Representation::binary(), Unit::binary()}}, Remaining::binary()}.
consume_number(Input) ->
  {Type, Parts, String, Rem} = consume_number_sign(Input, {integer, {1, 0, 0, {1, 0}}, <<>>}),
  Val = compute_number(Parts),
  case start_identifier(Rem) of
    true ->
      {Unit, Rem2} = consume_name(Rem, <<>>),
      {{{dimension, Type}, {Val, String, Unit}}, Rem2};
    false ->
      case Rem of
        <<$%, Rem2/binary>> ->
          {{'%', {Val, String}}, Rem2};
        _ ->
          {{{number, Type}, {Val, String}}, Rem}
      end
  end.

consume_number_sign(<<$+, Rem/binary>>, {Type, Val, String}) ->
  consume_number_integer(Rem, {Type, Val, <<String/binary, $+>>});
consume_number_sign(<<$-, Rem/binary>>, {Type, {_Sign, Integer, Fraction, Exponent}, String}) ->
  consume_number_integer(Rem, {Type, {-1, Integer, Fraction, Exponent}, <<String/binary, $->>});
consume_number_sign(Rem, {Type, Val, String}) ->
  consume_number_integer(Rem, {Type, Val, String}).

consume_number_integer(Input, {Type, {Sign, _Integer, Fraction, Exponent}, String}) ->
  {Val, Part, Rem} = consume_digits(Input, <<>>),
  consume_number_fraction(Rem, {Type, {Sign, Val, Fraction, Exponent}, <<String/binary, Part/binary>>}).

consume_number_fraction(<<$., C/utf8, Input/binary>>, {_Type, {Sign, Integer, _Fraction, Exponent}, String}) when ?is_digit(C) ->
  {Val, Part, Rem} = consume_digits(<<C/utf8, Input/binary>>, <<>>),
  consume_number_exponent(Rem, {number, {Sign, Integer, Val, Exponent}, <<String/binary, $., Part/binary>>});
consume_number_fraction(Rem, {Type, Val, String}) ->
  consume_number_exponent(Rem, {Type, Val, String}).

consume_number_exponent(<<E/utf8, C/utf8, Input/binary>>, {Type, {Sign, Integer, Fraction, {E_Sign, _Exponent}}, String})
  when (E =:= $e orelse E =:= $E) andalso ?is_digit(C) ->
  {Val, Part, Rem} = consume_digits(<<C/utf8, Input/binary>>, <<>>),
  {Type, {Sign, Integer, Fraction, {E_Sign, Val}}, <<String/binary, E/utf8, Part/binary>>, Rem};
consume_number_exponent(<<E/utf8, $+, C/utf8, Input/binary>>, {Type, {Sign, Integer, Fraction, {_E_Sign, _Exponent}}, String})
  when (E =:= $e orelse E =:= $E) andalso ?is_digit(C) ->
  {Val, Part, Rem} = consume_digits(<<C/utf8, Input/binary>>, <<>>),
  {Type, {Sign, Integer, Fraction, {1, Val}}, <<String/binary, E/utf8, $+, Part/binary>>, Rem};
consume_number_exponent(<<E/utf8, $-, C/utf8, Input/binary>>, {Type, {Sign, Integer, Fraction, {_E_Sign, _Exponent}}, String})
  when (E =:= $e orelse E =:= $E) andalso ?is_digit(C) ->
  {Val, Part, Rem} = consume_digits(<<C/utf8, Input/binary>>, <<>>),
  {Type, {Sign, Integer, Fraction, {-1, Val}}, <<String/binary, E/utf8, $-, Part/binary>>, Rem};
consume_number_exponent(Rem, {Type, Val, String}) ->
  {Type, Val, String, Rem}.

-spec consume_digits(Input::binary(), Number::binary()) -> {Value::non_neg_integer(), String::binary(), Remaining::binary()}.
consume_digits(<<C/utf8, Rem/binary>>, String) when ?is_digit(C) ->
  consume_digits(Rem, <<String/binary, C/utf8>>);
consume_digits(Rem, <<>>) ->
  {0, <<>>, Rem};
consume_digits(Rem, String) ->
  {binary_to_integer(String), String, Rem}.

-spec compute_number({-1 | 1, non_neg_integer(), non_neg_integer(), {-1 | 1, non_neg_integer()}}) -> float().
compute_number({Sign, Integer, Fraction, {E_Sign, Exponent}}) ->
  Sign * (Integer + make_fraction(Fraction)) * math:pow(10, E_Sign * Exponent).

-spec make_fraction(0) -> 0;
                   (pos_integer()) -> float().
make_fraction(0) -> 0;
make_fraction(Fraction) ->
  Fraction / math:pow(10, trunc(math:log10(Fraction)) + 1).

-spec consume_hash(Input::binary()) -> {{{hash, id | unrestricted}, Name::binary()}, Remaining::binary()}.
consume_hash(Input) ->
  Flag = case start_identifier(Input) of
    true -> id;
    false -> unrestricted
  end,
  {Name, Rem} = consume_name(Input, <<>>),
  {{{hash, Flag}, Name}, Rem}.

-spec consume_name(Input::binary(), Acc::binary()) -> {Name::binary(), Remaining::binary()}.
consume_name(<<C/utf8, Rem/binary>>, Name) when ?is_name(C) ->
  consume_name(Rem, <<Name/binary, C/utf8>>);
consume_name(<<$\\, C/utf8, Rem/binary>>, Name) when not ?is_nl(C) ->
  {Char, Rem2} = consume_escape(<<C/utf8, Rem/binary>>),
  consume_name(Rem2, <<Name/binary, Char/utf8>>);
consume_name(Rem, Name) ->
  {Name, Rem}.

-spec consume_identish(Input::binary()) ->
  {{function, Name::binary()}, Remaining::binary()} |
  {{ident, Name::binary()}, Remaining::binary()} |
  {{url, URL::binary()}, Remaining::binary()} |
  {bad_url, Remaining::binary()}.
consume_identish(Input) ->
  case consume_name(Input, <<>>) of
    {Name, <<$(, Rem/binary>>}
      when Name =:= <<"url">> orelse Name =:= <<"Url">> orelse Name =:= <<"uRl">> orelse
      Name =:= <<"urL">> orelse Name =:= <<"URl">> orelse Name =:= <<"UrL">> orelse
      Name =:= <<"uRL">> orelse Name =:= <<"URL">> ->
      consume_url(skip_ws(Rem), <<>>);
    {Name, <<$(, Rem/binary>>} ->
      {{function, Name}, Rem};
    {Name, Rem} ->
      {{ident, Name}, Rem}
  end.

-spec consume_url(Input::binary(), Acc::binary()) -> {{url, URL::binary()} | bad_url, Remaining::binary()}.
consume_url(<<>>, URL) ->
  {{url, URL}, <<>>};
consume_url(<<$), Rem/binary>>, URL) ->
  {{url, URL}, Rem};
consume_url(<<C/utf8, Rem/binary>>, URL) when ?is_ws(C) ->
  case skip_ws(Rem) of
    <<$), Rem2/binary>> ->
      {{url, URL}, Rem2};
    Rem2 ->
      {bad_url, skip_bad_url(Rem2)}
  end;
consume_url(<<C/utf8, Input/binary>>, <<>>) when ?is_quot(C) ->
  case consume_string(C, Input, <<>>) of
    {{string, String}, Rem} ->
      case skip_ws(Rem) of
        <<>> ->
          {{url, String}, <<>>};
        <<$), Rem2/binary>> ->
          {{url, String}, Rem2};
        Rem2 ->
          {bad_url, skip_bad_url(Rem2)}
      end;
    {bad_string, Rem} ->
      {bad_url, skip_bad_url(Rem)}
  end;
consume_url(<<C/utf8, Rem/binary>>, _URL) when ?non_print(C) orelse ?is_quot(C) orelse C =:= $( ->
  {bad_url, skip_bad_url(Rem)};
consume_url(<<$\\, C/utf8, Rem/binary>>, URL) when not ?is_nl(C) ->
  {Char, Rem2} = consume_escape(<<C/utf8, Rem/binary>>),
  consume_url(Rem2, <<URL/binary, Char/utf8>>);
consume_url(<<$\\, Rem/binary>>, _URL) ->
  {bad_url, skip_bad_url(Rem)};
consume_url(<<C/utf8, Rem/binary>>, URL) ->
  consume_url(Rem, <<URL/binary, C/utf8>>).

-spec consume_string(Delim::pos_integer(), Input::binary(), Acc::binary()) -> {{string, String::binary()} | bad_string, Remaining::binary()}.
consume_string(_Delim, <<>>, String) ->
  {{string, String}, <<>>};
consume_string(Delim, <<Delim/utf8, Rem/binary>>, String) ->
  {{string, String}, Rem};
consume_string(_Delim, <<C/utf8, _/binary>> = Rem, _String) when ?is_nl(C) ->
  {bad_string, Rem};
consume_string(Delim, <<$\\, 16#0D, 16#0A, Rem/binary>>, String) ->
  consume_string(Delim, Rem, String);
consume_string(Delim, <<$\\, C/utf8, Rem/binary>>, String) when ?is_nl(C) ->
  consume_string(Delim, Rem, String);
consume_string(Delim, <<$\\, Rem/binary>>, String) ->
  {Char, Rem2} = consume_escape(Rem),
  consume_string(Delim, Rem2, <<String/binary, Char/utf8>>);
consume_string(Delim, <<C/utf8, Rem/binary>>, String) ->
  consume_string(Delim, Rem, <<String/binary, C/utf8>>).

-spec consume_escape(Input::binary()) -> {pos_integer(), Remaining::binary()}.
consume_escape(<<>>) -> {16#FFFD, <<>>};
consume_escape(<<C0/utf8, C1/utf8, C2/utf8, C3/utf8, C4/utf8, C5/utf8, Rem/binary>>)
  when ?is_hex(C0) andalso ?is_hex(C1) andalso ?is_hex(C2) andalso ?is_hex(C3) andalso ?is_hex(C4) andalso ?is_hex(C5) ->
  {maybe_replace(binary_to_integer(<<C0/utf8, C1/utf8, C2/utf8, C3/utf8, C4/utf8, C5/utf8>>, 16)), trim_ws(Rem)};
consume_escape(<<C0/utf8, C1/utf8, C2/utf8, C3/utf8, C4/utf8, Rem/binary>>)
  when ?is_hex(C0) andalso ?is_hex(C1) andalso ?is_hex(C2) andalso ?is_hex(C3) andalso ?is_hex(C4) ->
  {maybe_replace(binary_to_integer(<<C0/utf8, C1/utf8, C2/utf8, C3/utf8, C4/utf8>>, 16)), trim_ws(Rem)};
consume_escape(<<C0/utf8, C1/utf8, C2/utf8, C3/utf8, Rem/binary>>)
  when ?is_hex(C0) andalso ?is_hex(C1) andalso ?is_hex(C2) andalso ?is_hex(C3) ->
  {maybe_replace(binary_to_integer(<<C0/utf8, C1/utf8, C2/utf8, C3/utf8>>, 16)), trim_ws(Rem)};
consume_escape(<<C0/utf8, C1/utf8, C2/utf8, Rem/binary>>)
  when ?is_hex(C0) andalso ?is_hex(C1) andalso ?is_hex(C2) ->
  {maybe_replace(binary_to_integer(<<C0/utf8, C1/utf8, C2/utf8>>, 16)), trim_ws(Rem)};
consume_escape(<<C0/utf8, C1/utf8, Rem/binary>>)
  when ?is_hex(C0) andalso ?is_hex(C1) ->
  {maybe_replace(binary_to_integer(<<C0/utf8, C1/utf8>>, 16)), trim_ws(Rem)};
consume_escape(<<C/utf8, Rem/binary>>)
  when ?is_hex(C) ->
  {maybe_replace(binary_to_integer(<<C/utf8>>, 16)), trim_ws(Rem)};
consume_escape(<<C/utf8, Rem/binary>>) -> {C, Rem}.
