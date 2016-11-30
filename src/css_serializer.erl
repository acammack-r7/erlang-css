-module(css_serializer).

-export([serialize_rules/1, serialize_declarations/1]).

-spec serialize_rules(Input::list()) -> Declarations::binary().
serialize_rules(Input) when is_list(Input) ->
  lists:foldl(fun(Single, Full) ->
    <<Full/binary, (serialize_rule(Single))/binary>>
  end, <<>>, Input).

-spec serialize_rule(Input::tuple()) -> Declarations::binary().
serialize_rule(Input) when is_tuple(Input) ->
  case Input of
    {rule, Selector, Prop} -> <<(serialize_tokens(Selector))/binary, "{", (serialize_declarations(Prop))/binary, "}">>;
    {at_rule, Name, Selector, Prop} -> <<"@", Name/binary, " ", (serialize_tokens(Selector))/binary, (serialize_token(Prop))/binary>>
  end.

-spec serialize_declarations(Input::list()) -> Declarations::binary().
serialize_declarations(Input) when is_list(Input) ->
  lists:foldl (fun(Single, Full) ->
      <<Full/binary, (serialize_declaration(Single))/binary, ";">>
    end, <<>>, Input).

-spec serialize_declaration(Input::tuple()) -> Declarations::binary().
serialize_declaration({{decl, Decl}, Prop, Val} = Input) when is_tuple(Input) ->
  Value = lists:foldl(fun(Single, Full) ->
      case Single of
        P -> <<Full/binary, (serialize_token(P))/binary>>
      end
    end, <<>>, Val),
  case Decl of
    important -> <<Prop/binary, ":", Value/binary, " !important">>;
    _ -> <<Prop/binary, ":", Value/binary>>
  end.

-spec serialize_tokens(list() | tuple() | atom()) -> Declerations::binary().
serialize_tokens(Tokens) ->
  lists:foldl(fun(Single, Full) ->
    <<Full/binary, (serialize_token(Single))/binary>>
  end, <<>>, Tokens).

-spec serialize_token(Input::tuple()) -> Declarations::binary().
serialize_token(Token) when is_tuple(Token) ->
  case Token of
    {ident, Ident} -> <<(css_escape_all(Ident))/binary, " ">>;
    {string, Ident} -> <<$",(css_escape_all(Ident))/binary, $", " ">>;
    {function, Ident} -> <<(css_escape_all(Ident))/binary, "(", " ">>;
    {function, Ident, Props} -> <<(css_escape_all(Ident))/binary, "(", (serialize_tokens(Props))/binary, ")", " ">>;
    {url, Ident} -> <<"url(", (css_escape_all(Ident))/binary, ")", " ">>;
    {{hash, id}, Ident} -> <<"#", (css_escape_all(Ident))/binary, " ">>;
    {{hash, unrestricted}, Ident} -> <<"#", (binary:replace(Ident, <<"\t">>, <<"\\\t">>))/binary, " ">>;
    {{dimension, integer}, {_, Number, Ident}} -> <<Number/binary, (css_escape_all(Ident))/binary, " ">>;
    {{dimension, number}, {_, Number, Ident}} -> <<Number/binary, (css_escape_all(Ident))/binary, " ">>;
    {{number, integer}, {_, Number}} -> <<Number/binary, " ">>;
    {{number, number}, {_, Number}} -> <<Number/binary, " ">>;
    {delim, Ident} -> <<Ident, " ">>;
    {{block, '{'}, Ident} -> <<"{", (serialize_tokens(Ident))/binary, "}">>;
    {{block, '['}, Ident} -> <<"[", (serialize_tokens(Ident))/binary, "]">>;
    {{block, '('}, Ident} -> <<"(", (serialize_tokens(Ident))/binary, ")">>;
    {unicode_range, Start, End} -> <<(serialize_unicode_range(Start, End))/binary>>;
    {'%', {_, Number}} -> <<Number/binary, "%">>;
    {'@', Ident} -> <<"@", Ident/binary>>
  end;
serialize_token(Token) when is_atom(Token) ->
  case Token of
    ')' -> <<")">>;
    '(' -> <<"(">>;
    ',' -> <<",">>;
    '[' -> <<"[">>;
    ']' -> <<"]">>;
    '{' -> <<"{">>;
    '}' -> <<"}">>;
    ':' -> <<":">>;
    ';' -> <<";">>;
    '$=' -> <<"$=">>;
    '*=' -> <<"*=">>;
    '^=' -> <<"^=">>;
    '~=' -> <<"~=">>;
    '|=' -> <<"|=">>;
    '||' -> <<"||">>
  end;
serialize_token(Token) when is_list(Token) ->
  <<";">>.

-spec css_escape_all(Input::binary()) -> Declarations::binary().
css_escape_all(Unescaped) ->
  << <<(css_escape(Char))/binary>> || <<Char/utf8>> <= Unescaped >>.

-spec css_escape(Input::binary()) -> Declarations::binary().
css_escape(Char) ->
  <<$\\, (integer_to_binary(Char, 16))/binary, 32>>.

-spec serialize_unicode_range(Input::integer(), Input::integer()) -> Declaraions::binary().
serialize_unicode_range(Start, End) ->
  case Start =:= End of
    true -> <<$U, $+, (integer_to_binary(Start, 16))/binary>>;
    false -> <<$U, $+, (integer_to_binary(Start, 16))/binary, $-, (integer_to_binary(End, 16))/binary>>
  end.

