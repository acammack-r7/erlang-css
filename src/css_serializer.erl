-module(css_serializer).

-export([serialize/1]).

-include("characters.hrl").

-spec serialize(Input) -> CSS::binary() when Input::list() | atom() | tuple().
serialize(Input) when is_list(Input) ->
    << <<(serialize(Single))/binary, " ">> || Single <- Input >>;
serialize({rule, Selector, Prop}) ->
  <<(serialize(Selector))/binary, ${, (serialize(Prop))/binary, $}>>;
serialize({at_rule, Name, Selector, Prop}) ->
  <<$@, (css_escape(Name))/binary, " ", (serialize(Selector))/binary, (serialize(Prop))/binary>>;
serialize({{decl, important}, Prop, Val}) ->
  <<(css_escape(Prop))/binary, $:, (serialize(Val))/binary, " !important;">>;
serialize({{decl, _Important}, Prop, Val}) ->
  <<(css_escape(Prop))/binary, $:, (serialize(Val))/binary, $;>>;
serialize(Token) when is_tuple(Token) ->
  case Token of
    {ident, Ident} -> <<(css_escape(Ident))/binary>>;
    {string, Ident} -> <<$",(css_escape(Ident))/binary, $">>;
    {function, Ident} -> <<(css_escape(Ident))/binary, $(>>;
    {function, Ident, Props} -> <<(css_escape(Ident))/binary, $(, (serialize(Props))/binary, $)>>;
    {url, Ident} -> <<"url(", (css_escape(Ident))/binary, $)>>;
    {{hash, id}, Ident} -> <<$#, (css_escape(Ident))/binary>>;
    {{hash, unrestricted}, Ident} -> <<$#, (css_escape(Ident))/binary>>;
    {{dimension, integer}, {_, Number, Ident}} -> <<Number/binary, (css_escape(Ident))/binary>>;
    {{dimension, number}, {_, Number, Ident}} -> <<Number/binary, (css_escape(Ident))/binary>>;
    {{number, integer}, {_, Number}} -> <<Number/binary>>;
    {{number, number}, {_, Number}} -> <<Number/binary>>;
    {delim, Ident} -> <<Ident>>;
    {{block, '{'}, Ident} -> <<${, (serialize(Ident))/binary, $}>>;
    {{block, '['}, Ident} -> <<$[, (serialize(Ident))/binary, $]>>;
    {{block, '('}, Ident} -> <<$(, (serialize(Ident))/binary, $)>>;
    {unicode_range, Start, End} -> <<(serialize_unicode_range(Start, End))/binary>>;
    {'%', {_, Number}} -> <<Number/binary, $%>>;
    {'@', Ident} -> <<$@, Ident/binary>>
  end;
serialize(Token) when is_atom(Token) ->
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
  end.

-spec css_escape(Name::binary()) -> Escaped::binary().
css_escape(Unescaped) ->
  << <<(css_escape_char(Char))/binary>> || <<Char/utf8>> <= Unescaped >>.

-spec css_escape_char(Character::integer()) -> Escaped::binary().
% Characters that always start an identifier never have to be escaped
css_escape_char(Char) when ?name_start(Char) ->
  <<Char/utf8>>;
css_escape_char(Char) ->
  <<$\\, (integer_to_binary(Char, 16))/binary, 32>>.

-spec serialize_unicode_range(Input::integer(), Input::integer()) -> Declaraions::binary().
serialize_unicode_range(Start, Start) ->
  <<$U, $+, (integer_to_binary(Start, 16))/binary>>;
serialize_unicode_range(Start, End) ->
  <<$U, $+, (integer_to_binary(Start, 16))/binary, $-, (integer_to_binary(End, 16))/binary>>.
