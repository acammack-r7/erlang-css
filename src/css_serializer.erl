-module(css_serializer).

-export([serialize/1]).

-spec serialize(Input) -> CSS::binary() when Input::list() | atom() | tuple(). 
serialize(Input) when is_list(Input) ->
  lists:foldl(fun(Single, Full) ->
    <<Full/binary, (serialize(Single))/binary>>
  end, <<>>, Input);
serialize({rule, Selector, Prop}) ->
  <<(serialize(Selector))/binary, ${, (serialize(Prop))/binary, $}>>;
serialize({at_rule, Name, Selector, Prop}) ->
  <<$@, Name/binary, " ", (serialize(Selector))/binary, (serialize(Prop))/binary>>;
serialize({{decl, important}, Prop, Val}) ->
  Value = lists:foldl(fun(Single, Full) ->
      <<Full/binary, (serialize(Single))/binary>>
    end, <<>>, Val),
  <<Prop/binary, $:, Value/binary, " !important;">>;
serialize({{decl, _Important}, Prop, Val}) ->
  Value = lists:foldl(fun(Single, Full) ->
      <<Full/binary, (serialize(Single))/binary>>
    end, <<>>, Val),
  <<Prop/binary, $:, Value/binary, $;>>;
serialize(Token) when is_tuple(Token) ->
  case Token of
    {ident, Ident} -> <<(css_escape_all(Ident))/binary, " ">>;
    {string, Ident} -> <<$",(css_escape_all(Ident))/binary, $", " ">>;
    {function, Ident} -> <<(css_escape_all(Ident))/binary, $(, " ">>;
    {function, Ident, Props} -> <<(css_escape_all(Ident))/binary, $(, (serialize(Props))/binary, $), " ">>;
    {url, Ident} -> <<"url(", (css_escape_all(Ident))/binary, $), " ">>;
    {{hash, id}, Ident} -> <<$#, (css_escape_all(Ident))/binary, " ">>;
    {{hash, unrestricted}, Ident} -> <<$#, (binary:replace(Ident, <<"\t">>, <<"\\\t">>))/binary, " ">>;
    {{dimension, integer}, {_, Number, Ident}} -> <<Number/binary, (css_escape_all(Ident))/binary, " ">>;
    {{dimension, number}, {_, Number, Ident}} -> <<Number/binary, (css_escape_all(Ident))/binary, " ">>;
    {{number, integer}, {_, Number}} -> <<Number/binary, " ">>;
    {{number, number}, {_, Number}} -> <<Number/binary, " ">>;
    {delim, Ident} -> <<Ident, " ">>;
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
  end;
serialize([]) ->
  <<$;>>.

-spec css_escape_all(Input::binary()) -> Declarations::binary().
css_escape_all(Unescaped) ->
  << <<(css_escape(Char))/binary>> || <<Char/utf8>> <= Unescaped >>.

-spec css_escape(Input::binary()) -> Declarations::binary().
css_escape(Char) ->
  <<$\\, (integer_to_binary(Char, 16))/binary, 32>>.

-spec serialize_unicode_range(Input::integer(), Input::integer()) -> Declaraions::binary().
serialize_unicode_range(Start, Start) ->
  <<$U, $+, (integer_to_binary(Start, 16))/binary>>;
serialize_unicode_range(Start, End) ->
  <<$U, $+, (integer_to_binary(Start, 16))/binary, $-, (integer_to_binary(End, 16))/binary>>.
