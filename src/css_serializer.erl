-module(css_serializer).

-export([serialize/1]).

-include("characters.hrl").

-spec serialize(Input) -> CSS::binary() when Input::atom() | tuple() | list().
serialize([]) -> <<>>;
serialize([ws | Rest]) ->
  <<" ", (serialize(Rest))/binary>>;
serialize([Single]) ->
  serialize(Single);
serialize([Single, Next | Rest]) ->
  <<(serialize(Single))/binary, (space(Single, Next))/binary, (serialize([Next | Rest]))/binary>>;
serialize({rule, Selector, Prop}) ->
  <<(serialize(Selector))/binary, ${, (serialize(Prop))/binary, $}>>;
serialize({at_rule, Name, Selector, []}) ->
  <<$@, (css_escape(Name))/binary, " ", (serialize(Selector))/binary, $;>>;
serialize({at_rule, Name, Selector, Prop}) ->
  <<$@, (css_escape(Name))/binary, " ", (serialize(Selector))/binary, (serialize(Prop))/binary>>;
serialize({{decl, important}, Prop, Val}) ->
  <<(css_escape(Prop))/binary, $:, (serialize(Val))/binary, " !important;">>;
serialize({{decl, _Important}, Prop, Val}) ->
  <<(css_escape(Prop))/binary, $:, (serialize(Val))/binary, $;>>;
serialize(Token) when is_tuple(Token) ->
  case Token of
    {ident, Ident} -> <<(css_escape(Ident))/binary>>;
    {string, String} -> <<$",(css_escape(String))/binary, $">>;
    {function, Ident} -> <<(css_escape(Ident))/binary, $(>>;
    {function, Ident, Body} -> <<(css_escape(Ident))/binary, $(, (serialize(Body))/binary, $)>>;
    {url, URL} -> <<"url(", (css_escape(URL))/binary, $)>>;
    {{hash, id}, Ident} -> <<$#, (css_escape(Ident))/binary>>;
    {{hash, unrestricted}, Val} -> <<$#, (css_escape(Val, unrestricted))/binary>>;
    {{dimension, _Integer}, {_, Number, Ident}} -> <<Number/binary, (css_escape(Ident))/binary>>;
    {{number, _Integer}, {_, Number}} -> <<Number/binary>>;
    {'an+b', A, B} when B >= 0 -> <<(integer_to_binary(A))/binary, "n+", (integer_to_binary(B))/binary>>;
    {'an+b', A, B} -> <<(integer_to_binary(A))/binary, $n, (integer_to_binary(B))/binary>>;
    {delim, Char} -> <<Char/utf8>>;
    {{block, '{'}, Body} -> <<${, (serialize(Body))/binary, $}>>;
    {{block, '['}, Body} -> <<$[, (serialize(Body))/binary, $]>>;
    {{block, '('}, Body} -> <<$(, (serialize(Body))/binary, $)>>;
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
    '||' -> <<"||">>;
    '<!--' -> <<"<!--">>;
    '-->' -> <<"-->">>;
    bad_string -> <<$", $">>;
    bad_url -> <<"url()">>
  end.

-spec css_escape(Name::binary()) -> Escaped::binary().
css_escape(Unescaped) ->
  css_escape(Unescaped, name).

-spec css_escape(Part::binary(), Type::atom()) -> Escaped::binary().
css_escape(Unescaped, Type) ->
  << <<(css_escape_char(Char, Type))/binary>> || <<Char/utf8>> <= Unescaped >>.

-spec css_escape_char(Character::integer(), Type::atom()) -> Escaped::binary().
% Characters that always start an identifier never have to be escaped
css_escape_char(Char, _Type) when ?name_start(Char) ->
  <<Char/utf8>>;
% Unrestricted hashes need less escaping
css_escape_char(Char, unrestricted) when ?is_name(Char) ->
  <<Char/utf8>>;
% Escape the rest
css_escape_char(Char, _Type) ->
  <<$\\, (integer_to_binary(Char, 16))/binary, 32>>.

-spec serialize_unicode_range(Begin::integer(), End::integer()) -> Range::binary().
serialize_unicode_range(Start, Start) ->
  <<$U, $+, (integer_to_binary(Start, 16))/binary>>;
serialize_unicode_range(Start, End) ->
  <<$U, $+, (integer_to_binary(Start, 16))/binary, $-, (integer_to_binary(End, 16))/binary>>.

-spec space(atom() | tuple(), atom() | tuple()) -> binary().
space({ident, _}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    Next =:= {delim, $-};
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    element(1, Next) =:= unicode_range;
    Next =:= '-->';
    Next =:= '(';
    element(1, Next) =:= {block, '('}
    -> <<" ">>;

space({'@', _}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    Next =:= {delim, $-};
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    element(1, Next) =:= unicode_range;
    Next =:= '-->'
    -> <<" ">>;

space({{hash, _}, _}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    Next =:= {delim, $-};
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    element(1, Next) =:= unicode_range;
    Next =:= '-->'
    -> <<" ">>;

space({{dimension, _}, _}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    Next =:= {delim, $-};
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    element(1, Next) =:= unicode_range;
    Next =:= '-->'
    -> <<" ">>;

space({delim, $#}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    Next =:= {delim, $-};
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    element(1, Next) =:= unicode_range
    -> <<" ">>;

space({delim, $-}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    element(1, Next) =:= unicode_range
    -> <<" ">>;

space({{number, _}, _}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    element(1, Next) =:= unicode_range
    -> <<" ">>;

space({delim, $@}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, Next) =:= url;
    Next =:= bad_url;
    Next =:= {delim, $-};
    element(1, Next) =:= unicode_range
    -> <<" ">>;

space({unicode_range, _, _}, Next) when
    element(1, Next) =:= ident;
    element(1, Next) =:= function;
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%';
    Next =:= '-->';
    Next =:= {delim, $?}
    -> <<" ">>;

space({delim, $.}, Next) when
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%'
    -> <<" ">>;

space({delim, $+}, Next) when
    element(1, element(1, Next)) =:= number;
    element(1, element(1, Next)) =:= dimension;
    element(1, Next) =:= '%'
    -> <<" ">>;

space({delim, $$}, {delim, $=}) -> <<" ">>;
space({delim, $*}, {delim, $=}) -> <<" ">>;
space({delim, $^}, {delim, $=}) -> <<" ">>;
space({delim, $~}, {delim, $=}) -> <<" ">>;
space({delim, $|}, {delim, $=}) -> <<" ">>;
space({delim, $|}, {delim, $|}) -> <<" ">>;
space({delim, $/}, {delim, $*}) -> <<" ">>;

space({delim, $\\}, _) -> <<"\n">>;

space(_, _) -> <<>>.
