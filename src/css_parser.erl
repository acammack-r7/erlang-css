-module(css_parser).

-export([parse_declarations/1, parse_rules/1]).

-spec parse_declarations([atom() | tuple]) -> list().

parse_rules(Tokens) ->
  lists:reverse(parse_rules(Tokens, [])).

parse_rules([], Rules) -> Rules;
parse_rules(['<!--' | Rem], Rules) -> parse_rules(Rem, Rules);
parse_rules(['-->' | Rem], Rules) -> parse_rules(Rem, Rules);
parse_rules([{'@', _Name} | _] = Tokens, Rules) ->
  {Rule, Rem} = parse_at_rule(Tokens),
  parse_rules(Rem, [Rule | Rules]);
parse_rules(Toks, Rules) ->
  case parse_rule(Toks) of
    {undefined, Rem} ->
      parse_rules(Rem, Rules);
    {Rule, Rem} ->
      parse_rules(Rem, [Rule | Rules])
  end.

parse_rule(Toks) ->
  case parse_rule(Toks, {[], []}) of
    {{Prelude, Block}, Rem} ->
      {{rule, lists:reverse(Prelude), parse_declarations(Block)}, Rem};
    {undefined, Rem} ->
      {undefined, Rem}
  end.

parse_rule([], _Rule) -> {undefined, []};
parse_rule([{{block, '{'}, Block} | Rem], {Prelude, _}) ->
  {{Prelude, Block}, Rem};
parse_rule(['{' | Toks], {Prelude, _}) ->
  {Block, Rem} = parse_block('{', Toks),
  {{Prelude, Block}, Rem};
parse_rule(Toks, {Prelude, Block}) ->
  {Tok, Rem} = parse_component(Toks),
  parse_rule(Rem, {[Tok | Prelude], Block}).

parse_declarations({{block, '{'}, Tokens}) when is_list(Tokens) ->
  Ws_Removed = [ Tok || Tok <- Tokens, Tok =/= ws ],
  lists:reverse(parse_declarations(Ws_Removed, []));
parse_declarations(Tokens) when is_list(Tokens) ->
  Ws_Removed = [ Tok || Tok <- Tokens, Tok =/= ws ],
  lists:reverse(parse_declarations(Ws_Removed, [])).

parse_declarations([], Decls) -> Decls;
parse_declarations([';' | Tokens], Decls) -> parse_declarations(Tokens, Decls);
parse_declarations([{ident, _Name} | _Rest] = Tokens, Decls) ->
  case parse_declaration(Tokens) of
    {undefined, Toks} ->
      parse_declarations(Toks, Decls);
    {Decl, Toks} ->
      parse_declarations(Toks, [Decl | Decls])
  end;
parse_declarations([{'@', _Name} | _Rest] = Tokens, Decls) ->
  {Decl, Toks} = parse_at_rule(Tokens),
  parse_declarations(Toks, [Decl | Decls]);
parse_declarations(Tokens, Decls) ->
  parse_declarations(seek(';', Tokens), Decls).

parse_at_rule([{'@', Name} | Rule]) ->
  {{Prelude, Block}, Rem} = parse_at_rule(Rule, {[], []}),
  {{at_rule, ascii_lower(Name), lists:reverse(Prelude), Block}, Rem}.

parse_at_rule([], Rule) -> {Rule, []};
parse_at_rule([';' | Rem], Rule) -> {Rule, Rem};
parse_at_rule(['{' | Rem], {Prelude, _}) ->
  {Block, Rest} = parse_block('{', Rem),
  {{Prelude, Block}, Rest};
parse_at_rule([{{block, '{'}, _B} = Block | Rem], {Prelude, _}) ->
  {{Prelude, Block}, Rem};
parse_at_rule(Toks, {Prelude, Block}) ->
  {Tok, Rem} = parse_component(Toks),
  parse_at_rule(Rem, {[Tok | Prelude], Block}).

parse_declaration([{ident, Name}, ':' | Toks]) ->
  {Decl, Rest} = lists:splitwith(fun in_decl/1, Toks),
  {parse_declaration(Decl, {decl, ascii_lower(Name)}), Rest};
parse_declaration([{ident, _Name} | Toks]) ->
  {undefined, seek(';', Toks)}.

parse_declaration(Decl, {decl, Name}) ->
  case is_important(Decl) of
    {true, Rem} ->
      {{decl, important}, Name, Rem};
    false ->
      {{decl, normal}, Name, Decl}
  end.

-spec parse_component(list()) -> {term(), list()}.
parse_component([Bracket | Toks])
  when Bracket =:= '{' orelse Bracket =:= '[' orelse Bracket =:= '(' ->
  parse_block(Bracket, Toks);
parse_component([{function, _Name} | _] = Toks) ->
  parse_function(Toks);
parse_component([Tok | Rem]) ->
  {Tok, Rem}.

parse_block('{', Toks) ->
  {Block, Rem} = parse_block('}', Toks, []),
  {{{block, '{'}, Block}, Rem};
parse_block('[', Toks) ->
  {Block, Rem} = parse_block(']', Toks, []),
  {{{block, '['}, Block}, Rem};
parse_block('(', Toks) ->
  {Block, Rem} = parse_block(')', Toks, []),
  {{{block, '('}, Block}, Rem}.

parse_block(_Delim, [], Block) -> {lists:reverse(Block), []};
parse_block(Delim, [Delim | Rem], Block) -> {lists:reverse(Block), Rem};
parse_block(Delim, Toks, Block) ->
  {Tok, Rem} = parse_component(Toks),
  parse_block(Delim, Rem, [Tok | Block]).

parse_function([{function, Name} | Toks]) ->
  {Body, Rem} = parse_function(Toks, []),
  {{function, ascii_lower(Name), lists:reverse(Body)}, Rem}.

parse_function([], Body) -> {Body, []};
parse_function([')' | Rem], Body) -> {Body, Rem};
parse_function(Toks, Body) ->
  {Tok, Rem} = parse_component(Toks),
  parse_function(Rem, [Tok | Body]).

is_important(Toks) ->
  case lists:reverse(Toks) of
    % TODO actually check
    [{ident, <<I/utf8, _:8/bytes>>}, {delim, $!} | Rest] when I =:= $i orelse I =:= $I ->
      % TODO: make more efficient
      {true, lists:reverse(Rest)};
    _ ->
      false
  end.

seek(_E, []) -> [];
seek(E, [E | Rest]) -> Rest;
seek(E, [_E | Rest]) -> seek(E, Rest).

in_decl(';') -> false;
in_decl(_) -> true.

ascii_lower(Bin) -> ascii_lower(Bin, <<>>).

ascii_lower(<<>>, Acc) -> Acc;
ascii_lower(<<C/utf8, Rest/binary>>, Acc) when C >= $A andalso C =< $Z ->
  ascii_lower(Rest, <<Acc/binary, (C + 32)/utf8>>);
ascii_lower(<<C/utf8, Rest/binary>>, Acc) ->
  ascii_lower(Rest, <<Acc/binary, C/utf8>>).
