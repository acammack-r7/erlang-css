-module(css).

-export([parse/1, parse_inline_style/1]).
-export([serialize/1, serialize_inline_style/1]).

-spec parse(Input::binary()) -> Rules::list().
parse(Input) when is_binary(Input) ->
  {Toks, <<>>} = css_tokenizer:consume(Input, 500000),
  css_parser:parse_rules(Toks).

-spec parse_inline_style(Input::binary()) -> Declarations::list().
parse_inline_style(Input) when is_binary(Input) ->
  {Toks, <<>>} = css_tokenizer:consume(Input, 500000),
  css_parser:parse_declarations(Toks).

-spec serialize(Input::list()) -> Declarations::binary().
serialize(Input) when is_list(Input) ->
  css_serializer:serialize_rules(Input).

-spec serialize_inline_style(Input::list()) -> Declarations::binary().
serialize_inline_style(Input) ->
  css_serializer:serialize_declarations(Input).
