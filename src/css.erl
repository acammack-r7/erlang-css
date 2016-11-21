-module(css).

-export([parse/1, parse_inline_style/1]).

-spec parse(Input::binary()) -> Rules::list().
parse(Input) when is_binary(Input) ->
  {Toks, <<>>} = css_tokenizer:consume(Input, 500000),
  css_parser:parse_rules(Toks).

-spec parse_inline_style(Input::binary()) -> Declarations::list().
parse_inline_style(Input) when is_binary(Input) ->
  {Toks, <<>>} = css_tokenizer:consume(Input, 500000),
  css_parser:parse_declarations(Toks).
