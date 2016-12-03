%%% Test macros for character groups, as defined by CSS Syntax Module Level 3

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
