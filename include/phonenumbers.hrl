%%%-------------------------------------------------------------------
%%% @author marinakr
%%% Created : 17. Apr 2018 13:03
%%%-------------------------------------------------------------------
-author("marinakr").

-define(FILE_PHONE_PHONE_FORMATS, "PhoneNumberMetadata.xml").
-define(REGEXP_PHONE, <<"^\\+[0-9]{8,15}$">>).

-define(ETS_TABLE, libphonenumber_erlang_registry).

-type phone_type() :: fixedLine |
                      noInternationalDialling |
                      pager |
                      personalNumber |
                      premiumRate |
                      sharedCost |
                      tollFree |
                      uan |
                      voicemail |
                      voip.

-record(phone_pattern, {
                        code = undefined,
                        id = undefined,
                        name,
                        possible_length_regexp = [],
                        pattern,
                        type :: phone_type(),
                        is_main_country :: boolean(),
                        sort = 1 :: integer(),
                        lengths,
                        options = []}).

-record(length, {
                 min = "",
                 max = "",
                 is_range = false,
                 part}).

-record(code_set, {id, lengths = [], name, pattern, metadata, type :: phone_type()}).

-record(countryphones, {
                        code :: binary(),
                        code_rules = [] :: list(#code_set{})
                       }).
