-module(libphonenumber_erlang_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([load_countryphonenumbers_rules/0]).

-include("phonenumbers.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    init_ets(),
    load_countryphonenumbers_rules(),

    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec load_countryphonenumbers_rules() -> ok.

load_countryphonenumbers_rules() ->
    PhoneNumberMetadata = code:priv_dir(libphonenumber_erlang) ++ "/" ++ ?FILE_PHONE_PHONE_FORMATS,
    Rules = libphonenumber_parser:xml_file2memory(PhoneNumberMetadata),
    OldKeys = [K || #countryphones{code = K} <- ets:tab2list(?ETS_TABLE)],
    DelKeys = lists:foldl(
                fun({Code, CodeRulesInfo}, Old) ->
                        CodeRules = rules_to_code_sets(CodeRulesInfo),
                        Record = #countryphones{
                                    code = Code,
                                    code_rules = CodeRules},
                        ets:insert(?ETS_TABLE, Record),
                        lists:delete(Code, Old)
                end, OldKeys, Rules),
    io:format("Deleting: ~p~n", [DelKeys]),
    _ = [ets:delete(?ETS_TABLE, K) || K <- DelKeys],
    ok.

rules_to_code_sets(CodeRulesInfo) ->
    rules_to_code_sets(CodeRulesInfo, []).

rules_to_code_sets([], Acc) ->
    lists:reverse(Acc);
rules_to_code_sets([#phone_pattern{
                       id = Id,
                       lengths = Length,
                       pattern = Pattern,
                       name = Name,
                       type = Type
                      } | Rest], Acc) ->
    Rec = #code_set{id = Id,
                    lengths = Length,
                    name = Name,
                    pattern = Pattern,
                    type = Type},
    rules_to_code_sets(Rest, [Rec | Acc]).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
init_ets() ->
    ets:new(?ETS_TABLE,
            [named_table, public, ordered_set, {keypos, 2},
             {write_concurrency, true}, {read_concurrency, true}]).
