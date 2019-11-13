%%%-------------------------------------------------------------------
%%% @author marinakr
%%% Created : 17. Apr 2018 15:20
%%%-------------------------------------------------------------------
-module(libphonenumber_parser).
-author("marinakr").

-include("phonenumbers.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%% API
-export([
         xml_file2memory/1
        ]).

%%%------------------------------------------------------------------------------------------------
%%% Parsing xml-file
%%% to generate map COUNTRY_PHONE_RULES in country_phones_rules.hrl
%%%------------------------------------------------------------------------------------------------
%% -------------------------------------------------------------------
%% @doc
%% Parse xml file
%% https://github.com/googlei18n/libphonenumber/blob/master/resources/PhoneNumberMetadata.xml
%% And return map stored in include/country_phone_rules.hrl
%% @end
%% -------------------------------------------------------------------
-spec xml_file2memory(list()) -> maps:map() | error.

xml_file2memory(FileName) ->
    case xmerl_scan:file(FileName) of
        {Xml, _} ->
            #xmlElement{content = [_, TerrirtoriesEl, _]} = Xml,
            TerritoriesInfo = TerrirtoriesEl#xmlElement.content,
            parse_countries(TerritoriesInfo);
        _ ->
            error
    end.

%% --------------------------------------------------------------------------
%% @private
%% This block of code math name of country and block of rules for territory
%% --------------------------------------------------------------------------
-spec parse_countries(list()) -> [] | list({binary(), list(#phone_pattern{})}).

parse_countries(Elements) ->
    parse_country(undefined, Elements, []).

-spec parse_country(Name, XmlDoc, Acc) -> Res when
      Name :: undefined | binary,
      XmlDoc :: list(),
      Acc :: [] | list({binary(), list(#phone_pattern{})}),
      Res :: [] | list({binary(), list(#phone_pattern{})}).

parse_country(_, [], Acc) ->
    Acc;
parse_country(undefined, [C = #xmlComment{} | Rest], Acc) ->
    Name = C#xmlComment.value,
    NormalizedName = normalize(Name),
    parse_country(NormalizedName, Rest, Acc);
parse_country(Name, [E = #xmlElement{name = territory} | Rest], Acc) when is_binary(Name) ->
    #xmlElement{name = territory, attributes = Attrs, content = Content} = E,
    IsMainCountry = is_main_country_for_code(Attrs),
    PhonePattern0 = parse_number_attributes(Attrs, #phone_pattern{}),
    PhonePattern = case IsMainCountry of
                       true ->
                           PhonePattern0#phone_pattern{sort = 0, is_main_country = IsMainCountry};
                       false ->
                           PhonePattern0
                   end,
    CountryPhoneInfos = parse_number_type_content(Content, PhonePattern),
    NewAcc = add_patterns(CountryPhoneInfos, Name, Acc),
    parse_country(undefined, Rest, NewAcc);
parse_country(State, [_H|Rest], Acc) ->
    parse_country(State, Rest, Acc).

%% @private
%% We need to make sure that the 'main' entry for a country is
%% first within the list of entries for a code
add_patterns([], _Name, Acc) ->
    Acc;
add_patterns([#phone_pattern{code = Code, possible_length_regexp = LengthInfo} = PhoneInfo | Rest], Name, Acc) ->
    Lengths = format_rules(LengthInfo),
    NRec = PhoneInfo#phone_pattern{name = Name, lengths = Lengths},
    NAcc = case lists:keytake(Code, 1, Acc) of
               false ->
                   [{Code, [NRec]} | Acc];
               {value, {Code, ORecs}, Acc2} ->
                   NRecs = lists:keysort(#phone_pattern.sort, [NRec | ORecs]),
                   [{Code, NRecs} | Acc2]
           end,
    add_patterns(Rest, Name, NAcc).

is_main_country_for_code(Attribs) ->
    case [M || #xmlAttribute{name = mainCountryForCode, value = "true"} = M <- Attribs] of
        [] -> false;
        [_]-> true
    end.

%% @private
-spec normalize(list()) -> binary().
normalize(Str) ->
    Norm = trim_first_last_whitespaces(Str),
    unicode:characters_to_binary(Norm, utf8).

%% -------------------------------------------------------------------
%% @private
%% Parse country attributes
%% -------------------------------------------------------------------
-spec parse_number_attributes(Attributes, State) -> State when
      Attributes :: list(#xmlAttribute{}),
      State :: #phone_pattern{}.

parse_number_attributes([], State) ->
    State;
parse_number_attributes([#xmlAttribute{name = id, value = Id} | Rest], State) ->
    parse_number_attributes(Rest, State#phone_pattern{id = list_to_binary(Id)});
parse_number_attributes([#xmlAttribute{name = countryCode, value = Code} | Rest], State) ->
    parse_number_attributes(Rest, State#phone_pattern{code = list_to_binary(Code)});
parse_number_attributes([_ | Rest], State) ->
    parse_number_attributes(Rest, State).

%% -------------------------------------------------------------------
%% @private
%% Parse country mobile possible length
%% -------------------------------------------------------------------
-spec parse_number_type_content(Elements, Patt) -> Acc when
      Elements :: list(#xmlElement{}),
      Patt :: #phone_pattern{},
      Acc :: list(#phone_pattern{}).

parse_number_type_content(Elements, Patt) ->
    parse_number_type_content(Elements, Patt, []).

parse_number_type_content([], _Patt, Acc) ->
    %% this is reversed by the foldl in the caller
    Acc;

parse_number_type_content([#xmlElement{name = Type} | Rest], Patt, Acc)
  when Type =:= availableFormats orelse Type =:= generalDesc ->
    %% Skip these
    parse_number_type_content(Rest, Patt, Acc);
parse_number_type_content([#xmlElement{name = Type, content = Content} | Rest], Patt, Acc) ->
    #{pattern := Pattern,
      length := LengthAttributes} = get_pattern_and_length(Content),
    ExampleNumber = get_example_number(Content),
    [NewLength] = parse_possible_length(LengthAttributes, []),
    NPatt = Patt#phone_pattern{
              possible_length_regexp = NewLength,
              pattern = Pattern,
              options = add_example(ExampleNumber),
              type = Type
             },
    parse_number_type_content(Rest, Patt, [NPatt | Acc]);
parse_number_type_content([_ | Rest], Patt, State) ->
    parse_number_type_content(Rest, Patt, State).

%% @private
-spec add_example(null | list()) -> list().
add_example(null) ->
    [];
add_example(ExampleNumber) ->
    [#{example_number => ExampleNumber}].

%% -------------------------------------------------------------------
%% @private
%% Get possible length and pattern by one run
%% -------------------------------------------------------------------
-spec get_pattern_and_length(list()) -> map().
get_pattern_and_length(Content) ->
    get_pattern_and_length(Content, #{}).

-spec get_pattern_and_length(Elements, Acc) -> Res when
      Acc :: maps:map(),
      Elements :: list(),
      Res :: map().
get_pattern_and_length(_, #{pattern := _Pattern, length := _LengthAttrs} = Acc) ->
    Acc;
get_pattern_and_length([#xmlElement{name = possibleLengths, attributes = Attrs} | Rest], Acc) ->
    CurrAttrs = maps:get(length, Acc, []),
    get_pattern_and_length(Rest, maps:put(length, [Attrs | CurrAttrs], Acc));
get_pattern_and_length([#xmlElement{name = nationalNumberPattern, content = C} | Rest], Acc) ->
    [PatternVal] = [V || #xmlText{value = V} <- C],
    Pattern = re:replace(PatternVal, "\s+|\n|\t", "", [global, {return, binary}]),
    %% @TODO : Do we need to do any escaping on the patterns since Erlang does have some funky double-escape rules for regexes
    get_pattern_and_length(Rest, maps:put(pattern, Pattern, Acc));
get_pattern_and_length([_|Rest], Acc) ->
    get_pattern_and_length(Rest, Acc).

%% -------------------------------------------------------------------
%% @private
%% Get example of valid phone, only for test
%% -------------------------------------------------------------------
get_example_number([]) ->
    null;
get_example_number([#xmlElement{name = exampleNumber, content = [#xmlText{value = ExampleNumber}]} | _]) ->
    ExampleNumber;
get_example_number([_E|Rest]) ->
    get_example_number(Rest).

%% -------------------------------------------------------------------
%% @private
%% Parse country mobile possible length from attributes
%% -------------------------------------------------------------------
-spec parse_possible_length(Attrs, Acc) -> ResAcc when
      Attrs :: list(#xmlAttribute{}),
      Acc :: list(binary()),
      ResAcc :: list(binary()).
parse_possible_length([], Acc) ->
    Acc;
parse_possible_length([Attrs | Rest], Acc) ->
    Ls =[V || #xmlAttribute{name = national, value = V} <- Attrs],
    parse_possible_length(Rest, Acc ++ Ls).

%% -------------------------------------------------------------------
%% @private
%% Formats length to valid value
%% Block of ugly code parse length range from xml for validator
%% -------------------------------------------------------------------
-spec format_rules(Length) -> Result when
      Length :: list(),
      Result :: {Min, Max},
      Min :: integer(),
      Max :: integer().
format_rules([]) ->
    no_rules;
format_rules(Length) ->
    format_length(Length).

format_length(Len) when is_list(Len) ->
    LenBin = iolist_to_binary(Len),
    Split = binary:split(LenBin, <<",">>, [global]),
    format_length(Split, []).

format_length([], Acc) ->
    lists:reverse(Acc);
format_length([<<"[", _/binary>> = Range0 | Rest], Acc) ->
    Range1 = binary:replace(Range0, [<<"[">>, <<"]">>], <<>>, [global]),
    [Min0, Max0] = binary:split(Range1, <<"-">>),
    format_length(Rest, [{binary_to_integer(Min0), binary_to_integer(Max0)} | Acc]);
format_length([Fixed0 | Rest], Acc) ->
    Fixed = binary_to_integer(Fixed0),
    format_length(Rest, [{Fixed, Fixed} | Acc]).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec trim_first_last_whitespaces(Name) -> NoWhitespaceName when
      Name :: list(),
      NoWhitespaceName :: list().
trim_first_last_whitespaces(Name) ->
    LName = trim_leading_whitespaces(Name),
    RName = trim_leading_whitespaces(lists:reverse(LName)),
    lists:reverse(RName).

-spec trim_leading_whitespaces(list()) -> list().
%% Be accurate here with tabulation,
%% Whitespace after '$ ' is matter, it is whitespace symbol, 32
trim_leading_whitespaces([$  | Name]) ->
    trim_leading_whitespaces(Name);
trim_leading_whitespaces([$\t | Name]) ->
    trim_leading_whitespaces(Name);
trim_leading_whitespaces([$\n | Name]) ->
    trim_leading_whitespaces(Name);
trim_leading_whitespaces(Name) ->
    Name.
