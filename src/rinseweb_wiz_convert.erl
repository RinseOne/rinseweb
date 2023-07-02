%%%-------------------------------------------------------------------
%% @doc rinseweb convert wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_convert).

%% API
-export([answer/2]).

%% Types
-type unit_or_unknown() :: binary() | {unknown, binary()}.
-type unit() :: binary().

-define(ANSWER_SOURCE, convert).
-define(ANSWER_TYPE, conversion_result).
-define(UNIT_MAX_LENGTH_BYTES, 30).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [FromNumBin, FromUnitBin, ToUnitBin]) ->
    FromUnit = binary_to_canonical_unit(FromUnitBin),
    ToUnit = binary_to_canonical_unit(ToUnitBin),
    answer_using_canonical_units(FromNumBin, FromUnit, ToUnit).

%%====================================================================
%% Internal functions
%%====================================================================

-spec answer_using_canonical_units(binary(), unit_or_unknown(), unit_or_unknown()) -> rinseweb_wiz:answer().
answer_using_canonical_units(_, {unknown, Unit}, _) -> rinseweb_wiz:shrug(?ANSWER_SOURCE, <<"Unknown unit '", Unit/binary, "'">>);
answer_using_canonical_units(_, _, {unknown, Unit}) -> rinseweb_wiz:shrug(?ANSWER_SOURCE, <<"Unknown unit '", Unit/binary, "'">>);
answer_using_canonical_units(FromNumBin, FromUnit, ToUnit) ->
    FromNum = rinseweb_util:round_precise(rinseweb_util:binary_to_number(FromNumBin)),
    ConversionResult = convert(FromNum, FromUnit, ToUnit),
    answer_conversion_result(FromNum, FromUnit, ToUnit, ConversionResult).

-spec answer_conversion_result(number(), unit(), unit(), {error, binary()} | number()) -> rinseweb_wiz:answer().
answer_conversion_result(_, _, _, {error, Error}) -> rinseweb_wiz:shrug(?ANSWER_SOURCE, Error);
answer_conversion_result(FromNum, FromUnit, ToUnit, ToNum) ->
    AnswerCustom = #{
        unit_from_name => FromUnit,
        unit_from_number => FromNum,
        unit_to_name => ToUnit,
        unit_to_number => ToNum
    },
    rinseweb_wiz:answer(?ANSWER_TYPE, ?ANSWER_SOURCE, AnswerCustom).

-spec convert(number(), unit(), unit()) -> number() | {error, binary()}.
convert(Num, FromUnit, ToUnit) ->
    Command = "units --terse",
    FromArg = units_from_arg(Num, FromUnit),
    ToArg = " '" ++ unit_arg(ToUnit) ++ "'",
    RawUnitsResult = os:cmd([Command, FromArg, ToArg]),
    Answer = string:trim(string:slice(RawUnitsResult, 0, string:str(RawUnitsResult, "\n"))),
    string_to_number(Answer).

-spec units_from_arg(number(), unit()) -> list().
units_from_arg(Num, <<"celsius">>) -> " 'tempC(" ++ number_to_list(Num) ++ ")'";
units_from_arg(Num, <<"fahrenheit">>) -> " 'tempF(" ++ number_to_list(Num) ++ ")'";
units_from_arg(Num, Unit) ->
    % extra space to avoid treating negative numbers as CLI options
    " ' " ++ number_to_list(Num) ++ " " ++ binary_to_list(Unit) ++ "'".

-spec unit_arg(unit()) -> list().
unit_arg(<<"celsius">>) -> "tempC";
unit_arg(<<"fahrenheit">>) -> "tempF";
unit_arg(UnitBin) when is_binary(UnitBin) -> binary_to_list(UnitBin).

-spec string_to_number(string()) -> number() | {error, binary()}.
string_to_number("conformability error") -> {error, <<"Unable to convert between non-conforming units">>};
string_to_number([$U,$n,$k,$n,$o,$w,$n,32,$u,$n,$i,$t,32|_] = Error) -> {error, list_to_binary(Error)}; % Handles "Unknown unit 'foo'" error
string_to_number([$E,$r,$r,$o,$r|_] = Error) -> {error, list_to_binary(Error)}; % Handles "Error in 'foo': Parse error" error
string_to_number(S) -> rinseweb_util:string_to_number(S).

-spec number_to_list(number()) -> list().
number_to_list(Num) when is_integer(Num) -> integer_to_list(Num);
number_to_list(Num) when is_float(Num) -> float_to_list(Num, [{decimals, 10}, compact]).

-spec binary_to_canonical_unit(binary()) -> unit_or_unknown().
binary_to_canonical_unit(<<"c">>) -> <<"celsius">>;
binary_to_canonical_unit(<<"celsius">>) -> <<"celsius">>;
binary_to_canonical_unit(<<"f">>) -> <<"fahrenheit">>;
binary_to_canonical_unit(<<"fahrenheit">>) -> <<"fahrenheit">>;
binary_to_canonical_unit(UnitBin) when byte_size(UnitBin) > ?UNIT_MAX_LENGTH_BYTES ->
    validate_unit(nomatch, UnitBin);
binary_to_canonical_unit(UnitBin) ->
    validate_unit(re:run(UnitBin, <<"^[a-zA-Z]+[\\^]?[2-3]?$">>, [{capture, none}]), UnitBin).

-spec validate_unit(atom(), binary()) -> unit_or_unknown().
validate_unit(match, Unit) when byte_size(Unit) < ?UNIT_MAX_LENGTH_BYTES -> Unit;
validate_unit(nomatch, <<UnitSub:30/binary,_/binary>> = Unit) when byte_size(Unit) > ?UNIT_MAX_LENGTH_BYTES -> {unknown, UnitSub};
validate_unit(nomatch, Unit) -> {unknown, Unit}.
