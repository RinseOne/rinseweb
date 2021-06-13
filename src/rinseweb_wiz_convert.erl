%%%-------------------------------------------------------------------
%% @doc rinseweb convert wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_convert).

%% API
-export([answer/2]).

%% Types
-type unit_or_unknown() :: binary() | unknown.
-type unit() :: binary().

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(Question, [FromNumBin, FromUnitBin, ToUnitBin]) ->
    FromUnit= binary_to_canonical_unit(FromUnitBin),
    ToUnit= binary_to_canonical_unit(ToUnitBin),
    answer_using_canonical_units(Question, FromNumBin, FromUnit, ToUnit).

%%====================================================================
%% Internal functions
%%====================================================================

-spec answer_using_canonical_units(rinseweb_wiz:question(), binary(), unit_or_unknown(), unit_or_unknown()) -> rinseweb_wiz:answer().
answer_using_canonical_units(_, _, unknown, _) -> shrug;
answer_using_canonical_units(_, _, _, unknown) -> shrug;
answer_using_canonical_units(Question, FromNumBin, FromUnit, ToUnit) ->
    FromNum = round_precise(binary_to_number(FromNumBin)),
    ConversionResult = convert(FromNum, FromUnit, ToUnit),
    answer_conversion_result(Question, FromNumBin, FromUnit, ToUnit, ConversionResult).

-spec answer_conversion_result(rinseweb_wiz:question(), binary(), unit(), unit(), number()) -> rinseweb_wiz:answer().
answer_conversion_result(_, _, _, _, undefined) -> shrug;
answer_conversion_result(Question, FromNumBin, FromUnit, ToUnit, ConversionResult) ->
    ToNum = round_precise(ConversionResult),
    ToBin = number_to_binary(ToNum),
    Answer = <<FromNumBin/binary, " ", FromUnit/binary, " = ", ToBin/binary, " ", ToUnit/binary>>,
    #{
        question => Question,
        type => text,
        short => Answer
    }.

-spec convert(number(), unit(), unit()) -> number() | undefined.
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

-spec round_precise(number()) -> number().
round_precise(Num) when is_integer(Num) -> Num;
round_precise(Num) when is_float(Num) ->
    if
        round(Num) == Num -> round(Num);
        true -> Num
    end.

-spec number_to_binary(number()) -> binary().
number_to_binary(Num) when is_integer(Num) -> integer_to_binary(Num);
number_to_binary(Num) when is_float(Num) -> float_to_binary(Num, [{decimals, 10}, compact]).

-spec binary_to_number(binary()) -> float() | integer().
binary_to_number(<<".", Rest/binary>>) -> binary_to_number(<<"0.", Rest/binary>>);
binary_to_number(<<"-.", Rest/binary>>) -> binary_to_number(<<"-0.", Rest/binary>>);
binary_to_number(B) ->
    try binary_to_float(B)
    catch
        error:badarg -> binary_to_integer(B)
    end.

-spec string_to_number(string()) -> number() | undefined.
string_to_number([]) -> undefined;
string_to_number("conformability error") -> undefined;
string_to_number([$U,$n,$k,$n,$o,$w,$n|_]) -> undefined; % Handles "Unknown unit 'foo'" error
string_to_number(S) ->
    case string:to_float(S) of
        {error, no_float} -> list_to_integer(S);
        {F, _Rest} -> F
    end.

-spec number_to_list(number()) -> list().
number_to_list(Num) when is_integer(Num) -> integer_to_list(Num);
number_to_list(Num) when is_float(Num) -> float_to_list(Num, [{decimals, 10}, compact]).

-spec binary_to_canonical_unit(binary()) -> unit_or_unknown().
binary_to_canonical_unit(<<"c">>) -> <<"celsius">>;
binary_to_canonical_unit(<<"celsius">>) -> <<"celsius">>;
binary_to_canonical_unit(<<"f">>) -> <<"fahrenheit">>;
binary_to_canonical_unit(<<"fahrenheit">>) -> <<"fahrenheit">>;
binary_to_canonical_unit(UnitBin) ->
    validate_unit(re:run(UnitBin, <<"^[a-zA-Z]+$">>, [{capture, none}]), UnitBin).

-spec validate_unit(atom(), binary()) -> unit_or_unknown().
validate_unit(match, Unit) when byte_size(Unit) < 30 -> Unit;
validate_unit(nomatch, _Unit) -> unknown.
