%%%-------------------------------------------------------------------
%% @doc rinseweb convert wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_convert).

%% API
-export([answer/2]).

%% Types
-type unit() ::
    % weight
    mg | g | kg | pound | ounce
    % volume
    | ml | liter | kiloliter | quart | gallon | cc
    % bandwidth
    | bit | byte | kilobit | kilobyte | megabit | megabyte | gigabit | gigabyte
    % distance
    | mm | cm | m | km | inch | foot | yard | mile
    % temperature
    | tempC | tempF
    % default
    | unknown.

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

-spec answer_using_canonical_units(rinseweb_wiz:question(), binary(), unit(), unit()) -> rinseweb_wiz:answer().
answer_using_canonical_units(_, _, unknown, _) -> shrug;
answer_using_canonical_units(_, _, _, unknown) -> shrug;
answer_using_canonical_units(Question, FromNumBin, FromUnit, ToUnit) ->
    FromNum = round_precise(binary_to_number(FromNumBin)),
    ConversionResult = convert(FromNum, FromUnit, ToUnit),
    answer_conversion_result(Question, FromNumBin, FromNum, FromUnit, ToUnit, ConversionResult).

-spec answer_conversion_result(rinseweb_wiz:question(), binary(), number(), unit(), unit(), number()) -> rinseweb_wiz:answer().
answer_conversion_result(_, _, _, _, _, undefined) -> shrug;
answer_conversion_result(Question, FromNumBin, FromNum, FromUnit, ToUnit, ConversionResult) ->
    ToNum = round_precise(ConversionResult),
    ToBin = number_to_binary(ToNum),
    ToUnitBin = canonical_unit_to_binary(ToNum, ToUnit),
    FromUnitBin = canonical_unit_to_binary(FromNum, FromUnit),
    Answer = <<FromNumBin/binary, " ", FromUnitBin/binary, " is equal to ", ToBin/binary, " ", ToUnitBin/binary>>,
    #{
        question => Question,
        type => text,
        short => Answer
    }.

-spec convert(number(), unit(), unit()) -> number() | undefined.
convert(Num, FromUnit, ToUnit) ->
    Command = "units --strict",
    FromArg = units_from_arg(Num, FromUnit),
    ToArg = " '" ++ atom_to_list(ToUnit) ++ "'",
    RawUnitsResult = os:cmd([Command, FromArg, ToArg]),
    FirstLineTrimmed = string:trim(string:slice(RawUnitsResult, 0, string:str(RawUnitsResult, "\n"))),
    % remove optional "* " from "* 123"
    Answer = string:trim(FirstLineTrimmed, leading, "* "),
    string_to_number(Answer).

-spec units_from_arg(number(), unit()) -> list().
units_from_arg(Num, tempC) -> " 'tempC(" ++ number_to_list(Num) ++ ")'";
units_from_arg(Num, tempF) -> " 'tempF(" ++ number_to_list(Num) ++ ")'";
units_from_arg(Num, Unit) ->
    % extra space to avoid treating negative numbers as CLI options
    " ' " ++ number_to_list(Num) ++ " " ++ atom_to_list(Unit) ++ "'".

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
string_to_number(S) ->
    case string:to_float(S) of
        {error, no_float} -> list_to_integer(S);
        {F, _Rest} -> F
    end.

-spec number_to_list(number()) -> list().
number_to_list(Num) when is_integer(Num) -> integer_to_list(Num);
number_to_list(Num) when is_float(Num) -> float_to_list(Num, [{decimals, 10}, compact]).

-spec binary_to_canonical_unit(binary()) -> unit().
% weight
binary_to_canonical_unit(<<"milligram">>) -> mg;
binary_to_canonical_unit(<<"milligrams">>) -> mg;
binary_to_canonical_unit(<<"mg">>) -> mg;
binary_to_canonical_unit(<<"gram">>) -> g;
binary_to_canonical_unit(<<"grams">>) -> g;
binary_to_canonical_unit(<<"g">>) -> g;
binary_to_canonical_unit(<<"kilogram">>) -> kg;
binary_to_canonical_unit(<<"kilograms">>) -> kg;
binary_to_canonical_unit(<<"kg">>) -> kg;
binary_to_canonical_unit(<<"pound">>) -> pound;
binary_to_canonical_unit(<<"pounds">>) -> pound;
binary_to_canonical_unit(<<"ounce">>) -> ounce;
binary_to_canonical_unit(<<"ounces">>) -> ounce;
binary_to_canonical_unit(<<"oz">>) -> ounce;
% volume
binary_to_canonical_unit(<<"milliliter">>) -> ml;
binary_to_canonical_unit(<<"milliliters">>) -> ml;
binary_to_canonical_unit(<<"ml">>) -> ml;
binary_to_canonical_unit(<<"liter">>) -> liter;
binary_to_canonical_unit(<<"kiloliter">>) -> kiloliter;
binary_to_canonical_unit(<<"kiloliters">>) -> kiloliter;
binary_to_canonical_unit(<<"quart">>) -> quart;
binary_to_canonical_unit(<<"quarts">>) -> quart;
binary_to_canonical_unit(<<"qt">>) -> quart;
binary_to_canonical_unit(<<"qts">>) -> quart;
binary_to_canonical_unit(<<"gallon">>) -> gallon;
binary_to_canonical_unit(<<"gallons">>) -> gallon;
binary_to_canonical_unit(<<"cc">>) -> cc;
% bandwidth
binary_to_canonical_unit(<<"bit">>) -> bit;
binary_to_canonical_unit(<<"bits">>) -> bit;
binary_to_canonical_unit(<<"byte">>) -> byte;
binary_to_canonical_unit(<<"bytes">>) -> byte;
binary_to_canonical_unit(<<"kb">>) -> kilobyte;
binary_to_canonical_unit(<<"kilobit">>) -> kilobit;
binary_to_canonical_unit(<<"kilobits">>) -> kilobit;
binary_to_canonical_unit(<<"kilobyte">>) -> kilobyte;
binary_to_canonical_unit(<<"kilobytes">>) -> kilobyte;
binary_to_canonical_unit(<<"megabit">>) -> megabit;
binary_to_canonical_unit(<<"megabits">>) -> megabit;
binary_to_canonical_unit(<<"mb">>) -> megabyte;
binary_to_canonical_unit(<<"megabyte">>) -> megabyte;
binary_to_canonical_unit(<<"megabytes">>) -> megabyte;
binary_to_canonical_unit(<<"gigabit">>) -> gigabit;
binary_to_canonical_unit(<<"gigabits">>) -> gigabit;
binary_to_canonical_unit(<<"gb">>) -> gigabyte;
binary_to_canonical_unit(<<"gigabyte">>) -> gigabyte;
binary_to_canonical_unit(<<"gigabytes">>) -> gigabyte;
% distance
binary_to_canonical_unit(<<"millimeter">>) -> mm;
binary_to_canonical_unit(<<"millimeters">>) -> mm;
binary_to_canonical_unit(<<"mm">>) -> mm;
binary_to_canonical_unit(<<"centimeter">>) -> cm;
binary_to_canonical_unit(<<"centimeters">>) -> cm;
binary_to_canonical_unit(<<"cm">>) -> cm;
binary_to_canonical_unit(<<"meter">>) -> m;
binary_to_canonical_unit(<<"meters">>) -> m;
binary_to_canonical_unit(<<"m">>) -> m;
binary_to_canonical_unit(<<"kilometer">>) -> km;
binary_to_canonical_unit(<<"kilometers">>) -> km;
binary_to_canonical_unit(<<"km">>) -> km;
binary_to_canonical_unit(<<"in">>) -> inch;
binary_to_canonical_unit(<<"inch">>) -> inch;
binary_to_canonical_unit(<<"inches">>) -> inch;
binary_to_canonical_unit(<<"foot">>) -> foot;
binary_to_canonical_unit(<<"feet">>) -> foot;
binary_to_canonical_unit(<<"yard">>) -> yard;
binary_to_canonical_unit(<<"yards">>) -> yard;
binary_to_canonical_unit(<<"mile">>) -> mile;
binary_to_canonical_unit(<<"miles">>) -> mile;
% temperature
binary_to_canonical_unit(<<"c">>) -> tempC;
binary_to_canonical_unit(<<"celsius">>) -> tempC;
binary_to_canonical_unit(<<"f">>) -> tempF;
binary_to_canonical_unit(<<"fahrenheit">>) -> tempF;
% default
binary_to_canonical_unit(_) -> unknown.

-spec canonical_unit_to_binary(number(), unit()) -> binary().
% weight
canonical_unit_to_binary(1, mg) -> <<"milligram">>;
canonical_unit_to_binary(_, mg) -> <<"milligrams">>;
canonical_unit_to_binary(1, g) -> <<"gram">>;
canonical_unit_to_binary(_, g) -> <<"grams">>;
canonical_unit_to_binary(1, kg) -> <<"kilogram">>;
canonical_unit_to_binary(_, kg) -> <<"kilograms">>;
% volume
canonical_unit_to_binary(_, ml) -> <<"ml">>;
canonical_unit_to_binary(_, cc) -> <<"cc">>;
% bandwidth
% distance
canonical_unit_to_binary(1, mm) -> <<"millimeter">>;
canonical_unit_to_binary(_, mm) -> <<"millimeters">>;
canonical_unit_to_binary(1, cm) -> <<"centimeter">>;
canonical_unit_to_binary(_, cm) -> <<"centimeters">>;
canonical_unit_to_binary(1, m) -> <<"meter">>;
canonical_unit_to_binary(_, m) -> <<"meters">>;
canonical_unit_to_binary(1, km) -> <<"kilometer">>;
canonical_unit_to_binary(_, km) -> <<"kilometers">>;
canonical_unit_to_binary(1, inch) -> <<"inch">>;
canonical_unit_to_binary(_, inch) -> <<"inches">>;
canonical_unit_to_binary(1, foot) -> <<"foot">>;
canonical_unit_to_binary(_, foot) -> <<"feet">>;
% temperature
canonical_unit_to_binary(_, tempC) -> <<"celsius">>;
canonical_unit_to_binary(_, tempF) -> <<"fahrenheit">>;
% default
canonical_unit_to_binary(1, Unit) -> atom_to_binary(Unit);
canonical_unit_to_binary(_, Unit) ->
    UnitBin = atom_to_binary(Unit),
    <<UnitBin/binary, "s">>.
