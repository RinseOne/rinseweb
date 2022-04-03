-module(convert_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([unit_coverage/1]).
-export([unit_invalid/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        unit_coverage,
        unit_invalid
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

result(UnitFromNum, UnitFrom, UnitToNum, UnitTo) ->
    #{
        type => conversion_result,
        source => convert,
        answer => #{
            unit_from_name => UnitFrom,
            unit_from_number => UnitFromNum,
            unit_to_name => UnitTo,
            unit_to_number => UnitToNum
        }
    }.

%% ============================================================================
%% Tests
%% ============================================================================

unit_coverage(_) ->
    TestCases = [
        % weight
        {<<"1">>, <<"g">>, <<"mg">>, result(1, <<"g">>, 1000, <<"mg">>)},
        {<<"1">>, <<"kg">>, <<"g">>, result(1, <<"kg">>, 1000, <<"g">>)},
        {<<"1">>, <<"pound">>, <<"ounce">>, result(1, <<"pound">>, 16, <<"ounce">>)},
        % volume
        {<<"1">>, <<"liter">>, <<"ml">>, result(1, <<"liter">>, 1000, <<"ml">>)},
        {<<"1">>, <<"kiloliter">>, <<"cc">>, result(1, <<"kiloliter">>, 1000000, <<"cc">>)},
        {<<"1">>, <<"gallon">>, <<"qt">>, result(1, <<"gallon">>, 4, <<"qt">>)},
        {<<"1">>, <<"gallon">>, <<"floz">>, result(1, <<"gallon">>, 128, <<"floz">>)},
        % bandwidth
        {<<"1">>, <<"byte">>, <<"bit">>, result(1, <<"byte">>, 8, <<"bit">>)},
        {<<"1">>, <<"kilobyte">>, <<"kilobit">>, result(1, <<"kilobyte">>, 8, <<"kilobit">>)},
        {<<"1">>, <<"megabyte">>, <<"megabit">>, result(1, <<"megabyte">>, 8, <<"megabit">>)},
        {<<"1">>, <<"gigabyte">>, <<"gigabit">>, result(1, <<"gigabyte">>, 8, <<"gigabit">>)},
        % distance
        {<<"1">>, <<"cm">>, <<"mm">>, result(1, <<"cm">>, 10, <<"mm">>)},
        {<<"1">>, <<"km">>, <<"m">>, result(1, <<"km">>, 1000, <<"m">>)},
        {<<"1">>, <<"mile">>, <<"yard">>, result(1, <<"mile">>, 1760, <<"yard">>)},
        {<<"1">>, <<"foot">>, <<"inch">>, result(1, <<"foot">>, 12, <<"inch">>)},
        % area
        {<<"1">>, <<"meter^2">>, <<"foot^2">>, result(1, <<"meter^2">>, 10.76391, <<"foot^2">>)},
        % volume
        {<<"1">>, <<"meter^3">>, <<"foot^3">>, result(1, <<"meter^3">>, 35.314667, <<"foot^3">>)},
        % temperature
        {<<"1">>, <<"celsius">>, <<"fahrenheit">>, result(1, <<"celsius">>, 33.8, <<"fahrenheit">>)}
    ],
    F = fun({UnitNum, UnitFrom, UnitTo, ExpectedAnswer}, Acc) ->
            Question = <<"convert ", UnitNum/binary, UnitFrom/binary, " to ", UnitTo/binary>>,
            ExpectedAnswer = rinseweb_wiz_convert:answer(Question, [UnitNum, UnitFrom, UnitTo]),
            Acc
        end,
    ok = lists:foldl(F, ok, TestCases).

unit_invalid(_) ->
    TestCases = [
        {<<"kg">>, <<"foo">>, <<"Unknown unit 'foo'">>},
        {<<"foo">>, <<"kg">>, <<"Unknown unit 'foo'">>},
        {<<"foo">>, <<"bar">>, <<"Unknown unit 'foo'">>},
        {<<"kg">>, <<"1234567890123456789012345678901">>, <<"Unknown unit '123456789012345678901234567890'">>},
        {<<"kg">>, <<"mm">>, <<"Unable to convert between non-conforming units">>},
        {<<"meters^2">>, <<"m^">>, <<"Error in 'm^': Parse error">>}
    ],
    UnitNum = <<"1">>,
    F = fun({UnitFrom, UnitTo, ErrorReason}, Acc) ->
            Question = <<"convert ", UnitNum/binary, UnitFrom/binary, " to ", UnitTo/binary>>,
            ExpectedAnswer = rinseweb_wiz:shrug(convert, ErrorReason),
            ExpectedAnswer = rinseweb_wiz_convert:answer(Question, [UnitNum, UnitFrom, UnitTo]),
            Acc
        end,
    ok = lists:foldl(F, ok, TestCases).
