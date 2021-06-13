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
%% Tests
%% ============================================================================

unit_coverage(_) ->
    TestCases = [
        % weight
        {<<"1">>, <<"g">>, <<"mg">>, <<"1 g = 1000 mg">>},
        {<<"1">>, <<"kg">>, <<"g">>, <<"1 kg = 1000 g">>},
        {<<"1">>, <<"pound">>, <<"ounce">>, <<"1 pound = 16 ounce">>},
        % volume
        {<<"1">>, <<"liter">>, <<"ml">>, <<"1 liter = 1000 ml">>},
        {<<"1">>, <<"kiloliter">>, <<"cc">>, <<"1 kiloliter = 1000000 cc">>},
        {<<"1">>, <<"gallon">>, <<"qt">>, <<"1 gallon = 4 qt">>},
        {<<"1">>, <<"gallon">>, <<"floz">>, <<"1 gallon = 128 floz">>},
        % bandwidth
        {<<"1">>, <<"byte">>, <<"bit">>, <<"1 byte = 8 bit">>},
        {<<"1">>, <<"kilobyte">>, <<"kilobit">>, <<"1 kilobyte = 8 kilobit">>},
        {<<"1">>, <<"megabyte">>, <<"megabit">>, <<"1 megabyte = 8 megabit">>},
        {<<"1">>, <<"gigabyte">>, <<"gigabit">>, <<"1 gigabyte = 8 gigabit">>},
        {<<"1">>, <<"cm">>, <<"mm">>, <<"1 cm = 10 mm">>},
        {<<"1">>, <<"km">>, <<"m">>, <<"1 km = 1000 m">>},
        % distance
        {<<"1">>, <<"mile">>, <<"yard">>, <<"1 mile = 1760 yard">>},
        {<<"1">>, <<"foot">>, <<"inch">>, <<"1 foot = 12 inch">>},
        % temperature
        {<<"1">>, <<"celsius">>, <<"fahrenheit">>, <<"1 celsius = 33.8 fahrenheit">>}
    ],
    F = fun({UnitNum, UnitFrom, UnitTo, ExpectedShort}, Acc) ->
            Question = <<"convert ", UnitNum/binary, UnitFrom/binary, " to ", UnitTo/binary>>,
            ExpectedAnswer = #{
                question => Question,
                short => ExpectedShort,
                type => text
            },
            ExpectedAnswer = rinseweb_wiz_convert:answer(Question, [UnitNum, UnitFrom, UnitTo]),
            Acc
        end,
    ok = lists:foldr(F, ok, TestCases).

unit_invalid(_) ->
    TestCases = [
        {<<"kg">>, <<"foo">>},
        {<<"foo">>, <<"kg">>},
        {<<"foo">>, <<"bar">>},
        {<<"kg">>, <<"mm">>}
    ],
    UnitNum = <<"1">>,
    F = fun({UnitFrom, UnitTo}, Acc) ->
            Question = <<"convert ", UnitNum/binary, UnitFrom/binary, " to ", UnitTo/binary>>,
            shrug = rinseweb_wiz_convert:answer(Question, [UnitNum, UnitFrom, UnitTo]),
            Acc
        end,
    ok = lists:foldr(F, ok, TestCases).
