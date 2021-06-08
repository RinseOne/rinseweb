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
        {<<"1">>, <<"g">>, <<"mg">>, <<"1 gram is equal to 1000 milligrams">>},
        {<<"1">>, <<"kg">>, <<"g">>, <<"1 kilogram is equal to 1000 grams">>},
        {<<"1">>, <<"pound">>, <<"ounce">>, <<"1 pound is equal to 16 ounces">>},
        {<<"1">>, <<"liter">>, <<"ml">>, <<"1 liter is equal to 1000 ml">>},
        {<<"1">>, <<"kiloliter">>, <<"cc">>, <<"1 kiloliter is equal to 1000000 cc">>},
        {<<"1">>, <<"gallon">>, <<"qt">>, <<"1 gallon is equal to 4 quarts">>},
        {<<"1">>, <<"byte">>, <<"bit">>, <<"1 byte is equal to 8 bits">>},
        {<<"1">>, <<"kb">>, <<"kilobit">>, <<"1 kilobyte is equal to 8 kilobits">>},
        {<<"1">>, <<"mb">>, <<"megabit">>, <<"1 megabyte is equal to 8 megabits">>},
        {<<"1">>, <<"gb">>, <<"gigabit">>, <<"1 gigabyte is equal to 8 gigabits">>},
        {<<"1">>, <<"cm">>, <<"mm">>, <<"1 centimeter is equal to 10 millimeters">>},
        {<<"1">>, <<"km">>, <<"m">>, <<"1 kilometer is equal to 1000 meters">>},
        {<<"1">>, <<"mile">>, <<"yard">>, <<"1 mile is equal to 1760 yards">>},
        {<<"1">>, <<"foot">>, <<"inch">>, <<"1 foot is equal to 12 inches">>},
        {<<"1">>, <<"celsius">>, <<"fahrenheit">>, <<"1 celsius is equal to 33.8 fahrenheit">>}
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
        {<<"foo">>, <<"bar">>}
    ],
    UnitNum = <<"1">>,
    F = fun({UnitFrom, UnitTo}, Acc) ->
            Question = <<"convert ", UnitNum/binary, UnitFrom/binary, " to ", UnitTo/binary>>,
            shrug = rinseweb_wiz_convert:answer(Question, [UnitNum, UnitFrom, UnitTo]),
            Acc
        end,
    ok = lists:foldr(F, ok, TestCases).
