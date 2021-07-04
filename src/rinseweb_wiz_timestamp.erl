%%%-------------------------------------------------------------------
%% @doc rinseweb unix timestamp wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_timestamp).

%% API
-export([answer/2]).

%% Types
-type time_unit() :: second | millisecond.

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(Question, []) ->
    Timestamp = erlang:system_time(second),
    #{
        question => Question,
        type => text,
        short => integer_to_binary(Timestamp)
    };
answer(Question, [Bin]) ->
    Type = timestamp_unit(Bin),
    Num = binary_to_integer(Bin),
    DateTime = calendar:system_time_to_universal_time(Num, Type),
    #{
        question => Question,
        type => text,
        short => format(DateTime)
    }.


%%====================================================================
%% Internal functions
%%====================================================================

-spec timestamp_unit(binary()) -> time_unit().
timestamp_unit(Bin) when byte_size(Bin) =:= 10 -> second;
timestamp_unit(Bin) when byte_size(Bin) =:= 13 -> millisecond.

-spec format(calendar:datetime()) -> binary().
format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    YearBin = integer_to_binary(Year),
    MonthBin = pad_date_digits(integer_to_binary(Month)),
    DayBin = pad_date_digits(integer_to_binary(Day)),
    HourBin = pad_date_digits(integer_to_binary(Hour)),
    MinuteBin = pad_date_digits(integer_to_binary(Minute)),
    SecondBin = pad_date_digits(integer_to_binary(Second)),
    <<YearBin/binary, "-", MonthBin/binary, "-", DayBin/binary, " ", HourBin/binary, ":", MinuteBin/binary, ":", SecondBin/binary, " UTC">>.

-spec pad_date_digits(binary()) -> binary().
pad_date_digits(Bin) ->
    pad_binary(2, <<"0">>, Bin).

-spec pad_binary(integer(), binary(), binary()) -> binary().
pad_binary(Length, _Byte, Bin) when byte_size(Bin) >= Length ->
    Bin;
pad_binary(Length, Byte, Bin) when byte_size(Bin) < Length ->
    pad_binary(Length, Byte, <<Byte/binary, Bin/binary>>).
