%%%-------------------------------------------------------------------
%% @doc rinseweb answer
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_answer).

%% API
-export([new/3]).
-export([new_numeric/3]).
-export([new_text/3]).
-export([new_shrug/2]).

%% Types
-type answer() :: #{
    type := type(),
    source := source(),
    answer => result()
}.
-type type() :: conversion_result | definition | hash | number | redirect | shrug | text | wiki.
-type source() :: atom().
-type result() :: any().

-export_type([source/0]).

%%====================================================================
%% API
%%====================================================================

-spec new(type(), source(), result()) -> answer().
new(Type, Source, Custom) ->
    #{
        type => Type,
        source => Source,
        answer => Custom
    }.

-spec new_numeric(type(), source(), number()) -> answer().
new_numeric(Type, Source, Num) ->
    AnswerCustom = #{
        number => Num
    },
    new(Type, Source, AnswerCustom).

-spec new_text(type(), source(), binary()) -> answer().
new_text(Type, Source, Bin) ->
    AnswerCustom = #{
        text => Bin
    },
    new(Type, Source, AnswerCustom).

-spec new_shrug(source(), binary()) -> answer().
new_shrug(Source, Text) ->
    #{
        type => shrug,
        source => Source,
        answer => Text
    }.
