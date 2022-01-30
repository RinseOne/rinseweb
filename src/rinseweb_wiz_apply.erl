%%%-------------------------------------------------------------------
%% @doc rinseweb apply wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_apply).

%% API
-export([answer/2]).

%% Types
-type operator() :: atom().
-type operand() :: number().
-type result() :: number().

-define(ANSWER_SOURCE, apply).
-define(ANSWER_TYPE, number).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [Operator, OperandsBin, _]) ->
    Operands = binary:split(rinseweb_util:binary_trim(OperandsBin), <<" ">>, [trim_all, global]),
    Result = apply_op(parse_operator(Operator), parse_operands(Operands)),
    answer(Result).

%%====================================================================
%% Internal functions
%%====================================================================

-spec answer(result() | undefined) -> rinseweb_wiz:answer().
answer(undefined) -> rinseweb_wiz:shrug(?ANSWER_SOURCE);
answer(Num) ->
    rinseweb_wiz:answer_number(?ANSWER_TYPE, ?ANSWER_SOURCE, Num).

-spec parse_operator(binary()) -> operator().
parse_operator(<<"+">>) -> '+';
parse_operator(<<"-">>) -> '-';
parse_operator(<<"/">>) -> '/';
parse_operator(<<"*">>) -> '*'.

-spec parse_operands([binary()]) -> [operand()].
parse_operands(BinOpList) ->
    lists:reverse(parse_operands(BinOpList, [])).

-spec parse_operands([binary()], [number()]) -> [number()].
parse_operands([], Acc) -> Acc;
parse_operands([BinOp|Rest], Acc) ->
    parse_operands(Rest, [parse_operand(BinOp)|Acc]).

-spec parse_operand(binary()) -> number().
parse_operand(BinOp) ->
    rinseweb_util:round_precise(rinseweb_util:binary_to_number(BinOp)).

-spec apply_op(operator(), [operand()]) -> result() | undefined.
apply_op('+', Operands) -> lists:sum(Operands);
apply_op('-', [First|Rest]) -> apply_op_sub(First, Rest);
apply_op('*', [First|Rest]) -> apply_op_mul(First, Rest);
apply_op('/', [First|Rest]) -> apply_op_div(First, Rest);
apply_op(_, _) -> undefined.

-spec apply_op_sub(operand(), [operand()]) -> result().
apply_op_sub(Result, []) -> Result;
apply_op_sub(Result, [Operand|Rest]) ->
    apply_op_sub(Result - Operand, Rest).

-spec apply_op_mul(operand(), [operand()]) -> result().
apply_op_mul(Result, []) -> Result;
apply_op_mul(Result, [Operand|Rest]) ->
    apply_op_mul(Result * Operand, Rest).

-spec apply_op_div(operand(), [operand()]) -> result().
apply_op_div(Result, []) -> Result;
apply_op_div(Result, [Operand|Rest]) ->
    apply_op_div(Result / Operand, Rest).
