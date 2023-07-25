%%%-------------------------------------------------------------------
%% @doc rinseweb apply wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_apply).

%% API
-export([answer/3]).

%% Types
-type operator() :: atom().
-type operand() :: number().
-type result() :: number().

-define(ANSWER_SOURCE, apply).
-define(ANSWER_TYPE, number).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()], rinseweb_req:req()) -> rinseweb_answer:answer().
answer(_Question, [Operator, OperandsBin, _], _) ->
    Operands = binary:split(rinseweb_util:binary_trim(OperandsBin), <<" ">>, [trim_all, global]),
    Result = apply_op(parse_operator(Operator), parse_operands(Operands)),
    answer(Result).

%%====================================================================
%% Internal functions
%%====================================================================

-spec answer(result() | {error, binary()}) -> rinseweb_answer:answer().
answer({error, Reason}) -> rinseweb_answer:new_shrug(?ANSWER_SOURCE, Reason);
answer(Num) ->
    rinseweb_answer:new_numeric(?ANSWER_TYPE, ?ANSWER_SOURCE, Num).

-spec parse_operator(binary()) -> operator() | {error, binary()}.
parse_operator(<<"+">>) -> '+';
parse_operator(<<"-">>) -> '-';
parse_operator(<<"/">>) -> '/';
parse_operator(<<"*">>) -> '*';
parse_operator(_) -> {error, <<"Unknown operator">>}.

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

-spec apply_op(operator() | {error, binary()}, [operand()]) -> result() | {error, binary()}.
apply_op('+', Operands) -> lists:sum(Operands);
apply_op('-', [First|Rest]) -> apply_op_sub(First, Rest);
apply_op('*', [First|Rest]) -> apply_op_mul(First, Rest);
apply_op('/', [First|Rest]) -> apply_op_div(First, Rest);
apply_op({error, Reason}, _) -> {error, Reason}.

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
