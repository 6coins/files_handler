%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 2014 duanzhichao
%%% @doc
%%%     files_handler_util模块的测试函数           
%%% @end

-module(files_handler_util_tests).


-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% fold_file/3的测试函数
%% @end
%%--------------------------------------------------------------------
fold_file_test() ->
    Filename = "tmp_fold_file_test.txt",                                            % 用于测试的文件名
    file:write_file(Filename, ["1aa\n","2bb\n","3cc\n"]),                           % 创建一个文件并写入3行数据用于测试
    Fun =fun("1" ++ Other, AccIn) -> [Other | AccIn]; 
            ("3" ++ Other, AccIn) -> [Other | AccIn];
            (_, AccIn) -> AccIn
    end,                                                                            % 定义每行的处理函数(即如果一行开头是"1"或"3"就记录其余的内容)
    Acc0 = [],                                                                      % 初始的累加器为空类表
    ["cc\n", "aa\n"] = files_handler_util:fold_file(Fun, Acc0, Filename),           % 调用要测试的函数,断言结果
    ok = file:delete(Filename),                                                     % 清理测试生成的文件
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% is_cantain/2的测试函数
%% @end
%%--------------------------------------------------------------------
is_cantain_test() ->
    true = files_handler_util:is_cantain("你好哈哈", "你好"),   % 断言
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% replace/3的测试函数
%% @end
%%--------------------------------------------------------------------
replace_test() ->
    "我好哈哈我好" = files_handler_util:replace("你好哈哈你好", "你好", "我好"),   % 断言
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% split/3的测试函数
%% @end
%%--------------------------------------------------------------------
split_test() ->
    %["Qq:123 ", "Rr1:456 Rr2:789 ", "Aa:000"] = files_handler_util:split("Qq:123 Rr1:456 Rr2:789 Aa:000", ["Rr", "Aa"]),   % 断言
    ["问题:Q1 ", "原因:R1 原因:R2 ", "解决:A1"] = files_handler_util:split("问题:Q1 原因:R1 原因:R2 解决:A1", ["原因", "解决"]),   % 断言
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% proplists_update/2的测试函数
%% @end
%%--------------------------------------------------------------------
proplists_update_test() ->
    Proplist1 = [{a, 2}, {b, 2}, {d, 2}],
    Proplist2 = [{a, 1}, {b, 1}, {c, 1}],
    Proplist3 = files_handler_util:proplists_update(Proplist1, Proplist2),
    [{a, 2}, {b, 2}, {d, 2}, {c, 1}] = Proplist3,               % 断言
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% string_to_date/1和date_to_string/1的测试
%% @end
%%--------------------------------------------------------------------
string_to_date_and_date_to_string_test() ->
    String = "2017.12.06",
    Date = {2017, 12, 6},
    Date = files_handler_util:string_to_date(String, "."),  % 断言
    String = files_handler_util:date_to_string(Date, "."),  % 断言
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% day_of_the_week/1的测试函数
%% @end
%%--------------------------------------------------------------------
day_of_the_week_test() ->
    "周三" = files_handler_util:day_of_the_week({2017,12,06}).   % 断言


%%--------------------------------------------------------------------
%% @private
%% @doc
%% get_before_day/1的测试函数
%% @end
%%--------------------------------------------------------------------
get_before_day_test() ->
    "2017.12.05" = files_handler_util:get_before_day("20171206").   % 断言



