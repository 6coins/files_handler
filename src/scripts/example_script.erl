%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright
%%% @doc
%%%     一个简单脚本的例子,并在files_handler_tests模块的测试函数中使用
%%% @end
-module(example_script).

-export([filename_fun/1, 
         is_start_fun1/1, 
         is_start_fun2/1, 
         is_end_fun1/1, 
         is_end_fun2/1, 
         content_fun1/1, 
         content_fun2/1]).


%% 处理文件路径,自己扩展的"文件数据"键值对列表
filename_fun(Filename) ->
    [{other, string:substr(Filename, length(Filename)-4, 1)}].      % 返回文件名末尾的数字


%% 判断当前行是否是第1章开头的函数
is_start_fun1(Line) ->
    files_handler_util:is_cantain(Line, "chapter 1").               % 以"chapter 1"区分开头


%% 判断当前行是否是第2章开头的函数
is_start_fun2(Line) ->
    files_handler_util:is_cantain(Line, "chapter 2").               % 以"chapter 2"区分开头


%% 判断当前行是否是第1章结尾的函数
is_end_fun1(Line) ->
    files_handler_util:is_cantain(Line, "^\n$").                      % 如果当前处理的是第1章,且某一行只有换行,则是第1章结尾


%% 判断当前行是否是第2章结尾的函数
is_end_fun2(Line) ->
    files_handler_util:is_cantain(Line, "^\n$").                      % 如果当前处理的是第2章,且某一行只有换行,则是第2章结尾


%% 处理第1章每行内容时原样返回
content_fun1(Line) ->
    {[Line], []}.


%% 处理第2章每行内容时,非标题行才处理累加
content_fun2(Line) ->
    case files_handler_util:is_cantain(Line, "chapter 2") of    % 判断是否是标题
        true ->                                                 % 如果是
            {[], []};                                               % 不处理也不累加结果
        false ->                                                % 如果不是
            Line1 = files_handler_util:replace(Line, "\n", ""), % 除换行
            {[Line1], []}                                       % 内容原样返回也不累加结果
    end.


