%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright
%%% @doc
%%%     脚本模块
%%% @end
-module(faq_script).

-export([is_start_fun/1, 
         is_end_fun/1, 
         content_fun/1]).


%% 判断当前行是否是章开头的函数
is_start_fun(Line) ->
    files_handler_util:is_cantain(Line, "^####").                   % 以"####"区分开头


%% 判断当前行是否是章结尾的函数
is_end_fun(Line) ->
    files_handler_util:is_cantain(Line, "^####").                   % 以"####"区分结尾


%% 处理章每行内容时,非标题行才处理累加
content_fun(Line) ->
    case files_handler_util:is_cantain(Line, "^####") of        % 判断是否是开头
        true ->                                                 % 如果是
            {"", []};                                               % 不累加结果
        false ->                                                % 如果不是
            {Line, []}                                              % 内容原样返累加结果
    end.


