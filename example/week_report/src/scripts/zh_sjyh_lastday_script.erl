%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     华夏巡检周报生成脚本模块
%%% @end
-module(zh_sjyh_lastday_script).

-export([filename_fun/1, 
         is_start_fun1/1, 
         is_end_fun1/1, 
         content_fun1/1
        ]).




%%%===================================================================
%%% API
%%%===================================================================


%% 处理文件路径,自己扩展的"文件数据"键值对列表
filename_fun(Filename) ->
    Basename = filename:basename(Filename),                                         % 得到文件名
    case re:split(Basename, "(\\d{8})", [{return, list}]) of                        % 根据日期把文件名切分成"前缀","日期","其余部分"
        [_Prefix, Date, _] ->                                                       % 如果切分成功
            [{date, files_handler_util:get_before_day(Date)}];                      % 返回日期(由于文件名中的日期是之后的一天,所以日期要前移一天)
        _ ->                                                                        % 如果切分失败
            []                                                                      % 返回空
    end.


%% 判断当前行是否是第1章开头的函数
is_start_fun1(Line) ->
    files_handler_util:is_cantain(Line, "（一）昨日相关数据统计").


%% 判断当前行是否是第1章结尾的函数
is_end_fun1(Line) ->
    files_handler_util:is_cantain(Line, "（二）每周交易类型").


%% 处理第1章
content_fun1(Line) ->
    case files_handler_util:is_cantain(Line, 
        "昨日相关数据统计|SQL_ID|NUMBER" % 包含这些字符串
        ++ "|^\n$"               % 或空行
    ) of                                                % 判断是否是4种不累加的行内容
        true ->                                         % 如是
            {[], []};                                       % 不累加
        false->                                         % 如不
            {[handle_line1(Line)], []}                       % 初步处理并累加
    end.


% 初步处理1章行数据
handle_line1(Line) ->
    Line1 = files_handler_util:replace(Line, "\n", ""),         % 除换行
    Line2 = files_handler_util:replace(Line1, " ", ""),         % 除空格
    Line2.


