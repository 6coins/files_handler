%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     程序负责
%%%         文本文件转成erlang数据
%%% @end
%%%-------------------------------------------------------------------
-module(file_handler).

-export([handle_file_data/2]).
-include("files_handler.hrl").


%% 调试用
%-compile(export_all).



%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 处理"文件数据"
%% @end
-spec handle_file_data(File_data1 :: files_handler:file_data(), 
                       files_handler:config()) 
    -> File_data2 :: files_handler:file_data().
%%--------------------------------------------------------------------
handle_file_data(File_data1, Config) ->          
    Filename = proplists:get_value(filename, File_data1),                           % 得到文件路径
    Fun = fun(Line, File_data_in) -> handle_line(Line, File_data_in, Config) end,   % 处理每行内容的函数
    File_data2 = files_handler_util:fold_file(Fun, File_data1, Filename),           % 处理每一行的内容
    _File_data3 = sort_chapters(File_data2).                                        % 排序"章数据"列表




%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理每行内容
%% @end
-spec handle_line(files_handler:line(), 
                  File_data_in :: files_handler:file_data(), 
                  files_handler:config()) 
    -> File_data_out :: files_handler:file_data().
%%--------------------------------------------------------------------
handle_line(Line, File_data_in, Config) -> 
    Info1 = proplists:get_value(handle_info, File_data_in, 
                                #handle_info{max_chapter_num = 0}),                 % 得到"处理的章信息"
    Info2 = count_handle_info(Info1, Line, Config),                                 % 计算计算"处理的章信息"(以便指定对应的处理函数)
    File_data2= handle_line_by_handle_info(Info2, Line, File_data_in, Config),      % 根据"处理的章信息"去处理当前行内容
    _File_data_out = files_handler_util:proplists_update([{handle_info, Info2}],
                                                     File_data2).                   % 将"处理的章信息"更新到"文件数据"(以便下次累加计算)


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 设置处理的章编号
%% @end
-spec count_handle_info(Info1 :: #handle_info{}, files_handler:line(), files_handler:config()) 
    -> Info2 :: #handle_info{}.
%%--------------------------------------------------------------------
count_handle_info(Info1, Line, Config) -> 
    Is_end = is_end(Info1, Line, Config),                                           % 判断"当前行内容"是否是"处理的章"的结尾
    Info2 = count_handle_info_by_is_end(Is_end, Info1),                             % 根据是否是章结尾计算"处理的章信息"
    Conf_num = get_start_chapter_conf_num(Line, Config),                            % 得到开始"章配置编号"
    _Info3 = count_handle_info_by_start_chapter_conf_num(Conf_num, Info2).          % 根据开始"章配置编号"计算"处理的章信息"


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据处理的章编号去处理当前行内容
%% @end
-spec handle_line_by_handle_info(#handle_info{}, 
                                 files_handler:line(), 
                                 File_data_in :: files_handler:file_data(), 
                                 files_handler:config()) 
    -> File_data_out :: files_handler:file_data().
%%--------------------------------------------------------------------
handle_line_by_handle_info(#handle_info{chapter_conf_num = undefined},
                           _Line, File_data1, _Config)  ->                          % 如果"正在处理的章配置编号"为undefined(表示当前没处理章)
    File_data1;                                                                         % 这什么也不做,直接返回"文件数据"
handle_line_by_handle_info(#handle_info{chapter_num = Num, 
                                        chapter_conf_num = Conf_num},
                           Line, File_data1, Config) ->                             % 否则(即当前有正在处理章)
    Content_datas1 = get_value_of_chapter(Num, content_datas, [], 
                                                File_data1),                            % 得到内容列表(如果没有默认空列表)
    {Content_data_list, Chapter_kvs} = apply_chapter_conf_fun(Conf_num, 
        content_fun, [Line], Config),                                                   % 用内容处理函数处理当前行
    Content_datas2 = Content_datas1 ++ Content_data_list,                               % 当前行处理结果累加到内容列表
    _File_data_out = update_chapter(Num, 
        [{content_datas, Content_datas2}| Chapter_kvs], File_data1).                    % 将处理结果更新到"章数据"


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 判断"当前行内容"是否是"正在处理的章"的结尾
%% @end
-spec is_end(#handle_info{}, files_handler:line(), files_handler:config()) 
    -> boolean().
%%--------------------------------------------------------------------
is_end(#handle_info{chapter_conf_num = undefined}, _Line, _Config) ->                 % 如果"正在处理的章配置编号"为undefined(表示当前没处理章)
    false;                                                                              % 返回不是结尾
is_end(#handle_info{chapter_conf_num = Conf_num}, Line, Config) ->                  % 否则(即当前有正在处理章)
    apply_chapter_conf_fun(Conf_num, is_end_fun, [Line], Config).                       % 判断是否是结尾,返回结果


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据是否是章结尾计算处理的章信息
%% @end
-spec count_handle_info_by_is_end(boolean(), Info1 :: #handle_info{}) 
    -> Info2 :: #handle_info{}.
%%--------------------------------------------------------------------
count_handle_info_by_is_end(false = _Is_end, Info1) ->                              % 如果"正在处理的章"没有结尾
    Info1;                                                                              % 返回原"处理的章信息"
count_handle_info_by_is_end(true = _Is_end, 
                            #handle_info{chapter_num = Num}) ->                     % 如果"正在处理的章"结尾了
    #handle_info{max_chapter_num = Num,                                                 % 把"处理过的最大章编号"设为"正在处理的章编号"
                 chapter_conf_num = undefined,                                          % 再把"正在处理的章编号"和"正在处理的章配置编号"设为undefined(表示当前没处理章)
                 chapter_num = undefined}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 得到开始"章配置编号"
%% @end
-spec get_start_chapter_conf_num(files_handler:line(), files_handler:config()) 
    -> undefined | files_handler:chapter_conf_number().
%%--------------------------------------------------------------------
get_start_chapter_conf_num(Line, Config) ->
    Chapters_conf = proplists:get_value(chapters_conf, Config),                     % 得到"章数据"列表配置
    Pred = fun({_, Chapter_conf}) -> 
            {M, F} = proplists:get_value(is_start_fun, Chapter_conf),
            apply(M, F, [Line])
    end,                                                                            % 判断当前行是否是某一章开头的函数
    case lists:partition(Pred, Chapters_conf) of                                    % 把开始和没开始的章分开
        {[{Conf_num, _}], _Other} ->                                                % 如果得到一个开始的章
            Conf_num;                                                                   % 返回开始的章配置编号
        {[], _Other} ->                                                             % 如果没得到一个开始的章
            undefined;                                                                  % 返回undefined
        {Start_chapters_conf, _Other} ->                                            % 如果得到多个开始的章
            throw({"too many start chapters!!", Start_chapters_conf, Line, Config})     % 抛出错误
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据开始"章配置编号"计算"处理的章信息"
%% @end
-spec count_handle_info_by_start_chapter_conf_num(
        Conf_num :: undefined | files_handler:chapter_conf_number(), 
        Info1 :: #handle_info{}) 
    -> Info2 :: #handle_info{}.
%%--------------------------------------------------------------------
count_handle_info_by_start_chapter_conf_num(undefined = _Conf_num, Info) ->         % 如果没有开始"章配置编号"
    Info;                                                                               % 返回原"处理的章信息"
count_handle_info_by_start_chapter_conf_num(
            Conf_num, #handle_info{max_chapter_num = Max_num} = Info) ->            % 否则
    Info#handle_info{chapter_conf_num = Conf_num, chapter_num = Max_num + 1}.           % 更新"正在处理的章配置编号"和"正在处理的章编号"


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据键得到章的值
%% @end
-spec get_value_of_chapter(files_handler:chapter_number(), Key :: term(), 
                           Default :: term(), files_handler:file_data()) 
    -> Value :: term().
%%--------------------------------------------------------------------
get_value_of_chapter(Num, Key, Default, File_data) ->
    Chapters = proplists:get_value(chapters, File_data, []),                        % 得到"章数据"列表
    Chapter = proplists:get_value(Num, Chapters, []),                               % 得到处理的"章数据"
    _Value = proplists:get_value(Key, Chapter, Default).                            % 根据键得到值


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 更新文件数据中的处理的"章数据"
%% @end
-spec update_chapter(files_handler:chapter_number(), [proplists:property()], 
                     File_data_in :: files_handler:file_data()) 
    -> File_data_out :: files_handler:file_data().
%%--------------------------------------------------------------------
update_chapter(Num, Proplists, File_data) -> 
    Chapters = proplists:get_value(chapters, File_data, []),                        % 得到"章数据"列表
    Chapter = proplists:get_value(Num, Chapters, []),                               % 得到"章数据"
    Chapter1 = files_handler_util:proplists_update(Proplists , Chapter),            % 将处理结果更新到"章数据"
    Chapters1 = files_handler_util:proplists_update([{Num, Chapter1}], Chapters),   % 将"章数据"更新到"章数据"列表
    _File_data1 = files_handler_util:proplists_update([{chapters, Chapters1}], 
                                                     File_data).                    % 将"章数据"列表更新到"文件数据"


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据键得到章的配置的值
%% @end
-spec get_value_of_chapter_conf(files_handler:chapter_conf_number(), 
                                Key :: term(), 
                                Default :: term(), 
                                files_handler:config()) -> Value :: term().
%%--------------------------------------------------------------------
get_value_of_chapter_conf(Conf_num, Key, Default, Config) -> 
    Chapters_conf = proplists:get_value(chapters_conf, Config),                     % 得到"章数据"列表配置
    Chapter_conf = proplists:get_value(Conf_num, Chapters_conf),                    % 得到处理的"章数据"的配置
    _Value = proplists:get_value(Key, Chapter_conf, Default).                       % 根据键得到值


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据函数键,执行"处理的章的配置"的函数,得到结果
%% @end
-spec apply_chapter_conf_fun(files_handler:chapter_conf_number(), 
                             Fun_key :: atom(), 
                             Args :: term(), 
                             files_handler:config()) -> Result :: term().
%%--------------------------------------------------------------------
apply_chapter_conf_fun(Conf_num, Fun_key, Args, Config) -> 
    {M, F} =  get_value_of_chapter_conf(Conf_num, Fun_key, [], Config),             % 得到对应函数
    _Result = apply(M, F, Args).                                                    % 执行函数,返回结果


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 排序"章数据"列表
%% @end
-spec sort_chapters(File_data_in :: files_handler:file_data()) -> File_data_out :: files_handler:file_data().
%%--------------------------------------------------------------------
sort_chapters(File_data_in) -> 
    Chapters1 = proplists:get_value(chapters, File_data_in, []),                    % 得到"章数据"列表
    Chapters2 = lists:keysort(1, Chapters1),                                        % 排序"章数据"列表
    _File_data_out = files_handler_util:proplists_update(
                [{chapters, Chapters2}], File_data_in).                             % 将"章数据"列表更新到"文件"



