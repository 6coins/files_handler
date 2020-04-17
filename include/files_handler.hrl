-record(handle_info, {                                           % 处理的章信息(用于关联files_handler:chapters()和files_handler:chapters_conf())
    chapter_conf_num :: files_handler:chapter_conf_number(),             % 正在处理的章配置编号
    chapter_num :: files_handler:chapter_number(),                       % 正在处理的章编号
    max_chapter_num :: files_handler:chapter_number()                    % 处理过的最大章编号
}).

