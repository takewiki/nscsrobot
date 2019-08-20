#' 读取日志文件并进行处理，主函数
#'
#' @param filter_equal 传入精确匹配
#' @param filter_like  传入模糊匹配
#' @param file 日志文件
#'
#' @return 返回数据框
#' @import dplyr
#' @export
#'
#' @examples
#' dealLog();
dealLog <- function(file='data-raw/input/2019-07-17.txt',
                    filter_equal,filter_like) {
  data <-read_kflog(file = file);
  log <- data$FkfLog;
  #' 针对数据进行删除处理，不包括------
  log <- log_delBash(log);
  # 针对日志进行分解，分为2列，日志内容及作者时间
  log_split <- str_splitByRighttBrace(x = log,suffix = ":");
  authorTime <- log_split[[1]];
  logContent <- log_split[[2]];

  #分离作者与datetime信息
  aut_datetime <- str_splitByLeftBrace(authorTime);
  aut_id <- aut_datetime[[1]];
  dlg_datetime <- aut_datetime[[2]];

  res <- data.frame(aut_id,dlg_datetime,logContent,stringsAsFactors = F);
  #View(res);
  #处理异常数据，删除日期为NA为的记录
  res <- res %>% log_delNA();
  #删除所有aut_id为空的记录
  res <- res[!is.na(res$aut_id),];
  #删除所有聊天记录中完全匹配的字段
  #如果是模糊匹配，请使用log_delRow_contains();
  #keys_equal <-c('亲',"  亲","  好的","  ","[卡片]","  [卡片]", "  [语音]"  );
  keys_equal <-filter_equal;

  res <-res %>% log_delRow_equals(keys_equal);
  #删除NA异常数据
  res <- res[!is.na(res$aut_id),];

  #删除模糊匹配的记录
  #keys_contains <-c('https://','http')
  keys_contains <- filter_like
  res <- res %>% log_delRow_contains(keys_contains);
  res <- res[!is.na(res$aut_id),];
  #
  # View(res);

  #删除ID异常记录,
  #目前发现地址信息混合到ID中
  res <- res[len(res$aut_id) <=30,]

  res$isA <- str_contain(res$aut_id,'捷豹路虎')
  res$dlg_date <- left(res$dlg_datetime,10);
  res$dlg_hms <-right(res$dlg_datetime,8);
  del_row <-del_aq(res$isA)+1;
  all_row <-nrow(res);
  res <- res[del_row:all_row, ];
  #判断会话的分组
  res$gp_id <-res$aut_id;
  res$gp_id[res$isA ==TRUE] <- "";
  res$gp_id <- str_copyPrevRow(res$gp_id);
  res$session_id <- getSessionId(res$isA);
  #按列筛选数据
  res <- res %>% df_selectCol(c('dlg_date','gp_id','session_id','isA','logContent'))
  res$action_id <- res$session_id *2;
  res$action_id[res$isA == FALSE] <- res$action_id[res$isA == FALSE] -1;

  # 数据进行分组处理
  g <-res$action_id;
  res <- split(res,g);
  # 针对多行数据进行合并；
  res <- lapply(res, log_combine_gp);

  #将结合调整回一个数据框；
  res <- do.call("rbind",res);

  #将数据进一步进行处理
  #删除actionId
  res <- res[,c( "dlg_date","gp_id","session_id","isA","logContent")]
  res_q <- res[res$isA == FALSE,];
  res_q$question <- res_q$logContent;
  res_a <- res[res$isA == TRUE,];
  res_a$answer <- res_a$logContent;
  res <- df_innerJoin_bySameColNames(res_q,res_a,"session_id");
  res_name_sel <-c("dlg_date.x","gp_id.x","session_id","question","answer")
  res<-res[,res_name_sel];
  names(res) <-c("dlg_date","gp_id","session_id","question","answer")
  return(res);
}



#' 用于数据处理
#'
#' @param file 日志名
#' @param file_filter_equal  文件级精确匹配
#' @param file_filter_like   文件级近似匹配
#' @param question_filter_equal 问题精确匹配
#' @param question_filter_like  问题近似匹配
#' @param answer_filter_equal  答案精确匹配
#' @param answer_filter_like  答案近似匹配
#' @param row_sep 分隔符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' data <-dealLog2(file='data-raw/input/2019-07-26.txt',
#' file_filter_equal,
#' file_filter_like,
#' question_filter_equal,
#' question_filter_like,
#' answer_filter_equal,
#' answer_filter_like,
#' row_sep );
dealLog2 <- function(file='data-raw/input/2019-07-26.txt',
                     file_filter_equal=c('好'),
                     file_filter_like=c('http://'),
                     question_filter_equal=c('在吗'),
                     question_filter_like=c("在的"),
                     answer_filter_equal=c('不客气'),
                     answer_filter_like=c('不用谢'),
                     row_sep = "") {
  res <-read_kflog2(file = file);


  res <-log_delFixAll(res);


#   #文件级的精确匹配;
#
  res <- log_delRow_equals(res,file_filter_equal);
#   #View(res);
#
#   #文件级的模糊匹配
  res <- log_delRow_contains(res,file_filter_like);
#   # View(res);
#   #形成qa列表
  # with a bug
#  res <- log_qaList(res,sep=row_sep);

##qa start
  res$isA <- str_contain(res$aut_id,'捷豹路虎')
  res$dlg_date <- left(res$dlg_datetime,10);
  res$dlg_hms <-right(res$dlg_datetime,8);
  del_row <-del_aq(res$isA)+1;
  all_row <-nrow(res);
  res <- res[del_row:all_row, ];

  #判断会话的分组
  res$gp_id <-res$aut_id;
  res$gp_id[res$isA ==TRUE] <- "";
  res$gp_id <- str_copyPrevRow(res$gp_id);
  res$session_id <- getSessionId(res$isA);
  # #按列筛选数据
  res <- res %>% df_selectCol(c('dlg_date','gp_id','session_id','isA','logContent'))
  res$action_id <- res$session_id *2;
  res$action_id[res$isA == FALSE] <- res$action_id[res$isA == FALSE] -1;
  #
  # # 数据进行分组处理
  g <-res$action_id;
  res <- split(res,g);
  # # 针对多行数据进行合并；
  res <- lapply(res, log_combine_gp,sep=row_sep);
  #
  # #将结合调整回一个数据框；
  res <- do.call("rbind",res);
  #
  # #将数据进一步进行处理
  # #删除actionId
  res <- res[,c( "dlg_date","gp_id","session_id","isA","logContent")]
  res_q <- res[res$isA == FALSE,];
  res_q$question <- res_q$logContent;
  res_a <- res[res$isA == TRUE,];
  res_a$answer <- res_a$logContent;
  res <- df_innerJoin_bySameColNames(res_q,res_a,"session_id");
  res_name_sel <-c("dlg_date.x","gp_id.x","session_id","question","answer")
  res<-res[,res_name_sel];
  names(res) <-c("dlg_date","gp_id","session_id","question","answer")



##qa end




#   #匹配问题
  res <- question_delRow_equal(res,question_filter_equal);

#   #针对问题进行模糊匹配
  res <- question_delRow_contains(res,question_filter_like );

#   #按答案进行精确匹配
#
  res <- answer_delRow_equal(res,answer_filter_equal);

#   #按答案进行模糊匹配

  res <- answer_delRow_contains(res,answer_filter_like);

  return(res);


}


#' 批量处理日志文件
#'
#' @param files 日志目录
#' @param file_filter_equal  文件过滤器
#' @param file_filter_like    文件过滤器
#' @param question_filter_equal 问题过滤器
#' @param question_filter_like  问题过滤器
#' @param answer_filter_equal 答案过滤器
#' @param answer_filter_like 答案过滤器
#' @param row_sep 分隔符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dealLog_dir();
dealLog_dir <- function(files='data-raw/input',
                        file_filter_equal=c('好'),
                        file_filter_like=c('http://'),
                        question_filter_equal=c('在吗'),
                        question_filter_like=c("在的"),
                        answer_filter_equal=c('不客气'),
                        answer_filter_like=c('不用谢'),
                        row_sep = "") {
files <-dir(files,full.names = TRUE);
res <-lapply(files, dealLog2,file_filter_equal=file_filter_equal,
             file_filter_like=file_filter_like,
             question_filter_equal=question_filter_equal,
             question_filter_like=question_filter_like,
             answer_filter_equal=answer_filter_equal,
             answer_filter_like=answer_filter_like,
             row_sep=row_sep
             )
res <- do.call('rbind',res);
return(res);

}


