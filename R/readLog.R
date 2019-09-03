#' 读取网商的订单日志信息
#'
#' @param file  文件名,要求txt另存为utf-8格式
#'
#' @return 返回值
#' @import readr
#' @import tsdo
#' @export
#'
#' @examples
#' read_kflog('data-raw/input/2019-07-17.txt');
read_kflog <- function(file='data-raw/input/2019-07-17.txt') {
   #读取文件
  res <- read_csv(file=file,
                  col_names = FALSE, trim_ws = FALSE);
  #转为df
  res <- tbl_as_df(res);
  #重命名为FkfLog
  names(res) <-'FkfLog'
  return(res);


}
#' 读取日志文件
#'
#' @param file 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' read_kflog2();
read_kflog2 <- function(file='data-raw/input/2019-07-17.txt') {
  data <-read_kflog(file);

  log <- data$FkfLog;
  #' 针对数据进行删除处理，不包括------
  log <- log_delBash(log);
  # 针对日志进行分解，分为2列，日志内容及作者时间
  log_split <- str_splitByRighttBrace(x = log,suffix = ": ");
  authorTime <- log_split[[1]];
  logContent <- log_split[[2]];

  #分离作者与datetime信息
  aut_datetime <- str_splitByLeftBrace(authorTime);
  aut_id <- aut_datetime[[1]];
  dlg_datetime <- aut_datetime[[2]];

  res <- data.frame(aut_id,dlg_datetime,logContent,stringsAsFactors = F);

}

#' 批量处理客服日志文件
#'
#' @param files 多个文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' read_kflogs()
read_kflogs <- function(files){
  res<- lapply(files,read_kflog2);
  res <- do.call('rbind',res);
  return(res);
}



#' 在原始log文件中删除会话符号----
#'
#' @param x 原始数据向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_delBash();
log_delBash <- function(x){
  res <-x[left(x,6) != '------'];
  return(res);
}

#' 删除日志中的日期中的NA数据
#'
#' @param data  日期数据
#'
#' @return 删除dlg_datetime中的日期数据后行
#' @export
#'
#' @examples
#' log_delNA()
log_delNA <- function(data) {
  res <- data[!is.na(data$dlg_datetime), ];
  res <- res[!is.na(res$aut_id), ];
  return(res);
}


#' 删除日志中aut_id为空的记录
#'
#' @param data 日志数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_delAutId();
log_delAutId <- function(data) {

  res <- data[!is.na(data$aut_id),];
}


#' 删除数据中的左空格
#'
#' @param data  log数据
#' @param space_count  空格数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_delLeftSpace();
log_delLeftSpace <- function(data,space_count=1) {
  data$logContent <-mid(data$logContent,space_count+1,nchar(data$logContent)-space_count);
  return(data);

}

#' 针对数据进行处理，删除内容中的""
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_delContentNull();
log_delContentNull <- function(data) {
  data <- data[data$logContent !="",];
  return(data)

}


#' 删除aut_id长于30的字符
#'
#' @param data log数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_delAutLong30();
log_delAutLong30 <- function(data) {
  res <- data[len(data$aut_id) <=30,]
  return(res);

}


#' 删除日志中的固定内容
#'
#' @param data 日志数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_delFixAll();
log_delFixAll <- function(data){

  #删除NA
  res <-log_delNA(data);
  #删除aut_id
  res <-log_delAutId(res);
  #删除左边的空格
  res <- log_delLeftSpace(res);
  #删除内容中的空格
  res <-log_delContentNull(res);
  #删除地址混入aut_id的数据
  res <-log_delAutLong30(res);
  return(res);

}

#' 删除数据中完全匹配keys的行记录
#'
#' @param data 数据
#' @param keys  关键词
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_delRow_equals();
log_delRow_equals <- function(data,keys) {
  x <-data$logContent;
  con <- !str_equals(x,keys);
  res <- data[con,];
  res <- log_delAutId(res);
  return(res);
}
#' 删除日志中包含多个关键词的行
#'
#' @param data 日志数据
#' @param keys 多个关键词
#'
#' @return 返回数据
#' @export
#'
#' @examples
#' log_delRow_contains();
log_delRow_contains <- function(data,keys) {
  x <-data$logContent;
  con <- !str_contains(x,keys);
  res <- data[con,];
  res <- log_delAutId(res);
  return(res);
}

#' 形成问题列表
#'
#' @param data 数据
#' @param sep  分隔符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_qaList();
log_qaList <- function(data,sep=""){
  res$isA <- str_contain(res$aut_id,'捷豹路虎')
  res$dlg_date <- left(res$dlg_datetime,10);
  res$dlg_hms <-right(res$dlg_datetime,8);
  #增加对数据完整性的判断
  del_row <-del_aq(res$isA,len=10)+1;
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
  res <- lapply(res, log_combine_gp,sep=sep);

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



#' 形成问题列表
#'
#' @param data 数据
#' @param sep  分隔符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_qaList2();
log_qaList2 <- function(data,sep=""){
  res$isA <- str_contain(res$aut_id,'捷豹路虎')
  res$dlg_date <- left(res$dlg_datetime,10);
  res$dlg_hms <-right(res$dlg_datetime,8);
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
  res <- lapply(res, log_combine_gp,sep=sep);

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



#' 针对aq情况进行处理
#'
#' @param x 原始数据
#' @param len 取数长度
#'
#' @return 返回值
#' @export
#'
#' @examples
#' del_aq();
del_aq <- function(x,len=10){
  res <- 0
  if (len > length(x)){
    len <- length(x);
  }
  if (x[1] == FALSE){
    res <-0

  }else{
    x <-x[1:len];
    for (i in 1:len){
      if (x[i] == TRUE){
        res <- res + 1;
      }else{
        res <- res+ 0
        x[i:len] <- FALSE;
      }


  }



  }
  return(res);
}



