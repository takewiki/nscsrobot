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
  return(res);
}


