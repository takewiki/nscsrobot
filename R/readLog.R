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


