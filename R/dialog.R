#' 根据会话的模式进行处理会话ID
#'
#' @param x 模式识别的逻辑变量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getSessionId(F,F,T,T,F,T,T)
getSessionId <- function(x){
  # ncount <- length(x);
  # del_n<-del_aq(x)+1;
  # x <- x[del_n:ncount];

  if (x[1] == TRUE){
    stop("数据应该从FALSE开始",call. = F);
  }
  ncount <- length(x);
  #定义一个结果变量
  res <- numeric(ncount);
  res[1] <- 1;
  #定义一个指针
  i <- 1;
  while( i < ncount){
    # 只有TF这样的模式需要加1
    #否则维持原有的值
    if (x[i] == TRUE & x[i+1] == FALSE){
       res[i+1] <- res[i] +1 ;
    }else{
      res[i+1] <- res[i];
    }
    i <- i+1;
  }
  return(res);

}

#' 根据Id合并数据处理
#'
#' @param data  单项数据
#' @param sep 多行分隔符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_conbine_gp();
log_combine_gp <-function(data,sep=""){
  dlg_date <- unique(data$dlg_date);
  gp_id <- unique(data$gp_id);
  session_id <- unique(data$session_id);
  isA <- unique(data$isA);
  action_id <- unique(data$action_id);
  logContent <- paste(data$logContent,collapse = sep);
  res <- data.frame(dlg_date,gp_id,session_id,isA,action_id,logContent,stringsAsFactors = F);
  return(res);

}

#' 在记录结果中按问题删除精确匹配数据
#'
#' @param data 数据
#' @param question_filter_equal 问题精确匹配
#'
#' @return 返回值
#' @export
#'
#' @examples
#' question_delRow_equal();
 question_delRow_equal<- function(data,question_filter_equal="?") {
   x <-data$question;
   con <- !str_equals(x,question_filter_equal);
   res <- data[con,];
   #res <- log_delAutId(res);
   return(res);

 }

#' 针对问题清单进行模糊匹配
#'
#' @param data 数据
#' @param question_filter_like 过滤器
#'
#' @return 返回值
#' @export
#'
#' @examples
#' question_delRow_contains();
question_delRow_contains <- function(data,question_filter_like) {
   x <-data$question;
   con <- !str_contains(x,question_filter_like);
   res <- data[con,];

   return(res);
}


#' 在记录结果中按答案删除精确匹配数据
#'
#' @param data 数据
#' @param answer_filter_equal 问题精确匹配
#'
#' @return 返回值
#' @export
#'
#' @examples
#' answer_delRow_equal();
answer_delRow_equal<- function(data,answer_filter_equal="?") {
  x <-data$answer;
  con <- !str_equals(x,answer_filter_equal);
  res <- data[con,];
  #res <- log_delAutId(res);
  return(res);

}

#' 针对问题清单进行模糊匹配
#'
#' @param data 数据
#' @param answer_filter_like 过滤器
#'
#' @return 返回值
#' @export
#'
#' @examples
#' question_delRow_contains();
answer_delRow_contains <- function(data,answer_filter_like) {
  x <-data$answer;
  con <- !str_contains(x,answer_filter_like);
  res <- data[con,];

  return(res);
}
