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
  ncount <- length(x);

  if (x[1] == TRUE){
    stop("数据应该从FALSE开始",call. = F);
  }
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
#'
#' @return 返回值
#' @export
#'
#' @examples
#' log_conbine_gp();
log_combine_gp <-function(data){
  dlg_date <- unique(data$dlg_date);
  gp_id <- unique(data$gp_id);
  session_id <- unique(data$session_id);
  isA <- unique(data$isA);
  action_id <- unique(data$action_id);
  logContent <- paste(data$logContent,collapse = ";");
  res <- data.frame(dlg_date,gp_id,session_id,isA,action_id,logContent,stringsAsFactors = F);
  return(res);

}
