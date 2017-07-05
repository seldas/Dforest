#' @title Decision Forest algorithm: Feature Selection in pre-processing
#' @description Decision Forest algorithm: feature selection for two-class predictions,
#'         kept statistical significant features pass the t-test
#'
#'
#' @param X Training Dataset
#' @param Y Training Labels
#' @param p_val Correlation Coefficient threshold to filter out high correlated features; default is 0.95
#'
#' @return Keep_feat: qualified features in data matrix after filtering
#' @export
#'
#'


DF_dataFs = function (X, Y, p_val=0.05){
  Y = factor(Y)
  if (length(levels(Y))!=2){
    stop("Not 2-class analysis! Cannot perform t-test for feature selection. Exit without changes ... \n")
    return (1:ncol(X))
  }else{
    cat(paste("Doing Feature selection based on Training dataset: p-value < ",p_val," \n",sep=""))
    p_value = DF_calp(X,Y)
    used_feat = which(p_value<=p_val)
    # X_new = X[,used_feat]
    cat(paste(length(used_feat)," of ",length(p_value)," features remained! \n",sep=""))
    return(used_feat)
  }
}
