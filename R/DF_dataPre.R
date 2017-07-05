#' @title Decision Forest algorithm: Data pre-processing
#' @description Decision Forest algorithm: Data pre-processing, remove All-Zero columns/features and high correlated features
#'
#'
#' @param X Training Dataset
#' @param thres Correlation Coefficient threshold to filter out high correlated features; default is 0.95
#'
#' @importFrom stats cor
#'
#' @return Keep_feat: qualified features in data matrix after filtering
#' @export
#'
#'

DF_dataPre = function (X, Y=NULL, thres=0.95){
  zero_col = which(apply(X,2,function (x) {length(which(x==0))})==nrow(X))
  if (length(zero_col>0)){
    X_fil = X[,-zero_col]
  }else{
    X_fil = X
  }
  cor_result = cor(X_fil)
  High_cor_pair=which(cor_result>thres, arr.ind=T)
  self_comp = which(High_cor_pair[,1]==High_cor_pair[,2])
  uniq_pair=High_cor_pair[-self_comp,]
  keep_feat=c()
  filter_feat = c()
  for (i in 1:nrow(uniq_pair)){
    if (length(which(keep_feat==uniq_pair[i,2]))==0 & length(which(filter_feat==uniq_pair[i,2]))==0){
      keep_feat = cbind(keep_feat,uniq_pair[i,2])
    }
    if (length(which(filter_feat==uniq_pair[i,1]))==0 & length(which(keep_feat==uniq_pair[i,1]))==0){
      filter_feat = cbind(filter_feat,uniq_pair[i,1])
    }
  }
  if (is.null(names(zero_col))){
    convert_col = 1:ncol(X)
    convert_col = convert_col[-zero_col]
    filtered_feat = c(zero_col,convert_col[filter_feat])
    keep_feat =  1:ncol(X)
    keep_feat = keep_feat[-filtered_feat]
  }else{
    filtered_feat = c(names(zero_col),colnames(X_fil[,filter_feat]))
    keep_feat = colnames(X)
    keep_feat = keep_feat[!keep_feat %in% filtered_feat]
  }
  return(keep_feat)
}
