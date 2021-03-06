#' @title Decision Forest algorithm: Model prediction
#' @description Decision Forest algorithm: Model prediction with constructed DF models.
#'   DT_models is a list of Decision Tree models (rpart.objects) generated by DF_train()
#'   DT_CV() is only designed for Cross-validation and won't generate models
#'
#'
#' @param DT_models Constructed DF models
#' @param X Test Dataset
#' @param Y Test data endpoint
#'
#' @return  .$accuracy:      Overall test accuracy
#' @return  .$predictions:    Detailed test prediction
#'
#' @importFrom stats predict
#'
#' @export
#'
#' @examples
#'   # data(demo_simple)
#'   X = data_dili$X
#'   Y = data_dili$Y
#'   names(Y)=rownames(X)
#'
#'   random_seq=sample(nrow(X))
#'   split_rate=3
#'   split_sample = suppressWarnings(split(random_seq,1:split_rate))
#'   Train_X = X[-random_seq[split_sample[[1]]],]
#'   Train_Y = Y[-random_seq[split_sample[[1]]]]
#'   Test_X = X[random_seq[split_sample[[1]]],]
#'   Test_Y = Y[random_seq[split_sample[[1]]]]
#'
#'   used_model = DF_train(Train_X, Train_Y)
#'   Pred_result = DF_pred(used_model,Test_X,Test_Y)
#'
#'
#'
DF_pred = function(DT_models,X,Y=NULL){
  if (length(DT_models$models)>0){
    label_num = dim(DT_models$pred)[2]
    DT_models = DT_models$models

    Pred_result=list()

    acc_total=matrix(0,nrow=nrow(X),ncol=label_num)
    for (i in 1:length(DT_models)){
      Pred_result[[i]] = predict(DT_models[[i]],X)
      tmp_pred = Pred_result[[i]]
      acc_total= acc_total+tmp_pred
    }
    pred = colnames(acc_total)[max.col(acc_total,ties.method = "first")]
    pred_conf = apply(acc_total,1,max)/length(DT_models)
    CV_bind = cbind(pred,pred_conf)
    rownames(CV_bind)=rownames(X)
    if (is.null(Y)){
      performance = "Not available without known labels!"
    }else{
      performance = DF_perf(pred,Y)
    }
    result = list(performance=performance, predictions=CV_bind)
  }else{
    cat("No valid model was constructed! please Check your model variable \n")
    result = list()
    result$performance = NA
    result$predictions = NA
  }
  return(result)
}
