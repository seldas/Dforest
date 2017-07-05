#' Construct Decision Tree model with pruning
#'
#' @param X dataset
#' @param Y data_Labels
#' @return Decision Tree Model with pruning
#'   Implemented by rpart
#' @seealso \code{rpart}
#'
#' @import rpart
#' @export
#'

Con_DT = function (X,Y,min_leaf=10,cp=0.01){

  # cp controls the model performance to make sure the model is not over-fitting
  # t=proc.time()
  rc = rpart.control(minbucket = min_leaf, minsplot = min_leaf*3, xval = 1, cp=cp)
  DT_Model <- rpart(Y ~ ., method="class", data=X, control = rc)
  # proc.time()-t
  # DT_Model$cptable
  # cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

  # DT_Model <- prune(fit,cp=cp)

  return(DT_Model)

}

#' Doing Prediction with Decision Tree model
#'
#' @source rpart
#' @param X dataset
#' @param model Decision Tree Model
#' @return Decision Tree Predictions
#'   Different endpoints presented in multiple columns
#' @seealso \code{rpart}
#'
#' @importFrom stats predict
#'
#' @export
#'

Pred_DT = function (model,X){

  Label=predict(model,X)

  return(Label)

}

#' Performance evaluation from Decision Tree Predictions
#'
#' @param pred Predictions
#' @param label Known-endpoint
#' @return result$ACC:   Predicting Accuracy
#' @return result$MIS:   MisClassfication Counts
#' @return result$MCC:   Matthew's Correlation Coefficients
#' @return result$bACC:  balanced Accuracy
#'
#' @export
#'
#'

DF_acc= function (pred,label){
  Pred_label = colnames(pred)[max.col(pred,ties.method = "first")]
  True_prediction = length(which(Pred_label==label))

  # MIS = length(label)-True_prediction
  ACC = round(True_prediction / length(label),3)

  label_level = levels(factor(label))

  accuracy_sep = matrix(0,nrow=length(label_level),ncol=1)
  for (i in 1:length(label_level)){
    accuracy_sep[i,1]=length(which(Pred_label==label & label == label_level[i]))/length(which(label == label_level[i]))
  }
  bACC = mean(accuracy_sep)

  if (length(levels(factor(label)))==2){
    TP = length(which(Pred_label==label & Pred_label ==label_level[1]))
    TN = length(which(Pred_label==label & Pred_label ==label_level[2]))
    FP = length(which(Pred_label!=label & Pred_label ==label_level[1]))
    FN = length(which(Pred_label!=label & Pred_label ==label_level[2]))

    MCC=((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  }else{
    MCC="Not Available for multi-class problem" # Not Available for multi-class prediction
  }
  result = list(ACC=ACC,MCC=MCC,bACC=bACC)
  return(result)
}

#' performance evaluation between two factors
#'
#' @param pred Predictions
#' @param label Known-endpoint
#' @return result$ACC:   Predicting Accuracy
#' @return result$MIS:   MisClassfication Counts
#' @return result$MCC:   Matthew's Correlation Coefficients
#' @return result$bACC:  balanced Accuracy
#'
#' @export
#'
#'

DF_perf= function (pred,label){
  Pred_label = pred
  True_prediction = length(which(Pred_label==label))

  MIS = length(label)-True_prediction
  ACC = round(True_prediction / length(label),3)

  label_level = levels(factor(label))

  accuracy_sep = matrix(0,nrow=length(label_level),ncol=1)
  for (i in 1:length(label_level)){
    accuracy_sep[i,1]=length(which(Pred_label==label & label == label_level[i]))/length(which(label == label_level[i]))
  }
  bACC = mean(accuracy_sep)

  if (length(levels(factor(label)))==2){
    TP = length(which(Pred_label==label & Pred_label ==label_level[1]))
    TN = length(which(Pred_label==label & Pred_label ==label_level[2]))
    FP = length(which(Pred_label!=label & Pred_label ==label_level[1]))
    FN = length(which(Pred_label!=label & Pred_label ==label_level[2]))

    MCC=((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  }else{
    MCC="Not Available for multi-class problem" # Not Available for multi-class prediction
  }
  result = list(ACC=ACC,MIS=MIS,MCC=MCC,bACC=bACC)
  return(result)
}



#' T-test for feature selection
#'
#'
#' @param X X variable matrix
#' @param Y Y label
#'
#' @importFrom stats t.test
#' @importFrom methods is
#'
DF_calp = function(X, Y){
  Y_lab = levels(Y)
  y1 = which(Y==Y_lab[1])
  y2 = which(Y==Y_lab[2])
  pV = apply(X, 2,
             function(x){ tt <- try(t.test(x=x[y1], y=x[y2]), silent=TRUE ); if(is(tt, "try-error")) 1 else tt$p.value});
  return(pV)
}

