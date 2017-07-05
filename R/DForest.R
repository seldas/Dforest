#' Demo script to lean Decision Forest package
#'   Demo data are located in data/ folder
#'
#'
#' @author Leihong.Wu
#'
#' @export

# Load Data
Dforest = function (){
	cat("##############Introduction: #################\n")
  cat(paste("Decision Forest (DF) combines the results of multiple heterogeneous",
        "but comparable decision tree (DT) models",
        "to produce a consensus prediction. \n\n",sep=""))
  cat("################  Usage: ####################\n")
  cat("0. Data pre-preparation: (filter All-Zeros and Highly Correlated Features) \n")
  cat("\tKeep_feature_set = DF_dataPre(X)\n")
  cat("\tX = X[,Keep_feature_set] \n")
  cat("1. Simplest situation: \n")
  cat("\tTraining: model = DF_train(Train_X, Train_Y) \n")
  cat("\tTesting: Pred_result = DF_pred(model, Test_X, Test_Y) \n")
  cat("2. Cross-validation:\n")
  cat("\tCross-validation within dataset: CV_result = DF_train_CV(Train_X, Train_Y, CV_fold=5) # 5-fold cross-validation \n")
  cat("3. Performance evaluation: \n")
  cat("\tFor training model :  DF_Trainsummary(used_model) \n")
  cat("\tFor Cross-validation: DF_CVsummary(CV_result) \n")
  cat("\tConfidence Plot (for prediction result): DF_ConfPlot(Pred_result, Label) \n")
  cat("\n")
  cat("################  Demos: ####################\n")
  cat("demo(\"Simple_demo\", package = \"Dforest\" ) \n")
}
