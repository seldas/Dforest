#' @title Decision Forest algorithm: Model training
#' @description Decision Forest algorithm: Model training
#'
#'
#' @param X Training Dataset
#' @param Y Training data endpoint
#' @param stop_step How many extra step would be processed when performance not improved, 1 means one extra step
#' @param Max_tree Maximum tree number in Forest
#' @param Discard All: Discard all used features in previous tree; One: Only discard the first feature in prevouos tree; N (integer): Discard first N features
#' @param cp parameters to pruning decision tree, default is 0.1
#' @param Filter doing feature selection before training
#' @param p_val P-value threshold measured by t-test used in feature selection, default is 0.05
#' @param Method Which is used for evaluating training process. MIS: Misclassification rate; ACC: accuracy
#' @param Quiet if TRUE (default), don't show any message during the process
#' @param Grace_ACC Grace Value in evaluation: the next model should have a performance (Accuracy) not bad than previous model with threshold
#' @param imp_ACC_accu improvement in evaluation: adding new tree should improve the overall model performance (accuracy) by threshold
#' @param Grace_bACC Grace Value in evaluation: (Balanced Accuracy)
#' @param imp_bACC_accu improvement in evaluation: (Balanced Accuracy)
#' @param Grace_MCC Grace Value in evaluation: (MCC)
#' @param imp_MCC_accu improvement in evaluation: (MCC)
#' @param Grace_MIS Grace Value in evaluation: (MIS)
#' @param imp_MIS_accu improvement in evaluation: (MIS)
#'
#' @return  .$accuracy:     Overall training accuracy
#' @return  .$pred:         Detailed training prediction (fitting)
#' @return  .$detail:       Detailed usage of Decision tree Features/Models and their performances
#' @return  .$models:       Constructed (list of) Decision tree models
#'
#' @export
#'
#' @examples
#'   ##data(iris)
#'   X = iris[,1:4]
#'   Y = iris[,5]
#'   names(Y)=rownames(X)
#'   used_model = DF_train(X,Y,stop_step=4, Method = "MCC")
#'
DF_train = function (X,Y,stop_step=2,
                     Max_tree=20,Discard='All',cp = 0.1,
                     Filter = F , p_val = 0.05, Method = "bACC", Quiet = T,
                     Grace_ACC = 0.05, imp_ACC_accu = 0.01,
                     Grace_bACC = 0.05, imp_bACC_accu = 0.01,
                     Grace_MCC = 0.05, imp_MCC_accu = 0.01,
                     Grace_MIS = ceiling(0.05*length(Y)), imp_MIS_accu = ceiling(0.01*length(Y)) ){
  Y =factor(Y)
  if (length(levels(Y))!=2 & Filter == T){
    if (Quiet == F){
      cat("Not 2-class analysis! ignore Filtering step ... \n")
    }
    Filter = F
  }

  if (Filter == T) {
    if (Quiet == F){
      cat(paste("Doing Feature selection based on Training dataset: p-value < ",p_val," \n",sep=""))
    }
    p_value = DF_calp(X,Y)
    used_feat = which(p_value<=p_val)
    X = X[,used_feat]
    cat(paste(length(used_feat)," of ",length(p_value)," features remained! \n",sep=""))
  }

  Train_data = X
  Valid_data = X # Fitting
  Train_label = Y
  Valid_label = Y # Fitting

  # For reference build, cp cut-off was set to 0.01 w/o pruning
  np_model = Con_DT(Train_data,Train_label,cp=0.01)
  # cp - first split
  ref_cp = np_model$cptable[1,'CP']
  if (cp > ref_cp){
    cp = round(ref_cp,2)
    if (cp > ref_cp) {
      cp = ref_cp - 0.01
    }
    cat(paste("Current CP setting is too large for the model, reset cp to most-available value: ",round(cp,2),"\n",sep=""))
  }

  # Calculating MIS
  model = Con_DT(Train_data,Train_label,cp=cp)
  pred  = Pred_DT(model, Valid_data)
  ref_result = DF_acc(pred,Valid_label)
  ref_MIS = ref_result$MIS
  ref_ACC = ref_result$ACC
  ref_bACC = ref_result$bACC
  ref_MCC = ref_result$MCC

  # start training
  Curr_tree = 0

  Tree_model = matrix(list(),nrow=0, ncol=9);
  colnames(Tree_model)=c("Used Feature","Accuracy","Balanced ACC","MCC",
                         "MisMatch","Reference val","Accumulate Performance",
                         "Stage","Predictions")

  models = list()

  stop_signal = 0
  curr_MIS=0
  ACC_accu=0
  bACC_accu=0
  MCC_accu=0
  MIS_accu=length(Y)
  used_feat = colnames(Train_data)
  pred_accumulate=matrix(0,nrow=length(Y),ncol=length(table(Y)))

  while(Curr_tree<Max_tree & stop_signal<stop_step & length(used_feat)>1 ){
      model = Con_DT(Train_data[,used_feat],Train_label)
      pred  = Pred_DT(model, Valid_data[,used_feat])
      feat_used  = as.matrix(unique(model$frame$var))
      feat_used  = feat_used[feat_used!="<leaf>",]
      if (Discard == "All"){
        used_var = feat_used
      }else if (Discard == "One"){
        used_var = feat_used[1]
      }else if (is.numeric(Discard)){
        used_var = feat_used[1:round(Discard)]
      }else{
        if (Quiet == F){
          cat("Discard parameter is not correctly defined! Use ALL in default \n")
        }
        Discard = "All"
        used_var = feat_used
      }

      tmp_result = DF_acc(pred,Valid_label)
      curr_ACC = tmp_result$ACC
      curr_bACC = tmp_result$bACC
      curr_MIS = tmp_result$MIS
      curr_MCC = tmp_result$MCC

      tmp_pred_accu = pred_accumulate
      tmp_pred_accu = tmp_pred_accu + pred
      tmp_result_accu = DF_acc(tmp_pred_accu,Valid_label)
      curr_ACC_accu = tmp_result_accu$ACC
      curr_bACC_accu = tmp_result_accu$bACC
      curr_MIS_accu = tmp_result_accu$MIS
      curr_MCC_accu = tmp_result_accu$MCC

      if (Method == "MCC" & length(levels(Y))!=2){
        if (Quiet == F){
          cat("Not 2-Class analysis! Use ACC in default ...")
        }
        Method = "bACC"
      }

      if (Method == "MIS" & curr_MIS <= ref_MIS + Grace_MIS & curr_MIS_accu <= MIS_accu - imp_MIS_accu ){
        Status="Improved"
      }else if (Method == "ACC" & curr_ACC >= ref_ACC - Grace_ACC & curr_ACC_accu >= ACC_accu + imp_ACC_accu){
        Status="Improved"
      }else if (Method == "bACC" & curr_bACC >= ref_bACC - Grace_bACC & curr_bACC_accu >= bACC_accu + imp_bACC_accu){
        Status="Improved"
      }else if (Method == "MCC" & curr_MCC >= ref_MCC - Grace_MCC & curr_MCC_accu >= MCC_accu + imp_MCC_accu){
        Status="Improved"
      }else{
        Status="Stop"
        stop_signal = stop_signal + 1
      }

      # write updated outputs
      Used_Tree_model = matrix(list(),nrow=1, ncol=9); # template for final result
      Used_Tree_model[[1,1]] = feat_used
      Used_Tree_model[[1,2]] = curr_ACC
      Used_Tree_model[[1,3]] = curr_bACC
      Used_Tree_model[[1,4]] = curr_MCC
      Used_Tree_model[[1,5]] = curr_MIS
      if (Method == "MIS"){
        Used_Tree_model[[1,6]] = ref_MIS
        Used_Tree_model[[1,7]] = curr_MIS_accu
      }else if (Method == "ACC"){
        Used_Tree_model[[1,6]] = ref_ACC
        Used_Tree_model[[1,7]] = curr_ACC_accu
      }else if (Method == "bACC"){
        Used_Tree_model[[1,6]] = ref_bACC
        Used_Tree_model[[1,7]] = curr_bACC_accu
      }else if (Method == "MCC"){
        Used_Tree_model[[1,6]] = ref_MCC
        Used_Tree_model[[1,7]] = curr_MCC_accu
      }
      Used_Tree_model[[1,8]] = "Train Fitting" # label for performance recognization
      Used_Tree_model[[1,9]] = pred

      if (Status == "Improved"){
        Curr_tree =Curr_tree +1
        stop_signal = 0
        # update criteria
        ACC_accu = curr_ACC_accu
        bACC_accu = curr_bACC_accu
        MIS_accu = curr_MIS_accu
        MCC_accu = curr_MCC_accu
        pred_accumulate = tmp_pred_accu
        Tree_model=rbind(Tree_model,Used_Tree_model)
        # save DT model for external prediction purpose
        models[[Curr_tree]]=model
      }

      # Remove used features in the initial feature sets
      used_feat = used_feat[!used_feat %in% used_var]
  }
  consensus_result = DF_acc(pred_accumulate,Valid_label)

  result = list(performance=consensus_result,pred=pred_accumulate,detail=Tree_model,models=models,Method=Method,cp=cp)
  return(result)
}
