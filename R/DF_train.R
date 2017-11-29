#' @title Decision Forest algorithm: Model training
#' @description Decision Forest algorithm: Model training
#'
#'
#' @param X Training Dataset
#' @param Y Training data endpoint
#' @param stop_step How many extra step would be processed when performance not improved, 1 means one extra step
#' @param Max_tree Maximum tree number in Forest
#' @param min_split minimum leaves in tree nodes
#' @param cp parameters to pruning decision tree, default is 0.1
#' @param Filter doing feature selection before training
#' @param p_val P-value threshold measured by t-test used in feature selection, default is 0.05
#' @param Method Which is used for evaluating training process. MIS: Misclassification rate; ACC: accuracy
#' @param Quiet if TRUE (default), don't show any message during the process
#' @param Grace_val Grace Value in evaluation: the next model should have a performance (Accuracy, bACC, MCC) not bad than previous model with threshold
#' @param imp_accu_val improvement in evaluation: adding new tree should improve the overall model performance (Accuracy, bACC, MCC) by threshold
#' @param imp_accu_criteria if TRUE, model must have improvement in accumulated accuracy
#'
#' @return  .$accuracy:     Overall training accuracy
#' @return  .$pred:         Detailed training prediction (fitting)
#' @return  .$detail:       Detailed usage of Decision tree Features/Models and their performances
#' @return  .$models:       Constructed (list of) Decision tree models
#' @return  .$Method:        pass evaluating Methods used in training
#' @return  .$cp:            pass cp value used in training decision trees
#'
#' @export
#'
#' @examples
#'   ##data(iris)
#'   X = iris[,1:4]
#'   Y = iris[,5]
#'   names(Y)=rownames(X)
#'   used_model = DF_train(X,factor(Y))
#'
DF_train = function (X, Y, stop_step=5,
                       Max_tree=20, min_split=10, cp = 0.1,
                       Filter = F, p_val = 0.05, Method = "bACC", Quiet = T,
                       Grace_val = 0.05, imp_accu_val = 0.01, imp_accu_criteria = F
                      ){

  Y = factor(Y)

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
    if (Quiet == F){
      cat(paste(length(used_feat)," of ",length(p_value)," features remained! \n",sep=""))
    }
  }

  Train_data = X
  Valid_data = X # Fitting
  Train_label = Y
  Valid_label = Y # Fitting


  # For reference build, cp cut-off was set to 0.01 w/o pruning
  np_model = Con_DT(Train_data,Train_label,min_split=min_split,cp=0.01)
  # cp - first split
  ref_cp = np_model$cptable[1,'CP']
  if (cp > ref_cp){
    cp = round(ref_cp,2)
    if (cp > ref_cp) {
      cp = ref_cp - 0.01
    }
    cat(paste("Current CP setting is too large for the model, reset cp to most-available value: ",round(cp,2),"\n",sep=""))
  }

  # Calculating Reference performance
  model = Con_DT(Train_data,Train_label,min_split=min_split,cp=cp)
  pred  = Pred_DT(model, Valid_data)
  ref_result = DF_acc(pred,Valid_label)

  # reference performance
  ref_val = ref_result[[Method]]
  # for MCC evaluation
  if (is.na(ref_val)){
    ref_val = 0
  }


  # start training
  Curr_tree = 0

  Tree_model = matrix(list(),nrow=0, ncol=6);
  colnames(Tree_model)=c("Used Feature","Current Performance","Compared to Ref","Accumulated Performance",
                         "Detailed Predictions","Model status")

  models = list()

  stop_sign = 0
  stop_signal = 0
  accu_val=0
  used_feat = colnames(Train_data)
  pred_accumulate=matrix(0,nrow=length(Y),ncol=length(table(Y)))

  while(Curr_tree<Max_tree & stop_signal<stop_step & length(used_feat)>1 & stop_sign ==0 ){
    model = Con_DT(Train_data[,used_feat],Train_label,min_split,cp=cp)
    pred  = Pred_DT(model, Valid_data[,used_feat])
    feat_used  = as.matrix(unique(model$frame$var))
    feat_used  = feat_used[feat_used!="<leaf>",]
    if (length(feat_used)==0){
      if (Quiet == F){
        cat("no more features available! Stopped... \n")
        cat(paste("Current built Trees: ",Curr_tree,"\n"))
      }
      stop_sign = 1
      break
    }
    first_feat = feat_used[1]

    tmp_result = DF_acc(pred,Valid_label)
    curr_val = tmp_result[[Method]]
    if (is.na(curr_val)){
      curr_val = 0
    }

    # accumulative performance with multiple trees
    tmp_pred_accu = pred_accumulate
    tmp_pred_accu = tmp_pred_accu + pred
    tmp_result_accu = DF_acc(tmp_pred_accu,Valid_label)
    curr_accu_val = tmp_result_accu[[Method]]
    if (is.na(curr_accu_val)){
      curr_accu_val = 0
    }


    if (Method == "MCC" & length(levels(Y))!=2){
      if (Quiet == F){
        cat("Not 2-Class analysis! Use ACC in default ...")
      }
      Method = "bACC"
    }


    if (curr_val >= ref_val - Grace_val){
      if (imp_accu_criteria == T & curr_accu_val < accu_val + imp_accu_val ){
        Status="Stop"
        stop_signal = stop_signal + 1
      }else{
        Status="Improved"
      }
    }else{
      Status="Stop"
      stop_signal = stop_signal + 1
    }

    # write updated outputs
    Used_Tree_model = matrix(list(),nrow=1, ncol=6); # template for final result
    Used_Tree_model[[1,1]] = feat_used
    Used_Tree_model[[1,2]] = round(curr_val,3)
    Used_Tree_model[[1,3]] = round((curr_val-ref_val),3)
    Used_Tree_model[[1,4]] = round(curr_accu_val,3)
    Used_Tree_model[[1,5]] = pred


    if (Status == "Improved"){
      Curr_tree =Curr_tree +1
      stop_signal = 0
      # update criteria
      accu_val = curr_accu_val
      pred_accumulate = tmp_pred_accu
      Used_Tree_model[[1,6]] = "Fit - Kept" # label for performance recognization
      # save DT model for external prediction purpose
      models[[Curr_tree]]=model
    } else {
      Used_Tree_model[[1,6]] = "Fit - Discarded" # label for performance recognization
    }


    Tree_model=rbind(Tree_model,Used_Tree_model)
    filtered_feat = feat_used # first_feat
    # Remove used features in the initial feature sets
    used_feat = used_feat[!used_feat %in% filtered_feat]
  }
  if (Curr_tree == 0){
    consensus_result=list()
    consensus_result$ACC="No model is built!"
    consensus_result$MCC="No model is built!"
    consensus_result$bACC="No model is built!"
  }else{
    consensus_result = DF_acc(pred_accumulate,Valid_label)
  }

  #Feature_count = Feature_count[Feature_count[,"Frequency"]>0,]
  #fin_feat_table = Feature_count[order(Feature_count[,"Frequency"],decreasing = T),]
  result = list(performance=consensus_result,pred=pred_accumulate,detail=Tree_model,
                models=models,Method=Method,cp=cp)
  return(result)
}
