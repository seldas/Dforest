#' @title Decision Forest preferred-1 algorithm: Model training with Cross-validation
#' @description Decision Forest algorithm: Model training with Cross-validation
#'   Default is 5-fold cross-validation
#'
#'
#' @param X Training Dataset
#' @param Y Training data endpoint
#' @param CV_fold Fold of cross-validation (Default = 5)
#' @param stop_step How many extra step would be processed when performance not improved, 1 means one extra step
#' @param param_T Parameter T in IDF: Maximum tree number in Forest
#' @param param_R parameter R in IDF: maximum occurrence of features
#' @param param_L parameter L in IDF: minimum leaves in tree nodes
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
#' @return  .$accuracy:      Overall training accuracy (Cross-validation)
#' @return  .$pred:          Detailed training prediction (Cross-validation)
#' @return  .$detail:        Detailed usage of Decision tree Features/Models and their performances in all CVs
#'
#' @export
#'
#' @examples
#'   ##data(iris)
#'   X = iris[,1:4]
#'   Y = iris[,5]
#'   names(Y)=rownames(X)
#'
#'   random_seq=sample(nrow(X))
#'   split_rate=3
#'   split_sample = suppressWarnings(split(random_seq,1:split_rate))
#'   Train_X = X[-random_seq[split_sample[[1]]],]
#'   Train_Y = Y[-random_seq[split_sample[[1]]]]
#'
#'   CV_result = DFp1_CV(Train_X, Train_Y)
#'
#'
DFp1_CV = function (X, Y, CV_fold=5,stop_step=4,
                        param_T=20, param_R=5, param_L=3, cp = 0.1,
                        Filter = F , p_val = 0.05, Method = "bACC", Quiet = T,
                        Grace_ACC = 0.05, imp_ACC_accu = 0.01,
                        Grace_bACC = 0.05, imp_bACC_accu = 0.01,
                        Grace_MCC = 0.05, imp_MCC_accu = 0.01,
                        Grace_MIS = ceiling(0.05*length(Y)), imp_MIS_accu = ceiling(0.01*length(Y)) ){

  Max_tree = param_T
  min_leaf = as.integer(param_L)
  Discard = as.integer(param_R)
  Y = factor(Y)

  # initial the feature usage counts
  Feature_count = matrix(0, nrow=ncol(X), ncol=1)
  rownames(Feature_count)=colnames(X)

  if (length(levels(Y))!=2 & Filter == T){
    if (Quiet == F){
      cat("Not 2-class analysis! ignore Filtering step ... \n")
    }
    Filter = F
  }

  if (Filter == T) {
    if (Quiet == F){
      cat(paste("Doing Feature selection based on Training dataset: p-value<", p_val, " \n",sep=""))
    }
    p_value = DF_calp(X,Y)
    used_feat = which(p_value<=p_val)
    X = X[,used_feat]
    cat(paste(length(used_feat), " of ", length(p_value), " features remained! \n", sep=""))
  }

  #Cross-validation batches
  random_seq=sample(nrow(X))
  split_sample = suppressWarnings(split(random_seq,1:CV_fold))

  CV_result=c()
  pred_all=matrix(0,nrow=0,ncol=length(table(Y)))
  Tree_model = matrix(list(),nrow=0, ncol=10);
  colnames(Tree_model)=c("Used Feature","Accuracy","Balanced ACC","MCC",
                           "MisMatch","Reference","Accumulate Performance",
                           "Stage","Status","Predictions")

  # Cross-validation main program
  for ( CV_iter in 1:CV_fold){

    Train_data = X[-random_seq[split_sample[[CV_iter]]],]
    Train_label = Y[-random_seq[split_sample[[CV_iter]]]]

    Valid_data = X[random_seq[split_sample[[CV_iter]]],]
    Valid_label = Y[random_seq[split_sample[[CV_iter]]]]


    # For reference build, cp cut-off was set to 0.01 w/o pruning
    np_model = Con_DT(Train_data,Train_label,min_leaf=min_leaf,cp=0.01)
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
    model = Con_DT(Train_data,Train_label,min_leaf=min_leaf,cp=cp)
    pred  = Pred_DT(model, Valid_data)
    ref_result = DF_acc(pred,Valid_label)
    ref_MIS = ref_result$MIS
    ref_MCC = ref_result$MCC
    ref_ACC = ref_result$ACC
    ref_bACC = ref_result$bACC

    # start training
    stop_sign = 0
    Curr_tree = 0
    stop_signal = 0
    curr_MIS=0
    ACC_accu=0
    bACC_accu=0
    MCC_accu=0
    MIS_accu=length(Y)
    used_feat = colnames(Train_data)
    pred_accumulate=matrix(0,nrow=length(Valid_label),ncol=length(table(Valid_label)))
    rownames(pred_accumulate)=rownames(Valid_label)

    best_tree=0
    while(Curr_tree<Max_tree & stop_signal<=stop_step & length(used_feat)>1 & stop_sign ==0){
      model = Con_DT(Train_data[,used_feat],Train_label,min_leaf=min_leaf,cp=cp)
      pred  = Pred_DT(model, Valid_data[,used_feat])
      feat_used  = as.matrix(unique(model$frame$var))
      feat_used  = feat_used[feat_used!="<leaf>",]
      if (length(feat_used)==0){
        cat("Training Stopped because of no more model can be constructed! \n")
        cat(paste("Current built Trees: ",Curr_tree,"\n"))
        stop_sign = 1
        break
      }
      first_feat = feat_used[1]
      Feature_count[feat_used,]=Feature_count[feat_used,]+1

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
      Used_Tree_model = matrix(list(),nrow=1, ncol=10); # template for final result
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
      Used_Tree_model[[1,8]] = CV_iter # label for performance recognization
      Used_Tree_model[[1,9]] = Status
      Used_Tree_model[[1,10]] = pred


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
      }

      max_feat = rownames(which(Feature_count>=Discard))
      filtered_feat = c(first_feat, max_feat)

      # Remove used features in the initial feature sets
      used_feat = used_feat[!used_feat %in% filtered_feat]

    }
    if (Curr_tree > 0){
      pred_all = rbind(pred_all,pred_accumulate)
    }
  }
  pred_all=pred_all[order(rownames(pred_all)),]
  Y=Y[order(names(Y))]
  Fin_performance = DF_acc(pred_all,Y)

  result = list(performance=Fin_performance,pred=pred_all,detail=Tree_model,Method=Method,cp=cp)
  return(result)
}
