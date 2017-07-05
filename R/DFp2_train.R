#' @title Decision Forest preferred-2 algorithm: Model training
#' @description Decision Forest algorithm: Model training
#'             In Preferred-2 settings, we used KNN instead of Decision Tree
#'
#' @param X Training Dataset
#' @param Y Training data endpoint
#' @param stop_step How many extra step would be processed when performance not improved, 1 means one extra step
#' @param Max_tree Maximum tree number in Forest
#' @param Max_feat maximum occurrence of features in the forest
#' @param min_leaf minimum leaves in tree nodes
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
#' @return Result:     Training Model/Performance by KNN
#'
#' @import class
#' @export
#'
#' @examples
#'   ##data(iris)
#'   X = iris[,1:4]
#'   Y = iris[,5]
#'   names(Y)=rownames(X)
#'   used_model = DF_train(X,Y,stop_step=4, Method = "MCC")
#'
DFp2_train = function (X, Y, stop_step=5,
                     Max_tree=20, Max_feat=5, k=5, grace_threshold = 0.01,
                     Filter = F, p_val = 0.05, Method = "bACC", Quiet = T){

  # Default settings
  # stop_step=5
  # Max_tree=20
  # Max_feat=5
  # k=5
  # Filter = F
  # p_val = 0.05
  # Method = "bACC"
  # Quiet = T


  Discard = Max_feat
  Y =factor(Y)

  # initial the feature usage counts
  Feature_count = matrix(0,nrow=ncol(X),ncol=1)
  rownames(Feature_count)=colnames(X)

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

  # start training
  Result = matrix(list(),nrow=0, ncol=6);
  colnames(Result)=c("Curr Trees","Used Feature","sub-performance","Accumulate performance",
                         "In Final Model","Stop Sign")
  # model = list()
  # model_perform = list()
  used_feat = colnames(Train_data)
  Curr_tree = 0
  accu_perform_val = c()
  stop_sign = 0
  used_sign = 0

  while(Curr_tree<Max_tree & length(used_feat)>1 & stop_sign <5 ){
    Curr_feat = 0
    remain_feat = c()
    remain_perf = c()
    while (Curr_feat < Max_feat){
      Curr_feat = Curr_feat + 1
      tmp_Result = apply(Train_data[,used_feat],2,function(x) DF_perf(knn.cv(cbind(Train_data[,remain_feat],x),cl=Train_label, k=k),Train_label)[[Method]])
      best_feat = which(tmp_Result==max(tmp_Result))
      if (best_feat[1] %in% remain_feat){
        break
      }else{
        remain_feat = cbind(remain_feat,names(best_feat[1]))
        remain_perf = cbind(remain_perf,max(tmp_Result))
      }
    }
    Curr_tree = Curr_tree +1
    # model[[Curr_tree]]=remain_feat
    # model_perform[[Curr_tree]]=remain_perf
    used_feat = used_feat[!used_feat %in% remain_feat]
    curr_perform = knn.cv(train=Train_data[,remain_feat],cl=Train_label,k=k,prob = T)
    accu_perform_val_tmp = DF_perf(curr_perform,Train_label)
    if (accu_perform_val_tmp[[Method]] > max(c(0,accu_perform_val))-grace_threshold){
      accu_perform_val = rbind(accu_perform_val,accu_perform_val_tmp[[Method]])
      used_sign = "In"
    }else{
      stop_sign = stop_sign +1
      used_sign = "Out"
    }

    tmp_model=list()
    tmp_model[[1]] = Curr_tree
    tmp_model[[2]] = remain_feat
    tmp_model[[3]] = remain_perf
    tmp_model[[4]] = round(accu_perform_val_tmp[[Method]],3)
    tmp_model[[5]] = used_sign
    tmp_model[[6]] = stop_sign
    Result = rbind(Result,tmp_model)
    cat(paste(Curr_tree," is done! stop-sign: ", stop_sign ," Best Perform: ", round(accu_perform_val_tmp[[Method]],3), "\n"))
  }

  # Return Result
  return(Result)
}
