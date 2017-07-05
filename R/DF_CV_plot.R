globalVariables(c("Feature","Performance", "CV_result","Batch","Overall"))

#' @title output summary for Dforest Cross-validation results
#' @description Draw plot for Dforest Cross-validation results
#'
#'
#' @param CV_result Training Dataset
#' @param plot if TRUE (default), draw plot
#'
#' @import ggplot2
#' @export
#'
#'
DF_CVsummary =function(CV_result,plot=T){
  if (CV_result$Method =="ACC"){
    perform_use = CV_result$performance$ACC
  }else if (CV_result$Method =="bACC"){
    perform_use = CV_result$performance$bACC
  }else if (CV_result$Method =="MCC"){
    perform_use = CV_result$performance$MCC
  }else if (CV_result$Method =="MIS"){
    perform_use = CV_result$performance$MIS
  }
  Detail_info = CV_result$detail
  CV_fold = max(unlist(Detail_info[,"Stage"]))

  Total_feature = unlist(Detail_info[,"Used Feature"])
  tmp_feat = as.data.frame(table(Total_feature))
  Feature_count=tmp_feat[order(tmp_feat[,"Freq"],decreasing = T),]
  cat(paste("Overall ",CV_fold,"-fold Cross-Validation result: ",round(perform_use,3),"\n",sep=""))
  cat(paste("Most 5 used features: ", paste(Feature_count[c(1:5),1],collapse = ', '),"\n",sep=""))
  cat(paste("Top 5 frequencies in (",CV_fold,"): ", paste(Feature_count[c(1:5),2],collapse = ', '),"\n",sep=""))

  if (plot == T){
    plot_matrix = matrix(0,nrow=CV_fold,ncol=3)
    for (i in 1:CV_fold){
      plot_matrix[i,1] = i
      plot_matrix[i,2] = max(unlist(Detail_info[Detail_info[,"Stage"]==i,7]))
      plot_matrix[i,3] = perform_use
    }
    plot_matrix=as.data.frame(plot_matrix)
    colnames(plot_matrix)=c("Batch","Performance","Overall")
    p=ggplot(plot_matrix)+
      geom_line(aes(x=Batch, y=Overall,color="Green",size=Overall))+
      scale_size_continuous(range=c(1,1),guide=FALSE)+
      scale_colour_manual(values="red",name=NULL, labels="CV Overall")+
      geom_bar(aes(x=Batch, y=Performance, fill=Performance, alpha = 0.7),stat = "Identity")+
      scale_fill_continuous(low="lightblue", high="darkblue", name = CV_result$Method )+
      scale_alpha_identity()+
      ylim(0,1) +
      labs(y = paste("Performance: ",CV_result$Method,sep=""), x = paste(CV_fold," Fold Cross Validation", sep=""))+
      scale_x_continuous(breaks=plot_matrix[,1])

    return(p)
  }else{
    return(Feature_count)
  }
}



#' @title output summary for Dforest test results
#' @description Draw plot for Dforest test results
#'
#'
#' @param used_model Training result
#' @param plot if TRUE (default), draw plot
#'
#' @import ggplot2
#' @export
#'
#'
DF_Trainsummary =function(used_model,plot=T){
  if (used_model$Method =="ACC"){
    perform_use = used_model$performance$ACC
  }else if (used_model$Method =="bACC"){
    perform_use = used_model$performance$bACC
  }else if (used_model$Method =="MCC"){
    perform_use = used_model$performance$MCC
  }else if (used_model$Method =="MIS"){
    perform_use = used_model$performance$MIS
  }
  Detail_info = used_model$detail
  Total_feature = unlist(Detail_info[,"Used Feature"])
  tmp_feat = as.data.frame(table(Total_feature))
  Feature_count=tmp_feat[order(tmp_feat[,"Total_feature"],decreasing = F),]
  cat(paste("Overall Training (",used_model$Method,"): ",round(perform_use,3),"\n",sep=""))
  cat(paste("Used features (in Forest): ", paste(Feature_count[,1],collapse = ', '),"\n",sep=""))
  if (plot == T){
    tmp_table = Detail_info
    data=data.frame("Feature"=1:nrow(tmp_table),"Performance"=unlist(tmp_table[,7]))
    p=ggplot(data,aes(x = Feature, y = Performance))+
          #geom_point(size=4,color="black")+
          geom_bar(aes(fill = Performance, alpha=1), color="black",stat = "Identity")+
          scale_fill_continuous(low="brown",high="Green")+
          scale_alpha_continuous(range = c(0.9,0.9),guide=FALSE)+
          #geom_line(stat = "identity",colour="darkgray", size = 2)+
          labs(x = "Combined Trees ", y = paste("Performance: ",used_model$Method,sep="")) + scale_x_continuous(breaks=(1:nrow(tmp_table)))
    if (max(unlist(tmp_table[,7]))<1){
      p=p+ylim(0,1)
    }
    return(p)
  }else{
    return(data)
  }
}

