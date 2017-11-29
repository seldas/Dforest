globalVariables(c("ConfidenceLevel","BalancedAccuracy", "Accuracy", "PercentageOfSamples", "p"))

#' @title Decision Forest algorithm: confidence level accumulated plot
#' @description Draw accuracy curve according to the confidence level of predictions
#'
#'
#' @param Pred_result Predictions
#' @param Label known label for Test Dataset
#' @param bin How many bins occurred in Conf Plot (Default is 20)
#' @param plot Draw Plot if True, otherwise output the datamatrix
#' @param smooth if TRUE, Fit the performance curve with smooth function (by ggplot2)
#'
#' @return  ACC_Conf: return data Matrix ("ConfidenceLevel", "Accuracy", "Matched Samples") for confidence plot (no plot)
#' @return  ConfPlot: Draw Confidence Plot if True, need install ggplot2
#'
#' @import ggplot2
#' @export


DF_ConfPlot = function (Pred_result, Label, bin=20, plot = T, smooth = F){
  if (is.list(Pred_result)){
    Pred_result = Pred_result$predictions
  }

  label_level = levels(factor(Label))

  min_x = 1/length(label_level)

  ACC_Conf=matrix(0,nrow=bin+1,ncol=4)
  for (i in 1:(bin+1)){
    conf_threshold_lower = min_x + (i-1)/bin*(1-min_x)
    conf_threshold_upper = min_x + i/bin*(1-min_x)
    tmp_samples = (Pred_result[,2]>=conf_threshold_lower & Pred_result[,2]<conf_threshold_upper)
    conf_result = Pred_result[tmp_samples,1]
    conf_label = Label[tmp_samples]


    ACC_Conf[i,1] = conf_threshold_lower
    if (length(conf_label)>0){
      True_prediction = length(which(conf_result==conf_label))

      label_level = levels(factor(conf_label))
      accuracy_sep = matrix(0,nrow=length(label_level),ncol=1)
      for (k in 1:length(label_level)){
        accuracy_sep[k,1]=length(which(conf_result==conf_label & conf_label == label_level[k]))/length(which(conf_label == label_level[k]))
      }
      bACC = mean(accuracy_sep)

      ACC_Conf[i,2] = round(True_prediction / length(conf_label),3)*100
      ACC_Conf[i,3] = round(bACC,3)*100
    }else{
      ACC_Conf[i,2] = 0
      ACC_Conf[i,3] = 0
    }
    ACC_Conf[i,4] = length(conf_label)
  }

  if (plot == T){
    plot_matrix = as.data.frame(ACC_Conf)
    colnames(plot_matrix)=c("ConfidenceLevel", "Accuracy", "BalancedAccuracy", "PercentageOfSamples")
    plot_matrix[,"PercentageOfSamples"] = plot_matrix[,"PercentageOfSamples"]/sum(plot_matrix[,"PercentageOfSamples"])*100
    empty_val = which(plot_matrix[,3]==0)
    if (length(empty_val>0)){
      plot_matrix = plot_matrix[-empty_val,]
    }
    #fit_line = lm(formula = plot_matrix[,2]~plot_matrix[,1])
    p=ggplot(plot_matrix)+
      geom_line(aes(x=ConfidenceLevel, y=Accuracy), stat = "Identity", colour="gray", size = 0.5)+
      geom_point(aes(x=ConfidenceLevel, y=Accuracy, color=Accuracy), size = 4)+
      scale_color_continuous(low="blue",high="darkblue",guide = F)+
      geom_bar(aes(x=ConfidenceLevel, y=PercentageOfSamples, fill = PercentageOfSamples),stat  = "Identity", colour = "gray")+
      scale_fill_continuous(low="firebrick1",high="firebrick4",guide = F)+
      scale_y_continuous(name = "Accuracy", breaks = c(0,20,40,60,80,100),
                         minor_breaks = c(10,30,50,70,90),limits = c(-5,100))+
      #geom_label(aes(x=median(ConfidenceLevel), y=95 , label = "Samples (%)"), fill = "firebrick4", color ="white", alpha = 0.5)+
      theme_bw()+
      theme(axis.title.y = element_text(color = "Blue", face="bold", size = 15))+
      theme(axis.title.x = element_text(color = "black", face="bold"))+
      labs(title = "Samples (%)")+ theme(plot.title = element_text(colour = "firebrick4", hjust = 0.5))

    if (smooth == T){
      p=p+geom_smooth(mapping = aes(x=ConfidenceLevel, y=BalancedAccuracy),fill=NA)
    }

    # p+theme(legend.position="bottom")
    # p+theme(legend.title = element_text(colour="blue", size=16, face="bold"))
    return(p)
  }else{
    return(ACC_Conf)
  }


}


#' @title Decision Forest algorithm: confidence level accumulated plot (accumulated version)
#' @description Draw accuracy curve according to the confidence level of predictions
#'
#'
#' @param Pred_result Predictions
#' @param Label known label for Test Dataset
#' @param bin How many bins occurred in Conf Plot (Default is 20)
#' @param plot Draw Plot if True, otherwise output the datamatrix
#' @param smooth if TRUE, Fit the performance curve with smooth function (by ggplot2)
#'
#' @return  ACC_Conf: return data Matrix ("ConfidenceLevel", "Accuracy", "Matched Samples") for confidence plot (no plot)
#' @return  ConfPlot: Draw Confidence Plot if True, need install ggplot2
#'
#' @import ggplot2
#' @export


DF_ConfPlot_accu = function (Pred_result, Label, bin=20, plot = T, smooth = F){
  if (is.list(Pred_result)){
    Pred_result = Pred_result$predictions
  }

  label_level = levels(factor(Label))

  min_x = 1/length(label_level)

  ACC_Conf=matrix(0,nrow=bin+1,ncol=4)
  for (i in 1:(bin+1)){
    conf_threshold_lower = min_x + (i-1)/bin*(1-min_x)
    tmp_samples = (Pred_result[,2]>=conf_threshold_lower)
    conf_result = Pred_result[tmp_samples,1]
    conf_label = Label[tmp_samples]

    ACC_Conf[i,1] = conf_threshold_lower
    if (length(conf_label)>0){
      True_prediction = length(which(conf_result==conf_label))

      accuracy_sep = matrix(0,nrow=length(label_level),ncol=1)
      for (k in 1:length(label_level)){
        accuracy_sep[k,1]=length(which(conf_result==conf_label & conf_label == label_level[k]))/length(which(conf_label == label_level[k]))
      }
      bACC = mean(accuracy_sep)

      ACC_Conf[i,2] = round(True_prediction / length(conf_label),3)*100
      ACC_Conf[i,3] = round(bACC,3)*100
    }else{
      ACC_Conf[i,2] = 0
      ACC_Conf[i,3] = 0
    }
    ACC_Conf[i,4] = length(conf_label)
  }

  if (plot == T){
    plot_matrix = as.data.frame(ACC_Conf)
    colnames(plot_matrix)=c("ConfidenceLevel", "Accuracy", "BalancedAccuracy", "PercentageOfSamples")
    plot_matrix[,"PercentageOfSamples"] = plot_matrix[,"PercentageOfSamples"]/length(Label)*100
    empty_val = which(plot_matrix[,3]==0)
    if (length(empty_val>0)){
      plot_matrix = plot_matrix[-empty_val,]
    }

    #fit_line = lm(formula = plot_matrix[,2]~plot_matrix[,1])
    p=ggplot(plot_matrix)+
      geom_bar(mapping = aes(x=ConfidenceLevel, y=PercentageOfSamples, fill = PercentageOfSamples, alpha = 0.5),stat  = "Identity", colour = "gray")+
      scale_fill_continuous(low="firebrick1",high="firebrick4",guide = F)+
      scale_alpha(range = c(0.4,0.4),guide = "none")+
      geom_line(mapping = aes(x=ConfidenceLevel, y=Accuracy), stat = "Identity", colour="gray", size =0.5)+
      geom_point(mapping = aes(x=ConfidenceLevel, y=Accuracy, color=Accuracy),size = 4)+
      scale_color_continuous(low="blue",high="darkblue",guide = F)+
      scale_y_continuous(name = "Accuracy", breaks = c(0,20,40,60,80,100),
                         minor_breaks = c(10,30,50,70,90),limits = c(-5,100))+
      #geom_label(aes(x=median(ConfidenceLevel), y=95 , label = "Samples (%)"), fill = "firebrick4", color ="white", alpha = 0.5)+
      theme_bw()+
      theme(axis.title.y = element_text(color = "Blue", face="bold", size = 15))+
      theme(axis.title.x = element_text(color = "black", face="bold"))+
      labs(title = "Samples (%)")+ theme(plot.title = element_text(colour = "firebrick4", hjust = 0.5))

    if (smooth == T){
      p=p+geom_smooth(mapping = aes(x=ConfidenceLevel, y=Accuracy),fill=NA)
    }
    # p+theme(legend.position="bottom")
    # p+theme(legend.title = element_text(colour="blue", size=16, face="bold"))
    return(p)
  }else{
    return(ACC_Conf)
  }


}
