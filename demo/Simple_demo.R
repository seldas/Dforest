rm(list=ls())
set.seed(0)

data(demo_simple)
X = Data_simple$X
Y = Data_simple$Y
names(Y)=rownames(X)

# result = DFp2_train(X,Y)

# data(iris)
# X = iris[,1:4]
# Y = iris[,5]
# names(Y)=rownames(X)

Keep_feature_set = DF_dataPre(X)
X = X[Keep_feature_set]

# generate Training and validating dataset
# using "1" for positive and "0" for negative endpoint, to simplify the training process
random_seq=sample(nrow(X))
split_rate=3
split_sample = suppressWarnings(split(random_seq,1:split_rate))
Train_X = X[-random_seq[split_sample[[1]]],]
Train_Y = Y[-random_seq[split_sample[[1]]]]
Test_X = X[random_seq[split_sample[[1]]],]
Test_Y = Y[random_seq[split_sample[[1]]]]


## cross-validation demo (for cross-validation)
#  Parameter setup. Use default value if not
#   CV_fold = 5 # Fold of cross-validation
#   Max_tree =20 # Maximum tree number in Forest

CV_result = DF_train_CV(Train_X, Train_Y, CV_fold=10, stop_step = 4, Method = "bACC")
DF_CVsummary(CV_result,plot_col=5,plot=T)

# Model training and Prediction demo (for external dataset)
#
used_model = DFp2_train(Train_X, Train_Y)
# View(used_model$detail)

Pred_result = DFp2_pred(used_model,Train_X, Train_Y, Test_X, Test_Y)
#Pred_result = DF_pred(used_model,Test_X)
DF_ConfPlot(Pred_result, Test_Y, bin = 15, smooth = F)

#Train_summary=DF_Trainsummary(used_model,plot_col=5,plot=F)
DF_Trainsummary(used_model)



