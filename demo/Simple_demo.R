# library(Dforest)
rm(list=ls())
set.seed(NULL)

data(demo_simple)
# save(file = "data/demo_simple.RData",data_dili)
X = data_dili$X
Y = data_dili$Y
names(Y)=rownames(X)

# only use two-class
X = X[Y!="Less-DILI-Concern",]
Y = factor(Y[Y!="Less-DILI-Concern"])


Keep_feature_set = DF_dataPre(X)
X = X[Keep_feature_set]

# generate Training and validating dataset
# using "1" for positive and "0" for negative endpoint, to simplify the training process

set.seed(0)
random_seq=sample(nrow(X))
split_rate=3
split_sample = suppressWarnings(split(random_seq,1:split_rate))
Train_X = X[-random_seq[split_sample[[1]]],]
Train_Y = Y[-random_seq[split_sample[[1]]]]
Test_X = X[random_seq[split_sample[[1]]],]
Test_Y = Y[random_seq[split_sample[[1]]]]

result = DF_easy(Train_X, Train_Y, Test_X, Test_Y, mode="default")

#Training
used_model = DF_train(Train_X, Train_Y)
# View(used_model$detail)
Pred_result = DF_pred(used_model, X = Test_X, Y = Test_Y)
#Pred_result = DF_pred(used_model,Test_X)
DF_ConfPlot(Pred_result, Test_Y, bin = 20)
DF_ConfPlot_accu(Pred_result, Test_Y, bin = 20)
DF_Trainsummary(used_model)

## cross-validation demo (for cross-validation)
#  Parameter setup. Use default value if not
#   CV_fold = 5 # Fold of cross-validation
#   Max_tree =20 # Maximum tree number in Forest

CV_result = DF_CV(Train_X, Train_Y, CV_fold=5)
# View(CV_result$detail)
DF_CVsummary(CV_result,plot=T)
