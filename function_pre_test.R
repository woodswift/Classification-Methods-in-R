library(MASS) # for the example dataset 
library(plyr) # for recoding data
library(ROCR) # for plotting roc
library(AUC) # for calculate AUC
library(class) # for knn
library(e1071) # for NB and SVM
library(rpart) # for decision tree
library(ada) # for adaboost

## set the seed so you can get exactly the same results whenever you run the code
set.seed(12345)

pre.test <- function(dataset, cl.name, r=0.6, prob.cutoff=0.5) {
  ## Let's use 60% random sample as training and remaining as testing
  ## by default use 0.5 as cut-off
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.train = floor(n.obs*r)
  train.idx = sample(1:n.obs,n.train)
  # train.idx
  train.set = dataset[train.idx,]
  test.set = dataset[-train.idx,]
  cat('\n')
  cat('pre-test',cl.name,':',
      '#training:',nrow(train.set),
      '#testing:',nrow(test.set),'\n','\n')
  ## prob is an array of probabilities for cases being positive
  prob = do.classification(train.set, test.set, cl.name)
  
  ## get confusion matrix
  predicted = as.numeric(prob > prob.cutoff)
  actual = test.set$Y
  confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
  
  ## calculate the error
  error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
  cat('error rate:',error,'\n')
  
  ## calculate the accuracy
  accuracy = (confusion.matrix[1,1]+confusion.matrix[2,2]) / nrow(test.set)
  cat('accuracy:',accuracy,'\n')
  
  ## calculate the precision
  precision = confusion.matrix[1,1]/sum(confusion.matrix[,1])
  cat('precision:',precision,'\n')
  
  ## calculate the recall
  recall = confusion.matrix[1,1]/sum(confusion.matrix[1,])
  cat('recall:',recall,'\n')
  
  ## calculate the F1 score
  f1score = 2*precision*recall/(precision+recall)
  cat('F1-score',f1score,'\n')
  
  ## plot ROC and calculate AUC
  result = data.frame(prob,actual)
  pred = prediction(result$prob,result$actual)
  
  ## plot ROC
  # roc_curve = performance(pred, "tpr","fpr")
  # plot(roc_curve)
  
  ## calculate AUC
  auc <- performance(pred,"auc")
  auc_val <- auc@y.values[[1]]
  cat('AUC',auc_val,'\n','\n')
}