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

my.classifier <- function(dataset, cl.name='knn', do.cv=F) {
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.cols <- ncol(dataset) # 1 + no. of predictors
  cat('my dataset:',
      n.obs,'observations',
      n.cols-1,'predictors','\n','\n')
  # print(dataset[1:3,])
  cat('label (Y) distribution:')
  print(table(dataset$Y))
  
  pre.test(dataset, cl.name)
  if (do.cv) k.fold.cv(dataset, cl.name)
}