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

k.fold.cv.result <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5) {
  ## default: 10-fold CV, cut-off 0.5 
  n.obs <- nrow(dataset) # no. of observations
  ## shuffle the row index
  s=sample(n.obs)
  
  errors=dim(k.fold)
  accuracys=dim(k.fold)
  precisions=dim(k.fold)
  recalls=dim(k.fold)
  f1scores=dim(k.fold)
  AUCs=dim(k.fold)
  
  probs=NULL
  actuals=NULL
  for (k in 1:k.fold) {
    test.idx=which(s%% k.fold==(k-1)) # use modular operator
    train.set=dataset[-test.idx,]
    test.set=dataset[test.idx,]
    cat(k.fold,'-fold CV run',k,cl.name,':',
        '#training:',nrow(train.set),
        '#testing:',nrow(test.set),'\n','\n')
    ## prob is an array of probabilities for cases being positive
    prob=do.classification(train.set, test.set, cl.name)
    actual=test.set$Y
    
    probs = c(probs,prob)
    actuals = c(actuals,actual)
    ## you may compute other measures and store them in arrays
  }
  
  result=data.frame(probs,actuals)
  return(result)
}