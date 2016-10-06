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

k.fold.cv <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5) {
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
    
    ## get confusion matrix
    predicted=as.numeric(prob>prob.cutoff)
    actual=test.set$Y
    confusion.matrix=table(actual,factor(predicted,levels=c(0,1)))
    confusion.matrix
    
    ## calculate the error
    error=(confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
    errors[k] = error
    cat('error:',error,'\n')
    
    ## calculate the accuracy
    accuracy = (confusion.matrix[1,1]+confusion.matrix[2,2]) / nrow(test.set)
    accuracys[k] = accuracy
    cat('accuracy:',accuracy,'\n')
    
    ## calculate the precision
    precision = confusion.matrix[1,1]/sum(confusion.matrix[,1])
    precisions[k] = precision
    cat('precision:',precision,'\n')
    
    ## calculate the recall
    recall = confusion.matrix[1,1]/sum(confusion.matrix[1,])
    recalls[k] = recall
    cat('recall:',recall,'\n')
    
    ## calculate the F1 score
    f1score = 2*precision*recall/(precision+recall)
    f1scores[k] = f1score
    cat('F1-score:',f1score,'\n')
    
    ## plot ROC and calculate AUC
    result = data.frame(prob,actual)
    pred = prediction(result$prob,result$actual)
    
    ## plot ROC
    # roc_curve = performance(pred, "tpr","fpr")
    # plot(roc_curve)
    
    ## calculate AUC
    auc <- performance(pred,"auc")
    auc_val <- auc@y.values[[1]]
    AUCs[k] = auc_val
    cat('AUC',auc_val,'\n','\n')
    
    probs = c(probs,prob)
    actuals = c(actuals,actual)
    ## you may compute other measures and store them in arrays
  }
  
  cat(k.fold,'-fold CV results with the cutoff being',prob.cutoff,':','\n','\n')
  
  avg.error=mean(errors)
  cat('average error:',avg.error,'\n')
  
  avg.accuracy=mean(accuracys)
  cat('average accuracy:',avg.accuracy,'\n')
  
  avg.precision=mean(precisions)
  cat('average precision:',avg.precision,'\n')
  
  avg.recall=mean(recalls)
  cat('average recall:',avg.recall,'\n')
  
  avg.f1score=mean(f1scores)
  cat('average F1-score:',avg.f1score,'\n')
  
  avg.AUC=mean(AUCs)
  cat('average AUC:',avg.AUC,'\n','\n')
  
  ## plot ROC
  result=data.frame(probs,actuals)
  pred=prediction(result$probs,result$actuals)
  perf=performance(pred, "tpr","fpr")
  plot(perf)  
  
  ## get other measures by using 'performance'
  get.measure <- function(pred, measure.name='auc') {
    perf = performance(pred,measure.name)
    m <- unlist(slot(perf, "y.values"))
    m
  }
  
  ## using different threshold, the average value of metircs
  ## including: error, accuracy, precision, recall, F1-score
  err=mean(get.measure(pred, 'err'))
  acc=1-err
  pre=mean(get.measure(pred, 'prec'),na.rm=T)
  rec=mean(get.measure(pred, 'rec'),na.rm=T)
  fsc=mean(get.measure(pred, 'f'),na.rm=T)
  auc=get.measure(pred, 'auc')
  
  cat('Average results with the cutoff from 0 to 1:','\n','\n')
  cat('average error:',err,'\n')
  cat('average accuracy:',acc,'\n')
  cat('average precision:',pre,'\n')
  cat('average recall:',rec,'\n')
  cat('average F1-score:',fsc,'\n')
  cat('average AUC:',auc,'\n')
}