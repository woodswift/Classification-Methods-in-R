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

do.classification <- function(train.set, test.set, cl.name, verbose=F) {
  ## note: to plot ROC later, we want the raw probabilities,
  ## not binary decisions
  switch(cl.name, 
         knn={ ## k nearest neighbors
           ## here we test k=3; you should evaluate different k's
           prob=knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k=3, prob=T)
           prob=attr(prob,"prob")
           # print(cbind(prob,as.character(test.set$y)))
           prob
         },
         lr={ ## logistic regression
           model=glm(Y~., family=binomial(link='logit'), data=train.set, control=list(maxit=100))
           if (verbose) {
             print(summary(model))             
           }
           prob=predict(model, newdata=test.set[,-1], type="response") 
           # print(cbind(prob,as.character(test.set$y)))
           prob
         },
         nb={ ## naive bayesian
           model=naiveBayes(Y~., data=train.set)
           prob=predict(model, newdata=test.set[,-1], type="raw") 
           # print(cbind(prob,as.character(test.set$y)))
           prob=prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         dtree={
           model=rpart(Y~., data=train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }           
           prob=predict(model, newdata=test.set[,-1])
           
           if (0) { # here we use the default tree, 
             ## you should evaluate different size of tree
             ## prune the tree 
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set[,-1])
             ## plot the pruned tree 
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)             
           }
           
           # print(cbind(prob,as.character(test.set$y)))
           prob=prob[,2]/rowSums(prob) # renormalize the prob.
           # prob
         },
         svm={
           ## SVM with default setting
           model=svm(Y~., data=train.set, probability=T)
           prob=predict(model, newdata=test.set[,-1], probability=T)
           prob=attr(prob,"probabilities")
           # print(cbind(prob,as.character(test.set$y)))
           # print(dim(prob))
           prob=prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svmLin={
           ## SVM with linear kernel
           model=svm(Y~., data=train.set, probability=T, kernel="linear")
           prob=predict(model, newdata=test.set[,-1], probability=T)
           prob=attr(prob,"probabilities")
           # print(cbind(prob,as.character(test.set$y)))
           # print(dim(prob))
           prob=prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svmPoly={
           ## SVM with polynomial kernel
           ## fine-tune the model with polynomial kernel and parameters
           ## evaluate the range of degree between 3 and 9
           ## and coef0 parameter between -1 and 1
           ## and cost parameter from 0.1 until 10
           tuned <- tune.svm(Y~., data=train.set, 
                             kernel="polynomial", 
                             degree=3:9,
                             coef0=-1:1,
                             cost=10^(-1:1))
           #print(summary(tuned))
           degree = tuned[['best.parameters']]$degree
           coef0 = tuned[['best.parameters']]$coef0
           cost = tuned[['best.parameters']]$cost
           model = svm(Y~., data=train.set, probability=T, 
                       kernel="polynomial",
                       degree=degree,
                       coef0=coef0,
                       cost=cost)
           prob=predict(model, newdata=test.set[,-1], probability=T)
           prob=attr(prob,"probabilities")
           # print(cbind(prob,as.character(test.set$y)))
           # print(dim(prob))
           prob=prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svmRad={
           ## SVM with radius (Gaussian) kernel
           ## fine-tune the model with radius kernel and parameters
           ## evaluate the range of gamma parameter between 0.000001 and 0.1
           ## and cost parameter from 0.1 until 10
           tuned <- tune.svm(Y~., data=train.set, 
                             kernel="radial", 
                             gamma=10^(-6:-1), 
                             cost=10^(-1:1))
           #print(summary(tuned))
           gamma = tuned[['best.parameters']]$gamma
           cost = tuned[['best.parameters']]$cost
           model = svm(Y~., data=train.set, probability=T, 
                       kernel="radial", 
                       gamma=gamma, 
                       cost=cost)
           prob=predict(model, newdata=test.set[,-1], probability=T)
           prob=attr(prob,"probabilities")
           # print(cbind(prob,as.character(test.set$y)))
           # print(dim(prob))
           prob=prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svmSig={
           ## SVM with sigmoid kernel
           ## fine-tune the model with radius kernel and parameters
           ## evaluate the range of gamma parameter between 0.000001 and 0.1
           ## and coef0 parameter between -1 and 1
           ## and cost parameter from 0.1 until 10
           tuned <- tune.svm(Y~., data=train.set, 
                             kernel="sigmoid", 
                             gamma=10^(-6:-1), 
                             coef0=-1:1,
                             cost=10^(-1:1))
           #print(summary(tuned))
           gamma = tuned[['best.parameters']]$gamma
           coef0 = tuned[['best.parameters']]$coef0
           cost = tuned[['best.parameters']]$cost
           model = svm(Y~., data=train.set, probability=T, 
                       kernel="sigmoid", 
                       gamma=gamma, 
                       coef0=coef0,
                       cost=cost)
           prob=predict(model, newdata=test.set[,-1], probability=T)
           prob=attr(prob,"probabilities")
           # print(cbind(prob,as.character(test.set$y)))
           # print(dim(prob))
           prob=prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         ada={
           model=ada(Y~., data=train.set)
           prob=predict(model, newdata=test.set[,-1], type='probs')
           # print(cbind(prob,as.character(test.set$y)))
           prob=prob[,2]/rowSums(prob)
           prob
         }
  ) 
}