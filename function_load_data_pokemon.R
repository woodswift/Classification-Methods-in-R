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

## function to check if there is missing value
isMissing <- function(x) {
  sign <- is.na(x);
  tL <- length(sign[sign == TRUE]);
  # fL <- length(sign[sign != TRUE]);
  if (tL>0){
    return ("TRUE");
  }
  else{
    return ("FALSE");
  }
}

## function to check the number of missing value
missingNo <- function(x) {
  sign <- is.na(x);
  tL <- length(sign[sign == TRUE]);
  return (tL);
}

## function to load the original pokemon dataset
## check the missing values at first
## then do the pre-processing, including:
## 1. re-encode
## 2. remove unrelated variables
## 3. create design matrix

load.data.pokemon <- function(){
  datafile <- "pokemon.csv"
  raw.data <- read.csv(datafile)
  # raw.data[1:3,]
  # str(raw.data)
  # summary(raw.data)
  
  ## Step 1: re-encode
  ## (1) encode different names of type into discrete numbers
  levels(raw.data$Type.1)
  levels(raw.data$Type.2)
  
  ## if Type.2 is "No", re-encode it as the same as Type.1
  no.index <- which(raw.data$Type.2=="NO")
  raw.data$Type.2[no.index] <- raw.data$Type.1[no.index]
  
  ## re-check the type names in Type.2
  ## remove "NO" class in Type.2
  raw.data$Type.2 <- factor(raw.data$Type.2)
  # summary(raw.data$Type.2)
  
  type1 <- mapvalues(raw.data$Type.1, from=levels(raw.data$Type.1), to=1:18)
  type2 <- mapvalues(raw.data$Type.2, from=levels(raw.data$Type.2), to=1:18)
  
  ## add the re-encoded type1 and type2 into dataset
  raw.data <- cbind(raw.data, Type1=type1, Type2=type2)
  # levels(raw.data$Type1)
  # summary(raw.data$Type1)
  # levels(raw.data$Type2)
  # summary(raw.data$Type2)
  
  ## remove the original labels
  raw.data$Type.1 <- NULL
  raw.data$Type.2 <- NULL
  
  ## (2) encode generation as factor
  raw.data$Generation <- as.factor(raw.data$Generation)
  # levels(raw.data$Generation)
  # summary(raw.data$Generation)
  
  ## (3) encode legendary as factor
  ## "FALSE":0, "TRUE":1
  false.index <- which(raw.data$Legendary=="FALSE")
  raw.data$Legendary[false.index] <- 0
  raw.data$Legendary[-false.index] <- 1
  raw.data$Legendary <- as.factor(raw.data$Legendary)
  # levels(raw.data$Legendary)
  # summary(raw.data$Legendary)
  
  ## (4) encode total as factor
  ## "<=500":0, ">500":1
  y <- mapvalues(raw.data$Total, from=c("<=500",">500"), to=c(0,1))
  summary(y)
  raw.data <- cbind(Y=y, raw.data)
  # levels(raw.data$Y)
  # summary(raw.data$Y)
  
  ## remove the original labels
  raw.data$Total <- NULL
  
  ## Step 2: remove unrelated labels
  ## unrelated labels: Number, Name
  raw.data$Number <- NULL
  raw.data$Name <- NULL
  
  # str(raw.data)
  # summary(raw.data)
  
  ## Step 3: create desgin matrix
  # the categorical variables: Y, Generation, Legendary, Type1, Type2
  categ.val <- c("Y", "Generation", "Legendary", "Type1", "Type2")
  categ <- raw.data[,categ.val];
  # categ[c(1,2,3),]
  
  ## the rest variables are numerical
  categ.val.loc <- dim(categ.val)
  for (i in 1:length(categ.val)){
    categ.val.loc[i] <- which(names(raw.data)==categ.val[i])
  }
  num <- raw.data[,-categ.val.loc];
  # num[c(1,2,3),]
  
  ## create design matrix; indicators for categorical variables (factors)
  Xcateg <- model.matrix(Y~.,data=categ)[,-1];
  Xcateg[1:3,]
  
  ## build dataset used in different machine learning algorithms
  data <- data.frame(Y=raw.data$Y, num, Xcateg)
  
  ## print the first three rows in the dataset after pre-processing
  # cat("The first three rows in the dataset after pre-processing:",'\n')
  # print(data[1:3,])
  
  ## print the number of missing value in each column
  col.names <- names(raw.data);
  missing <- dim(names(raw.data));
  for(x in col.names){
    missing[x] <- missingNo(raw.data[,x]);
  }
  
  cat("The number of missing value in each column:",'\n')
  print(missing)
  
  return(data)
}