set.seed(12345)
n <- 5000
p <- 3
j <- 2
K_1 <- 5
K_2 <- 7

Z <- matrix(rnorm(n * p), n, p)
colnames(Z) = paste0("Z",1:p)
X1 <- as.factor(sample(letters[1:K_1], size = n, replace = TRUE))
X2 <- as.factor(sample(letters[1:K_2], size = n, replace = TRUE))
library(fastDummies)
X = dummy_cols(data.frame(X1,X2))
X = X[,(j+1):ncol(X)]
X = X[,!grepl("_a",colnames(X))]
data = data.frame(X,Z)
data = data[,sort(names(data))]

beta = c(rep(2,2),rep(-0.5,2),rep(1,3),rep(-1,3),0.03,-0.03,0.5)
cbind(beta,colnames(data))
data = as.matrix(data)


xb = -0.5 + data %*% beta
pr = 1/(1 + exp(-xb))
summary(pr)
Y = rbinom(n=n,size=1,prob=pr)
table(Y)


data = data.frame(X1,X2,Z,Y)
categ_thr=10
smp_size=0.7
j=2
itgrasp=100

#' @title validate_cat
#' @description Make sure that all categories of categorical variable have enough observations to allow for representation in training and testing samples
#' @param data a data.frame, the categorical variables to be clustered should be factors, the rest of the variables including binary categorical should be numeric,
#' the binary response variable should be numeric and in last column and it should be called "Y"
#' @param categ_thr a minimum number of observations for each category
#' @param smp_size percentage of observations to use in training, values from 0 to 1
#' @details This is a data preprocessing step necessary for the clustering algorithm to work. For more details see
#' Carrizosa, E., Galvis Restrepo, M., and Romero Morales, D. (2019). On clustering categories of categorical predictors in generalized linear models. Working paper, Copenhagen Business School.
#' @export

validate_cat = function(data,categ_thr){
  if (colnames(data)[ncol(data)] != "Y") {stop("The response in not in the last column")}
  if (is.numeric(data[,ncol(data)])==FALSE) {stop("The response is not numeric")}
  datac = data.frame(Filter(is.factor,data),Filter(is.numeric,data))
  j= ncol(Filter(is.factor,datac))
  names = colnames(datac)
  colnames(datac) = c(paste0("X",1:j),paste0("Z",1:(ncol(datac)-j-1)),"Y")
  namesnew = colnames(datac)
  cols = ncol(datac)
  for(i in 1:j){
    datac[,ncol(datac)+1] = table(datac[,i])[datac[,i]]
  }

  newdata = datac
  for(i in (cols+1):ncol(newdata)){
    newdata <- newdata[which(newdata[,i]>categ_thr),]
  }

  datac = newdata[,1:cols]

  levels = as.list(1:j)
  for(i in 1:j){
    levels[[i]] = levels(datac[,i])
  }
  for(i in 1:j){
    datac[,i] = factor(datac[,i])
    levels(datac[,i]) = c(0:(nlevels(datac[,i])-1))
    datac[,i] = factor(datac[,i])
  }

  data_val = list(datac,names,levels)
  return(data_val)
}


#' @title ordered_cat
#' @description A method to cluster categorical variables using Logistic regression.
#' @param data a data.frame, the response variable should be in last column and it should be called "Y"
#' @param smp_size a random sample from data
#' @param categ_thr a minimum number of observations for each category
#' @param j the number of categorical variables in the dataset
#' @details There are three functions, ordered_categ gives the order of the categorical variables given by the order of the coefficients of a logistic regression
#' feasible_clusterings gives a set of feasible ways to cluster together the categories of the categorical variables in two groups each (one dummy), clustered_model
#' gives the final dataset with the clustered categorical variables. For more details see
#' Carrizosa, E., Galvis Restrepo, M., and Romero Morales, D. (2019). On clustering categories of categorical predictors in generalized linear models. Working paper, Copenhagen Business School.
#' @import fastDummies
#' data.table
#' magrittr
#' dplyr
#' MLmetrics
#' @export


ordered_categ <- function(data,j,categ_thr,smp_size) {
  data = validate_cat(data,categ_thr)[[1]]
  smp_size = floor(smp_size*nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  model = glm(Y~.,train,family="binomial",maxit=100)
  coef =  setDT(data.frame(coef(summary(model))), keep.rownames= TRUE)[]
  coef = coef[,1:2]
  colnames(coef) = c("variables","coef")
  coef$variables <- sub("(.{2})(_*)","\\1_\\2",coef$variables)
  X = dummy_cols(data.frame(data[,1:j]))
  X = X[,(j+1):ncol(X)]

  categorical_vars <- as.data.frame(colnames(X))
  colnames(categorical_vars) <- c("variables")
  categorical_vars$obs <- 1:nrow(categorical_vars)
  #Merge the two
  categorical_vars <- merge(categorical_vars,coef,all.x=TRUE)
  categorical_vars$coef <- ifelse(is.na(categorical_vars$coef),0,categorical_vars$coef)
  categorical_vars$number<-substr(categorical_vars$variables,2,2)
  categorical_vars <- categorical_vars %>% arrange(number, coef) %>%
    group_by(number) %>%
    mutate(rank = rank(coef, ties.method = "first"))
  #For categorical variables with more than 26 categories (the number of letters in the alphabet)
  categorical_vars <- categorical_vars[order(categorical_vars$rank),]
  categorical_vars$rank <- ifelse(categorical_vars$rank>26,paste0("z",letters),sapply(categorical_vars$rank,function(i)letters[i]))
  categorical_vars$rank = ifelse(categorical_vars$rank=="za","zza",categorical_vars$rank)
  categorical_vars$var_names <- paste("X",categorical_vars$number,categorical_vars$rank,sep="_")

  #1.b. Order the coefficients according to the order in which they come in the dummies#####
  categorical_vars = categorical_vars[order(categorical_vars$obs),]

  return(categorical_vars)
}


#' @title feasible_clusterings
#' @description A method to cluster categorical variables using Logistic regression.
#' @param data a data.frame, the response variable should be in last column and it should be called "Y"
#' @param categ_thr a minimum number of observations for each category
#' @param smp_size a random sample from data
#' @param j the number of categorical variables in the dataset
#' @details There are three functions, ordered_categ gives the order of the categorical variables given by the order of the coefficients of a logistic regression
#' feasible_clusterings gives a set of feasible ways to cluster together the categories of the categorical variables in two groups each (one dummy), clustered_model
#' gives the final dataset with the clustered categorical variables. For more details see
#' Carrizosa, E., Galvis Restrepo, M., and Romero Morales, D. (2019). On clustering categories of categorical predictors in generalized linear models. Working paper, Copenhagen Business School.
#' @import fastDummies
#' data.table
#' magrittr
#' dplyr
#' MLmetrics
#' @export
#Generate a dataset with continuous and categorical variables
#Creating feasible clusterings
categ_thr=10
smp_size=0.7
j=2
itgrasp=100
feasible_clusterings = function(data,j,smp_size,categ_thr){
  data = validate_cat(data,categ_thr)[[1]]
  X = dummy_cols(data.frame(data[,1:j]))
  X = X[,(j+1):ncol(X)]
  categorical_vars = ordered_categ(data,j,categ_thr,smp_size)
  colnames(X) <- categorical_vars$var_names # Change the name for the ordered name
  X <- X[,sort(names(X[,1:ncol(X)]))]
  clustered <- X

  var = colnames(data[,1:j])
  var2 = gsub('[X]', 'X_', var)
  levels = c(seq_along(var))
  for( i in 1:length(var)){
    levels[i] = nlevels(data[,i])
  }


  clustered = as.list(1:length(var2))


  for(i in 1:length(var2)){
    clustered[[i]] = X[,grepl(var2[i],colnames(X))] #We are going to build one dataset for each feasible clustering of each i
  }

  aux = clustered

  for(i in seq_along(aux)){
    for(k in 1:levels[i])
      clustered[[i]][k] = apply(data.frame(aux[[i]][1:k]),1,sum)
  }


  clustered = bind_cols(clustered) ###Final fesible clusterings
  return(clustered)
}



#' @title clustered_model
#' @description A method to cluster categorical variables using Logistic regression.
#' @param data a data.frame, the response variable should be in last column and it should be called "Y"
#' @param categ_thr minimum number of observations per category
#' @param smp_size a random sample from data
#' @param j the number of categorical variables in the dataset
#' @param itgrasp number of iterations of the grasp algorithm, recommended 50 iterations
#' @details There are three functions, ordered_categ gives the order of the categorical variables given by the order of the coefficients of a logistic regression
#' feasible_clusterings gives a set of feasible ways to cluster together the categories of the categorical variables in two groups each (one dummy), clustered_model
#' gives the final dataset with the clustered categorical variables. For more details see
#' Carrizosa, E., Galvis Restrepo, M., and Romero Morales, D. (2019). On clustering categories of categorical predictors in generalized linear models. Working paper, Copenhagen Business School.
#' @import fastDummies
#' data.table
#' magrittr
#' dplyr
#' MLmetrics
#' @export
### This function creates the final dataset with the best clustered categorical variables###
clustered_model = function(data,j,smp_size,categ_thr=10,itgrasp){
  data = validate_cat(data,categ_thr)[[1]]
  categorical_vars = ordered_categ(data,j,categ_thr,smp_size )
  X = dummy_cols(data.frame(data[,1:j]))
  X = X[,(j+1):ncol(X)]
  categorical_vars = ordered_categ(data,j,categ_thr,smp_size)
  colnames(X) <- categorical_vars$var_names # Change the name for the ordered name
  X <- X[,sort(names(X[,1:ncol(X)]))]
  clustered <- X

  var = colnames(data[,1:j])
  var2 = gsub('[X]', 'X_', var)
  levels = c(seq_along(var))
  for( i in 1:length(var)){
    levels[i] = nlevels(data[,i])
  }


  clustered = as.list(1:length(var2))


  for(i in 1:length(var2)){
    clustered[[i]] = X[,grepl(var2[i],colnames(X))] #We are going to build one dataset for each feasible clustering of each i
  }

  aux = clustered

  for(i in seq_along(aux)){
    for(k in 1:levels[i])
      clustered[[i]][k] = apply(data.frame(aux[[i]][1:k]),1,sum)
  }


  clustered = bind_cols(clustered) ###Final fesible clusterings

  smp_size = floor(smp_size*nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  #1.d Data-frame with categories in the order given by our method####
  clusteredtrain = clustered[train_ind,]
  clusteredtest = clustered[-train_ind,]

  data_orig = data
  set.seed(1234)

  var <-  names( data[,1:j])
  acc = list(length(var))

  categorical_vars=data.frame(categorical_vars[order(categorical_vars$var_names),])
  categorical_vars$levels = sub(".*_","",categorical_vars$variables)
  categorical_vars$vars = substr(categorical_vars$variables,1,2)

  for(i in 1:length(var)){
    data[,i] = factor(data[,i],levels=categorical_vars[grepl(var[i],categorical_vars$vars),"levels"])
  }

  ###2. GRASP ALGORITHM #####
  var2 = gsub('[X]', 'X_', var)

  trainord = list(1:length(var2))
  testord = list(1:length(var2))
  for(i in 1:length(var2)){
    trainord[[i]] = clusteredtrain[,grepl(var2[i],colnames(clusteredtrain))] #We are going to build one dataset for each feasible clustering of each j
    testord[[i]] = clusteredtest[,grepl(var2[i],colnames(clusteredtest))]
  }

  #List of dataframes including only the variables used in the regressions
  for(i in 1:length(var)){
    for(j in 1:ncol(trainord[[i]]))
      trainord[[i]][[j]] = cbind(train[,!grepl(var[i],colnames(train))],
                                 trainord[[i]][,j])
  }

  for(i in 1:length(var)){
    for(j in 1:ncol(testord[[i]]))
      testord[[i]][[j]] = cbind(test[,!grepl(var[i],colnames(test))],
                                testord[[i]][,j])
  }

  for( i in 1:length(var)){
    for(j in 1:ncol(trainord[[i]]))
      colnames(trainord[[i]][[j]])[ncol(trainord[[i]][[j]])] = names(trainord[[i]])[j]
  }
  for( i in 1:length(var)){
    for(j in 1:ncol(testord[[i]]))
      colnames(testord[[i]][[j]])[ncol(testord[[i]][[j]])] = names(testord[[i]])[j]
  }

  probabilities <- testord

  for(i in seq_along(testord)){ #Estimate a GLM for each clustering
    for (j in 1:length(testord[[i]])){
      probabilities[[i]][[j]] <- data.frame(predict(glm(Y~., data=trainord[[i]][[j]], family="binomial",model=FALSE,y=FALSE),
                                                    newdata=testord[[i]][[j]],type="response"))
    }}

  for(i in seq_along(testord)){
    for (j in 1:length(testord[[i]])){
      probabilities[[i]][[j]] <- ifelse(probabilities[[i]][[j]]>0.5,1,0)
    }}

  library(MLmetrics)

  accuracy <- testord

  for(i in seq_along(testord)){#Calculate accuracy
    for(j in seq_along(testord[[i]])){
      accuracy[[i]][[j]] <- Accuracy(probabilities[[i]][[j]],testord[[i]][[j]]$Y)
    }
  }

  for(i in 1:length(var)){
    accuracy[[i]] = accuracy[[i]][1,]
    accuracy[[i]] = data.frame(t(accuracy[[i]]))
  }
  accuracy
  final_formula = list(1:itgrasp)
  final_data = list(1:itgrasp)

  #####13. Repeats of the algorithm####
  for(repeats in 1:itgrasp){

    #choose randomly one accuracy from the top h%
    best = accuracy

    for(i in 1:length(var)){
      best[[i]] = setDT(accuracy[[i]],keep.rownames = TRUE)
      colnames(best[[i]])[ncol(best[[i]])] = "accuracy"
      best[[i]]$number = substr(best[[i]]$rn,3,3)
      best[[i]]$rank = rank(-best[[i]]$accuracy,ties.method = "first")
    }


    tgrasp <- 3
    top = as.list(seq_along(best))
    for(i in seq_along(top)){
      top[[i]] <- best[[i]][which(best[[i]]$rank<=tgrasp),]$rn
    }

    a = c(seq_along(var))
    for(i in seq_along(top)){
      a[i] = sample(top[[i]],1,replace=FALSE)
    }

    for(i in seq_along(top)){
      best[[i]] = best[[i]][order(best[[i]]$rn),]
    }
    best
    m = as.list(seq_along(best))
    for(i in seq_along(m)){
      m[[i]] = matrix(nrow=nrow(best[[i]]),ncol=nrow(best[[i]]))
      rownames(m[[i]]) =c(1:nrow(best[[i]]))}

    for(i in seq_along(m)){
      for(j in 1:nrow(best[[i]]))
        m[[i]][,j] = ifelse(as.numeric(rownames(m[[i]]))<=j,1,0)
    }

    m

    for(i in seq_along(best)){
      colnames(m[[i]]) = best[[i]]$rn
    }
    x = a
    for(i in seq_along(a)){
      x[i] = substr(sub("X_","X",a[i]),1,2)
    }
    x
    data2 = data.frame(data)
    for( i in seq_along(var)){
      levels(data2[,x[i]]) = m[[i]][,a[i]]
    }
    #Cluster the corresponding categorical variable with the cluster identified in the previous step
    head(data2)
    data2 = data2[,!sapply( data2, function(top) nlevels(top)==1)]# Eliminate the variable in case that?s the chose cluster

    train = data2[train_ind,]
    test = data2[-train_ind,]

    head(data2)

    model_final = glm(Y~.,data=train,family="binomial")
    pred = ifelse(predict(model_final,newdata=test,type="response")>0.5,1,0)
    accuracy_final = Accuracy(pred,test$Y)

    final_data[[repeats]] = data2
    final_formula[[repeats]] = c(a,accuracy_final)
    final_formula
  }

  names(final_formula) = c(1:itgrasp)
  final_formula2 = bind_rows(final_formula)
  final_formula2 = data.frame(t(final_formula2))
  final_formula2[,ncol(final_formula2)] = as.character(final_formula2[,ncol(final_formula2)])
  final_formula2[,ncol(final_formula2)] = as.numeric(final_formula2[,ncol(final_formula2)])
  mod = rownames(final_formula2[which(final_formula2[,ncol(final_formula2)]==max(final_formula2[,ncol(final_formula2)])),])[1]
  final_data = final_data[[as.numeric(mod)]]

  return(final_data)
}
