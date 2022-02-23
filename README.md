# On clustering categories of categorical variables in GLM

This package creates an alternative representation of the categorical variables in a dataset by clustering them using a logistic regression guided by out-of-sample accuracy. The full paper is available in: https://www.sciencedirect.com/science/article/abs/pii/S0957417421006771


* The proposed approach uses a numerical method guided by the learning performance.
* The underlying structure of the categories and their relationship is identified using proximity graphs.
* Complexity is reduced and accuracy results are competitive against benchmark one-hot encoding of categorical features.


# To install this package, run the following command in R (assuming you have installed devtools).
```Rcode
devtools::install_github("mgr-eco/clustcat")


# Generate a dataset with continuous and categorical variables

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

library(clustcat)

validate = validate_cat(data,categ_thr=10) # This function makes the dataset ready for processing
ordered = ordered_categ(data,j=2,smp_size=0.7,categ_thr=10) #extract the order for the categories # This function creates the dummies with the order of the coefficients
fesible = feasible_clusterings(data,j=2,smp_size=0.7,categ_thr=10) #create feasible clusterings # This function creates feasible clusterings for each categorical predictor
data_final = clustered_model(data,j=2,categ_thr=10,smp_size=0.7,itgrasp=100) #create dataset with the chosen feasible clusterings after GRASP algorithm # This function outputs the final dataset with the clustered categorical predictors (one dummy for each of them)
```
# References
Carrizosa, E., Restrepo, M. G., & Morales, D. R. (2021). On clustering categories of categorical predictors in generalized linear models. Expert Systems with Applications, 115245. https://doi.org/10.1016/j.eswa.2021.115245.
