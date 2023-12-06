# anna.valk.github.io

  browser <- read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")
 
  dim(browser)
  head(browser)

#frequentist

 mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)
 
#same results as in slides
 
 
#BOOTSTRAP
 
 B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
#answer is 78.93607
#this is different than the results in the slides, but that makes sence because it is random
#bootstrap algoritm line by line explained  
#B <- 1000; variable that have value of a thousant, meaning that there are a thousent bootstrap samples 
#  mub <- c() ; stores the means of the bootstraps
# for (b in 1:1000){   : a loop of a thousant
#    samp_b <- sample.int(nrow(browser), replace=TRUE); each loops generates a random sample of the browser data and does this with replacement
#    mub <- c(mub, mean(browser$spend[samp_b])); takes the mean of the sample and stores it in mub
  }
#  sd(mub); shows the std error of the mub
 
  h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
  lines(xfit, yfit, col = "black", lwd = 2)
  
#this code provides an histogram with a bar graph of the bootstrap we just did, and a normal distribution line of the frequentist 
 
 
#EXPERIEMENT 1, what iF we do not use replacement, so if an observation is drawn, it cannot be drawn again
 
 rm(B, mub, samp_b, b, xfit, yfit)


 B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
# answer is 81.40998, and histogram looks the same
 
 
 
 
#experiment, what happens if B <- 500
 rm(B, mub, samp_b, b, xfit, yfit)

 
 B <- 500
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
# answer 77.457 and histogram looks simulare


##experiment, what happens if B <- 10
 
 B <- 10
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
# answer 79.807, seems like not mucha changes
 
 
#BOOTSTRAP REGRESSION
 B <- 1000
  betas <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
    betas <- rbind(betas, coef(reg_b))
  }; head(betas, n=3)
 cov(betas[,"broadband"], betas[,"anychildren"])
 
 
#BH ALGORITHM
 
 browser <- read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")

  spendy <- glm(log(spend) ~ . -id, data=browser)
#fits a generalizad linear model to the data of all the variables on log spend, and it puts the regression in the variable spendy
  round(summary(spendy)$coef,2)
#this summarizes the regression, round is just to round on 2 decimiles. Basicly the effect of each var on log spend, with the p values in the table
  pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
#this line collects all P values of the regression and stores them in pval
  pvalrank <- rank(pval)
#this line ranks the pvalues of the regression
  reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) 
#here if pvalue is befor the BH threshold, then we can reject Ho. Here the rejected values are stored in reject
  
  plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
  lines(pvalrank, (0.1/9)*pvalrank)
#this plots the pvalues ranked and collers the one that are significant red. 

reject
#this desplays the variables and the values they have in reject. If they have value 2, they are significantly explaining spend.

#EXPERIMENT BH ALGORITHM: what is the effect of setting the q higher. to q=0.5

pendy <- glm(log(spend) ~ . -id, data=browser)
  round(summary(spendy)$coef,2)
  pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
  pvalrank <- rank(pval)
  reject <- ifelse(pval< (0.5/9)*pvalrank, 2, 1) 
reject

# now all exept one value reject Ho, so all are seen as having significant effects. It seems hard to say what is the ideal level of q, since in q=0.1, already 5/9 variables rejected Ho. 


rm(browser, pendy, spendy, pval, pvalrank, reject)
#REGULARIZATION
#CROSS VALIDATION, GO THROUGH EXAMPLE

SC <- read.csv("C:\\Users\\deeni\\Downloads\\semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
#put a regression of all variables on FAIL, where family=binomial means that FAIL is binomial. This regression is put in fail
1 - full$deviance/full$null.deviance
#this is the formula of R2


# Step 1 - K-fold functions
## first, define the deviance and R2 functions

# Out of sample prediction experiment
# Define the deviance and R2 functions

# pred must be probabilities (0 < pred < 1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
# Ensure the specified family is valid
  family <- match.arg(family)
  
  if(family=="gaussian"){
# Calculate deviance for Gaussian distribution (sum of squared differences)
    return( sum( (y-pred)^2 ) )
  } else {
# If family is binomial, calculate deviance using negative log-likelihood
    if(is.factor(y)) y <- as.numeric(y)>1
    
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

# get null deviance too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
# Ensure the specified family is valid
  fam <- match.arg(family)
  
  if(fam=="binomial"){
# If family is binomial, convert y to binary numeric vector if it's a factor
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  
# Calculate the deviance of the model and the null model
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  
# Return R2 using the formula 1 - dev/dev0
  return(1 - dev/dev0)
}


# Step 2 - K-fold Partition/Experiment

# setup the experiment
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
Out <- data.frame(full=rep(NA,K)) 
# use a for loop to run the experiment
for(k in 1:K){ 
	train <- which(foldid!=k) # train on all but fold `k'
		
## fit regression on full sample
	rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)

## get prediction: type=response so we have probabilities
	predfull <- predict(rfull, newdata=SC[-train,], type="response")

## calculate and log R2
	Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")

## print progress
	cat(k, " ")
}


# Step 3 - K-fold plots


boxplot(Out, col="plum", ylab="R2")



#FORWARD STEPWISE PEGRESSION
null <- glm(FAIL ~ 1, data = SC)
#The formula FAIL ~ 1 specifies a model with only an intercept term, and data = SC indicates that the data for the model is the SC data frame.
fwd <- step(null, scope = formula(full), dir = "forward")
#This line uses the step function to perform forward variable selection. It takes the null model null as the starting point and adds variables step by step based on the specified scope. scope = formula(full) indicates the scope of the search. Here, full is assumed to be the model you previously defined, and the forward variable selection process will be conducted within the scope of this model. dir = "forward" specifies that the selection process is forward.
length(coef(fwd))
#length(coef(fwd)) is counting the number of coefficients in the model obtained through forward variable selection


#LASSO
install.packages("gamlr")
rm(list = ls())


library(gamlr)
## Browsing History. 
## web has 3 colums: [machine] id, site [id], [# of] visits
web <- read.csv("C:\\Users\\deeni\\Downloads\\browser-domains.csv")
## Read in actual website names and relabel site factor
sitenames <- scan("C:\\Users\\deeni\\Downloads\\browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
## also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))
## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]
## use this info in a sparse matrix
## this is something you'll be doing a lot; familiarize yourself.
xweb <- sparseMatrix(
	i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
	dims=c(nlevels(web$id),nlevels(web$site)),
	dimnames=list(id=levels(web$id), site=levels(web$site)))
# what sites did household 1 visit?
#head(xweb[1, xweb[1,]!=0])
## now read in the spending data 
yspend <- read.csv("C:/Users/deeni/Downloads/browser-totalspend.csv", row.names = 1)
# us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix

```

```{r}
## run a lasso path plot
spender <- gamlr(xweb, log(yspend), verb=TRUE); spender
```

---

```{r}
plot(spender) ## path plot


cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)


#LECTURE 5
rm(list = ls())

library(MASS)
data(fgl)
dim(fgl)
head(fgl, n = 2)

par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
#plots per component how much of it is in it per type of glass

x <- scale(fgl[,1:9]) 
# column 10 is class label, scale converts to mean 0 sd 1. is a function in R that standardizes the columns of a matrix or data frame. Standardization involves subtracting the mean and dividing by the standard deviation for each column.
apply(x,2,sd) 
# apply function sd to columns of x. is a versatile function in R that applies a function over the margins of an array (e.g., matrix or data frame).

# Load the class package, which contains the knn function
library(class)

# Generate a random sample of 10 rows as the test set
test <- sample(1:214, 10)

# Perform KNN classification with k=1
nearest1 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 1)

# Perform KNN classification with k=5
nearest5 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 5)

# Create a data frame with the true labels and predicted labels for k=1 and k=5
result_df <- data.frame(fgl$type[test], nearest1, nearest5)

# Print the result data frame
print(result_df)


#CLASSIFICATION, ACCOUNTING FOR MISCLASSIFICATIONS
#### ******* German Credit Data ******* ####
credit <- read.csv("C:/Users/deeni/Downloads/credit.csv"
)


# Re-level the 'history' variable and change levels to more meaningful labels
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")

# Adjust the levels and labels of the 'foreign' variable
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))

# Create a new binary variable 'rent' based on the condition 'housing=="A151"'
credit$rent <- factor(credit$housing=="A151")

# Adjust the levels of the 'purpose' variable
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

# Subset the dataset to include specific columns
credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]


head(credit)
dim(credit)

library(gamlr)
source("naref.R")
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)
```

---

.pull-left[

```{r}
default <- credit$Default
credscore <- cv.gamlr(credx, default, family="binomial")
```

]

.pull-right[

```{r}
par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)


sum(coef(credscore, s="min")!=0) # min
sum(coef(credscore$gamlr)!=0) # AICc
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]
```

---

```{r}
## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))




