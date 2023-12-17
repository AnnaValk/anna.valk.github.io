#read me week4 
rm(list = ls())
SC <- read.csv("C:\\Users\\deeni\\Downloads\\semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
#put a regression of all variables on FAIL, where family=binomial means that FAIL is binomial. This regression is put in fail
1 - full$deviance/full$null.deviance
#this is the formula of R2, it predicts 56% of fail


# Step 1 - K-fold functions
## first, define the deviance and R2 functions

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
  return(1 - dev/dev0) }


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




#EXPIRIMENT: what if famility is only binomial. Does the bosplot change?
# I first tried in step 1 only but ofcourse then you can not see the difference in boxplot
# i still only changed step 1 a little and then rest code just same
deviance <- function(y, pred, family=c("binomial")){ 
#only mention binomial
  family <- match.arg(family)
  if(family=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

R2 <- function(y, pred, family=c("binomial")){
  fam <- match.arg(family)   
  if(fam=="binomial"){ 
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  
  return(1 - dev/dev0)
}
n <- nrow(SC) 
K <- 10
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
Out <- data.frame(full=rep(NA,K)) 
for(k in 1:K){ 
  train <- which(foldid!=k) 
  
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  
  ## calculate and log R2
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")

  cat(k, " ")
}

# Step 3 - K-fold plots

boxplot(Out, col="plum", ylab="R2")

#conclusion: this boxplot also shows R2 in minus, from -2 and the lowest dot at -16.
#the pink bos is from -3 to -7
#in original the plot is from 0 tot -20 
#the pink bos is from -2 to -15
#this might well be because of the randomness in the samples used



#FORWARD STEPWISE PEGRESSION
null <- glm(FAIL ~ 1, data = SC)
#The formula FAIL ~ 1 specifies a model with only an intercept term, and data = SC indicates that the data for the model is the SC data frame.
fwd <- step(null, scope = formula(full), dir = "forward")
#This line uses the step function to perform forward variable selection. It takes the null model null as the starting point and adds variables step by step based on the specified scope. scope = formula(full) indicates the scope of the search. Here, full is assumed to be the model you previously defined, and the forward variable selection process will be conducted within the scope of this model. dir = "forward" specifies that the selection process is forward.
length(coef(fwd))
#length(coef(fwd)) is counting the number of coefficients in the model obtained through forward variable selection
#result: 69


#EXPIREMENT: what does scope do? what happens if we lower it
#scope full is that all variables are considerd and if low no variables are considerd
#Setting scale = 0 makes the stepwise procedure more conservative, as it relies solely on changes in deviance. A smaller scale value (closer to 0) makes the algorithm less likely to include variables, as it requires a greater reduction in deviance for a variable to be added.
# Experimenting with Different Scopes
# Start with a more restricted lower scope
lower_scopes <- c("~ 1", "SIG1", "SIG2", "SIG3")

for (lower_scope in lower_scopes) {
  # Define the full model
  full_model <- glm(FAIL ~ ., data = SC, family = binomial)
  
  # Create the null model based on the lower scope
  null_model <- glm(FAIL ~ 1, data = SC)
  
  # Use stepwise regression with different lower scopes
  fwd <- step(null_model, scope = formula(full_model), direction = "forward", scale = 0)
  
  # Print or store relevant information (e.g., selected variables, AIC)
  print(paste("Lower Scope:", lower_scope))
  print(summary(fwd))
}
#observarion: takes very long to run compared to other one
length(coef(fwd))

#conclusion: Null deviance: 93.230  on 1476  degrees of freedom
#Residual deviance: 74.555  on 1408  degrees of freedom
#AIC: -79.121
# 69 variables, simular as before


#LASSO: gives penalty for using more variables
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


## run a lasso path plot
spender <- gamlr(xweb, log(yspend), verb=TRUE); spender

plot(spender) ## path plot


cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)
#plot has dip at -3.75


#EXPIRIMENT: what labda is being use and the effect of changing that
# Extract lambda values and corresponding performance metrics
lambda_values <- cv.spender$lambda
cv_errors <- cv.spender$cvm

# Print the lambda values and corresponding cross-validated errors
cbind(lambda = lambda_values, cv_error = cv_errors)
# Find the lambda value with minimum cross-validated error
min_lambda <- lambda_values[which.min(cv_errors)]

# Print the minimum lambda
print(min_lambda)

#seems that a lot of different labdas are being used here

#EXPIRIMENT: setting labda to 

cv.spender <- cv.gamlr(xweb, log(yspend), lambda_sequence = seq(0.1, 1, by = 0.1))

plot(cv.spender)
#both plots have same dimentison,but second one is more steeper

cv.spender <- cv.gamlr(xweb, log(yspend), lambda_sequence = seq(0.1, 4, by = 0.1))

plot(cv.spender)
#most notible difference is that the varience at the end is later now

cv.spender <- cv.gamlr(xweb, log(yspend), lambda_sequence = seq(0.5, 1, by = 0.1))

plot(cv.spender)
#now the gray lines are even larger and the blue line is flatter




