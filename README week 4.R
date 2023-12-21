#read me week4 
rm(list = ls())
SC <- read.csv("C:\\Users\\deeni\\Downloads\\semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
#put a regression of all variables on FAIL, where family=binomial means that FAIL is binomial. This regression is put in fail
1 - full$deviance/full$null.deviance
#this is the formula of R2, it predicts that the X's explain 56% of the variance of "fail"


# Step 1 - K-fold out of sample example. This algorithm breaks the sample into K random peaces.
#for each K fold, it makes regression on -k folds (so everything except K) and then checks the fit on K

## first, define the deviance and R2 functions to make sure we can let K folds algorithm work and check how good the fits are
# for binominal, the function should take between 0 and 1
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


# Step 2 - K-fold Partition/Experiment, here the goal is to split the sample randomly and then we can run the regressions

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

#just as in class, the R2 is somehow negative. This is possible because it explains the model less directly
#what this means is that the suggested model might be a worse fit then the null model (so only including the intercept.)


#EXPIRIMENT: what if famility is only binomial. Does the boxplot change?
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
#so it still seems that the null model might be a better fit then the suggested model. 



#EXPERIMENT: what happens if we use a different regression here with only a couple possible variables? will the model be worse?
#change is that model is worse because less varaibles for the algorithm to pick from. 
n <- nrow(SC) 
K <- 10 
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
Out <- data.frame(full=rep(NA,K)) 
for(k in 1:K){ 
  train <- which(foldid!=k) 

  rfull <- glm(FAIL~ SIG1 + SIG2 + SIG3 + SIG4 + SIG5, data=SC, subset=train, family=binomial)
  
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  
  cat(k, " ")
}

# Step 3 - K-fold plots

boxplot(Out, col="plum", ylab="R2")

#this moddel actually has a possitive R2, meaning that it explains more variance of Y then the Null model
#It might be that this model is better because it has less complexity


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


#EXPERIMENT: what if we lower the level of complexity allowed by the model of forward stepwise regression?
null <- glm(FAIL ~ 1, data = SC)
#adding a K=0.1, lower K means higher pentality for complexity
fwd <- step(null, scope = formula(full), dir = "forward", k = 0.1)
length(coef(fwd))
#conclusion: this algorithm took a very long time. it seems like the complexity is actually way higher then in the original, because coef is now 175. 
#Lets try again with a lower K to see if that works better. 
null <- glm(FAIL ~ 1, data = SC)
#adding a K=0.001, lower K means higher pentality for complexity
fwd <- step(null, scope = formula(full), dir = "forward", k = 0.001)
length(coef(fwd))
#it seems actually that lower does not mean less complex, since this one also runs for very long and is adding many variables
#actually this one has even more variables, 197 to be exact. 

#Lets try one more time with a higher K to see if that works better. 
null <- glm(FAIL ~ 1, data = SC)
#adding a K=0.5, lower K means higher pentality for complexity
fwd <- step(null, scope = formula(full), dir = "forward", k = 0.5)
length(coef(fwd))
#conclusion: it is going down but still has 132 variables. Apperently the level of complexity was very low in the original
#one final try with K=0.95
null <- glm(FAIL ~ 1, data = SC)
#adding a K=0.95, lower K means higher pentality for complexity
fwd <- step(null, scope = formula(full), dir = "forward", k = 0.95)
length(coef(fwd))
#conclusion: 107 coeffiencients, still way higher then before.
#chatgpt helped me to understand: The behavior you're observing might be due to the interplay of the penalty parameter with the specific characteristics of your data. In certain cases, very low values of 
#k might actually encourage the algorithm to include more variables, and very high values might not penalize complexity enough.


#EXPERIMENT: simplefying the model of forward step to max 5 variables
null <- glm(FAIL ~ 1, data = SC)
#setting the max number of variables to 5, which is way less then 69, but also could be good if you want a simple to interpret model
#it would still include the 5 most relevant variables
max_vars <- max(5, length(coef(null)))
fwd <- step(null, scope = formula(full), direction = "forward", steps = max_vars)
length(coef(fwd))
#this works to get a simpler regression.



#LASSO: gives penalty for using more variables. This algorithm finds a couple of suitable models that could work.One model per Lambda value
#after this we need to find the model we want to use
install.packages("gamlr")
rm(list = ls())

library(gamlr)
# Browsing History Analysis

# Read in browsing history data
web <- read.csv("C:\\Users\\deeni\\Downloads\\browser-domains.csv")

# Read actual website names and relabel site factor
sitenames <- scan("C:\\Users\\deeni\\Downloads\\browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)

# Also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))

# Get total visits per machine and percentage of time on each site
machinetotals <- as.vector(tapply(web$visits, web$id, sum)) 
visitpercent <- 100 * web$visits / machinetotals[web$id]

# Use this info in a sparse matrix
xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id), nlevels(web$site)),
  dimnames=list(id=levels(web$id), site=levels(web$site)))

# Read in spending data 
yspend <- read.csv("C:/Users/deeni/Downloads/browser-totalspend.csv", row.names = 1)
yspend <- as.matrix(yspend) # Convert to matrix

# Run a lasso path plot, which shows at which value the coeficient is driven to zero
spender <- gamlr(xweb, log(yspend), verb=TRUE)
spender

# Plot the lasso path
plot(spender)
#this is the crazy plot with all the different lines and collors

# Cross-validation for optimal lambda value, so here actually K folds is being used 
cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)
# Plot indicates a dip at lambda = -3.75, which means the optimal plot is at log lambda is -3.75


#EXPERIMENT: what if we limit the algorithm by only letting it use the first hundred variables availible?

# Use this info in a sparse matrix, considering only the first 100 variables
library(Matrix)

# Extract unique machine IDs and site IDs
unique_ids <- as.numeric(factor(web$id, levels = unique(web$id)))
unique_sites <- as.numeric(factor(web$site, levels = unique(web$site)))

# Filter for the first 100 variables
filtered_ids <- unique_ids[unique_ids <= 100]
filtered_sites <- unique_sites[unique_sites <= 100]

# Create the sparse matrix with the filtered variables
xweb <- sparseMatrix(
  i = as.numeric(web$id), 
  j = as.numeric(web$site), 
  x = visitpercent,
  dims = c(max(filtered_ids), max(filtered_sites)),
  dimnames = list(id = levels(web$id), site = levels(web$site))
)

# Run a lasso path plot, which shows at which value the coefficient is driven to zero
spender <- gamlr(xweb, log(yspend), verb=TRUE)
spender
# Plot the lasso path
plot(spender)
# Cross-validation for optimal lambda value, so here actually K folds are being used 
cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)

#I actually really tried a lot but this did not want to work :(
