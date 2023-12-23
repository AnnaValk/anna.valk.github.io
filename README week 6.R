#README week6
oj <- read.csv("C:\\Users\\deeni\\Downloads\\oj.csv")

#simple OLS
basefit <- lm(log(sales) ~ log(price), data=oj)
coef(basefit)
#is the consumer demand elasticity

#multilinear regression
brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
coef(brandfit)
#if we control for brand, the elasticity increases

#EXPERIMENT, using brandminute.maid as reference chatagorie
oj$brand <- factor(oj$brand)
# Set 'brandminute.maid' as the reference category
oj$brand <- relevel(oj$brand, ref = "minute.maid")
brandfit <- lm(log(sales) ~ brand + log(price), data = oj)
coef(brandfit)
#the effect of log(price) does not change because of this


#multiple regressions and calculations
pricereg <- lm(log(sales) ~ brand, data=oj)
phat <- predict(pricereg, newdata=oj) 
presid <- log(oj$price) - phat
residfit <- lm(log(sales) ~ presid, data=oj)
coef(basefit)


#shows how LTE works
# Open the data and make some new variables
data <- read.table("C:\\Users\\deeni\\Downloads\\abortion.dat", skip=1, sep="\t")
# Assign names to the columns of the 'data' dataframe
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
# Remove rows where the 'state' column is equal to 2, 9, or 12
data <- data[!(data$state %in% c(2,9,12)),]
# Remove rows where the 'year' column is not between 85 and 97 (inclusive)
data <- data[data$year > 84 & data$year < 98,]
# Take the natural logarithm of the 'pop' column and assign it to 'pop'
data$pop <- log(data$pop)

# Create a new variable 't' representing the time difference from the year 1985
t <- data$year - 85
# Create a new factor variable 's' from the 'state' column
s <- factor(data$state)
# Create a new dataframe 'controls' containing specific columns from 'data'
controls <- data.frame(data[,c(3,10:17)])


## y is de-trended log crime rate, a is as described below
#detrended log crime rates
y <- data$y_murd

d <- data$a_murd

summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]

dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]

exp(dcoef) - 1

#get a regression of as depended the log crime rate, and it uses controls to estimate this.



#EXPERIMENT: what if we do not filter for the year variables. 
rm(data)
data <- read.table("C:\\Users\\deeni\\Downloads\\abortion.dat", skip=1, sep="\t")
# Assign names to the columns of the 'data' dataframe
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
# Remove rows where the 'state' column is equal to 2, 9, or 12
data <- data[!(data$state %in% c(2,9,12)),]
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state)
controls <- data.frame(data[,c(3,10:17)])

y <- data$y_murd
d <- data$a_murd
summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]
dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]
exp(dcoef) - 1
#conclusion: the estimates actually dont seem to change at all


#this part shows that cellphone rates move simular to abortion and murder rates
cell <- read.csv("C:\\Users\\deeni\\Downloads\\us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
#calculates cellphone uses rates

#plots cellphone usage and abortion rates
par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
#there is a super clear trend that is almost identical between abortions and cellphones

#EXPERIMENT: how to make nice plots with different collors
abortion_color <- "yellow"
cellphone_color <- "purple"
par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=abortion_color)
points(1985:1997, cellrate, bg=cellphone_color, pch=21)
legend("topleft", fill=c(abortion_color, cellphone_color), legend=c("abortions", "cellphones"), bty="n")
#it works, is nice to use for thesis


#now put the relation into a regression, of abortion and cellphone
phone <- cellrate[ t + 1 ]
tech <- summary(glm(y ~ phone + t + s +., data=controls))$coef['phone',]
phonecoef <- tech[1]
exp(phonecoef) - 1
#has a big effect on murder rate.

#EXPERIMENT: How does this effect change if you dont use the control variables. 
phone <- cellrate[ t + 1 ]
tech <- summary(glm(y ~ phone, data=controls))$coef['phone',]
phonecoef <- tech[1]
exp(phonecoef) - 1
#effect is a lot smaller, does this say something about robustness? 


#logistic regression with interaction terms and quadretic terms 
t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]
#almost same number as above, no significant difference


#Now the goal is the do the same regression but then using the LASSO LTE methods,
#Lasso LTE basically first looks at variables effecting treatment, and then holds those constant
#then it optimizes regression on Y
library(gamlr)
## This creates some factors
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
dim(x)


## naive lasso regression (idk why Naive)
naive <- cv.gamlr(cbind(d,x),y); head(coef(naive))
coef(naive)["d",] 

treat <- cv.gamlr(x,d, lmr=1e-3); head(summary(treat))
predtreat <- predict(treat, x, select="min"); head(predtreat)
dhat <- drop(predtreat); length(dhat)


par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3)) 
## little to resemble an experiment here...
#it is naive because it is a very simple approach
#stil looks like a strong relation in the plot. 

## what is in sample R2?
cor(drop(dhat),d)^2
## Note: IS R2 indicates how much independent signal you have for estimating 
coef(summary( glm( y ~ d + dhat) ))
# re-run lasso, with this (2nd column) included unpenalized (free=2)
causal <- cv.gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-3)
coef(causal, select="min")["d",] 


#WIERD: in the slides it shows a 0, so there is no causal effect, but i got a 0.113?
#does that mean that AIC claims causal effect? I think i might have done an expiriment effecting these results



#this code performs a LTE model 
#using a combination of lasso regularization and logistic regression, 
#incorporating cross-validation and confidence interval estimation.

#loading data
library(gamlr)
data(hockey)
head(goal, n=2)
#filtering the data
player[1:2, 2:7] #players on ice. +1 is home players. 0 is off ice. 
team[1, 2:6] #Sparse Matrix with indicators for each team*season interaction: +1 for home team, -1 for away team. 
config[5:6, 2:7] #Special teams info. For example, S5v4 is a 5 on 4 powerplay, +1 if it is for the home-team and -1 for the away team.

#making a matric
x <- cbind(config,team,player)
y <- goal$homegoal
#creating folds for cross validatation, so it makes on -k and test on k
fold <- sample.int(2,nrow(x),replace=TRUE) 
head(fold)
#Lasso
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
                   free=1:(ncol(config)+ncol(team)), 
                   family="binomial", standardize=FALSE)
#Fitting a regression with non zero coeficients of the best model, which has been desided by cross validation on the lasso models. 

selected <- which(coef(nhlprereg)[-1,] != 0)
xnotzero <- as.data.frame(as.matrix(x[,selected]))
nhlmle <- glm( y ~ ., data=xnotzero, 
               subset=which(fold==2), family=binomial )
#showing the model made
summary(nhlmle)

#confidence interval
x[1,x[1,]!=0] #check first observation for players on the ice
fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$fit; fit
se.fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$se.fit; se.fit
CI = fit + c(-2,2)*se.fit
CI #90% confidence interval for probability that Edmonton scored the goal is 
#0.3050716 0.5682728, so it is statistically significant effect

#EXPERIMENT: what if we do cross validation if replacement = false
fold <- sample.int(2,nrow(x),replace=FALSE) 
head(fold)
#Lasso
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
                   free=1:(ncol(config)+ncol(team)), 
                   family="binomial", standardize=FALSE)
#Fitting a regression with non zero coeficients of the best model, which has been desided by cross validation on the lasso models. 

selected <- which(coef(nhlprereg)[-1,] != 0)
xnotzero <- as.data.frame(as.matrix(x[,selected]))
nhlmle <- glm( y ~ ., data=xnotzero, 
               subset=which(fold==2), family=binomial )

summary(nhlmle)
x[1,x[1,]!=0] #check first observation for players on the ice
fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$fit; fit
se.fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$se.fit; se.fit
CI = fit + c(-2,2)*se.fit
CI #90% confidence interval for probability that Edmonton scored the goal is 
#conclusion: Error in sample.int(2, nrow(x), replace = FALSE) : 
#cannot take a sample larger than the population when 'replace = FALSE'
# so if there is no replacement, the sample is larger then the population so it does not work


#EXPERIMENT: what if we use standardize=TRUE
fold <- sample.int(2,nrow(x),replace=TRUE) 
head(fold)
#Lasso
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
                   free=1:(ncol(config)+ncol(team)), 
                   family="binomial", standardize=TRUE)
#Fitting a regression with non zero coeficients of the best model, which has been desided by cross validation on the lasso models. 

selected <- which(coef(nhlprereg)[-1,] != 0)
xnotzero <- as.data.frame(as.matrix(x[,selected]))
nhlmle <- glm( y ~ ., data=xnotzero, 
               subset=which(fold==2), family=binomial )

summary(nhlmle)
x[1,x[1,]!=0] #check first observation for players on the ice
fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$fit; fit
se.fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$se.fit; se.fit
CI = fit + c(-2,2)*se.fit
CI #90% confidence interval for probability that Edmonton scored the goal is 
#conclusion: This runs a lot longer then the other one. 
#My R actually closed during this so i will leave it at that..


#part 2
#This part uses the orthogonal ML, so that is basically cross validation but then for causal interverence. 
#first preparing the data
library(Matrix)
data <- read.table("C:\\Users\\deeni\\Downloads\\abortion (1).dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
#this defines outcome and treatment variable
y <- data$y_murd
d <- data$a_murd
#how to calculate cellphone rates 
cell <- read.csv("C:\\Users\\deeni\\Downloads\\us_cellphone (1).csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
phone <- cellrate[ t + 1 ]
#setting up the matrix
t <- factor(t)
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]

library(AER)
library(gamlr)

#this defines functions of regularization
dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) }
yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) }


# Orthogonal ML R Function, actually using the orthogonal ML

orthoLTE <- function(x, d, y, dreg, yreg, nfold=2)
{
  # randomly split data into folds
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold, 
                    times = ceiling(nobs/nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  # create residualized objects to fill
  ytil <- dtil <- rep(NA, nobs)
  # run OOS orthogonalizations
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
    yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
    dhat <- predict(dfit, x[I[[b]],], type="response")
    yhat <- predict(yfit, x[I[[b]],], type="response")
    dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
    ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
    cat(b," ")
  }
  rfit <- lm(ytil ~ dtil)
  gam <- coef(rfit)[2]
  se <- sqrt(vcovHC(rfit)[2,2])
  cat(sprintf("\ngamma (se) = %g (%g)\n", gam, se))
  
  return( list(gam=gam, se=se, dtil=dtil, ytil=ytil) )
}



# OrthoML and effect of abortion access on crime
#the actual regression using the folds
resids <- orthoLTE( x=x, d=d, y=y, 
                    dreg=dreg, yreg=yreg, nfold=5) 
#showing the outcomes
head(resids$dtil)
head(resids$ytil)
#calculating the P value of the regression
2*pnorm(-abs(resids$gam)/resids$se) #p-value supports no effect of abortion access on crime
#WIERD: I actually have very different results then the slides
#ofcourse there is randomization, but these numbers are wayyy different, even signs are changing
#means that it might not be stable, or i did something wrong
#> head(resids$dtil)
#[1]  0.0408149196  0.0000320417  0.0078787082  0.0055741836  0.0054012168
#[6] -0.0032739624
#> head(resids$ytil)
#[1] -0.005454531 -0.094294933 -0.097393072  0.037117589  0.042902639
#[6]  0.158536660
#> 2*pnorm(-abs(resids$gam)/resids$se) #p-value supports no effect of abortion access on crime
#dtil 
#0.109359


#EXPERIMENT: the original used 2 folds, what if we use more? 5 folds
resids <- orthoLTE(x=x, d=d, y=y, dreg=dreg, yreg=yreg, nfold=5)
head(resids$dtil)
head(resids$ytil)
2*pnorm(-abs(resids$gam)/resids$se) #p-value supports no effect of abortion access on crime
#conclusion: the results change masively, but my run was also very different from the slides, so might just be sensitive in general



#Doing the HTE, which has treatment effect that can be different for different people
library(foreign)

descr <- read.dta("C:\\Users\\deeni\\Downloads\\oregonhie_descriptive_vars.dta")
prgm <- read.dta("C:\\Users\\deeni\\Downloads\\oregonhie_stateprograms_vars.dta")
s12 <- read.dta("C:\\Users\\deeni\\Downloads\\oregonhie_survey12m_vars.dta")

# nicely organized, one row per person
all(s12$person_id == descr$person_id)
all(s12$person_id == prgm$person_id)

P <- descr[,c("person_id","household_id", "numhh_list")]
P$medicaid <- as.numeric(prgm[,"ohp_all_ever_firstn_30sep2009"]=="Enrolled")
P$selected <- as.numeric(descr[,"treatment"]=="Selected")
levels(P$numhh_list) <- c("1","2","3+")

# 12 month is the survey that really matters
# need to control for household size interacted with survey return time
Y <- s12[,c("weight_12m",
            "doc_any_12m","doc_num_mod_12m",
            "er_any_12m","er_num_mod_12m",
            "hosp_any_12m","hosp_num_mod_12m")]
Y$doc_any_12m <- as.numeric(Y$doc_any_12m=="Yes")
Y$er_any_12m <- as.numeric(Y$er_any_12m=="Yes")
Y$hosp_any_12m <- as.numeric(Y$hosp_any_12m=="Yes")

# smk_ever_12m - num19_12m are sources of heterogeneity, plus descr
X <- s12[,121:147]
X$dt_returned <- factor(format(s12$dt_returned_12m, "%Y-%m"))

insurv <- which(s12$sample_12m_resp == "12m mail survey responder")
X <- X[insurv,]
Y <- Y[insurv,]
P <- P[insurv,]

sapply(Y,function(y) sum(is.na(y)))
nomiss <- which( !apply(Y,1, function(y) any(is.na(y))) )
X <- X[nomiss,]
Y <- Y[nomiss,]
P <- P[nomiss,]

# pull out the weights and attach doc_any to P
weights <- Y[,1]
Y <- Y[,-1]

# replace some ridiculous values in survey and drop num19
X$hhsize_12m[X$hhsize_12m>10] <- 10
X$num19_12m <- NULL

# organize to make it pretty for text
P$doc_any_12m <- Y$doc_any_12m # you can explore other responses if you want
P <- P[,c(1,2,6,5,4,3)]
names(P)[6] <- "numhh"

# data has been cleaned in the background
head(P,n=3)
dim(P)
table(P$selected)

  
  #Average effects can be computed as follows 

ybar <- tapply(P$doc_any_12m, P$selected, mean)
( ATE = ybar['1'] - ybar['0'] )

nsel <- table(P[,c("selected")])
yvar <- tapply(P$doc_any_12m, P$selected, var)
( seATE = sqrt(sum(yvar/nsel)) )

ATE + c(-2,2)*seATE
#same result as slide, so it worked. 
  
  #Control for number of household members because randomization was imperfect. Randomized across households. If your household was chosen, then everyone in your household became eligible. 

lin <- glm(doc_any_12m ~ selected + numhh, data=P);
round( summary(lin)$coef["selected",],4) # 6-7% increase in prob



