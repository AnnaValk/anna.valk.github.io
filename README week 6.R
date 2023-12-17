#README week6
oj <- read.csv("C:\\Users\\deeni\\Downloads\\oj.csv")

#simple OLS
basefit <- lm(log(sales) ~ log(price), data=oj)
coef(basefit)

#multilinear regression
brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
coef(brandfit)


#multiple regressions and calculations
pricereg <- lm(log(sales) ~ brand, data=oj)
phat <- predict(pricereg, newdata=oj) 
presid <- log(oj$price) - phat
residfit <- lm(log(sales) ~ presid, data=oj)
coef(basefit)

#Open the data and make some  new variables
data <- read.table("C:\\Users\\deeni\\Downloads\\abortion.dat", skip=1, sep="\t")
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
#detrended log crime rates
y <- data$y_murd

d <- data$a_murd

summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]

dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]

exp(dcoef) - 1



cell <- read.csv("C:\\Users\\deeni\\Downloads\\us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
#calculates cellphone uses rates

#plots cellphone usage and abortion rates
par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
#there is a super clear trend that is almost identical

#now put the relation into a regression
phone <- cellrate[ t + 1 ]
tech <- summary(glm(y ~ phone + t + s +., data=controls))$coef['phone',]
phonecoef <- tech[1]
exp(phonecoef) - 1

#logistic regression with interaction terms
t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]
#almost same number as above




# Revisiting the abortion example

library(gamlr)
## refactor state to have NA reference level
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
dim(x)
## naive lasso regression
naive <- cv.gamlr(cbind(d,x),y); head(coef(naive))
coef(naive)["d",] 

treat <- cv.gamlr(x,d, lmr=1e-3); head(summary(treat))
predtreat <- predict(treat, x, select="min"); head(predtreat)
dhat <- drop(predtreat); length(dhat)


par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3)) 
## little to resemble an experiment here...


## IS R^2?
cor(drop(dhat),d)^2
## Note: IS R2 indicates how much independent signal you have for estimating 
coef(summary( glm( y ~ d + dhat) ))
# re-run lasso, with this (2nd column) included unpenalized (free=2)
causal <- cv.gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-3)
coef(causal, select="min")["d",] 
# AICc says abortion rate has no causal effect on crime.



library(gamlr)
data(hockey)
head(goal, n=2)
player[1:2, 2:7] #players on ice. +1 is home players. 0 is off ice. 
team[1, 2:6] #Sparse Matrix with indicators for each team*season interaction: +1 for home team, -1 for away team. 
config[5:6, 2:7] #Special teams info. For example, S5v4 is a 5 on 4 powerplay, +1 if it is for the home-team and -1 for the away team.


x <- cbind(config,team,player)
y <- goal$homegoal
fold <- sample.int(2,nrow(x),replace=TRUE) 
head(fold)
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
                   free=1:(ncol(config)+ncol(team)), 
                   family="binomial", standardize=FALSE)
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





#part 2
library(Matrix)
data <- read.table("../Data/abortion.dat", skip=1, sep="\t")
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
y <- data$y_murd
d <- data$a_murd
cell <- read.csv("../Data/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
phone <- cellrate[ t + 1 ]
t <- factor(t)
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]

library(AER)
library(gamlr)

dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) }
yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) }

# Orthogonal ML R Function

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

resids <- orthoLTE( x=x, d=d, y=y, 
                    dreg=dreg, yreg=yreg, nfold=5) 
head(resids$dtil)
head(resids$ytil)
2*pnorm(-abs(resids$gam)/resids$se) #p-value supports no effect of abortion access on crime




library(foreign)

descr <- read.dta("../Data/oregonhie_descriptive_vars.dta")
prgm <- read.dta("../Data/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("../Data/oregonhie_survey12m_vars.dta")

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



  
  #Control for number of household members because randomization was imperfect. Randomized across households. If your household was chosen, then everyone in your household became eligible. 

lin <- glm(doc_any_12m ~ selected + numhh, data=P);
round( summary(lin)$coef["selected",],4) # 6-7% increase in prob


levels(X$edu_12m)
source("naref.R")
levels(naref(X$edu_12m))
X <- naref(X) #makes NA the base group

#Continuous variables: 
  #+ set the missing values to 0 or the sample mean


xnum <- X[,sapply(X,class)%in%c("numeric","integer")]
xnum[66:70,]
colSums(is.na(xnum))
# flag missing
xnumna <- apply(is.na(xnum), 2, as.numeric)
xnumna[66:70,]

# impute the missing values
mzimpute <- function(v){ 
  if(mean(v==0,na.rm=TRUE) > 0.5) impt <- 0
  else impt <- mean(v, na.rm=TRUE)
  v[is.na(v)] <- impt
  return(v) }
xnum <- apply(xnum, 2,  mzimpute)
xnum[66:70,]

# replace/add the variables in new data frame 
for(v in colnames(xnum)){
  X[,v] <- xnum[,v]
  X[,paste(v,"NA", sep=".")] <- xnumna[,v] }
X[144:147,]

  
  #After filling in the missings, put everything into a sparse model matrix. Add numhh to control for imperfect randomization.

xhte <- sparse.model.matrix(~., data=cbind(numhh=P$numhh, X))[,-1]
xhte[1:2,1:4]
dim(xhte)


#Now we are ready to deal with heterogeneous treatment effects. 


dxhte <- P$selected*xhte
colnames(dxhte) <- paste("d",colnames(xhte), sep=".")
htedesign <- cbind(xhte,d=P$selected,dxhte)
# include the numhh controls and baseline treatment without penalty 
htefit <- gamlr(x=htedesign, y=P$doc_any_12m, free=c("numhh2","numhh3+","d"))
gam <- coef(htefit)[-(1:(ncol(xhte)+1)), ]
round(sort(gam)[1:6],4)
round(sort(gam, decreasing=TRUE)[1:6],4)


load("../Data/dominicks-beer.rda")
head(wber)
wber = wber[sample(nrow(wber), 100000), ]
head(upc)
dim(upc)
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"]) #ln price per 12 ounces

coef( margfit <- lm(log(MOVE) ~ lp, data=wber[,]) )
#10% increase in price decreases  quantity sold by 6%
#ATE

wber$s <- factor(wber$STORE); wber$u <- factor(wber$UPC); wber$w <- factor(wber$WEEK)
xs <- sparse.model.matrix( ~ s-1, data=wber); xu <- sparse.model.matrix( ~ u-1, data=wber); xw <- sparse.model.matrix( ~ w-1, data=wber)
# parse the item description text as a bag o' words
library(tm)
descr <- Corpus(VectorSource(as.character(upc$DESCRIP)))
descr <- DocumentTermMatrix(descr)
descr <- sparseMatrix(i=descr$i,j=descr$j,x=as.numeric(descr$v>0), # convert from stm to Matrix format
                      dims=dim(descr),dimnames=list(rownames(upc),colnames(descr)))
descr[1:5,1:6]
descr[287,descr[287,]!=0]
controls <- cbind(xs, xu, xw, descr[wber$UPC,]) 
dim(controls)


# naive lasso
naivefit <- gamlr(x=cbind(lp=wber$lp,controls)[,], y=log(wber$MOVE), free=1, standardize=FALSE)
print( coef(naivefit)[1:2,] )
# orthogonal ML 
resids <- orthoLTE( x=controls, d=wber$lp, y=log(wber$MOVE), dreg=dreg, yreg=yreg, nfold=5)

# interact items and text with price
#lpxu <- xu*wber$lp
#colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")
# create our interaction matrix
xhte <- cbind(BASELINE=1,descr[wber$UPC,])
d <- xhte*wber$lp
colnames(d) <- paste("lp",colnames(d),sep=":")

eachbeer <- xhte[match(rownames(upc),wber$UPC),]
rownames(eachbeer) <- rownames(upc)
# fullhte
lnwberMOVE <- log(wber[['MOVE']])
fullhte <- gamlr(x=cbind(d,controls), y=lnwberMOVE, lambda.start=0)
#gamfull <- coef(fullhte)[2:(ncol(lpxu)+1),]
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])

coef(fullhte)


hist(gamfull, main="", xlab="elasticity", col="darkgrey", freq=FALSE)



