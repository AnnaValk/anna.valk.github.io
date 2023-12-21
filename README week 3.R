#README week3

rm(list = ls())

oj <- read.csv("C:\\Users\\deeni\\Downloads\\oj.csv")


#display data
head(oj, n=5)    
tail(oj, n=5)    

#regression of log sales, with brand and log price
glm(log(sales) ~ brand + log(price), data=oj)

#display this regression in x.In x you can see per observation, predicted value of log sales
x <- model.matrix(~ brand + log(price), data=oj); head(x); tail(x)

#EXPERIMENT: put log sales in
y <- model.matrix(log(sales)~brand + log(price), data=oj)
#conclusion: exact same outcome, apparently don't need to write log sales, it automatically knows

#EXPERIMENT: different regression
Z <- model.matrix(log(sales) ~ brand, data=oj)
#conclusion: Now only 3 lines are visable in the variable Z instead of 4

#creates factors of the brand variable
oj$brand = as.factor(oj$brand)
x <- model.matrix(~ brand + log(price), data=oj); head(x)
#this model looks exactly like the previous matrix in X, makes sence, because in a regression, r automatically makes dummies of categorical vars

#also creates factors of the brand variable, but with a different reference group
oj$mybrand = relevel(oj$brand, "tropicana")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)
#now tropicana is the reference group, and we see the maid brand and dominicks

#EXPERIMENT: using minute.maid as reference group
oj$mybrand = relevel(oj$brand, "minute.maid")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)
#works

#creates a regression with interation terms, this creates interaction terms for all variables included
glm(log(sales) ~ log(price)*brand*feat, data=oj)
#shows all the seperate regression coefficients and the interation terms

#EXPERIMENT: interation without the term "feat"
glm(log(sales) ~ log(price)*brand, data=oj)
#lot less terms visable now feat is not involved




email <- read.csv("C:\\Users\\deeni\\Downloads\\spam.csv")
#shows dimensions of data
dim(email)
#shows column names
colnames(email) 

#logistic regression of spam on all variables, logistic because family is binomial
#if it is not a linear regression, should always specify the family of relationship that is happening
glm(spam ~ ., data=email, family='binomial')
#fit regression in spammy
spammy <- glm(spam ~ ., data=email, family='binomial')

#per variable, shows the coeficient
coef(spammy)["word_free"]; exp(coef(spammy)["word_free"])
coef(spammy)["word_george"]; exp(coef(spammy)["word_george"]); 1/exp(coef(spammy)["word_george"])

#EXPIRIMENT: try with different vars
coef(spammy)["word_email"]; exp(coef(spammy)["word_email"])
coef(spammy)["word_credit"]; exp(coef(spammy)["word_credit"])  

#creat prediction of the first 4000 emails, on if it is spam or not
predict(spammy, newdata = email[c(1,4000),], type="response")

#EXPIRIMENT: see if results with last 600 responses
predict(spammy, newdata = email[c(4000,4600),], type="response")
#the variable 4000 of course shows the same predictions, the rest shows the predictions of the additional 600 variables

#shows the summary of the regressions
summary(spammy)$deviance
summary(spammy)$null.deviance

#calculate R2 in the program for logistical regressions, by divide deviance by null deviance
D <- summary(spammy)$deviance; D
D0 <- summary(spammy)$null.deviance; D0
R2 <- 1 - D/D0; R2
# r2 (generalized) = 0.749
#deviance is the measured difference from the fitted model to the saturated model. 
#null deviance shows the goodness of fit when only considering the intercept Bo
# R2 here is not the unsual interpetation but actually the porportion of deviance reduced by adding predictors. 
# or the percentage of Y varianece explained by X
# 0.749 is a pretty high R2, so pretty strong regression. 


# try to get other form of Rsquared. 
residuals <- residuals(spammy)
total_var <- sum((spammy$model$'log(sales)' - mean(spammy$model$'log(sales)'))^2)
residual_var <- sum(residuals^2)

R2_linear <- 1 - (residual_var / total_var)
R2_linear
#does not work because it is not a linear regression I think

# try another way
residuals <- residuals(spammy)
total_var <- sum((spammy$model$spam - mean(spammy$model$spam))^2)
residual_var <- sum(residuals^2)

if (total_var != 0) {
  R2_linear <- 1 - (residual_var / total_var)
  R2_linear
} else {
  warning("Total variance is zero. R-squared is not defined.")
}
#answer now is -0.41, which is wierd for an Rsquared, idk if this is a trustworthy method. 
#I think this is just not possible. But good to see how it would work. 

