#README WEEK 2
rm(list = ls())

browser <- read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")

#show dementions
dim(browser)
#show first rows of data set
head(browser)

#frequentist, to estimate the distribution of a variable, assuming normal distribution
# here you will see the mean of the variable and the whith of the distributio
mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)
#same results as in slides


#BOOTSTRAP: this does not assume a normal distribution for the var

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))}
sd(mub)

#answer is 78.93607
#this is different than the results in the slides, but that makes sence because it is random
#bootstrap algoritm line by line explained  
#B <- 1000; variable that have value of a thousant, meaning that there are a thousent bootstrap samples 
#  mub <- c() ; stores the means of the bootstraps
# for (b in 1:1000){   : a loop of a thousant
#    samp_b <- sample.int(nrow(browser), replace=TRUE); each loops generates a random sample of the browser data and does this with replacement
#    mub <- c(mub, mean(browser$spend[samp_b]))}; takes the mean of the sample and stores it in mub
#  sd(mub); shows the std error of the mub

h <- hist(mub)
#in h places a histogram of the variable mub
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
#creats a normal distribution line of mub
yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
lines(xfit, yfit, col = "black", lwd = 2)
#creats the barplot of mub
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
# answer 79.807, seems like not much changes
#conclusion is that if you use less samples, it might decrease accuracy but the results here a very simlar

#BOOTSTRAP REGRESSION: so what is happening here is that many samples are taken in the bootstrap
# then of all these samples (in a loop), regressions on spend are made with broadband and anychildren
#then in last row, heads shows the first 3 regressions 
B <- 1000
betas <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
  betas <- rbind(betas, coef(reg_b))
}; head(betas, n=3)

#calculates the covarience between the two variables
cov(betas[,"broadband"], betas[,"anychildren"])

# to see the average coefficient of all regressions use this code. 
#this sould be a good representative regression
average_coeffs <- colMeans(betas)
# Display the average coefficients
print(average_coeffs)
#(Intercept)   broadband anychildren 
#5.68652967  0.55184000  0.08117284 

#for reference, glm to see normal regression
k <- glm(log(spend) ~broadband + anychildren, data=browser)
print (k)
#(Intercept)    broadband  anychildren  
#5.68508      0.55285      0.08216 
#conclusion, very simular results from bootstrap

#EXPERIMENT: different sample size
B <- 1000
betas <- c()
sample_sizes <- c(50, 100, 200, 500)

for (size in sample_sizes) {
  for (b in 1:B) {
    samp_b <- sample.int(nrow(browser), size = size, replace = TRUE)
    reg_b <- glm(log(spend) ~ broadband + anychildren, data = browser[samp_b,])
    betas <- rbind(betas, coef(reg_b))
  }
}

head(betas, n = 3)
average_coeffs <- colMeans(betas)
print(average_coeffs)
#(Intercept)   broadband anychildren 
#5.68138353  0.55728399  0.07551483 
#conclusion: there are some minor differences between the regression I got out of the code provided and this one
# but with 1000 samples, it might mean that this has significant effect

#EXPERIMENT: how to creat confidence interval
B <- 1000
betas <- matrix(NA, nrow = B, ncol = 3)  

for (b in 1:B) {
  samp_b <- sample.int(nrow(browser), replace = TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data = browser[samp_b,])
  betas[b, ] <- coef(reg_b)
}

# Calculate confidence intervals for each coefficient
ci <- apply(betas, 2, function(x) quantile(x, c(0.025, 0.975)))
# Display confidence intervals
ci
#conclusion: here i can see if the coeficients are significant       
#[,1]      [,2]       [,3]
#2.5%  5.605343 0.4657910 0.02108967
#97.5% 5.772418 0.6329115 0.15140125

#EXPERIMENT: plot histogram of all regressions
B <- 1000
betas <- matrix(NA, nrow = B, ncol = 3)  

for (b in 1:B) {
  samp_b <- sample.int(nrow(browser), replace = TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data = browser[samp_b,])
  betas[b, ] <- coef(reg_b)
}

# Plot histograms for each coefficient
par(mfrow = c(1, 3), mar=c(4,4,2,1))  # Adjust the layout based on the number of coefficients

for (i in 1:ncol(betas)) {
  hist(betas[, i], main = paste("Coefficient ", i), xlab = "Value")
}


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

# conclusion: now all except one value reject Ho, so all are seen as having significant effects. It seems hard to say what is the ideal level of q, since in q=0.1, already 5/9 variables rejected Ho.


