# anna.valk.github.io

 data <- read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv",header=TRUE)
  browser <- read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")
 
  dim(browser)
  head(browser)

 mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)
 
 
#bootstrap
 
 B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
#answer is 76.5508
 
  h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
  #can you explain why we need each term in the last expression? 
  lines(xfit, yfit, col = "black", lwd = 2)
  
#nice histogram
 
 
#experiment, what is we do not use replacement, so if a someone is drawn, he cannot be drawn again
 
 B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=FALSE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
# answer is 0, This process essentially creates a permutation of the original data, and since you are always resampling the same data, your resampled means will be identical for each resample. Therefore, the standard deviation is effectively zero. This represents a situation where there's no variability among the means because each resample is the same.
 
  h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
  #can you explain why we need each term in the last expression? 
  lines(xfit, yfit, col = "black", lwd = 2)
 
# just a gray box
 
 
#experiment, what happens if B <- 500
 
 B <- 500
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
# answer 80.994
 
  h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
  #can you explain why we need each term in the last expression? 
  lines(xfit, yfit, col = "black", lwd = 2)
 
# nice histogram


##experiment, what happens if B <- 10
 
 B <- 10
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
 
# answer 79.807
 
  h <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
  #can you explain why we need each term in the last expression? 
  lines(xfit, yfit, col = "black", lwd = 2)
 
# nice histogram
 

 
 
 data <- read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")
 browser = read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")



> head (data)

> mean(brower$spend)

data <- read.table(text = "id;anychildren;broadband;hispanic;race;region;spend header = TRUE, sep = ";")
print(data)


> mean(data$spend); var(data$spend)/1e4; sqrt(var(data$spend)/1e4)
[1] 1065.5
[1] 98.94415
[1] 9.947067

 rm (b,B)
> B <- 1000
> mub <- c()
> for (b in 1:1000){
+     samp_b <- sample.int(nrow(data), replace=TRUE)
+     mub <- c(mub, mean(data$spend[samp_b]))
+ }
> sd(mub)
[1] 366.7413

data <- read.csv("C:/Users/deeni/Downloads/survey_response_data.csv")

  round(summary(spend)$coef,2)
  pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
  pvalrank <- rank(pval)
  reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) 
  png(file="figs/BHAlgoExample.png",
  width=600, height=350)
  plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
  lines(pvalrank, (0.1/9)*pvalrank)
  dev.off()


