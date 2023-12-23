#README week 5 
#classification uses data to predict in what label the observations belong
rm(list = ls())

library(MASS)
data(fgl)
dim(fgl)
head(fgl, n = 2)
#opening the data set and check dimentions and first two rows


par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
#plots per component how much of it is in it per type of glass
#here we get the different plots showing per type the components in it

#EXPERIMENTING: i want to see how to make the plots but only showing two types of glass or just of two materials
#this might be nice if you have big data but only are interested in parts
#two materials
par(mfrow=c(1,2))  # Adjust the layout to have one row and two columns
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
#two types of glass
par(mfrow=c(2, 2))
plot(RI ~ type, data=subset(fgl, type %in% c("WinF", "Veh")), col=c(grey(.2), "red", "blue"), las=2)
plot(Al ~ type, data=subset(fgl, type %in% c("WinF", "Veh")), col=c(grey(.2), "red", "blue"), las=2)
plot(Na ~ type, data=subset(fgl, type %in% c("WinF", "Veh")), col=c(grey(.2), "red", "blue"), las=2)
plot(Mg ~ type, data=subset(fgl, type %in% c("WinF", "Veh")), col=c(grey(.2), "red", "blue"), las=2)
plot(Ba ~ type, data=subset(fgl, type %in% c("WinF", "Veh")), col=c(grey(.2), "red", "blue"), las=2)
plot(Si ~ type, data=subset(fgl, type %in% c("WinF", "Veh")), col=c(grey(.2), "red", "blue"), las=2)
#seems to work but still mentions the other types in plot


x <- scale(fgl[,1:9]) 
# column 10 is class label, scale converts to mean 0 sd 1. is a function in R that standardizes the columns of a matrix or data frame. Standardization involves subtracting the mean and dividing by the standard deviation for each column.
apply(x,2,sd) 
# apply function sd to columns of x. is a versatile function in R that applies a function over the margins of an array (e.g., matrix or data frame).
#same results


#using the KNN method to classify the observation to check the X neirest neibors and pick the mode of these as the prediction
# Load the class package, which contains the knn function
library(class)

# Generate a random sample of 10 rows as the test set
test <- sample(1:214, 10)

# Perform KNN classification with k=1, so one neighbor considered
nearest1 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 1)

# Perform KNN classification with k=5, 5 neighbors considered
nearest5 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 5)

# Create a data frame with the true labels and predicted labels for k=1 and k=5
result_df <- data.frame(fgl$type[test], nearest1, nearest5)

# Print the result data frame
print(result_df)
#this randomly shows ten observarions and there neirest neibors and what the observations are predicted to be
# the neirest one and the neirest 5 neigbors show very simulair results

#EXPERIMENT: what if we look also at neirest 10 neigbors? what about 20 
test <- sample(1:214, 10)

nearest1 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 1)
nearest5 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 5)
nearest10 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 10)
nearest20 <- knn(train = x[-test, ], test = x[test, ], cl = fgl$type[-test], k = 20)
# Create a data frame with the true labels and predicted labels for k=1 and k=5
result_df <- data.frame(fgl$type[test], nearest1, nearest5, nearest10, nearest20)
print(result_df)
#conclusion: They all run, still the results of all the test are almost the some except for 1, where nearest1 is different (probably wrong)



#CLASSIFICATION, ACCOUNTING FOR MISCLASSIFICATIONS
credit <- read.csv("C:/Users/deeni/Downloads/credit.csv")

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
#same as slide


library(gamlr)
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)

    default <- credit$Default
    credscore <- cv.gamlr(credx, default, family="binomial")
#predicts the outcome variable defailt in the data
#extracts the default column
    
  par(mfrow=c(1,2))
  plot(credscore$gamlr)
  plot(credscore)
  
  #looks like same plots as before, in the last lecture with Lasso, because this algorithm also uses lasso in the classification
  
  
  
#EXPIRIMENT; what if the levels used only use "poor"
  
  
  credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
  levels(credit$history) = c("poor","poor","poor","poor","poor")
  
  credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))

  
  credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
  levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")
  
  credit <- credit[,c("Default", "duration", "amount",
                      "installment", "age", "history",
                      "purpose", "foreign", "rent")]
  
  library(gamlr)
  credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)
  
  default <- credit$Default
  credscore <- cv.gamlr(credx, default, family="binomial")
  
  par(mfrow=c(1,2))
  plot(credscore$gamlr)
  plot(credscore)
  
  #credit$rent did not run
  #in the plots there are way less lines, because probably there are less cariables involved
  # in the binomial deviance instead of a U shape there is a increasing line
  #R2 is also a bit lower
  
  
  sum(coef(credscore, s="min")!=0) # min
  sum(coef(credscore$gamlr)!=0) # AICc
  sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
  # the OOS R^2
  1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]
  
  
 
  ## What are the underlying default probabilities
  ## In sample probability estimates, visualized in a boxplot
  pred <- predict(credscore$gamlr, credx, type="response")
  pred <- drop(pred) # remove the sparse Matrix formatting
  boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))
  
  #EXPIRIMENT: what if you dont drop pred?
  pred <- predict(credscore$gamlr, credx, type="response")
  boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))
  
  #looks like nothing changes
  
  
 #The code after this for the roc curve honstly did not run, i really tried my best but it kept on giving errors..

  