#README week 5 
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
  
  
  
  # Creates thresholds for the matrix
  rule <- 1/5  # Move this around to see how these change
  
  # False positive rate at 1/5 rule
  sum((pred > rule)[default == 0]) / sum(pred > rule)
  
  # False negative rate at 1/5 rule
  sum((pred < rule)[default == 1]) / sum(pred < rule)
  
  # Sensitivity
  sum((pred > rule)[default == 1]) / sum(default == 1)
  
  # Specificity
  sum((pred < rule)[default == 0]) / sum(default == 0)
  
  # Out-of-sample evaluation
  test <- sample.int(1000, 500)
  credhalf <- gamlr(credx[-test, ], default[-test], family = "binomial")
  predoos <- predict(credhalf, credx[test, ], type = "response")
  defaultoos <- default[test]
  
  # ROC Curve (Receiver Operating Characteristic)
  source("roc.R")  # This might be the source of the error
  
  # Plot ROC Curve for in-sample
  roc(p = pred, y = default, bty = "n", main = "in-sample")
  
  # Our 1/5 rule cutoff
  points(x = 1 - mean((pred < 0.2)[default == 0]),
         y = mean((pred > 0.2)[default == 1]),
         cex = 1.5, pch = 20, col = 'red')
  
  # A standard 'max prob' (p=0.5) rule
  points(x = 1 - mean((pred < 0.5)[default == 0]),
         y = mean((pred > 0.5)[default == 1]),
         cex = 1.5, pch = 20, col = 'blue')
  
  # Legend for in-sample ROC
  legend("bottomright", fill = c("red", "blue"),
         legend = c("p=1/5", "p=1/2"), bty = "n", title = "cutoff")
  
  # ROC Curve for out-of-sample
  roc(p = predoos, y = defaultoos, bty = "n", main = "out-of-sample")
  
  # Our 1/5 rule cutoff
  points(x = 1 - mean((predoos < 0.2)[defaultoos == 0]),
         y = mean((predoos > 0.2)[defaultoos == 1]),
         cex = 1.5, pch = 20, col = 'red')
  
  # A standard 'max prob' (p=0.5) rule
  points(x = 1 - mean((predoos < 0.5)[defaultoos == 0]),
         y = mean((predoos > 0.5)[defaultoos == 1]),
         cex = 1.5, pch = 20, col = 'blue')
  
  # Save ROC Curve plot to a file
  png(file = "ROCCurve.png", width = 600, height = 350)
  
  
  # Close the PNG file
  dev.off()
  
  # Plotting factor levels of 'Default' against 'history' in the 'credit' dataset
  par(mai = c(.8, .8, .1, .1))
  plot(factor(Default) ~ history, data = credit, col = c(8, 2), ylab = "Default")
  
  
  
  
  
  
  
  
  
  
  
  