#README WEEK 1
 
CEO_Diary <- read.csv("C:\\Users\\deeni\\Downloads\\survey_response_data (1).csv")

#open data
View(CEO_Diary)

#showing data type
apply(CEO_Diary,2,class)

#summary of first 5 columns of data set
summary(CEO_Diary[1:5])

#EXPERIMENT, what code to use to get speciffic summaries
summary(CEO_Diary)
summary(CEO_Diary[1:10])
summary(CEO_Diary[10])
summary(CEO_Diary$hr)
#conclusion, code sensitive to capital letters and commas
#If want to summaries everything, then just put the name of data set,
#If want to summaries specific far, put number column or far name

#chatgpt told me to instal these 
install.packages(c("ggplot2", "officer", "flextable"))
library(ggplot2)
library(officer)
library(flextable)
dir.create("figs")

# the following code helps creat a picture of a graph with the shares of time the CEO puts in each activity
#the third line shows that it is a barplot, with probabilities of doing each activity
png(file = "C:\\Users\\deeni\\Downloads\\CEOTypes.png", width = 800, height = 300)
par(mar = c(9, 3, 1, 1))
barplot(prop.table(table(CEO_Diary$type)), las = 2)
dev.off()

#EXPERIMENT: making a probability plot of the duration of each ativity
png(file = "C:\\Users\\deeni\\Downloads\\CEOTypes.png", width = 800, height = 300)
par(mar = c(9, 3, 1, 1))
barplot(prop.table(table(CEO_Diary$F_duration)), las = 2)
dev.off()
#conclusion: seems to work, also automatically overrides the previous plot, so if want multiple plots, name them different

#EXPERIMENT: what happens if you change the margins? 
#(initially: margin first position to 20, but apperently to large)
# Margin first position to 12
png(file = "C:\\Users\\deeni\\Downloads\\CEOTypes1.png", width = 800, height = 300)
par(mar = c(12, 3, 1, 1))
barplot(prop.table(table(CEO_Diary$type)), las = 2)
dev.off()

#margin last position to 0
png(file = "C:\\Users\\deeni\\Downloads\\CEOTypes3.png", width = 800, height = 300)
par(mar = c(9, 3, 1, 0))
barplot(prop.table(table(CEO_Diary$type)), las = 2)
dev.off()
#conclusion: Margin just changes the position of the graph, it does not seem much different


#table with how much each outcome happens
table(CEO_Diary$type)
#table with proportion each outcome happens
prop.table(table(CEO_Diary$type))

#in R, you can do regressions with lm for simple and glm for complex
#you can store these regressions in a variable
# ~ means: the following are the independent
#always state the data set, as multiple data sets could be open
fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); summary(fit)

#EXPERIMENT: what does summary fit mean
fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary)
#conclusion: makes sense, it is the summary of the variable fit. 
#first store the regression in the variable, then look at what it looks like

#EXPERIMENT: different regression
fit1 <- glm(strategy ~ chairman + coo, data=CEO_Diary); summary(fit1)
# it works

