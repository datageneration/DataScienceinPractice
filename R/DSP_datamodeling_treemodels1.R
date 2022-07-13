## Supervised Learning (Classification): Tree-based models

#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")

library("rpart")
library("rpart.plot")
library("rattle")

# AER Package(AER: Applied Econometrics with R)
# install.packages("AER")
library(AER)

# CreditCard dataset
# card:   "Was the application for a credit card accepted?"
# reports: Number of major derogatory reports.
# age:     Age in years plus twelfths of a year.
# income:  Yearly income (in USD 10,000)
# owner:   Does the individual own their home?
# months:  Months living at current address.
# help("CreditCard") for dataset detail

data(CreditCard)

bankcard <- subset(CreditCard, select = c(card, reports, age, income, owner, months))

# Record card to 0, 1
bankcard$card <- ifelse(bankcard$card == "yes", 1, 0);
bankcard$card

set.seed(1001)
# Order data by row number
newbankcard <- bankcard[sample(nrow(bankcard)),]

# Indexing for training data
t_idx <- sample(seq_len(nrow(bankcard)), size = round(0.70 * nrow(bankcard)))

# Build train and test data
traindata <- newbankcard[t_idx,]
testdata <- newbankcard[ - t_idx,]
summary(bankcard$owner)

# Decision tree model
dtree_creditcard <- rpart::rpart(formula = card ~ ., data = traindata, method = "class", control = rpart.control(cp = 0.001)) # complexity parameter

# Plot Decision tree 
rattle::fancyRpartPlot(dtree_creditcard, type = 1, main = "Decision tree", caption = "Credit card approval" )


result <- predict(dtree_creditcard, newdata = testdata, type = "class")
# Confusion matrix
cm_creditcard <- table(testdata$card, result, dnn = c("Actual", "Predicted"))
cm_creditcard

# Approval ratio
cm_creditcard[4] / sum(cm_creditcard[, 2])

# Denial ratio 
cm_creditcard[1] / sum(cm_creditcard[, 1])

# Accuracy
accuracydt <- sum(diag(cm_creditcard)) / sum(cm_creditcard)
accuracydt

# install.packages("party") 
library(party)

# Conditional Inference Tree
card1 = as.factor(bankcard$card)
ct <- ctree(card1 ~ ., data = bankcard)
plot(ct, main = "Conditional Inference Tree")
table(card1, predict(ct))
predict(ct)
card1
# Random Forest
# install.packages("randomForest")
library(randomForest)
set.seed(1001)
table(bankcard$card)
# randomForest model
rf_creditcard <- randomForest(card ~ ., data = traindata, importance = T, proximity = T, do.trace = 100)
rf_creditcard

plot(rf_creditcard)

round(importance(rf_creditcard), 3) # to three decimal place

result <- predict(rf_creditcard, newdata = testdata)
result_Approved <- ifelse(result > 0.6, 1, 0)
# result_Approved

# Confusion matrix
cm_creditcardrf <- table(testdata$card, result_Approved, dnn = c("Actual", "Predicted"))
cm_creditcardrf

# Approval
cm_creditcardrf[4] / sum(cm_creditcardrf[, 2])

# Denial
cm_creditcardrf[1] / sum(cm_creditcardrf[, 1])

# Accuracy
accuracyrf <- sum(diag(cm_creditcardrf)) / sum(cm_creditcardrf)
accuracyrf

