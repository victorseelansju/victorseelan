library(e1071)

# Load dataset
data(iris)
summary(iris)

# Train SVM model
model = svm(Species ~ ., data = iris, kernel = "radial")
model

# Make predictions
pred = predict(model, iris)
pred

# Confusion matrix
table(Predicted = pred, Actual = iris$Species)

#===========================================Example 1 =================
library(e1071)

# Create sample dataset
emails = data.frame(
  word_freq = c(10,20,5,30,40,15,7,35),
  link_count = c(2,3,0,4,5,1,0,4),
  spam = factor(c("Yes","Yes","No","Yes","Yes","No","No","Yes"))
)
emails

# Fitting SVM model
svm_model = svm(spam ~ word_freq + link_count,
                 data = emails,
                 kernel = "linear", probability = TRUE)
#The radial kernel is used to handle possible nonlinear separation.
#kernel = "linear")
#kernel = "polynomial", degree = 3)
#kernel = "radial")
#kernel = "sigmoid")


svm_model
summary(svm_model)

# Prediction
predicted = predict(svm_model, emails)
predicted

attributes(predicted)

observed = emails$spam

comparision = data.frame(observed, predicted)
comparision
#==================== plot ========================
# Plot data
plot(emails$word_freq, 
     emails$link_count, 
     col=as.numeric(emails$spam), pch=13)

# Highlight support vectors
points(emails$word_freq[svm_model$index], 
       emails$link_count[svm_model$index],
       pch=12, cex=1)


#========== Evaluation metrics =============================
# Confusion Matrix
cm = table(Predicted = predicted,Actual = emails$spam)
cm

# Accuracy
mean(predicted == emails$spam)

# Extract values from confusion matrix
TP = cm["Yes","Yes"]
TN = cm["No","No"]
FP = cm["Yes","No"]
FN = cm["No","Yes"]

# Precision
precision = TP / (TP + FP)

# Recall
recall = TP / (TP + FN)

# F1 Score
f1_score = 2 * (precision * recall) / (precision + recall)

#specificity
specificity = TN/(TN+FP)

precision
recall
f1_score
specificity


library(pROC)

# ROC Curve and AUC
pred_prob = predict(svm_model, emails, probability = TRUE)

probabilities = attr(pred_prob, "probabilities")[,"Yes"]

roc = roc(emails$spam, probabilities)

# Plot ROC
plot(roc, main="ROC Curve for SVM")

# AUC value
auc(roc)


#======================== Example 2 = large samples ====
set.seed(123)   # For reproducibility

# creating new dataset , size = 200
emails1 = data.frame(
  word_freq1 = sample(1:50, 200, replace = TRUE),
  link_count1 = sample(0:10, 200, replace = TRUE) 
)
emails1


# Create spam classification rule (condition "OR" OPERATOR)
emails1$spam1 = factor(ifelse(emails1$word_freq1 > 25 | emails1$link_count1 > 4,
                             "Yes", "No"))
emails1$spam1

emails1

# View first few rows
head(emails1)
View(emails1)

# Check dataset size
dim(emails1)

# Fitting SVM model
svm_model1 = svm(spam1 ~ word_freq1 + link_count1,
                data = emails1,
                kernel = "radial", probability = TRUE)



#The radial kernel is used to handle possible nonlinear separation.
svm_model1
summary(svm_model1)

# Prediction
predicted1 = predict(svm_model1, emails1)
predicted1
attributes(predicted1)



observed1 = emails1$spam1

comparision1 = data.frame(observed1, predicted1)
comparision1

#========== Evaluation metrics =============================
# Confusion Matrix
cm1 = table(Predicted = predicted1,Actual = emails1$spam1)
cm1
attributes(predicted)


# Accuracy
mean(predicted == emails1$spam1)



# Extract values from confusion matrix
TP1 = cm1["Yes","Yes"]
TN1 = cm1["No","No"]
FP1 = cm1["Yes","No"]
FN1 = cm1["No","Yes"]

# Precision
precision1 = TP1 / (TP1 + FP1)

# Recall
recall1 = TP1 / (TP1 + FN1)

# F1 Score
f1_score1 = 2 * (precision1 * recall1) / (precision1 + recall1)

#specificity
specificity1 = TN1/(TN1+FP1)

precision1
recall1
f1_score1
specificity1


library(pROC)

# ROC Curve and AUC
pred_prob1 = predict(svm_model1, emails1, probability = TRUE)

probabilities1 = attr(pred_prob1, "probabilities")[,"Yes"]
attributes(predicted)




roc1 = roc(emails1$spam1, probabilities1)

# Plot ROC
plot(roc1, main="ROC Curve for SVM")

# AUC value
auc(roc1)
