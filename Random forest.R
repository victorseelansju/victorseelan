install.packages("randomForest")
library(randomForest)
# No = No Default
# yes = Default
bankloan = data.frame(
  CreditScore = c(750,680,720,600,590,710,670,640,730,620),
  AnnualIncome = c(80000,50000,65000,40000,35000,72000,48000,45000,77000,42000),
  Age = c(45,34,40,28,25,42,36,30,50,27),
  Default = factor(c("No","Yes","No","Yes","Yes","No","Yes","Yes","No","Yes"))
)
bankloan

#model building
model1 = randomForest(Default ~ CreditScore + AnnualIncome + Age,
                         data = bankloan,
                         ntree = 100)
#number of trees in the forest.
print(model1)

#prediction
pred = predict(model1, bankloan)
pred

#confusion matrix
table(Predicted = pred, Actual = bankloan$Default)

#Most important variable
importance(model1)
varImpPlot(model1)



# regression
new_cus_alex = data.frame(
  CreditScore = 700,
  AnnualIncome = 60000,
  Age = 38
)
predict(model1, new_cus_alex)
