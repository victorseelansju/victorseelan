install.packages("rpart")

library(rpart)
library(rpart.plot)

# Create dataset
salary_data = data.frame(
  Age = c(22,23,24,25,26,27,28,29,30,31,35,37,40,42,45,48),
  Experience = c(1,2,3,4,5,6,7,8,9,10,12,14,16,18,20,22),
  Education = factor(c("UG","UG","UG","UG","UG","UG","UG","UG",
                       "PG","PG","PG","PG","PG","PG","PG","PG")),
  Salary_Level = factor(c("Low","Low","Low","Low","Low","Low","Low","Low",
                          "High","High","High","High","High","High","High","High"))
)

salary_data
str(salary_data)
head(salary_data)



# Model building
model1 = rpart(Salary_Level ~ Age + Experience + Education,
                data = salary_data,
                method = "class",
                control = rpart.control(cp = 0, minsplit = 2))

# Model summary
summary(model1)

# Print model
print(model1)

#Plot decision tree (classification)
rpart.plot(model1)

#prediction
new_employee = data.frame(
  Age = 34,
  Experience = 11,
  Education = factor("PG", levels = c("UG","PG"))
)

predict(model1, new_employee, type = "class")
