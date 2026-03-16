install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)


salary_data = data.frame(
  
  Age = c(22,23,24,25,26,27,28,29,30,31,
          32,33,34,35,36,37,38,39,40,41,
          42,43,44,45,46,47,48,49,50,51),
  
  Experience = c(1,2,1,3,2,4,3,5,6,5,
                 7,8,9,10,11,12,13,14,15,16,
                 17,18,19,20,21,22,23,24,25,26),
  
  Education = factor(c(
    "UG","UG","UG","UG","UG","UG","UG","UG","UG","UG",
    "PG","PG","PG","PG","PG","PG","PG","PG","PG","PG",
    "PG","PG","PG","PG","PG","PG","PG","PG","PG","PG"
  )),
  
  Salary_Level = factor(c(
    "Low","Low","Low","Low","Low","Low","Low","Low","Low","Low",
    "Low","Low","High","High","High","High","High","High","High","High",
    "High","High","High","High","High","High","High","High","High","High"
  ))
)

salary_data

str(salary_data)
head(salary_data)
summary(salary_data)


model2 = rpart(Salary_Level ~ Age + Experience + Education,
                data = salary_data,
                method = "class",
                control = rpart.control(cp = 0.01))

summary(model2)
print(model2)


rpart.plot(model2)


#prediction
new_employee = data.frame(
  Age = 36,
  Experience = 12,
  Education = factor("PG", levels=c("UG","PG"))
)

predict(model1,new_employee,type="class")
