#===========================================
# Example 1 : SVM Spam Classification
#===========================================

# Load library
library(e1071)

# Create sample dataset
emails = data.frame(
  word_freq = c(10,20,5,30,40,15,7,35),
  link_count = c(2,3,0,4,5,1,0,4),
  spam = factor(c("Yes","Yes","No","Yes","Yes","No","No","Yes"))
)

# View dataset
emails

#-------------------------------------------
# Fit SVM Model
#-------------------------------------------

svm_model = svm(spam ~ word_freq + link_count,
                data = emails,
                kernel = "radial",
                probability = TRUE)

# Display model
svm_model
summary(svm_model)

#-------------------------------------------
# Prediction
#-------------------------------------------

predicted = predict(svm_model, emails)

predicted

# Observed values
observed = emails$spam

# Comparison table
comparison = data.frame(observed, predicted)

comparison

#-------------------------------------------
# Plot Data Points
#-------------------------------------------

plot(emails$word_freq,
     emails$link_count,
     col = as.numeric(emails$spam),
     pch = 19,
     xlab = "Word Frequency",
     ylab = "Link Count",
     main = "SVM Classification with Curved Decision Boundary")

#-------------------------------------------
# Highlight Support Vectors
#-------------------------------------------

points(emails$word_freq[svm_model$index],
       emails$link_count[svm_model$index],
       pch = 5,
       cex = 2)

#-------------------------------------------
# Create Grid for Curved Boundary
#-------------------------------------------

x_range = seq(min(emails$word_freq)-5,
              max(emails$word_freq)+5,
              length = 100)

y_range = seq(min(emails$link_count)-2,
              max(emails$link_count)+2,
              length = 100)

grid = expand.grid(word_freq = x_range,
                   link_count = y_range)

#-------------------------------------------
# Predict on Grid Points
#-------------------------------------------

pred_grid = predict(svm_model, grid)

# Convert to matrix
z = matrix(as.numeric(pred_grid),
           length(x_range),
           length(y_range))

#-------------------------------------------
# Draw Curved Decision Boundary
#-------------------------------------------

contour(x_range,
        y_range,
        z,
        add = TRUE,
        drawlabels = FALSE,
        col = "blue",
        lwd = 2)

#-------------------------------------------
# Legend
#-------------------------------------------

legend("topleft",
       legend = c("Spam","Not Spam","Support Vectors"),
       col = c(2,1,1),
       pch = c(19,19,5),
       pt.cex = c(1,1,2))