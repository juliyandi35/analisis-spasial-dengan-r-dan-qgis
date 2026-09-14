require(devtools)
require(tidyr)
require(dplyr)
require(sp)
require(stringr)
require(ggplot2)
library(spcv)

m.sample <- read.csv("April.csv")
str(m.sample)
m.sample$conc <- m.sample$Magnitude

set.seed(123)
# Memuat fungsi createDataPartition untuk pembagian data
train_index <- createDataPartition(y = m.sample$conc, p = 0.9, list = FALSE)

# Memisahkan data menjadi data train dan data test
data_train <- m.sample[train_index, ]
data_test <- m.sample[-train_index, ]
coordinates(data_train) <- c("Longitude","Latitude")
coordinates(data_test) <- c("Longitude","Latitude")

cv_range = seq(1,50, by=.32)
cv.t <- lapply(cv_range , function(i) spcv(df.sp = data_train, 
                                           my_idp = i, 
                                           var_name = "conc"))
MAPE <- function(actual,residuals) {mean(abs(residuals/actual)*100)}
MSE <- function(residuals) {sum((residuals)^2) / length(residuals)}

CV_MAPE <- c()
CV_MSE <- c()
CV_RMSE <- c()
for (i in 1:length(cv_range)){CV_MAPE[i] <- MAPE(cv.t[[i]]$cv.input$conc,cv.t[[i]]$cv.error)}
for (i in 1:length(cv_range)){CV_MSE[i] <- MSE(cv.t[[i]]$cv.error)}
for (i in 1:length(cv_range)) {CV_RMSE[i]<-cv.t[[i]]$cv.rmse}
CV_Result <- data.frame(MAPE = CV_MAPE,
                        MSE = CV_MSE,
                        RMSE = CV_RMSE)
CV_Result
cv_range[which.min(CV_MAPE)]

library(gstat)
cv.1 <- spcv(df.sp = data_test, my_idp = 50 , var_name = "conc")
cv_test <- spcv(df.sp = data_test, my_idp = cv_range[which.min(CV_MAPE)] , var_name = "conc")
Test_compare <- data.frame(Actual = cv_test$cv.input$conc,
                           Prediction = cv_test$cv.pred)
Pred_sd <- sd(cv_test$cv.pred)
Pred_se <- Pred_sd / sqrt(length(cv_test$cv.pred))
z <- qnorm(0.975)
lower_bound <- mean(cv_test$cv.pred) - z * Pred_se
upper_bound <- mean(cv_test$cv.pred) + z * Pred_se
Pred_CI <- c(lower_bound,upper_bound)
Test_result <- data.frame(SD=Pred_sd,SE=Pred_se,CI=Pred_CI)
Test_MAPE <- MAPE(cv_test$cv.input$conc,cv_test$cv.error)
Test_MAPE

par(mfrow=c(1,2))
plot(cv.1$cv.input$conc, cv.1$cv.error,
            xlab = "hold-out value",
            ylab = "prediction error", main = "Before CV") + abline(0,0, col = "gray")
plot(cv_test$cv.input$conc, cv_test$cv.error,
              xlab = "hold-out value",
              ylab = "prediction error", main = "After CV") + abline(0,0, col = "gray")
