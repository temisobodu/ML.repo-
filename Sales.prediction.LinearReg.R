#linear.reg.ml
attach(ml_datasets_revenue)
sales = ml_datasets_revenue
sales
summary(sales)
plot(sales)

#split data

?set.seed
set.seed(2)
library(caTools)
install.packages("caTools")
split = sample.split(sales, SplitRatio = 0.7)
split
train = subset(sales, split= T)
test = subset(sales, split = F)
train
test

#create Model
Model = lm(Profit~.,data = train)
Model
summary(Model)

#prediction
pred = predict(Model, test)
pred

#comparing pred v. actual
plot(test$Profit, type = "l", lty = 1.8 , col= "red", ylim = c(0,200000))
lines(pred, type = "l", col ="blue")
plot(pred, type = "l", lty = 1.8, col = "blue", ylim = c(0,200000))

#Finding accuracy
rmse = sqrt(mean(pred - sales$Profit)^2)
rmse
