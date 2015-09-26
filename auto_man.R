library(datasets)
library(dplyr)

cars <- tbl_df(mtcars)
row.names(cars) <- row.names(mtcars)
cars$am <- factor(cars$am)
levels(cars$am) <- c("Automatic", "Manual")
cars$vs <- factor(cars$vs)
levels(cars$vs) <- c("V", "Straight")
cars$cyl <- factor(cars$cyl)
cars$gear <- factor(cars$gear)
cars$carb <- factor(cars$carb)

library(ggplot2)

ggplot(cars, aes(x=am, y=mpg, color=am, group=1)) +
  geom_point(size = 5) + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, size=1) +
  labs(title="]Scatterplot of MPG by Transmission", x="Transmission Type", y="Miles per gallon")

ggplot(cars, aes(x=am, y=mpg, fill=am)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=8) +  guides(fill=FALSE) +
  labs(title="Boxplot of MPG by Transmission", x="Transmission Type", y="Miles per gallon")

m1 <- lm(mpg ~ am, data = cars)

ggplot(cars, aes(x=am, y=mpg, color=am, group=1)) +
  geom_point(size = 5) + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, size=1) +
  labs(title="MPG by Transmission", x="Transmission Type", y="Miles per gallon")

ggplot(cars, aes(x=predict(m1),y=resid(m1), color=am, group=1)) + geom_point(size = 5) + geom_hline(yintercept=0) + geom_smooth(method=lm) +
  labs(title="Residuals vs Fitted", x="Fitted Values", y="Residuals")

fit1 <- lm(mpg~., data=cars)
fit2 <- update(fit1, lm(mpg ~ . - carb, data = cars))
fit3 <- update(fit2, lm(mpg ~ . - carb - cyl, data = cars))
fit4 <- update(fit3, lm(mpg ~ . - carb - cyl - gear, data = cars))
fit5 <- update(fit4, lm(mpg ~ . - carb - cyl - gear - disp, data = cars))
fit6 <- update(fit5, lm(mpg ~ . - carb - cyl - gear - disp - hp, data = cars))
fit7 <- update(fit6, lm(mpg ~ am + wt + qsec + drat, data = cars))
fit8 <- update(fit7, lm(mpg ~ am + wt + qsec, data = cars))

ggplot(cars, aes(x=predict(m1),y=resid(m1), color=am, group=1)) + geom_point(size = 5) + geom_hline(yintercept=0) + geom_smooth(method=lm) +
  labs(title="Linear Model Residuals vs Fitted", x="Fitted Values", y="Residuals")

library(car)

summary(fit8)
vif(fit8)
anova(m1, fit8, fit6, fit3, fit1)

ggplot(cars, aes(x=predict(fit8),y=resid(fit8), color=am, group=1)) + geom_point(size = 5) + geom_hline(yintercept=0) + geom_smooth(method=lm) +
  labs(title="Multivariable Regression Model Residuals vs Fitted", x="Fitted Values", y="Residuals")

qqPlot(fit8, ylab="Standardized Residuals", xlab="Theoretical Quantiles", main="Fig. 5: Normal Q-Q Plot")