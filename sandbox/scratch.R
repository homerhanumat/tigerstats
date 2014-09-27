lmGC2(efficiency~speed,data=fuel,degree=2,graph=TRUE)
mod <- lm(efficiency~speed,data=fuel)
predict(mod,newdata=data.frame(speed=c(50,60)))
as.formula("y ~ x")
