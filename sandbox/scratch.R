lmGC2(efficiency~speed,data=fuel,degree=4,graph=TRUE)
mod <- lmGC2(efficiency~speed,data=fuel,degree=4)
predict(mod,x=50)
as.name("hello")

res <-quote("hello")
str(res)

mod <- lmGC2(sheight~fheight,data=galton,degree=1,graph=TRUE)
predict(mod,x=65)

mod <- lmGC2(OBP~Season,data=henderson,degree=2,graph=TRUE)

lmGC2(gasbill~temp,data=Utilities,degree=2,check=TRUE)
