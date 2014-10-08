lmGC2(efficiency~speed,data=fuel,degree=4,graph=TRUE)
mod <- lmGC2(efficiency~speed,data=fuel,degree=4)
predict(mod,x=50)
as.name("hello")

res <-quote("hello")
str(res)

mod <- lmGC2(sheight~fheight,data=galton,graph=TRUE)
predict(mod,x=65)

mod <- lmGC2(OBP~Season,data=henderson,degree=2,graph=TRUE)
lmGC2(OBP~Season,data=henderson,degree=2,check=TRUE)

lmGC2(gasbill~temp,data=Utilities,degree=2,check=TRUE)

mod <- lmGC2(volume~avgtemp,data=RailTrail,degree=2)

lmGC2(OBP~Season,data=henderson,degree=3)

seas2 <- henderson$Season - 1990

lmGC2(OBP~seas2,data=henderson,degree=23,graph=TRUE)

mod3 <- lmGC2(OBP~seas2,data=henderson,degree=20)
predict(mod3,x=12)

lmGC2(ChugTime~Weight,data=chugtime,check=TRUE)
