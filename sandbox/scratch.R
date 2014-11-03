mystuff <- binomtestGC(~sex,data=m111survey,success="female",p=0.5,graph=TRUE,conf.level=0.90)
mystuff

binomtestGC(x=40,n=71,p=0.5,alternative="greater",graph=T)

x <- rep(c("a","b"),3)
y <- as.character(c(1,1,1,2,2,2))
x <- factor(x,levels=letters[1:3])
y <- factor(y,levels=as.character(1:3))
xtabs(~x+y)
complete.cases(x,y)
complete.cases(xtabs(~x+y))
chisq.test(xtabs(~x+y))

chisqtestGC(~sex+seat,data=m111survey,graph=T,simulate.p.value=T,B=2500)
chisqtestGC(c(20,25,20),data=m111survey,p=rep(1/3,3),graph=T,simulate.p.value=T,B=5000)

#Goodness of fit test for one factor variable:
chisqtestGC(~seat,data=m111survey,p=c(1/3,1/3,1/3))

#Test for relationship between two factor variables:
chisqtestGC(~sex+seat,data=m111survey)

#You can input a two-way table directly into chisqtestGC():
SexSeat <- xtabs(~sex+seat,data=m111survey)
chisqtestGC(SexSeat)

#For small datasets, severa ltypes of simulation are possible, e.g.:
chisqtestGC(~weather+crowd.behavior,data=ledgejump,simulate.p.value=F,B=2500,verbose=F)

#For less ouptut, set argument verbose to FALSE:
chisqtestGC(~sex+seat,data=m111survey,verbose=FALSE)
