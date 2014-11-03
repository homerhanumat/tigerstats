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
