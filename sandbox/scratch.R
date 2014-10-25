require(tigerstats)

sexSeat <- xtabs(~sex+seat,data=m111survey)

chisqtestGC(~sex+seat,data=m111survey,simulate.p.value=T,graph=T,B=1000)

res <- chisqtestGC(~sex+seat,data=m111survey,simulate.p.value="random",graph=T,B=1000)
res

res <- chisqtestGC(c(one=20,two=20,blah=20),data=m111survey,p=c(0.25,0.5,0.25))
res
