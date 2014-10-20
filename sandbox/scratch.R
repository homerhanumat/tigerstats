require(tigerstats)

sexSeat <- xtabs(~sex+seat,data=m111survey)

chisqtestGC(~sex+seat,data=m111survey,simulate.p.value=T,graph=T,B=10000)

res <- chisqtestGC(~sex+seat,data=m111survey,simulate.p.value="fixed",graph=T,B=10000)
