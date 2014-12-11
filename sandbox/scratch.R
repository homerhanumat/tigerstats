ttestGC(fastest~Sex,m111survey,alternative="l")
proptestGC(~sex+love_first,m111survey,p=0,alternative="t")
ChisqSimSlow(~weather+crowd.behavior,ledgejump,effects="f")
res <- binomtestGC(~seat,m111survey,p=0.3,success="1_front",alternative="g")
barchartGC(~sex+seat,m111survey,type="%",flat=T)

fn <- function(data){
  dataName <- deparse(substitute(data))
  dataName
}

res <- chisqtestGC(~sex+seat,data=m111survey,simulate.p.value=T)
densityplot(~Sims,data=res)

