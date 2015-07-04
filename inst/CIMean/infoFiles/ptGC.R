# Borrowed from tigerstats package, until shinyapps ignores packages loaded in subusuduary
# source files.

ptGC <- function(bound,region="between",df=1,graph=FALSE) {
  if (!is.numeric(bound)) stop("Specify one or two numerical boundaries")
  if (length(bound)==1 & !(region %in% c("below","above"))) stop("Specify region=\"below\" or
                                                                 region=\"above\"")
  if (length(bound)==2 & !(region %in% c("between","outside"))) stop("Specify region=\"between\" or
                                                                     region=\"outside\"")
  if (length(bound)>2) stop("Specify one or two numerical boundaries")
  
  if (length(bound)==2 & bound[1]>bound[2])  bound <- rev(bound)
  
  if (region=="below")  {
    area <- pt(bound,df=df)
    if (graph) {
    upper <- max(4,bound+1)
    lower <- min(-4,bound-1)
    curve(dt(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=50,ylim=c(0,1.1*dt(0,df=df)),
          main=paste("t-curve, df = ",df,"\n Shaded Area = ",round(area,4)))
    UnderShade(low=lower,high=bound,func=dt,df=df)
    axis(2)
    places <- c(lower,bound,0,upper)
    axis(1,at=places,labels=c("",as.character(places[2:3]),""))
    }
  }
  
  if (region=="above")  {
    area <- 1-pt(bound,df=df)
    if (graph) {
    upper <- max(4,bound+1)
    lower <- min(-4,bound-1)
    curve(dt(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=50,ylim=c(0,1.1*dt(0,df=df)),
          main=paste("t-curve, df = ",df,"\n Shaded Area = ",round(area,4)))
    UnderShade(low=bound,high=upper,func=dt,df=df)
    axis(2)
    places <- c(lower,bound,0,upper)
    axis(1,at=places,labels=c("",as.character(places[2:3]),""))
    }
  }
  
  if (region=="between")  {
    area <- pt(bound[2],df=df)-pt(bound[1],df=df)
    if (graph) {
    upper <- max(4,bound+1)
    lower <- min(-4,bound-1)
    curve(dt(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=50,ylim=c(0,1.1*dt(0,df=df)),
          main=paste("t-curve, df = ",df,"\n Shaded Area = ",round(area,4)))
    UnderShade(low=bound[1],high=bound[2],func=dt,df=df)
    axis(2)
    places <- c(lower,bound[1],bound[2],upper)
    axis(1,at=places,labels=c("",as.character(places[2:3]),""))
    }
  }
  
  if (region=="outside")  {
    area <- pt(bound[1],df=df)+pt(bound[2],df=df,lower.tail=FALSE)
    if (graph) {
    upper <- max(4,bound+1)
    lower <- min(-4,bound-1)
    curve(dt(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=50,ylim=c(0,1.1*dt(0,df=df)),
          main=paste("t-curve, df = ",df,"\n Shaded Area = ",round(area,4)))
    UnderShade(low=lower,high=bound[1],func=dt,df=df)
    UnderShade(low=bound[2],high=upper,func=dt,df=df)
    axis(2)
    places <- c(lower,bound[1],bound[2],upper)
    axis(1,at=places,labels=c("",as.character(places[2:3]),""))
    }
  }
  
  return(area)
  
}#end of ptGC

# Utitlity function

UnderShade <- function(low,high,func,...) { #Utility
  x.coords <- c(low,seq(low,high,length.out=301),high)
  y.coords <- c(0,func(seq(low,high,length.out=301),...),0)
  polygon(x.coords,y.coords,col="lightblue",cex=2)
}