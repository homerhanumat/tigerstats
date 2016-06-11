#' @title Extensive Help Via Vignettes

#' @description A convenience function to show vignettes associated with
#' package \code{tigerstats}.  Vignette will open in the user's default
#' browser.
#' 
#' @rdname helpGC
#' @usage helpGC(topic)
#' @param topic filename of the vignette, exclusive of the .html extension
#' @return side effects
#' @export
#' @author Homer White (hwhite0@@georgetowncollege.edu)
#' @examples
#' \dontrun{
#' helpGC(lmGC)
#' }
helpGC <- function(topic) {
  
  topic <- as.character(substitute(topic))
  
  baseURL <- "http://homerhanumat.github.io/tigerstats/"
  
  topicList <- c("descriptive",
                 "inferential",
                 "bwplot",
                 "densityplot",
                 "histogram",
                 "xyplot",
                 "scatterplot",
                 "favstats",
                 "xtabs",
                 "rowPerc",
                 "colPerc",
                 "lmGC",
                 "polyfitGC",
                 "pbinomGC",
                 "pnormGC",
                 "qnorm",
                 "qnormGC",
                 "binomtestGC",
                 "proptestGC",
                 "chisqtestGC",
                 "ttestGC")
  
  fileList <-   c("R_descriptive",
                  "R_inferential",
                  "bwplot",
                  "densityplot",
                  "histogram",
                  "xyplot",
                  "xyplot",
                  "favstats",
                  "xtabs",
                  "xtabs",
                  "xtabs",
                  "lmGC",
                  "polyfitGC",
                  "pbinomGC",
                  "pnormGC",
                  "qnorm",
                  "qnormGC",
                  "binomtestGC",
                  "proptestGC",
                  "chisqtestGC",
                  "ttestGC")
  
  urlList <- paste0(baseURL,fileList,".html")
  
  
  if (!(topic %in% topicList)) {
    cat(paste0("Sorry, there is no vignette for the topic:  ",topic,".\n"))
    cat("The possible topics are:\n ")
    return(cat(paste0(topicList,"\n")))
    
  } else {
    urlRow <- which(topicList==topic)
    vigURL <- urlList[urlRow]
  }
  
  utils::browseURL(vigURL)
}

