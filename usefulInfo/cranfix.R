# From CRAN check get something like this:

txt <- "abline as.formula axis barplot chisq.test coef complete.cases curve dbeta 
dbinom dchisq density df dnorm dt fitted hist installed.packages is legend 
      lines lm mtext pbinom pchisq plot pnorm
      points polygon predict pt qbinom qchisq qnorm qt r2dtable rainbow
      rbeta rect resid rgb rmultinom rnorm rug runif segments text vignette
      xtabs"

# then construct:
funcNames <- scan(what = character(), text = txt, quiet = TRUE)
packageNames <- unlist(lapply(funcNames, find))
packageNames <- gsub(x = packageNames, patter = "package:", replace = "")
docLines <- paste0("#' @importFrom ", packageNames, " ", funcNames)
writeLines(docLines)

# add output to tigerstats-package.R
# NOTES:  need to declare methods as an import in DESCRIPTION file

