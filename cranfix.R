# From CRAN check get:

txt <- "abline as.formula axis barplot chisq.test coef complete.cases curve dbeta 
dbinom dchisq density df dnorm dt fitted hist installed.packages is legend 
      lines lm mtext pbinom pchisq plot pnorm
      points polygon predict pt qbinom qchisq qnorm qt r2dtable rainbow
      rbeta rect resid rgb rmultinom rnorm rug runif segments text vignette
      xtabs"

# define function:

imports_for_undefined_globals <-
  function(txt, lst, selective = TRUE)
  {
    if(!missing(txt))
      lst <- scan(what = character(), text = txt, quiet = TRUE)
    nms <- lapply(lst, find)
    ind <- sapply(nms, length) > 0L
    imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
    if(selective) {
      sprintf("importFrom(%s)",
              vapply(Map(c, names(imp), imp),
                     function(e)
                       paste0("\"", e, "\"", collapse = ", "),
                     ""))
    } else {
      sprintf("import(\"%s\")", names(imp))
    }
  }                       

# use it to generate:
writeLines(imports_for_undefined_globals(txt))

# must add output by hand to NAMESPACE after devtools::document()
