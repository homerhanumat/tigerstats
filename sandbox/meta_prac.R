library(rlang)
library(lobstr)
data("m111survey", package = "tigerstats")
library(ggplot2)


expr(mean(x, na.rm = TRUE))
#> mean(x, na.rm = TRUE)
expr(10 + 100 + 1000)
#> 10 + 100 + 1000
capture_it <- function(x) {
  enexpr(x)
}
capture_it(a + b + c)
#> a + b + c

df <- data.frame(x = 1:5, y = sample(5))
with2 <- function(df, expr) {
  eval_tidy(enexpr(expr), df)
}

with2(df, x + y)
#> [1] 6 5 7 5 7

ast(y ~ x)

make_tab <- function(formula, data, graph = TRUE) {
  form <- rlang::enexpr(formula)
  e <- quo_get_env(enquo(formula))
  print(ls(e))
  if (length(form) == 3) {
    resp_var <- form[[2]]
    exp_var <- form[[3]]
    table_call <- rlang::call2("table", resp_var, exp_var)
    tab <- rlang::eval_tidy(table_call, data, env = e)
    if (graph) {
      df_call <- rlang::call2("data.frame", resp_var, exp_var)
      df <- rlang::eval_tidy(df_call, data, env = e)
      p <- 
      ggplot(df, aes(x = {{ exp_var }})) +
        geom_bar(aes(fill = {{ resp_var }}), position = "dodge")
      print(p)
    }
  }
  return(tab)
}

e <- c(rep("a", 51), rep("b", 20))

make_tab(seat ~ sex, data = m111survey)

