#' @title Multiple Confidence Interval for a Proportion

#' @description An app to explore confidence interval for a proportion.
#' 
#' @rdname watch_statisticians
#' @usage watch_statisticians(n, p, interval_number = 50, level = 0.95)
#' @param n the sample size
#' @param p the population proportion
#' @param interval_number number of intervals to make (limit and default are 50)
#' @param level desired level of confidence
#' @return Graph side-effects
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' watch_statisticians(n = 100, p = 0.5, interval_number = 50, level = 0.95)
#' }
watch_statisticians <- function(n, p, interval_number = 50, level = 0.95) {
  if (n < 50) return(cat("Choose a sample size of at least 50!"))
  if (interval_number < 1 | interval_number > 50) {
    return(cat(paste0("Number of intervals should be ",
                      "at least 1 and no more than 50")))
  }
  if (p < 0.01 | p > 0.99) {
    return(cat("Need 0.01 <= p <= 0.99"))
  }
  title <- paste0(
    interval_number, " Confidence Intervals for p"
  )
  subtitle <- "(dotted blue line is at p, non-covering intervals are red)"
  df <- make_intervals(
    n = n,
    p_hats = rbinom(interval_number, size = n, prob = p) / n,
    level = level,
    p = p
  )
  ggplot2::ggplot(df, ggplot2::aes_string(x = "int", y = "upper")) +
    ggplot2::geom_segment(ggplot2::aes_string(
      x = "int", xend = "int", 
      y = "lower", yend = "upper", 
      color = "lemon")) +
    ggplot2::geom_point(ggplot2::aes_string(x = "int", 
                            y = "p_hats", 
                            color = "lemon"), 
                        size = 1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = p), 
                                 lty = 2, color = "blue") +
    ggplot2::labs(x = "intervals", y = NULL,
         title = title, subtitle = subtitle) +
    ggplot2::theme(legend.position = "none",
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c("gray70", "red"))+
    ggplot2::ylim(c(0, 1)) +
    ggplot2::xlim(c(0, interval_number)) +
    ggplot2::coord_flip()
}

#' @title Multiple Confidence Interval for a Proportion, One at a Time

#' @description An app to explore confidence interval for a proportion.
#' 
#' @rdname watch_statisticians_slow
#' @usage watch_statisticians_slow(n, p, interval_number = 50, level = 0.95)
#' @param n the sample size
#' @param p the population proportion
#' @param interval_number number of intervals to make (limit and default are 50)
#' @param level desired level of confidence
#' @return Graph side-effects
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' watch_statisticians_slow(n = 100, p = 0.5, interval_number = 50, level = 0.95)
#' }
watch_statisticians_slow <- function(n, p, 
                                     interval_number = 50, 
                                     level = 0.95) {
  if (n < 50) return(cat("Choose a sample size of at least 50!"))
  if (interval_number < 1 | interval_number > 50) {
    return(cat(paste0("Number of intervals should be ",
                      "at least 1 and no more than 50")))
  }
  if (p < 0.01 | p > 0.99) {
    return(cat("Need 0.01 <= p <= 0.99"))
  }
  graphing <- TRUE
  count <- 0
  subtitle <- "(dotted blue line is at p, non-covering intervals are red)"
  df <- data.frame(
    n = numeric(),
    lower = numeric(),
    upper = numeric(),
    p_hats = numeric(),
    lemon = logical()
  )
  while(count < interval_number) {
    df <- rbind(df,
                make_interval(
                  n = n,
                  p_hat = rbinom(1, size = n, prob = p) / n,
                  level = level,
                  p = p
                  )
    )
    count <- count + 1
    df$int <- 1:count
    title <- paste0(
      count, " Confidence interval made so far ..."
    )
    plot_so_far <-
      ggplot2::ggplot(df, ggplot2::aes_string(x = "int", y = "upper")) +
      ggplot2::geom_segment(ggplot2::aes_string(
        x = "int", xend = "int", 
        y = "lower", yend = "upper", 
        color = "lemon")) +
      ggplot2::geom_point(ggplot2::aes_string(x = "int", 
                                              y = "p_hat", 
                                              color = "lemon"), 
                          size = 1) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = p), 
        lty = 2, color = "blue") +
      ggplot2::labs(x = "intervals", y = NULL,
                    title = title, subtitle = subtitle) +
      ggplot2::theme(legend.position = "none",
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks.y=ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank()) +
      ggplot2::scale_color_manual(values = c("gray70", "red"))+
      ggplot2::ylim(c(0, 1)) +
      ggplot2::xlim(c(0, interval_number)) +
      ggplot2::coord_flip()
    print(plot_so_far)
    checking <- readline(
      "Take another sample? (Enter to continue, q to quit)  "
      )
    no_more <- checking == "q"
    if (no_more) break
    df$int <- NULL
  }
  return(cat("All done!"))
}

make_intervals <- function(n, p_hats, level, p) {
  z<- qnorm(1 - (1 - level) / 2)
  se <- sqrt(p_hats * (1 - p_hats) / n)
  lower <- p_hats - z * se
  upper <- p_hats + z * se
  lemon <- p < lower | p > upper
  data.frame(
    int = 1:length(p_hats),
    lower = p_hats - z * se,
    upper = p_hats + z * se,
    p_hats = p_hats,
    lemon = lemon
  )
}

make_interval <- function(n, p_hat, level, p) {
  z<- qnorm(1 - (1 - level) / 2)
  se <- sqrt(p_hat * (1 - p_hat) / n)
  lower <- p_hat - z * se
  upper <- p_hat + z * se
  lemon <- p < lower | p > upper
  data.frame(
    lower = p_hat - z * se,
    upper = p_hat + z * se,
    p_hat = p_hat,
    lemon = lemon
  )
}