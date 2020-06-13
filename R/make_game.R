#' @title Play with a Discrete Random Variable

#' @description Makes a function that simulates a game based where
#' your winnings are the sum of a specified number of plays of a
#' discrete random variable with a specified distribution. 
#' 
#' @rdname make_game
#' @usage make_game(outcomes, probs, plays)
#' @param outcomes numerical vector of possible values of the random variable
#' @param probs numerical vector giving the probability distribution
#' @param plays number of times the random variable is simulated
#' @return a function of a single parameter n, with default value 1.
#' n is the number of times you simulate the net winnings.
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' play_game <- make_gmae(
#'   outcomes = c(-1, 0, 5)
#'   probs = c(0.4, 0.5, 0.1)
#'   plays = 2000
#' )
#' ## Play "plays" times, get net winnings:
#' sampler()
#' ## Play "plays" times again:
#' sampler()
#' ## Play "plays" times, a third time:
#' sampler()
#' ## 1000 more simulations of the net winnings:
#' sampler(n = 1000)
#' }
#' @export
make_game <- function(outcomes, probs, plays) {
  results <- numeric()
  function(n = 1) {
    new_results <- 
      replicate(n, sum(sample(outcomes, size = plays, prob = probs, replace = TRUE)))
    if (n == 1) {
      for (i in 1:n) cat("In ", plays, " plays you won ", new_results[i], " bucks.\n")
    }
    results <<- c(results, new_results)
    if (length(results) > 2) {
      if (plays > 1) {
        df <- data.frame(net_winnings = results)
        p <-
          ggplot2::ggplot(df, ggplot2::aes (x = net_winnings)) +
          ggplot2::geom_density(fill = "burlywood", alpha = 0.3) +
          ggplot2::geom_rug() +
          ggplot2::labs(x = "net winnings")
      } else {
        tab <- table(results)
        net_winnings <- as.numeric(names(tab))
        df <- data.frame(
          net_winnings,
          y = as.vector(tab)
        )
        p <-
          ggplot2::ggplot(df, ggplot2::aes(x = net_winnings, ymin = 0, ymax = .data$y)) +
          ggplot2::geom_linerange() +
          ggplot2::scale_x_continuous(
            breaks = net_winnings, labels = as.character(net_winnings)
          )
      }
      print(p)
    }
  }
}