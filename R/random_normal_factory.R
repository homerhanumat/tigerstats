#' @title Make Normal Sampling Functions

#' @description Makes a function that samples from a normal distribution. 
#' 
#' 
#' @rdname random_normal_factory
#' @usage random_normal_factory(mean, sd)
#' @param mean mean of normal distribution from which to sample
#' @param sd standard deviation of the normal distribution
#' @return a function of a single parameter n, with default value 1.
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' sampler <- random_normal_factory(mean = 70, sd = 5)
#' ## sample one
#' sampler()
#' ## sample another
#' sampler()
#' ## sample a third time
#' sampler()
#' ## sample another 1000
#' sampler(n = 1000)
#' }
#' @export
random_normal_factory <- function(mean, sd) {
  xaxis <- c(mean - 2 * sd, mean - sd, mean, mean + sd, mean + 2 *sd)
  random_data <- numeric()
  function(n = 1) {
    samp <- rnorm(n, mean = mean, sd = sd)
    random_data <<- c(random_data, samp)
    if (n == 1) {
      cat("Expecting about ", mean, " give or take ", sd, " or so.\n", sep = "")
      cat("Got ", round(samp, 3), ".\n")
    }
    cat("Sampled", length(random_data), "so far.\n\n")
    if (length(random_data) > 2) {
      outside <- length(random_data[abs(random_data - mean) > 3*sd])
      ok <- random_data[abs(random_data - mean) <= 3*sd]
      p <-ggplot2::ggplot(data.frame(x = ok), ggplot2::aes(x = x)) +
        ggplot2::geom_density(fill = "burlywood", alpha = 0.3) +
        ggplot2::geom_rug() +
        ggplot2::scale_x_continuous(breaks = xaxis, labels = as.character(xaxis),
                           limits = c(mean - 3 * sd, mean + 3 * sd))
      print(p)
      if (outside > 0) {
        cat(outside, "extreme values were not plotted.\n\n")
      }
    }
  }
}
