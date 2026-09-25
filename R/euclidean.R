#' Euclidean Algorithm
#'
#' Calculates the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.
#'
#' @param a A numeric scalar or integer.
#' @param b A numeric scalar or integer.
#'
#' @return A numeric scalar that is the greatest common divisor of the inputs.
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
euclidean <- function(a, b) {
  if (!is.numeric(a) || length(a) != 1 || is.na(a)) stop("Argument 'a' must be a numeric scalar.")
  if (!is.numeric(b) || length(b) != 1 || is.na(b)) stop("Argument 'b' must be a numeric scalar.")

  a <- abs(a)
  b <- abs(b)
  while (b != 0) {
    remainder <- a %% b
    a <- b
    b <- remainder
  }
  return(a)
}
