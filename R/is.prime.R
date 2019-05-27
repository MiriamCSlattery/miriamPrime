#' Testing if Prime
#'
#' The function takes a vector of positive integers greater than or equal to 2
#' and returns a boolean vector indicating whether it is prime or not.
#'
#' @param x a numeric vector
#'
#' @return boolean vector indicating prime numbers
#' @export
#'
#' @examples
#' x <- c(2,3,4)
#' is.prime(x)
is.prime <- function(x) {
  tol <- 10^(-4)
  if (any(x <= 1)) {stop("Error: Input must be greater than or equal to 2")}
  if (any(abs(x-round(x)) > tol)) {stop("Error: Input must be integer")}

  n = length(x)
  y = logical(length = n)

  for (i in 1:n) {
  if (x[i]==2) {y[i] = TRUE}
  else if (any(x[i] %% 2:(x[i]-1)==0)) {y[i] = FALSE}
  else {y[i] = TRUE}
  }
  return(y)
}
