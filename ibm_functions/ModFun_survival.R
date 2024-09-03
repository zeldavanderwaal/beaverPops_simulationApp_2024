cat("survival  - ")

survival <- function(x, mort) {
  sum(rbinom(x, 1, 1-mort))
}
