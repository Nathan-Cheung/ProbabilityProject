#' Rejection Sampling
#' @description This is a function that peforms rejection sampling from a pdf
#' @param n The number of samples
#' @param pdf A function that is the pdf of the random variable that you wish to sample from
#' @param a a numeric that is the lower bound of the random variable you wish to sample from
#' @param b a numeric that is the upper bound of the random variable you with to sample from
#' @param C a numeric that is such that f(x) <= C for all values of x
#' @return Samples of length n from the given pdf
#' @examples
#' @export

rejectionsample <- function(n,pdf,a,b,C){
  #browser()
  sim_data <- replicate(10000, {
    u <- runif(1, a, b)
    v <- runif(1, 0, C)
    if(v < pdf(u)) {#then accept else reject
      u
    } else {
      NA
    }
  })

  sim_data <- sim_data[!is.na(sim_data)]

  if (length(sim_data) >= n) {
    sim_data <- sim_data[1:n]
  }else if (length(sim_data) < n){
    while (length(sim_data) < n){
      u <- runif(1, a, b)
      v <- runif(1, 0, C)
      if (v < pdf(u))
        sim_data = c(sim_data,u)}
      }

hist(sim_data, probability = TRUE)
curve(pdf, add = TRUE)

sim_data
}

