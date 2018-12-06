#' Rejection Sampling
#' @description This is a function that peforms rejection sampling from a pdf
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' @export

rejectionsample <- function(n,pdf,a,b,C){
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
    while (length(sim_data) < n)
      u <- runif(1, a, b)
      v <- runif(1, 0, C)
      if (v < pdf(u))
        sim_data = c(sim_data,u)
      }
sim_data

hist(sim_data, probability = TRUE)
curve(pdf, add = TRUE)
}

