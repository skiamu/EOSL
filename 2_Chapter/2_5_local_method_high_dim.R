library(tidyverse)

g <- function(x) {
    exp(-8 * base::norm(x, "2"))
} # g

f <- function(x) {
    if (class(x) == "numeric") {
       x <- matrix(x, ncol = 1) 
    }
    apply(X = x, MARGIN = 1, FUN = g)
}

generate_data <- function(p, N) {
    matrix(runif(n = N * p, min = -1, max = 1), ncol = p, nrow = N)
} # generate_data

avg_distance <- function(X) {
    apply(X, MARGIN = 1, function(x) sqrt(norm(x, "2"))) %>% mean()
} # avg_distance

distance_1NN <- function(X) {
    
} # distance_1NN

data <- generate_data(3, 10)
avg_distance(data)
xx <- seq(-1, 1, by = 0.01)
plot(xx, f(xx))
points(x = data, y = rep(0, length(data)), col = "red")
