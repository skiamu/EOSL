library(MASS)
library(class)
library(tidyverse)
library(tidymodels)
library(zeallot)
generate_data <- function(means, Sigma, N, response_var = NULL) {
    data <- means %>%
        purrr::array_branch(margin = 1) %>%
        purrr::map( ~ {
            MASS::mvrnorm(n = N, mu = .x, Sigma = Sigma/5)
        }) %>%
        purrr::reduce(rbind) %>%
        `colnames<-`(stringr::str_c("X", 1:ncol(.))) %>%
        as_tibble()
    if (!is.null(response_var)) {
        data <- data %>%
            tibble::add_column(Y = response_var)
    }
    return(data)
} # generate_data

## -----------------------------------------------------------------------------
# 1) DATA GENERATION
## -----------------------------------------------------------------------------
Sigma <- diag(rep(1, 2))
mu_blu <- c(1, 0)
mu_orange <- c(0, 1)

K <- 10
means_blu <- MASS::mvrnorm(n = K, mu = mu_blu, Sigma = Sigma)
means_orange <- MASS::mvrnorm(n = K, mu = mu_orange, Sigma = Sigma)
data_blu <- generate_data(means = means_blu, Sigma = Sigma, N = K, response_var = 0)
data_orange <- generate_data(means = means_orange, Sigma = Sigma, N = K, response_var = 1)
data_fit <- bind_rows(data_blu, data_orange)
data_test <- bind_rows(
    generate_data(means_blu, Sigma = Sigma, N = 10),
    generate_data(means_orange, Sigma = Sigma, N = 10)
)
## -----------------------------------------------------------------------------
# 2) LINEAR REGRESSION CLASSIFIER
## -----------------------------------------------------------------------------
lm_obj <- data %>%
    lm(Y ~ X1 + X2, data = .)
c(beta_0, beta_1, beta_2) %<-% coef(lm_obj)
lm_tidy <- lm_obj %>%
    broom::tidy()
classifier_LR <- lm_obj %>%
    broom::augment(newdata = data_fit) %>%
    mutate(group = factor(if_else(.fitted > .5, "orange", "blue")))

## -----------------------------------------------------------------------------
# 3) kNN CLASSIFIER
## -----------------------------------------------------------------------------
classifier_kNN <- class::knn(
    train = data_fit %>% dplyr::select(-Y),
    test = data_test,
    cl = factor(data_fit$Y),
    k = 15
) %>% mutate(.data = data_fit, .fitted = .) %>%
    mutate(group = factor(if_else(.fitted == 1, "orange", "blue")))

## -----------------------------------------------------------------------------
# 4) PLOTTING
## -----------------------------------------------------------------------------
color_map <- c("orange" = "orange1", "blue" = "royalblue1")
## 4.1) Plot Linear Regression Boundary ----------------------------------------
intercept <- (0.5 - beta_0) / beta_2
slope <- -beta_1 / beta_2

data_fit %>%
    mutate(group = factor(if_else(Y == 1, "orange", "blue"))) %>%
    ggplot() + 
    geom_point(aes(x = X1, y = X2, color = group)) + 
    scale_color_manual(values = color_map) + 
    geom_abline(intercept = intercept, slope = slope)

