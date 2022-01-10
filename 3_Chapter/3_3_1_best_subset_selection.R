library(tidyverse)
library(ElemStatLearn)
library(zeallot)
library(tidymodels)
library(stringr)
data("prostate")

make_formula <- function(regressor_names) {
    str_c("lpsa ~ ", str_c(regressor_names, collapse = " + "))
} # make_formula

## -----------------------------------------------------------------------------
# TRAIN-TEST SPLITTING
## -----------------------------------------------------------------------------
c(prostate_test, prostate_train) %<-% (prostate %>%
                                           mutate_at(vars(-lpsa), ~(.x - mean(.x)) / sd(.x)) %>%
                                           group_by(train) %>%
                                           group_map( ~ {
                                               .x  
                                           }))
## -----------------------------------------------------------------------------
# 2) BEST SUBSET SELECTION
## -----------------------------------------------------------------------------

regressors_name <- prostate_test %>%
    select(-lpsa) %>%
    colnames()

regression_df <- 1:length(regressors_name) %>%
    map_dfr(function(x) {
        combn(regressors_name, m = x, simplify = FALSE) %>%
            map( ~ make_formula(.x)) %>%
            map(~ lm(.x, data = prostate_train)) %>%
            map_dfr(~ {
                broom::glance(.x) %>%
                    mutate(k = x)
            })
    })


regression_df <- 1:length(regressors_name) %>%
    map_dfr(function(x) {
        combn(regressors_name, m = x, simplify = FALSE) %>%
            map( ~ make_formula(.x)) %>%
            map(~ lm(.x, data = prostate_train)) %>%
            map_dfr(~ {
                tmp <- broom::augment(.x) 
                tibble(k = x, RSS = sum(tmp[[".resid"]]))    
            })
    })

