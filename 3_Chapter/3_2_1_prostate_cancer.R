library(tidyverse)
library(ElemStatLearn)
library(tidymodels)
library(zeallot)
library(car)
data("prostate")

cor( prostate[,1:8] )
## -----------------------------------------------------------------------------
# TRAIN-TEST SPLITTING
## -----------------------------------------------------------------------------
c(prostate_test, prostate_train) %<-% (prostate %>%
    mutate_at(vars(-lpsa), ~scale(.x)) %>%
    group_by(train) %>%
    group_map( ~ {
        .x  
            
    })) 
apply(bind_rows(prostate_train, prostate_test),2,sd)
## -----------------------------------------------------------------------------
# LINEAR MODEL FITTING
## -----------------------------------------------------------------------------
lm_fit <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
             data = prostate_train)
summary(lm_fit)
## -----------------------------------------------------------------------------
# MODEL SELECTION
## -----------------------------------------------------------------------------
B <- matrix(c(1,0,0,1,0,0,0,1,0), nrow = 1)
car::linearHypothesis(lm_fit, B)
## -----------------------------------------------------------------------------
# PREDICTION ERROR
## -----------------------------------------------------------------------------
prostate_test %>%
    mutate(fitted_lpsa = predict(
        lm_fit,
        newdata = prostate_test
    ) 
    ) %>%
    mutate(mean_lpsa = mean(lpsa)) %>%
    summarise(prediction_error_lm = mean((lpsa-fitted_lpsa)^2),
              prediction_error_base = mean((lpsa-mean_lpsa)^2))
## -----------------------------------------------------------------------------
# SUBSET SELECTION
## -----------------------------------------------------------------------------
library(leaps)
res <- leaps::leaps(
    x = prostate_train %>% select(-lpsa) %>% as.matrix(),
    y = prostate_train$lpsa
) 
## tidy output -----------------------------------------------------------------
leaps_df <- tibble(
    size = factor(res$size),
    metric = res$Cp
)

leaps_df <- leaps_df %>%
    group_by(size) %>%
    mutate(is_min = metric == min(metric)) %>%
    ungroup()
leaps_df %>%
    ggplot() + 
    geom_point(aes(x = size, y = metric)) +
    geom_line(aes(x = size, y = metric),data = subset(leaps_df, is_min == TRUE))
