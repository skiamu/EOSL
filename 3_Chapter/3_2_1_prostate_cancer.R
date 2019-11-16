library(tidyverse)
library(ElemStatLearn)
library(tidymodels)
library(zeallot)

data("prostate")

prostate <- as_tibble(prostate) %>%
    mutate_at(vars(gleason, svi),  ~factor(.x))



c(prostate_fit, prostate_test) %<-% (prostate %>%
    group_by(train) %>%
    group_map( ~ {
        .x %>%
            mutate_if(is.numeric, ~ .x - mean(.x))
    })) 


lm_fit <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
             data = prostate_fit)
summary(lm_fit)

prostate_fit %>% colMeans()

cor(prostate_fit)
