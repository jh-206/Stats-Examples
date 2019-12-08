
# Setup -------------------------------------------------------------------

  options(scipen = 999)

  pacman::p_load(tidyverse)


# Simulate Data -----------------------------------------------------------

  set.seed(111)

  # Values tested for inflation
  df_true <- data.frame(
    id = 1:100,
    x = runif(100, min = 100000, max = 1000000)
  )

  # Extrapolation Distributions
  ## Narrower Distribution
  rnorm(1001, mean = df_true$x[1] / 1.08, sd = 50000)
  
  l1 <- lapply(df_true$x, function(x) rnorm(1001, mean = x / 1.08, sd = 50000))
  l1 <- lapply(l1, function(v) {names(v) = paste0('pred_', 1:length(v)); v})
  
  ## Wider Distribution
  l2 <- lapply(df_true$x, function(x) rnorm(1001, mean = x / 1.08, sd = 100000))
  l2 <- lapply(l2, function(v) {names(v) = paste0('pred_', 1:length(v)); v})
  
  df = tibble(id = 1:100, dist1 = l1, dist2 = l2)
  

# Bootstrap Methodology ---------------------------------------------------

  # Bootstrap sample of rows
  inds <- sample(df_extrap1$id, nrow(df_extrap1), replace = T)
  
  bootWrap <- function(x) {
    return(sample(x, length(x), replace = T))
  }
  
  # Boostrap sample of vals
  df2 <- df[inds,]
  
  df2 <- df2 %>% 
    mutate(boot_dist1 = purrr::map(dist1, bootWrap),
           boot_dist2 = purrr::map(dist2, bootWrap),
           m1 = purrr::map_dbl(boot_dist1, mean),
           m2 = purrr::map_dbl(boot_dist2, mean))
    