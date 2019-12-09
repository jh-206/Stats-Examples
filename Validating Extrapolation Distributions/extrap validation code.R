
# Setup -------------------------------------------------------------------

  options(scipen = 999)

  pacman::p_load(tidyverse, ggridges)


# Simulate Data -----------------------------------------------------------

  set.seed(111)

  # Values tested for inflation
  df_true <- data.frame(
    x = 1:100
  )

  # Extrapolation Distributions
  ## Narrower Distribution
  # rnorm(1001, mean = df_true$x[1] / 1.08, sd = 50000)
  
  l1 <- lapply(df_true$x, function(x) rnorm(1001, mean = x, sd = 1))
  l1 <- lapply(l1, function(v) {names(v) = paste0('pred_', 1:length(v)); v})
  
  ## Wider Distribution
  l2 <- lapply(df_true$x, function(x) rnorm(1001, mean = x, sd = 1.5))
  l2 <- lapply(l2, function(v) {names(v) = paste0('pred_', 1:length(v)); v})
  
  df = tibble(x = 1:100, dist1 = l1, dist2 = l2)
  

# Bootstrap Methodology ---------------------------------------------------

  # Bootstrap sample of rows
  inds <- sample(df$x, nrow(df), replace = T)
  
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
    
  
  hist(df2$dist1[[1]])
  
  tit <- 'Plot Title'
  ggplot(mapping = aes(df2$dist1[[1]])) + geom_histogram(alpha = .6, color = 'red',
                                                         fill = 'red') + theme_bw() + labs(title = tit, x = 'X')
  
  xx = unnest(head(df, 12), cols = c(dist1, dist2))
  
  ggplot(xx, aes(x = dist1, y = as.factor(x))) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")
  
  ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")
  
  