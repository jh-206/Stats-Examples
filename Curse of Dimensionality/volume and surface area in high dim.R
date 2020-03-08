# Increasing Dimensionality
x = 2 # fixing side length of cube and radius of sphere at 2
df <- data.frame(Dimension = 2:9)
df$Volume <- with(df, x^Dimension)
df$"Surface Area" <- with(df, 2*Dimension*x^(Dimension - 1))

library(tidyverse)
df2 <- df %>% 
  pivot_longer(cols = -Dimension, names_to = 'Metric', values_to = 'Value')

df2 %>% 
  ggplot(aes(x = Dimension, y = Value, color = Metric)) + 
  geom_point() +
  scale_color_manual(values = c('darkgreen', 'green')) +
  stat_function(fun = function(d) 2^d, color = 'green') +
  stat_function(fun = function(d) 2*d*x^(d - 1), color = 'darkgreen') +
  theme_minimal()



  
