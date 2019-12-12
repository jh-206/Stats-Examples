# Generator Functions
# x = sidelength, n = dimensionality
cube <- function(x, n){
  vec = c('x' = x, 'n' = n)
  structure(vec, class = c('cube', class(vec)))
} 

# r = radius, n = dimensionality
sphere <- function(r, n) {
  vec = c('r' = r, 'n' = n)
  structure(vec, class = c('sphere', class(vec)))
} 

# Generic Functions
volume <- function(x) UseMethod("volume", x)
volume.cube <- function(x) return(unname(x['x'] ^ x['n']))
volume.sphere <- function(x) {
  r = x['r']
  n = x['n']
  
  unname(pi^(n/2) / gamma(n/2 + 1) * r^n)
}

surfaceArea <- function(x) UseMethod("surfaceArea", x)
surfaceArea.cube <- function(x) {
  n = x['n']
  s = x['x']
  unname(2*n*s^(n - 1))
}
surfaceArea.sphere <- function(x) {
  r = x['r']
  n = x['n']
  
  unname(2 * pi^(n/2) / gamma(n/2) * r^(n-1))
}



# Increasing Dimensionality
x = 2 # fixing side length of cube and radius of sphere at 2
df <- data.frame(dimension = 1:10)
df$cube_v <- sapply(df$dimension, function(n) volume(cube(2, n)))
df$cube_s <- sapply(df$dimension, function(n) surfaceArea(cube(2, n)))
# df$sphere_v <- sapply(df$dimension, function(n) volume(sphere(2, n)))
# df$sphere_s <- sapply(df$dimension, function(n) surfaceArea(sphere(2, n)))

library(tidyverse)
df %>% 
  pivot_longer(cols = -dimension) %>% 
  ggplot(aes(x = dimension, y = value, color = name)) + geom_line() + theme_bw()


