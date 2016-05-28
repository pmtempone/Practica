addNA <- function(x, noise = 0.1, seed = 432429) {
  set.seed(seed)
  idx = sample(1:length(x), size = length(x)*noise)
  x = replace(x = x, list = idx, NA)  
  return(x)
}