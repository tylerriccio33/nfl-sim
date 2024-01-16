f_rescale <- function(x) {
  
  # Get min and max
  xmin <- min(x)
  xmax <- max(x)
  
  # Make minimum 0
  x_c <- x - xmin
  
  # Scale to [0,1] 
  x_scaled <- x_c / (xmax - xmin)
  
  return(x_scaled)
  
}