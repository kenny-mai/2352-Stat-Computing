###################################################
### Computing class LAB #4
###
### Write your own “rough” density estimation 
### routine using boxcar weights.
### Boxcar means a fixed width 'window' on the data.
###
###################################################

###################################################
### boxcar_density
###################################################

boxcar_density <- function(x, frac = 0.25, n = 1000)
{
  # compute density of x using boxcar weights
  # n: the number of grid points (THE NUMBER OF GRID POINTS DETERMINES THE SPEED AT WHICH WINDOW MOVES).
  #   Example: if the data range from 4 to 10, you want to evaluate the density at n points in that
  #   range, so if n = 1000, then seq(4, 10, length.out = 1000) is the set of points being evaluated.
  # frac: given as fraction of available data on either side of evaluation point.  That is, if the data
  #   range from 4 to 10, then they span 10 - 4 = 6 units. The fraction of that range that is used in
  #   the boxcar is frac * 6 in this case, so the boxcar width would be 1.5. But this is just an
  #   example. Notice you get the lower and upper bounds of the range in the next line setting x_range.
  x_range <- range(x, na.rm = TRUE) # get the range of x (2 values)
  
  x_grid <- seq(x_range[1], x_range[2], length.out = n) # set up grid of points
  
  # allocate a vector to store result (which is a count of how many points are within the boxcar range)
  result <- rep(0, n)
  
  # find a way to get a left and right side of the evaluation point to include in the calculation of
  # the local mean
  
  # this is the size of the box on either side of the center (point of evaluation)
  half_box_car <- 0.5 * frac * diff(x_range)
  
  # loop through values of x_grid and count number of observed x within the boxcar
  for (i in 1:n) {
    pt_left   <- x_grid[i] - half_box_car
    pt_right  <- x_grid[i] + half_box_car
    
    result[i] <- sum(x >= pt_left & x < pt_right)
  }
  
  # A density sums to one; when it is discretized, as we are doing here, each grid point represents
  # 1/n-th of the axis. Technically, if you want the sum of the heights times the width of the grid
  # to be one, you need to do a bit of work.
  
  # First, normalize the result vector so that sum is 1. Next, rescale this sum to be the range of
  # x times 1 / n. This yields a true density.
  # COMMENT: why do we not use length(x)?
  
  y <- result / sum(result)  # sum to one
  y <- y / ((x_range[2] - x_range[1]) / n) # sum to 1/R, with R as defined above.
  
  # return the values in a list
  # COMMENT: why is a list important to use here? Discuss.
  list_result <- list(x = x_grid, y = y)
  
  return(list_result)
}

# plot function for boxcar_density results -- how hard to do? are x & y enough?

library(datasets)
dens <- boxcar_density(precip)
plot(dens, type = "l")

# scratchpad of a simpler way to build a density:
# bins:
x <- rnorm(100)
x <- sort(x)
groups <- -3:3
categories <- cut(x, groups)


counts <- table(categories)
barplot(counts)
# there are a couple of major issues with this approach that need to be addressed:
#   the boxes have gaps, which the data do not
#   the categories are all given equal size
#   the order of the categories may not be correct

# solution:
#   use the rect function to draw boxes, covering the range of each of bin

