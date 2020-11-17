# Implement a Winsorized mean function

# For reference, here is the trimmed mean function that we came up with
# in class.
mean.tr <- function(x, trim) {
  x <- sort(x)
  cut.lower <- floor(trim * length(x))
  cut.upper <- ceiling((1 - trim) * length(x))
  
  cat("taking mean of ranks: [", cut.lower + 1, ", ", cut.upper, "]\n", sep = "")
  mean(x[seq.int(cut.lower + 1, cut.upper)])
}

mean.win <- function(x, trim) {
  x <- sort(x)
  cut.lower <- floor(trim * length(x))
  cut.upper <- ceiling((1 - trim) * length(x))
  x[1:cut.lower] <- x[cut.lower+1]
  x[cut.upper:length(x)] <- x[cut.upper]
  return(mean(x))
}


# You can compare your code to the winmean function in the WRS2 package.

household <-
  read.csv(file = "~/R/2352-Stat-Compu/VLSSperCapita.csv",
           row.names = "ID")
library(WRS2)

winmean(household$Dollars, tr = 0.2)
mean.win(household$Dollars, tr = 0.2)

# If you implemented your version by assigning values into
# x that are copies of the value at the end, and you finish early,
# try to implement an efficient version that does not do this
# assignment

mean.win.2 <- function(x, trim) {
  x <- sort(x)
  cut.lower <- floor(trim * length(x))
  cut.upper <- ceiling((1 - trim) * length(x))
  x <- ifelse(x<=x[cut.lower],x[cut.lower+1],x)
  x <- ifelse(x>=x[cut.upper],x[cut.upper],x)
  return(mean(x))
}
mean.win.2(household$Dollars, tr = 0.2)
