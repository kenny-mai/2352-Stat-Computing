
# Replace with path/to/VLSSperCapita.csv
household <- read.csv("~/R/2352-Stat-Computing/VLSSperCapita.csv",
                      row.names = "ID")

sample_from_density <- function(n, size, d) {
  x.grid.width <- diff(d$x)[1]
  
  replicate(n, {
      grid.indices <- sample(length(d$y),
                             size,
                             prob = d$y / sum(d$y),
                             replace = TRUE)
      d$x[grid.indices] + runif(size, -0.5 * x.grid.width, 0.5 * x.grid.width)
  })
}

d.marginal <- density(household$Dollars)
Dollars.rep <- sample_from_density(1000, nrow(household), d.marginal)
Dollars.rep[Dollars.rep < 0] <- 0
dim(Dollars.rep)




# compare Dollars.rep to the first Dollars replication
par(mfrow = c(1, 2))

hist.bins <- seq(0, 100 * ceiling(max(household$Dollars) / 100), 100)
hist(household$Dollars, main = "Original",
     breaks = hist.bins, col = "grey", border = "white")

hist(Dollars.rep[,1], main = "First Replication",
     breaks = hist.bins, col = "grey", border = "white")


# now do similar but for a handful of density estimates
par(mfrow = c(1, 1))
plot(d.marginal, lwd = 2, main = "Density Estimates")
for (i in 1:10) {
  d.rep <- density(Dollars.rep[,i])
  lines(d.rep, col = "grey", lwd = 0.5)
}
legend("topright", lwd = c(2, 0.5), col = c("black", "grey"),
       legend = c("original", "replication"))


#####################
# NOTE: It is important to understand how Dollars.rep relates to 
# Dollars in the original data set. Make sure that you do before
# moving on.
##################


# Compute the mean of dollars in each replication. Your result should
# be 1000 different numbers, each one of which is the average of a
# sample from the density estimate of Dollars.

# Visually Compare that collection of values to the mean in the original
# data set. Calculate the number of replicated estimates that fall within
# +/- 1.96 standard errors from the original sample mean.

mean.orig <- mean(household$Dollars)
mean.se   <- sd(household$Dollars) / sqrt(nrow(household))
mean.rep  <- rep(NA,1000)
  for (i in 1:1000) {
    mean.rep[i] <- mean(sample_from_density(100, nrow(household), d.marginal))
  }
mean.rep 
   # this should be 1000 replicated sample means

 # produce plots and numeric summaries
ggplot() +
  geom_density(data=household, aes(x=Dollars)) +
  geom_line(data=household, aes(x=mean(Dollars)))


















