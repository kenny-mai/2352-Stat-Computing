##############
# Exercise 1 #
##############

# Take the code below and modify it so that the indices in the data are not
# hard-coded. Specifically, your solution should not rely on any data being
# available in the global environment, and instead should pass everything
# to mean.diff and lat.gen through the boot function itself.

#################
# Original Code #
#################
latino <- read.csv("~/Documents/2352-Stat-Computing/LatinoEd.csv", row.names = "ID")
latino$z.achieve <- scale(x = latino$Achieve)
boot.data <- latino[,c("z.achieve","Mex")]

mean.diff <- function(data) {
  # This is what we want to fixup, and we can do it by having the data argument
  # be a data frame that contains additional information.
  maxrows <- nrow(data)
  cutoff <- maxrows - sum(data$Mex)
  mean(data[(cutoff+1):maxrows,1]) - mean(data[1:cutoff,1])
}

boot.mle <- list(mu = mean(latino$Achieve),sigma = sd(latino$Achieve))

lat.gen <- function(data, mle) {
  # However, this will have to change as well.
  data.frame(zscore=rnorm(n = nrow(data), mean = mle$mu, sd = mle$sigma),Mex=data$Mex)
} 

library(boot)
set.seed(10101)
par.boot <- boot(data = boot.data,
                 statistic = mean.diff,
                 R = 4999,
                 sim = "parametric",
                 ran.gen = lat.gen,
                 mle = boot.mle)


#################
# Original Code #
#################

# Hint: use a debugging strategy (insert a call to browser or add some print
# statements to the mean.diff function) to see what happens when you pass a
# data.frame to boot. Specifically, replace the data argument in boot with:
#   latino[,c("z.achieve", "Mex")].
# If you choose to insert print statements, decrease the number of replications
# for testing purposes.

# These sentences from the help file for boot might be helpful in fixing up 
# lat.gen:
#   The returned value should be a simulated data set of the same form as the
#   observed data which will be passed to statistic to get a bootstrap
#   replicate. It is important that the returned value be of the same shape and
#   type as the original dataset.

YOUR_CODE_HERE

#########
# Bonus #
#########

# If you finish early, omit the step where the data are standardized and pass
# in values to lat.gen using mle instead.
