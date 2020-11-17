##############
# Exercise 2 #
##############

# Use optim to find the optimal values of beta for a fixed value of 
# delta when using the Huber/Windsorized loss function.


### Huber Loss ###

# First we define the function abstractly, without regard to the data.

# y    : the value for which we want to calculate the loss
# y.hat: predicted value of y
# delta: hyperparameter of Huber loss
loss.huber <- function(y, y.hat, delta) {
  a <- abs(y - y.hat)
  ifelse(a <= delta, 0.5 * a^2, delta * (a - 0.5 * delta))
}

# If you are curious what loss.huber looks like relative to other loss
# functions, evaluate the inner block of the following:
if (FALSE) { # block comment
  curve(loss.huber(x, 0, 1), -4, 4, ylab = "loss")
  curve(0.5 * x^2, add = TRUE, lty = 2)
  curve(0.5 * abs(x), add = TRUE, lty = 3)
  legend("bottomright", lty = 1:3, bty = "n",
         c("huber", "sq-err", "abs"))
}

# The following function will be used to wrap any vectorized loss function and
# return the risk for a linear regression problem. We will use the .. arguments
# to pass on delta to loss.huber.
calculate.risk <- function(betas, x, y, loss, ...) {
  intercept <- betas[1]
  slope     <- betas[2]
  
  y.hat <- intercept + slope * x
  
  mean(loss(y, y.hat, ...))
}

nels <- read.csv("~/R/2352-Stat-Computing/NELS.csv", row.names = "ID")

# Just to test that the risk function works, evaluate it near the OLS estimate.
calculate.risk(c(47, 2),
               x = nels$Homework,
               y = nels$Achievement,
               loss = loss.huber,
               delta = 15)


### Huber Risk and optim ###

# Now use optim with calculate.risk and loss.huber to calculate the values of
# beta that minimize the Huber risk. To recap, give optim a reasonable starting
# point, the function you want to minimize, and any ... arguments necessary
# for that function to work correctly.

YOUR_CODE_HERE


# Check that the fitted value is close to the OLS estimate, but not exactly the
# same.
fit.huber$par



### Hyperparameters and grids ###

# delta is a "hyperparameter", and cannot be estimated from the data. Repeat
# the above procedure and calculate the Huber risk along a grid of values of
# delta.

deltas <- seq(1, 30, length.out = 15)
# Store the optim results for each value of delta in this list:
fits.huber <- list()

YOUR_CODE_HERE

for (i in 1:length(deltas)) {
  YOUR_CODE_HERE
}


# The following will visualize the training risk as a function of delta.

risk.train <- numeric(length(fits.huber))
for (i in 1:length(fits.huber))
  risk.train <- fits.huber[[i]]$value

# equivalently:
#   risk.train <- sapply(fits.huber, "[[", "value")

plot(deltas, risk.train,
     type = "l",
     xlab = expression(delta), ylab = "Huber Risk",
     main = "Training Risk")


### Hyperparameter estimation ###

# If you finish early, subdivide the data into a training and a validation set.
# They can be of any size, although anywhere from 1:1 to 9:1 are typical ratios.
# Repeat the above estimation procedure using the training data, but evaluate
# and plot the validation risk. For validation, use squared error loss.

# Not needed to set the seed, but can be helpful for debugging.
set.seed(0)
# In order to subdivide the data, permute the indices 1:nrow(nels)
ind.permutation <- sample.int(nrow(nels))
# Take the first XX% of them, make them training data; use the remaining as
# test data.
ind.train      <- ind.permutation[YOUR_CODE_HERE]
ind.validation <- ind.permutation[YOUR_CODE_HERE]

x.train      <- nels$Homework[ind.train]
x.validation <- nels$Homework[ind.validation]
y.train      <- nels$Achievement[ind.train]
y.validation <- nels$Achievement[ind.validation]

# Calculate the best-fits for each delta on the training sets.

deltas <- seq(1, 30, length.out = 15)
fits.huber <- list()


for (i in 1:length(deltas)) {
  YOUR_CODE_HERE
}

# Now calculate the squared error losses across the validation sets.
loss.sq_err <- function(y, y.hat) {
  (y - y.hat)^2
}
risk.validation <- YOUR_CODE_HERE


# And add it to the plot. The two risks are on different scales, so we adjust
# to match.
lines(deltas,
      max(risk.train) * risk.validation / max(risk.validation),
      col = "red")


# And if you still finish early, repeat all of the above but use leave-one-out
# crossvalidation or K-fold crossvalidation.
