##############
# Background #
##############

# Inverse CDF sampling is a way to generate any random variable using uniform
# random variables when the inverse CDF (quantile) for that distribution exists.

# Notation:
#   X.tar - r.v. with target distribution
#   X.rep - r.v. that we will generate, hope that distribution of X.rep = X.tar
#   p - cumulative probability, or P(X <= some point) = p
#   q - quantile, inverse of p; P(X <= q) = some probability

# F.tar(q) is the CDF of target r.v. X.tar and F.tar^-1(p) is its inverse, then
# define r.v. X.rep:
#   X.rep = F.tar^-1(U)
# Where U has a random uniform distribution on [0, 1]

# We can prove that X.rep has the same distribution as X.tar by calculating the
# CDF of X.rep and seeing that it is the same CDF as X.tar:
#   P(X.rep <= q) = P(F.tar^-1(U) <= q)
#                 = P(F.tar(F.tar^-1(U)) <= F.tar(q))
#                 = P(U <= F.tar(q))
#                 = F.tar(q)

############
# Exercise #
############

# Take the CDF
#  F.tar(q) = 1 - exp(-sqrt(q)), q >= 0; 0 otherwise


# Define F.tar as a function
F.tar <- function(q){
  ifelse(q >= 0,1 - exp(-sqrt(q)),0)
}

# Graph it, using curve or plot (ggplot not recommended, because we will be
# modifying this plot later)
#   You will have to determine reasonable limits for the x axis, although the
#   y axis should be between 0 and 1
# for axes labels, use:
#   xlab = "q",
#   ylab = "p = P(X.tar <= q)"
X.tar <- F.tar(seq(0,10,by = 0.1))
plot(X.tar,xlab = "q",ylab = "p = P(X.tar <= q)")

# The inverse CDF is given by:
#   F^{-1}(p) = (log(1 - p))^2, q >= 0; 0 otherwise

# Define the inverse function
I.tar <- function(q){
  p <- X.tar(q)
  ifelse(p >= 0,(log(1 - p))^2,0)
}

# Generate a single random uniform number
set.seed(0)
u <- I.tar(0.5)
u

# On your graph, draw a horizontal line at the height of u; give it a color
# or line-type if you desire.
YOUR_CODE_HERE

# Find the value along the q-axis where the horizontal line intersects with
# the CDF. Store this value.
x.rep <- YOUR_CODE_HERE

# Draw a line segment that extends down from the point of intersection to the
# q-axis.
YOUR_CODE_HERE

# Calculate the probability that a random variable X.rep is to the left of
# x.rep. State how this relates to U.
YOUR_CODE_HERE

# Repeat this procedure once more:
#   sample a height along the p axis, in [0, 1]
#   draw a horizontal line at that height
#   find the intersection of that line and the CDF
#   use the q value associated with that intersection as the transformed
#     random variable
u.2     <- YOUR_CODE_HERE
x.rep.2 <- YOUR_CODE_HERE
# plot line and line segment


# According to the graph/the CDF itself, calculate:
#   P(x.rep.2 <= X.rep <= x.rep) = P(X.rep <= x.rep) - P(X.rep <= x.rep.2)
# (if x.rep < x.rep.2, change the sign)
YOUR_CODE_HERE
# Similarly, calculate:
u - u.2


# Test your sampling procedure to see that it has the target distribution.
# Generate 1000 samples of x.rep
x.rep <- YOUR_CODE_HERE

plot(density(x.rep), xlim = c(0, 20))
# the true density is :
#   0.5 q^{-0.5} exp(-sqrt(q))
curve(0.5 * exp(-sqrt(x)) / sqrt(x), add = TRUE,
      lty = "dashed")
