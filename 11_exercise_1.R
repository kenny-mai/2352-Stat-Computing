# Rather that use sample to permute the response variable, use it to create
# random treatment assignments.

asp <- read.csv(file = "~/Documents/2352-Stat-Computing/AfterSchool.csv",
                row.names = "ID")
# calculate observed test statistic
with(asp,
  mean(Delinq[Treatment == 1]) - mean(Delinq[Treatment == 0])
)

set.seed(0)
# Create a random assignment of treatment and response
treatment.permuted <- sample(asp$Treatment)

# single sample of permutation
with(asp, mean(Delinq[treatment.permuted == 1]) -
          mean(Delinq[treatment.permuted == 0]))

# Try to do this in the following ways:
#   taking a pre-existing vector of 0s and 1s (for example, asp$Treatment)
#   finding just the indices of observations that will be treated

trt <- sample(nrow(asp),sum(asp$Treatment))
ctrl <- setdiff(1:nrow(asp),trt)
with(asp, mean(Delinq[trt]) -
       mean(Delinq[ctrl]))

# If you finish early, implement a version that does not use the sample
# function but instead is only able to draw a single random uniform
# number on (0, 1) at a time. This can be drawn by using the function
# runif(1). Random variables being 0 or 1 with probability p can be 
# simulated by testing
#   as.numeric(runif(1) <= p)
# alternatively:
#   rbinom(1, 1, p)

sample.treatment <- function(n.tot, n.trt) {
  rbinom(n=n.tot,size=1,prob=n.trt/n.tot)
}

# to test, run a crude replication
treatment.permuted <- replicate(500, sample.treatment(10, 6))

# make sure that all assignments only have 6 people treated
all(colSums(treatment.permuted) == 6)
 

# each position should roughly have 1/6 chance of being treated
rowMeans(treatment.permuted)

