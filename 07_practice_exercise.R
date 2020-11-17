# The file eitc_example.csv.gz in the data folder on Classes contains example
# data from a tax filing program designed to help people claim the Earned Income
# Tax Credit - a US tax refund for low income earners that often goes unclaimed.

# Load in the data, by pointing the following code to where you have downloaded
# the data file.
eitc <- read.csv("~/R/2352-Stat-Computing/eitc_example.csv.gz")

dim(eitc)
head(eitc)

n <- nrow(eitc)

# The data in this example include 5 columns on the kind of income
# that the filer had:
#   had_wages
#   had_self_employment_income
#   had_interest_income
#   had_other_income
#   had_retirement_income

income_columns <- c("had_wages", "had_self_employment_income",
                    "had_interest_income", "had_other_income",
                    "had_retirement_income")

# Ultimately, the goal with data like this would be to assess whether or not
# demographics or tax-situations are predictive of successfully claiming the tax
# credit (!is.na(completed_at)), or how long it takes to file
# (difftime(completed_at, started_at)).

# However, the purpose of this exercise is to do some minimal "feature
# engineering" on the income columns. Specifically, you are to define two new
# columns:
#   1. had_any_income
#   2. number_of_income_types.
#
# The first should be 1 for every individual that had one or more of the income 
# columns as "yes", while the second should count the number of income columns
# that are "yes". Try to use the apply family of functions or vectorized
# operations when possible.

YOUR_CODE_HERE











































#########
# Hints #
#########

# As always, there are a number of ways to achieve the desired result. Two
# general starting points might be to
#   1. Convert the columns of interest from a data frame into a matrix using
#      string comparisons (== "yes")
#   2. Use the rows directly in an apply function

YOUR_CODE_HERE











































#########
# Hints #
#########

# Consider the type and structure of:
income_df <- eitc[,income_columns]

typeof(income_df)
is.data.frame(income_df)

income_mat <- income_df == "yes"

typeof(income_mat)
is.data.frame(income_mat)
is.matrix(income_mat)

# Ultimately, you want to calculate something like what follows, but without
# using loops.
number_of_income_types <- rep(0, n)
had_any_income <- rep(0, n)

for (i in 1:n) {
  for (j in 1:ncol(income_mat)) {
    if (income_mat[i,j] == TRUE) {
      number_of_income_types[i] <- number_of_income_types[i] + 1
      
      # logic note: think of why this comparison is not necessary, and we could
      # omit it
      if (had_any_income[i] == 0)
        had_any_income[i] <- 1
    }
  }
}

# A brute-force solution (that we should try to avoid, but might be informative)
# would look like:

# Here, R will convert the TRUEs/FALSEs to 0/1s for us
number_of_income_types <- 
  income_mat[,1] + income_mat[,2] + income_mat[,3] + income_mat[,4] + 
  income_mat[,5]
had_any_income <- as.numeric(
  income_mat[,1] | income_mat[,2] | income_mat[,3] | income_mat[,4] |
  income_mat[,5]
)


YOUR_CODE_HERE







































#########
# Hints #
#########

# Here is another way of handling the data with loops
income_df <- eitc[,income_columns]

number_of_income_types <- rep(0, n)
had_any_income <- rep(0, n)

for (i in 1:n) {
  had_any_income[i] <- any(income_df[i,] == "yes")
  number_of_income_types[i] <- sum(income_df[i,] == "yes")
}

YOUR_CODE_HERE







































#########
# Hints #
#########

# Ultimately, the goal is to replace the above with an apply function, or some
# other vectorized calculation.

# Consider the output for the following apply function, used across the rows.
income_df <- eitc[,income_columns]

# just use the first few rows
ignored <- apply(head(income_df), 1, function(row) {
  cat("the values for this row are: '", paste0(row, collapse = "', '"), "\n",
      sep = "")
})

# compare this to
head(income_df)

# Now replace what the function does to each row to return a useful result
# and store that result in one of the desired variables.

YOUR_CODE_HERE <- apply(income_df, 1, function(row) {
  YOUR_CODE_HERE
})










































#########
# Hints #
#########

# If we go the matrix route, we can answer one of the questions easily using
# the rowSums function.

income_mat <- eitc[,income_columns] == "yes"

number_of_income_types <- rowSums(income_mat)

# had_any_income is trickier. For that, we need a "row max" or a "row any"
# You can Google to find several solutions to this problem.

YOUR_CODE_HERE











































############
# Solution #
############

# Picking up where the matrix calculation
income_mat <- eitc[,income_columns] == "yes"

number_of_income_types_mat <- rowSums(income_mat)
had_any_income_mat <- as.numeric(number_of_income_types_mat > 0)
# alternatively
had_any_income_mat <- as.numeric(apply(income_mat, 1, any))

income_df <- eitc[,income_columns]
# And the row-wise comparison
number_of_income_types_df <- apply(income_df, 1, function(row) {
  sum(row == "yes")
})
had_any_income_df <- apply(income_df, 1, function(row) {
  as.numeric(any(row == "yes"))
})


# Check that they give the same answer
all(had_any_income_mat == had_any_income_df)
all(number_of_income_types_mat == number_of_income_types_df)
