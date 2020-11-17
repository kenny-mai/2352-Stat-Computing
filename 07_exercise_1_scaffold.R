library(dplyr)
library(readr)

# Replace with path/to/VLSSperCapita.csv
vlss.capita <- read_csv("~/R/2352-Stat-Computing/VLSSperCapita.csv")

# Here are "base" R ways of calculating the prompts.

##########
# Part 1 #
##########
mean(vlss.capita$Dollars[vlss.capita$Area == "Rural"])
# or
with(vlss.capita, mean(Dollars[Area == "Rural"]))
# or
with(subset(vlss.capita, Area == "Rural"), mean(Dollars))

##########
# Part 2 #
##########
# brute force
regions <- sort(unique(vlss.capita$Region))
with(vlss.capita, mean(Dollars[Area == "Rural" & Region == 1]))
with(vlss.capita, mean(Dollars[Area == "Rural" & Region == 2]))
# ...
with(vlss.capita, mean(Dollars[Area == "Rural" & Region == 7]))

# with a loop
result <- rep(NA, length(regions))
for (i in 1:length(regions)) {
  result[i] <- with(vlss.capita, mean(Dollars[Area == "Rural" & Region == regions[i]]))
}
names(result) <- regions

# using tapply
with(subset(vlss.capita, Area == "Rural"),
     tapply(Dollars, Region, mean))

# using by
by(vlss.capita, vlss.capita$Region, function(vlss.capita.region) {
  with(vlss.capita.region, mean(Dollars[Area == "Rural"]))
})

##########
# Part 3 #
##########
# easy if we first re-define income
vlss.capita$dollars.log <- log(vlss.capita$Dollars)

with(subset(vlss.capita, Area == "Rural"),
     tapply(dollars.log, Region, mean))

# Now use dplyr to perform the above calculations





# Rural means of income
as.data.frame(
  vlss.capita %>% 
    filter(Area == "Rural") %>% 
    summarise(mean = (mean(Dollars)), n = n())
)

# Rural means of income by region
as.data.frame(
  vlss.capita %>% 
  group_by(Region) %>% 
  filter(Area == "Rural") %>% 
  summarise(mean = (mean(Dollars)), n = n())
)

# Rural means of log income by region
as.data.frame(
  vlss.capita %>% 
    mutate(logDollars = log(Dollars)) %>% 
    group_by(Region) %>% 
    filter(Area == "Rural") %>% 
    summarise(mean = (mean(logDollars)), n = n())
)






