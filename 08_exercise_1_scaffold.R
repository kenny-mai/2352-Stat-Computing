library(dplyr)
library(ggplot2)

# Replace with path/to/VLSSperCapita.csv
household <- read.csv("~/R/2352-Stat-Computing/VLSSperCapita.csv",
                      row.names = "ID")

##############
# Figure 4.1 #
##############

d.marginal <- density(household$Dollars)
plot(d.marginal, xlab = "U.S. Dollars", main = "")


# Replicate this using ggplot2 and geom_density
ggplot() +
  geom_density(data=household, aes(x=Dollars))


##############
# Figure 4.2 #
##############

rural.households <- household$Dollars[household$Area == "Rural"]
urban.households <- household$Dollars[household$Area == "Urban"]

d.rural <- density(rural.households)
d.urban <- density(urban.households)

plot(d.rural,
     main = "",
     xlab = "Household Per Capita Expenditures (in U.S. Dollars)",
     lty = "solid",
     bty = "l")
lines(d.urban, lty = "dotted")

# Replicate this using ggplot2 and geom_density
ggplot() +
  geom_density(data=household, aes(x=Dollars, linetype=Area))

##############
# Figure 4.3 #
##############

boxplot(rural.households, urban.households, names = c("Rural", "Urban")) 

# Replicate this using ggplot2 and geom_boxplot
ggplot() +
  geom_boxplot(data=household, aes(y=Dollars, group=Area))


############
# Optional #
############

# use geom_histogram in a way that contextualizes the marginal distribution
# of Area and the conditional distributions of Dollars given Area at the
# same time

# use ggplots theme, labs, and scale_XXXX_continuous to modify the plot
# appearance: try to remove the y axis tick marks/text on the density plots
# and minimize wasted plot space

# From chapter 3, adaptive KDE were obtained by first setting up a grid
# of x values (often from a call to density) and then calling
#  quantreg::akj(x = YOUR_CODE_HERE, z = YOUR_CODE_HERE)
# In order to use this with ggplot, the result must be modified and put
# into a data.frame.

library(quantreg)
x <- household$Dollars
z <- seq(min(density(household$Dollars)$x), max(density(household$Dollars)$x), length=200)
akde = data.frame(akj(x, z))

ggplot() +
  geom_histogram(data=akde, aes(x=dens))
















