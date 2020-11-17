# Make the following code from the book cleaner, by removing hard-coded (magic)
# numbers and reuse constants.

household <- read.csv("VLSSperCapita.csv",row.names = "ID")

central.coast     <- household$Dollars[household$Region == 1]
central.highlands <- household$Dollars[household$Region == 2]
mekong.delta      <- household$Dollars[household$Region == 3]
north.coast       <- household$Dollars[household$Region == 4]
northern.uplands  <- household$Dollars[household$Region == 5]
red.river.delta   <- household$Dollars[household$Region == 6]
south.east        <- household$Dollars[household$Region == 7]

library(RColorBrewer)
brewer.pal(n = 7, name = "Set1")
plot(density(central.coast), 
     xlim = c(0, 3100), ylim = c(0, 0.008),
     main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",
     bty = "l", col = "#E41A1C")
lines(density(central.highlands), col = "#377EB8")
lines(density(mekong.delta),      col = "#4DAF4A")
lines(density(north.coast),       col = "#984EA3")
lines(density(northern.uplands),  col = "#FF7F00")
lines(density(red.river.delta),   col = "#FFFF33")
lines(density(south.east),        col = "#A65628")
legend(x = 2000, y = 0.0075,
       legend = c("Central Coast", "Central Highlands", "Mekong Delta",
                  "North Coast", "Northern Uplands", "Red River Delta",
                  "South East"),
       lty = "solid",
       col = c("#E41A1C", "#FFFF33", "#4DAF4A", "#984EA3",
               "#FF7F00", "#A65628", "#377EB8"))

# Things to consider:
#   * Store the region names in character vector.
#   *   Optionally, use the names attribute of the character vector to store
#       the region's number.
#   * Store the color palette in a variable.
#   * Use a loop to draw all of the densities plots.
#   * Determine the range for the plot dynamically and not by hard-coding.
#   * Place the legend dynamically as well.


household <- read.csv("VLSSperCapita.csv",row.names = "ID")
library(RColorBrewer)
palvar <- brewer.pal(n = 7, name = "")
plot("Density", 
     xlim = c(floor(min(household$Dollars)), ceiling(max(household$Dollars))), ylim = c(0, 0.008),
     main = "", xlab = "Household Per Capita Expenditures (in U.S. Dollars)",
     bty = "l")
for (i in 1:length(unique(household$Region))) {
        lines(density(household$Dollars[household$Region == i]), col = palvar[i])     
}
legend(x = 1500, y = 0.0075,
       legend = c("Central Coast", "Central Highlands", "Mekong Delta",
                  "North Coast", "Northern Uplands", "Red River Delta",
                  "South East"),
       lty = "solid",
       col = palvar[1:length(unique(household$Region))])



# Dear God, I hate base R plotting. ggplot gods help me.
library(dplyr)
library(ggplot2)
household <- read.csv("VLSSperCapita.csv",row.names = "ID")
household %>% mutate(Region=as.factor(Region)) %>% 
        ggplot(.) +
        geom_density(aes(x=Dollars,color=Region)) +
        scale_color_discrete(name="Regions",
                             breaks=1:length(unique(household$Region)),
                             labels=c("Central Coast", "Central Highlands", "Mekong Delta",
                                      "North Coast", "Northern Uplands", "Red River Delta",
                                      "South East"))



        




