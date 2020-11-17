# Fix the incredibly minor problem with this plot where the mixed
# color polygon call slightly overwrites the dotted lines for 
# the urban polygon call. In order to do so, think about the order
# in which the "paint" is applied to the canvas, and which regions
# are painted with each call.

library(colorspace)

household <-
  read.csv(file = "~/R/2352-Stat-Computing/VLSSperCapita.csv",
           row.names = "ID")


d.rural <- with(household, density(Dollars[Area == "Rural"]))
d.urban <- with(household,
  density(Dollars[Area == "Urban"], from = min(d.rural$x), to = max(d.rural$x)))


color.rural <- RColorBrewer::brewer.pal(n = 3, name = "RdBu")[1]
color.urban <- RColorBrewer::brewer.pal(n = 3, name = "RdBu")[2]
color.mix   <- hex(mixcolor(0.5, hex2RGB(color.rural), hex2RGB(color.urban)))


# Here is the original code
plot(d.rural,
     main = "",
     xlab = "Household Per Capita Expenditures (in U.S. Dollars)",
     bty = "l",
     type = "n")
polygon(d.rural,
        col = color.rural,
        lty = "solid")
polygon(d.urban,
        col = color.urban,
        lty = "dotted")
polygon(d.rural$x, pmin(d.rural$y, d.urban$y),
        col = color.mix,
        border = NA)




plot(d.rural,
     main = "",
     xlab = "Household Per Capita Expenditures (in U.S. Dollars)",
     bty = "l",
     type = "n")
polygon(d.rural,
        col = color.rural,
        lty = "solid")
polygon(d.urban,
        col = color.urban,
        lty = "dotted")
polygon(d.rural$x, pmin(d.rural$y, d.urban$y),
        col = color.mix,
        border = NA)
lines(d.urban,
        lty = "dotted")

# If you finish early, re-do the polygon calls so that they do not overlap.
# For the sake of simplicity, you can ignore the region on the right
# where the rural density estimate is larger than the urban one. The
# rev function can be helpful here.









