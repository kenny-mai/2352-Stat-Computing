# download the race_ethnicity data from Classes in the data folder and read it into R
racial_ethnic_identity <- trimws(readLines("~/R/2352-Stat-Computing/race_ethnicity.txt.gz"))

# look at the data
racial_ethnic_identity[1:10]

# if you try to table this data, it will be very messy
table(racial_ethnic_identity)

# split the categories on commas
race_ethnicity_cat <- strsplit(racial_ethnic_identity, ",")
race_ethnicity_cat[1:10]

# strip out the extra json characters
race_ethnicity_cat <- lapply(race_ethnicity_cat, function(x) gsub("\\{|\\}|\"", "", x))
race_ethnicity_cat <- lapply(race_ethnicity_cat, function(x) { if (anyNA(x)) x[is.na(x)] <- "Decline to state"; x })

race_ethnicity_cat[1:10]

# these are the number of times each category is used, but not the number of people
# some people checked more than one box
length(race_ethnicity_cat)
table(unlist(race_ethnicity_cat))
sum(table(unlist(race_ethnicity_cat)))

# Use the sapply function to apply to each individual logic that performs the following mapping to 
# US Census categories:

# 1) If _any_ of the identities are in
#   1.1) "Hispanic or Latino (any other race)", "White (Hispanic or Latino)", or "Black or African American (Hispanic or Latino)"
#       return "Hispanic of any race"
# 2) If _all_ of the identities are in:
#   2.1) "White (non-Hispanic or Latino)"
#        return "White non-Hispanic"
#   2.2) "Black or African American (non-Hispanic or Latino)"
#        return "Black non-Hispanic"
#   2.3) "Asian Indian", "Cambodian", "Chinese", "Filipino", "Hmong", "Japanese", "Korean", "Laotian", "Other Asian", "Thai", or "Vietnamese"
#        return "Asian"
#   2.4) "American Indian or Alaska Native", "Guamanian", "Indigenous - Latin America", "Native Hawaiian", or "Samoan"
#        return "American Indian/Pacific Islander non-Hispanic"
# 3) If the length of the identities is greater than 1
#    return "2 or more races, non-Hispanic"
# 4) Return the original identitiy

# double check your work by comparing the original category to your mapped version for individuals
#   47, 56, 168, 207, 213, 1797


map_race_ethnicity <- function(x_i) {
  result = NA
  if (any(x_i %in% c("Hispanic or Latino (any other race)","White (Hispanic or Latino)","Black or African American (Hispanic or Latino)")))
    result = "Hispanic of any race"
  else if (length(unlist(x_i)) > 1)
    result = "2 or more races, non-Hispanic"
  else if (x_i == "White (non-Hispanic or Latino)")
    result = "White non-Hispanic"
  else if (x_i == "Black or African American (non-Hispanic or Latino)")
    result = "Black non-Hispanic"
  else if (x_i %in% c("Asian Indian","Cambodian","Chinese","Filipino","Hmong","Japanese","Korean","Laotian","Other Asian","Thai","Vietnamese"))
    result = "Asian"
  else if (x_i %in% c("American Indian or Alaska Native","Guamanian","Indigenous - Latin America","Native Hawaiian","Samoan"))
    result = "American Indian/Pacific Islander non-Hispanic"
  else if (x_i == "Decline to state")
    result = "Decline to state"
  else if (x_i == "Other")
    result = "Other"
  else
    result = NA
  return(result)
}

race_ethnicity <- sapply(race_ethnicity_cat, map_race_ethnicity)

length(race_ethnicity) # there should be 5000
proportions(table(race_ethnicity)) # about 80% should be Hispanic/Latino
race_ethnicity[c(47, 56, 168, 207, 213, 1797)]
race_ethnicity_cat[c(47, 56, 168, 207, 213, 1797)]


