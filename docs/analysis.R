rm(list = ls())
library(dplyr)

incarceration_trends <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv")
View(incarceration_trends) # Incarcerated population/area by county-level

jail_jurisdiction <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends_jail_jurisdiction.csv")
View(jail_jurisdiction) # Incarcerated population by jurisdiction-level



# General Overview of `incarceration_trends` and `jail_jurisdiction`
obs_counties <- nrow(incarceration_trends)
nfeats_counties <- ncol(incarceration_trends)
paste("There are", obs_counties, "observations and", nfeats_counties, "features (columns) in `incarceration_trends`.")

obs_jurisdiction <- nrow(jail_jurisdiction)
nfeats_jurisdiction <- ncol(jail_jurisdiction)
paste("There are", obs_jurisdiction, "observations and", nfeats_jurisdiction, "features (columns) in `jail_jurisdiction`.")



# Total Incarcerations by Year
colnames(incarceration_trends)
co_sortyr <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # The following columns summarize the general population
    total_pop = sum(total_pop, na.rm = TRUE), # total within the county
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64

    # The following columns summarize the incarcerated population by gender
    total_incarcerated = sum(total_jail_pop, na.rm = TRUE),
    female_incarcerated = sum(female_jail_pop, na.rm = TRUE),
    male_incarcerated = sum(male_jail_pop, na.rm = TRUE),
    adult_female_inc = sum(female_adult_jail_pop, na.rm = TRUE),
    juvenile_female_inc = sum(female_juvenile_jail_pop, na.rm = TRUE),
    adult_male_inc = sum(male_adult_jail_pop, na.rm = TRUE),
    juvenile_male_inc = sum(male_juvenile_jail_pop, na.rm = TRUE),
    
    # The following columns summarize the incarcerated population by race
    aapi_incarcerated = sum(aapi_jail_pop, na.rm = TRUE)
    
  )
View(co_sortyr)

# race
# general population, jail population
# MAKE SENSE / DIFFERENTIATE COLUMNS, LIKE
# aapi_jail_pop VS aapi_prison_pop