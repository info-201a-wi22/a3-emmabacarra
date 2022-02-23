rm(list = ls())
library(dplyr)

incarceration_trends <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv")
View(incarceration_trends)
# information about incarcerated population/area by county

jail_jurisdiction <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends_jail_jurisdiction.csv")
View(jail_jurisdiction)
# information about jail populations in jail locations


