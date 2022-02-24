rm(list = ls())
library(dplyr)

incarceration_trends <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv")
View(incarceration_trends) # Incarcerated population/area by county-level

jail_jurisdiction <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends_jail_jurisdiction.csv")
View(jail_jurisdiction) # Incarcerated population by jurisdiction-level



# ---------- General Overview of `incarceration_trends` and `jail_jurisdiction` ----------
obs_counties <- nrow(incarceration_trends)
nfeats_counties <- ncol(incarceration_trends)
paste("There are", obs_counties, "observations and", nfeats_counties, "features (columns) in `incarceration_trends`.")

obs_jurisdiction <- nrow(jail_jurisdiction)
nfeats_jurisdiction <- ncol(jail_jurisdiction)
paste("There are", obs_jurisdiction, "observations and", nfeats_jurisdiction, "features (columns) in `jail_jurisdiction`.")

# Note:
# Jail refers to individuals who are held in lawful custody (i.e. have been convicted of
# a minor offense or are waiting to go on trial for their accusation) for a short period
# of time. The word jail implies that it is a smaller, local facility. Prison refers to
# individuals who have been convicted of more serious offenses and are sentenced to longer
# terms. Prison can also be referred to as "state prison" or "federal prison", and refers
# to a larger facility of confinement.



# ---------- Annual Total Incarcerations ----------
colnames(incarceration_trends)

# Annual County Jail Incarceration Statistics By Gender
jail_sortgender <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total within the county
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64

    # Gender Totals in Jail
    total_incarcerated = sum(total_jail_pop, na.rm = TRUE),
    female_incarcerated = sum(female_jail_pop, na.rm = TRUE),
    male_incarcerated = sum(male_jail_pop, na.rm = TRUE),
    
    # Number of Adults in Jail
    adult_females = sum(female_adult_jail_pop, na.rm = TRUE),
    adult_males = sum(male_adult_jail_pop, na.rm = TRUE),
    
    # Number of Juveniles in Jail
    juvenile_females = sum(female_juvenile_jail_pop, na.rm = TRUE),
    juvenile_males = sum(male_juvenile_jail_pop, na.rm = TRUE),
    
    # Number of Deaths in Jail Custody (DCRP)
    
    
  )
View(jail_sortgender)

# Annual County Jail Incarceration Statistics By Race
jail_sortrace <-
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total within the county
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64
    
    # White
    
    # Black
    
    # Latinx
    
    # AAPI (Asian American and Pacific Islander)
    aapi_incarcerated = sum(aapi_jail_pop, na.rm = TRUE),
    
    # Native American
    
    # Other Race
  )
View(jail_sortrace)

# Annual State/Federal Prison Incarceration Statistics by Gender
prison_sortgender <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total within the county
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64
    
    
  )
View(prison_sortgender)

# Annual State/Federal Prison Incarceration Statistics by Race
prison_sortrace <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total within the county
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64
    
  )
View(prison_sortrace)



# ---------- Incarceration Statistics by County ----------

# group by county, compare change in numbers from earliest to most recent date
