rm(list = ls())
library(dplyr)

incarceration_trends <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv")
View(incarceration_trends)

jail_jurisdiction <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends_jail_jurisdiction.csv")
View(jail_jurisdiction)

# For this assignment, I will be comparing data drawn from `incarceration_trends` and  
# `jail_jurisdiction`, observing any differences between demographics such as race and 
# gender. The information recorded in `incarceration_trends` is based on incarcerated 
# populations & the surrounding areas on a county-level basis. `jail_jurisdiction` is
# based on incarcerated populations in local county jails.



# ---------- General Overview of `incarceration_trends` and `jail_jurisdiction` ----------
obs_counties <- nrow(incarceration_trends)
nfeats_counties <- ncol(incarceration_trends)
paste("There are", obs_counties, "observations and", nfeats_counties, "features (columns) in `incarceration_trends`.")

obs_jurisdiction <- nrow(jail_jurisdiction)
nfeats_jurisdiction <- ncol(jail_jurisdiction)
paste("There are", obs_jurisdiction, "observations and", nfeats_jurisdiction, "features (columns) in `jail_jurisdiction`.")

# Note:
# Jail refers to individuals who are held in lawful custody (i.e. have been convicted
# of a minor offense or are waiting to go on trial for their accusation) for a short
# period of time. The word jail implies that it is a smaller, local facility. Prison
# refers to individuals who have been convicted of more serious offenses and are
# sentenced to longer terms. Prison can also be referred to as "state prison" or 
# "federal prison", and refers to a larger facility of confinement than a county jail.

colnames(incarceration_trends)
# column names

# ---------- Annual Total Incarcerations ----------

# Annual County Jail Incarceration Statistics By Gender
jail_sortgender <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total recorded population
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64
    females_15to64 = sum(female_pop_15to64, na.rm = TRUE), # total females between ages 15-64
    males_15to64 = sum(male_pop_15to64, na.rm = TRUE), # total males between ages 15-64    

    # Gender Totals in Jail
    total_incarcerated = sum(total_jail_pop, na.rm = TRUE),
    females_incarcerated = sum(female_jail_pop, na.rm = TRUE),
    males_incarcerated = sum(male_jail_pop, na.rm = TRUE),
    
    # Number of Adults in Jail
    adult_females = sum(female_adult_jail_pop, na.rm = TRUE),
    adult_males = sum(male_adult_jail_pop, na.rm = TRUE),
    
    # Number of Juveniles in Jail
    juvenile_females = sum(female_juvenile_jail_pop, na.rm = TRUE),
    juvenile_males = sum(male_juvenile_jail_pop, na.rm = TRUE),
    
    # Number of Deaths in Jail Custody (DCRP)
    jail_deaths = sum(total_jail_pop_dcrp, na.rm = TRUE),
    female_deaths = sum(female_jail_pop_dcrp, na.rm = TRUE),
    male_deaths = sum(male_jail_pop_dcrp, na.rm = TRUE)
  )
View(jail_sortgender)

# Annual County Jail Incarceration Statistics By Race
jail_sortrace <-
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total recorded population
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64
    total_incarcerated = sum(total_jail_pop, na.rm = TRUE), # total population in jail
    
    # White
    white_totpop_15to64 = sum(white_pop_15to64, na.rm = TRUE), # gen population white between 15-64
    white_incarcerated = sum(white_jail_pop, na.rm = TRUE),
    
    # Black
    black_totpop_15to64 = sum(black_pop_15to64, na.rm = TRUE), # gen population black individuals between 15-64
    black_incarcerated = sum(black_jail_pop, na.rm = TRUE),
    
    # Latinx
    latinx_totpop_15to64 = sum(latinx_pop_15to64, na.rm = TRUE), # gen population latinx between 15-64
    latinx_incarcerated = sum(latinx_jail_pop, na.rm = TRUE), 
    
    # AAPI (Asian American and Pacific Islander)
    aapi_totpop_15to64 = sum(aapi_pop_15to64, na.rm = TRUE), # gen population AAPI between 15-64
    aapi_incarcerated = sum(aapi_jail_pop, na.rm = TRUE),
    
    # Native American
    native_totpop_15to64 = sum(native_pop_15to64, na.rm = TRUE), # gen population native between 15-64
    native_incarcerated = sum(native_jail_pop, na.rm = TRUE),
    
    # Other Race
    other_totpop_15to64 = 
      total_pop_15to64 - (sum(
        white_totpop_15to64,
        black_totpop_15to64,
        latinx_totpop_15to64,
        aapi_totpop_15to64,
        native_totpop_15to64,
        na.rm = TRUE
        )),
    other_incarcerated = sum(other_race_jail_pop, na.rm = TRUE)
  )
View(jail_sortrace)

# Annual State/Federal Prison Incarceration Statistics by Gender
prison_sortgender <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total recorded population
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64
    females_15to64 = sum(female_pop_15to64, na.rm = TRUE), # total females between ages 15-64
    males_15to64 = sum(male_pop_15to64, na.rm = TRUE), # total males between ages 15-64        
    
    # Gender Totals in Prison
    total_incarcerated = sum(total_prison_pop, na.rm = TRUE),
    females_incarcerated = sum(female_prison_pop, na.rm = TRUE),
    males_incarcerated = sum(male_prison_pop, na.rm = TRUE)
    
    # Note:
    # In-depth gender statistics provided for county jail incarcerations
    # are not provided for records at the state/federal prison level.
  )
View(prison_sortgender)

# Annual State/Federal Prison Incarceration Statistics by Race
prison_sortrace <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # General Population
    total_pop = sum(total_pop, na.rm = TRUE), # total recorded population
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), # total between the ages 15-64
    total_incarcerated = sum(total_prison_pop, na.rm = TRUE), # total population in prison
    
    # White
    white_totpop_15to64 = sum(white_pop_15to64, na.rm = TRUE), # gen population white between 15-64
    white_incarcerated = sum(white_prison_pop, na.rm = TRUE),
    white_females = sum(white_female_prison_pop, na.rm = TRUE),
    white_males = sum(white_male_prison_pop, na.rm = TRUE),
    
    # Black
    black_totpop_15to64 = sum(black_pop_15to64, na.rm = TRUE), # gen population black individuals between 15-64
    black_incarcerated = sum(black_prison_pop, na.rm = TRUE),
    black_females = sum(black_female_prison_pop, na.rm = TRUE),
    black_males = sum(black_male_prison_pop, na.rm = TRUE),
    
    # Latinx
    latinx_totpop_15to64 = sum(latinx_pop_15to64, na.rm = TRUE), # gen population latinx between 15-64
    latinx_incarcerated = sum(latinx_prison_pop, na.rm = TRUE),
    latinx_females = sum(latinx_female_prison_pop, na.rm = TRUE),
    latinx_males = sum(latinx_male_prison_pop, na.rm = TRUE),
    
    # AAPI (Asian American and Pacific Islander)
    aapi_totpop_15to64 = sum(aapi_pop_15to64, na.rm = TRUE), # gen population AAPI between 15-64
    aapi_incarcerated = sum(aapi_prison_pop, na.rm = TRUE),
    aapi_females = sum(aapi_female_prison_pop, na.rm = TRUE),
    aapi_males = sum(aapi_male_prison_pop, na.rm = TRUE),
    
    # Native American
    native_totpop_15to64 = sum(native_pop_15to64, na.rm = TRUE), # gen population native between 15-64
    native_incarcerated = sum(native_prison_pop, na.rm = TRUE),
    native_females = sum(native_female_prison_pop, na.rm = TRUE),
    native_males = sum(native_male_prison_pop),
    
    # Other Race
    other_totpop_15to64 = 
      total_pop_15to64 - (sum(
        white_totpop_15to64,
        black_totpop_15to64,
        latinx_totpop_15to64,
        aapi_totpop_15to64,
        native_totpop_15to64,
        na.rm = TRUE
      )),
    other_incarcerated = sum(other_race_prison_pop, na.rm = TRUE),
    other_females = sum(other_race_female_prison_pop, na.rm = TRUE),
    other_males = sum(other_race_male_prison_pop, na.rm = TRUE)
  )
View(prison_sortrace)



# ---------- Comparing Incarceration Statistics ----------

county_info <-
  # filtering out unique values in a row and summarizing selected columns of those rows
  incarceration_trends[!duplicated(incarceration_trends$county_name), ] %>%
  summarize(
    county_name,
    state,
    urbanicity,
    region,
    division,
    commuting_zone,
    metro_area,
    land_area,
  )
View(county_info)


# total_jail_adm (admitted), total_jail_dis (discharged)
# deaths (DCRP)
# compare recent and latest year and changes in between

jail_info <-
  incarceration_trends %>%
  filter(
    year == max(year) | year == min(year)
  ) %>% # used logical operator "OR" to apply multiple arguments
  summarize(
    county_name,
    state,
    total_incarcerated = sum(total_jail_pop, na.rm = TRUE),
    females_incarcerated = sum(female_jail_pop, na.rm = TRUE),
    males_incarcerated = sum(male_jail_pop, na.rm = TRUE),
  )
View(jail_info)
