rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

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

# selecting columns that contain the string "jail"
jail_columns <- 
  incarceration_trends %>%
  select(contains("jail"))
colnames(jail_columns)

# selecting columns that contain the string "prison"
prison_columns <-
  incarceration_trends %>%
  select(contains("prison"))
colnames(prison_columns)


# background information on the counties recorded in `incarceration_trends`
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
    total_incarcerated = mean(c(total_jail_pop, total_jail_pop_dcrp), na.rm = TRUE), # average between 2 sources
    females_incarcerated = mean(c(female_jail_pop, female_jail_pop_dcrp), na.rm = TRUE),
    males_incarcerated = mean(c(male_jail_pop, male_jail_pop_dcrp), na.rm = TRUE),
    
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
    total_incarcerated = mean(c(total_jail_pop, total_jail_pop_dcrp), na.rm = TRUE), # average between 2 sources
    
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
#View(jail_sortrace)

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
#View(prison_sortgender)

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
#View(prison_sortrace)



# ---------- Comparing Jail Incarceration Statistics ----------

# Comparing Incarceration Population in County Jails by Gender
jail_genderstats <-
  incarceration_trends %>%
#  filter(
#    year == max(year) | year == min(year)
#  ) %>% # used logical operator "OR" to apply multiple arguments
  summarize(
    year,
    county_name,
    state,
    
    # total population of inmates within that year
    total_incarcerated = total_jail_pop,
    females_incarcerated = female_jail_pop,
    males_incarcerated = male_jail_pop,
    
    # total within population that are newly admitted
    total_new_inmates = total_jail_adm,
    total_pct_increase = total_jail_pop_rate,
    
    # females in jail
    new_female_inmates = female_jail_adm_dcrp,
    female_pct_increase = female_jail_pop_rate,
    
    # males in jail
    new_male_inmates = male_jail_adm_dcrp,
    male_pct_increase = male_jail_pop_rate,
    
    # total within population that are recently discharged
    total_discharged = total_jail_dis
  )
View(jail_genderstats)


# Comparing Incarceration Population in County Jails by Race
jail_racestats <-
  incarceration_trends %>%
  filter(
    year == max(year) | year == min(year)
  ) %>% # used logical operator "OR" to apply multiple arguments
  summarize(
    year,
    county_name,
    state,
    
    # total population of inmates within that year
    total_incarcerated = total_jail_pop,
    total_new_inmates = total_jail_adm,
    total_pct_increase = total_jail_pop_rate,
    
    # White
    white_incarcerated = white_jail_pop,
    white_rate = white_jail_pop_rate,
    
    # Black
    black_incarcerated = black_jail_pop,
    black_rate = black_jail_pop_rate,
    
    # Latinx
    latinx_incarcerated = latinx_jail_pop,
    latinx_rate = latinx_jail_pop_rate,
    
    # AAPI
    aapi_incarcerated = aapi_jail_pop,
    aapi_rate = aapi_jail_pop_rate,
    
    # Native American
    native_incarcerated = native_jail_pop,
    native_rate = native_jail_pop_rate,
    
    # Other
    other_incarcerated = other_race_jail_pop,
    # no population rate provided for "other"
    
    # total within population that are recently discharged
    total_discharged = total_jail_dis
  )
View(jail_racestats)



# ---------- Comparing Prison Incarceration Statistics ----------

# Comparing Newly Admitted Inmates in State/Federal Prisons by Gender
prison_genderadm <-
  incarceration_trends %>%
  filter(
    year == max(year) | year == min(year)
  ) %>% # used logical operator "OR" to apply multiple arguments
  summarize(
    year,
    county_name,
    state,
    
    # total population of inmates within that year
    total_incarcerated = total_prison_pop,
    females_incarcerated = female_prison_pop,
    males_incarcerated = male_prison_pop,
    
    # total within population that are newly admitted
    total_new_inmates = total_prison_adm,
    total_pct_increase = total_prison_pop_rate,
    
    # females in prison
    new_female_inmates = female_prison_adm,
    female_pct_increase = female_prison_adm_rate,
    
    # males in prison
    new_male_inmates = male_prison_adm,
    male_pct_increase = male_prison_adm_rate
    
    # no data for total population recently discharged
    
  )
View(prison_genderadm)


# Comparing Newly Admitted Inmates in State/Federal Prisons by Race
prison_raceadm <-
  incarceration_trends %>%
  filter(
    year == max(year) | year == min(year)
  ) %>% # used logical operator "OR" to apply multiple arguments
  summarize(
    year,
    county_name,
    state,
    
    # total population of inmates within that year
    total_incarcerated = total_prison_pop,
    total_new_inmates = total_prison_adm,
    total_pct_increase = total_prison_adm_rate,
    
    # White
    white_incarcerated = white_prison_pop,
    white_adm = white_prison_adm,
    white_admrate = white_prison_adm_rate,
    white_female_adm = white_female_prison_adm,
    white_male_adm = white_female_prison_adm,
    
    # Black
    black_incarcerated = black_prison_pop,
    black_adm = black_prison_adm,
    black_admrate = black_prison_adm_rate,
    black_female_adm = black_female_prison_adm,
    black_male_adm = black_male_prison_adm,
    
    # Latinx
    latinx_incarcerated = latinx_prison_pop,
    latinx_adm = latinx_prison_adm,
    latinx_admrate = latinx_prison_adm_rate,
    latinx_female_adm = latinx_female_prison_adm,
    latinx_male_adm = latinx_male_prison_adm,
    
    # AAPI
    aapi_incarcerated = aapi_prison_pop,
    aapi_adm = aapi_prison_adm,
    aapi_admrate = aapi_prison_adm_rate,
    aapi_female_adm = aapi_female_prison_adm,
    aapi_male_adm = aapi_male_prison_adm,
    
    # Native American
    native_incarcerated = native_prison_pop,
    native_adm = native_prison_adm,
    native_admrate = native_prison_adm_rate,
    native_female_adm = native_female_prison_adm,
    native_male_adm = native_male_prison_adm,
    
    # Other
    other_incarcerated = other_race_prison_pop,
    other_adm = other_race_prison_adm,
    other_female_adm = other_race_female_prison_adm,
    other_male_adm = other_race_male_prison_adm
  )
View(prison_raceadm)



# ---------- Trends Over Time Chart ----------

totc <- 
  jail_sortgender %>%
  summarize(
    Year = year,
    Total = total_incarcerated,
    Females = females_incarcerated,
    Males = males_incarcerated
  )
View(totc)

# rearranging `totc` to be formatted for ggplot
melt_totc <- 
  melt(totc, id = c("Year")) %>%
  rename(
    Gender = variable,
    Population = value
  )
View(melt_totc)

ggplot(
  melt_totc, 
  aes(
    x = Year, 
    y = Population, 
    group = Gender, 
    color = Gender
  )) +
  geom_line() +
  ggtitle("Incarceration Populations By Gender")



# ---------- Variable Comparison Chart ----------
# compare which races make up the most of population




# ---------- Map ----------

# map of stats from most recent year showing inmate population


