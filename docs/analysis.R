rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(tidyr)

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
# View(county_info)



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
# View(jail_sortgender)

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
# View(jail_sortrace)

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
# View(prison_sortgender)

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
# View(prison_sortrace)



# ---------- Comparing Jail Incarceration Statistics ----------

# Comparing Incarceration Population in County Jails by Gender
jail_genderstats <-
  incarceration_trends %>%
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
# View(jail_genderstats)


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
# View(jail_racestats)



# ---------- Comparing Prison Incarceration Statistics ----------

# Comparing Newly Admitted Inmates in State/Federal Prisons by Gender Over Time
prison_genderadm <-
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # total population of inmates within that year
    total_incarcerated = sum(total_prison_pop, na.rm = TRUE),
    females_incarcerated = sum(female_prison_pop, na.rm = TRUE),
    males_incarcerated = sum(male_prison_pop, na.rm = TRUE),
    
    # total within population that are newly admitted
    total_new_inmates = sum(total_prison_adm, na.rm = TRUE),
    total_pct_increase = sum(total_prison_pop_rate, na.rm = TRUE),
    
    # females in prison
    new_female_inmates = sum(female_prison_adm, na.rm = TRUE),
    female_pct_increase = sum(female_prison_adm_rate, na.rm = TRUE),
    
    # males in prison
    new_male_inmates = sum(male_prison_adm, na.rm = TRUE),
    male_pct_increase = sum(male_prison_adm_rate, na.rm = TRUE)
    
    # no data for total population recently discharged
  )
# View(prison_genderadm)


# Comparing Newly Admitted Inmates in State/Federal Prisons by Race Over Time
prison_raceadm <-
  incarceration_trends %>%
  group_by(year) %>%
  summarize(
    # total population of inmates within that year
    total_incarcerated = sum(total_prison_pop, na.rm = TRUE),
    total_new_inmates = sum(total_prison_adm, na.rm = TRUE),
    total_pct_increase = sum(total_prison_adm_rate, na.rm = TRUE),
    
    # White
    white_incarcerated = sum(white_prison_pop, na.rm = TRUE),
    white_adm = sum(white_prison_adm, na.rm = TRUE),
    white_admrate = sum(white_prison_adm_rate, na.rm = TRUE),
    white_female_adm = sum(white_female_prison_adm, na.rm = TRUE),
    white_male_adm = sum(white_female_prison_adm, na.rm = TRUE),
    
    # Black
    black_incarcerated = sum(black_prison_pop, na.rm = TRUE),
    black_adm = sum(black_prison_adm, na.rm = TRUE),
    black_admrate = sum(black_prison_adm_rate, na.rm = TRUE),
    black_female_adm = sum(black_female_prison_adm, na.rm = TRUE),
    black_male_adm = sum(black_male_prison_adm, na.rm = TRUE),
    
    # Latinx
    latinx_incarcerated = sum(latinx_prison_pop, na.rm = TRUE),
    latinx_adm = sum(latinx_prison_adm, na.rm = TRUE),
    latinx_admrate = sum(latinx_prison_adm_rate, na.rm = TRUE),
    latinx_female_adm = sum(latinx_female_prison_adm, na.rm = TRUE),
    latinx_male_adm = sum(latinx_male_prison_adm, na.rm = TRUE),
    
    # AAPI
    aapi_incarcerated = sum(aapi_prison_pop, na.rm = TRUE),
    aapi_adm = sum(aapi_prison_adm, na.rm = TRUE),
    aapi_admrate = sum(aapi_prison_adm_rate, na.rm = TRUE),
    aapi_female_adm = sum(aapi_female_prison_adm, na.rm = TRUE),
    aapi_male_adm = sum(aapi_male_prison_adm, na.rm = TRUE),
    
    # Native American
    native_incarcerated = sum(native_prison_pop, na.rm = TRUE),
    native_adm = sum(native_prison_adm, na.rm = TRUE),
    native_admrate = sum(native_prison_adm_rate, na.rm = TRUE),
    native_female_adm = sum(native_female_prison_adm, na.rm = TRUE),
    native_male_adm = sum(native_male_prison_adm, na.rm = TRUE),
    
    # Other
    other_incarcerated = sum(other_race_prison_pop, na.rm = TRUE),
    other_adm = sum(other_race_prison_adm, na.rm = TRUE),
    other_female_adm = sum(other_race_female_prison_adm, na.rm = TRUE),
    other_male_adm = sum(other_race_male_prison_adm, na.rm = TRUE),
  )
# View(prison_raceadm)



# ---------- Trends Over Time Chart ----------

totc <- 
  prison_raceadm %>%
  summarize(
    Year = year,
    White = white_adm,
    Black = black_adm,
    Latinx = latinx_adm,
    AAPI = aapi_adm,
    `Native American` = native_adm,
    Other = other_adm
  )
# View(totc)

melt_totc <- 
  melt(totc, id = c("Year")) %>%
  rename(
    Race = variable,
    `New Admits` = value
  ) # rearranging `totc` to be formatted for ggplot
# View(melt_totc)

ggplot(
  melt_totc, 
  aes(
    x = Year, 
    y = `New Admits`, 
    group = Race, 
    color = Race
  )) +
  geom_line() +
  ggtitle("New Admits by Race Over Time")

# Disclaimer: for at least the first 10 years, the inmates' races were not identified
# and recorded, which is why Latinx, AAPI, Native American, and Other start out at 0.
# There are also no recordings of racial groups in the years 2017 and 2018.



# ---------- Variable Comparison Chart ----------
# Intention: compare earliest and most recent year, racial distribution

# View(jail_racestats)

# Racial Distribution in Jail For Earliest Year of Data
vcc_jailmin <-
  jail_racestats %>%
  filter(year == min(year)) %>%
  summarize(
    Year = mean(year),
    White = (sum(white_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    Black = (sum(black_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    Latinx = (sum(latinx_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    AAPI = (sum(aapi_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    `Native American` = (sum(native_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100
  )
vcc_jailmin

# Oh no! Unfortunately, there's no data from this year that shows the racial ethnic
# groups of the inmate population. Although a total population number was recorded
# in `jail_racestats` for the earliest year, the respective races of that population
# was not. Calling the `vcc_jailmin` variable returns this message in the console:
#
#        Year White Black Latinx AAPI Native American
#      1 1970     0     0      0    0               0
#
#
# Let's compare racial ethnic groups from the most recent year of data...


# Racial Distribution in Jail For Most Recent Year of Data
vcc_jailmax <-
  jail_racestats %>%
  filter(year == max(year)) %>%
  summarize(
    Year = mean(year),
    White = (sum(white_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    Black = (sum(black_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    Latinx = (sum(latinx_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    AAPI = (sum(aapi_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100,
    `Native American` = (sum(native_incarcerated, na.rm = TRUE) / sum(total_incarcerated, na.rm = TRUE)) * 100
  )
# View(vcc_jailmax)

melt_jailmax <- 
  melt(vcc_jailmax, id = c("Year")) %>%
  rename(
    Race = variable,
    Percent = value
  )
# View(melt_jailmax)

ggplot(
  melt_jailmax,
  aes(x = Race, y = Percent)) +
  geom_bar(stat = "identity") +
  ggtitle(paste("Racial Distribution of Incarcerated Population (", melt_jailmax[1, "Year"], ")", sep = "")
)



# ---------- Map ----------

# map of stats from most recent year showing inmate population,
# separating by race

#us_map <- 
#  read.csv("https://github.com/info-201a-wi22/a3-emmabacarra/raw/clone/docs/statelatlong.csv") %>%
#  rename(state = City, abbreviation = State, lat = Latitude, long = Longitude)
#View(us_map)

# facet_wrap(x ~ y), grid of plots
# x is variable to group x axis on
# y is variable to group y axis on

# group indicates which state each point belongs to

map_jail <-
  incarceration_trends %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(
    White = sum(white_jail_pop, na.rm = TRUE),
    Black = sum(black_jail_pop, na.rm = TRUE),
    Latinx = sum(latinx_jail_pop, na.rm = TRUE),
    AAPI = sum(aapi_jail_pop, na.rm = TRUE),
    `Native American` = sum(native_jail_pop, na.rm = TRUE),
    Other = sum(other_race_jail_pop, na.rm = TRUE)
  ) %>%
  melt(id = c("state")) %>%
  rename(Race = variable, Population = value)
View(map_jail)

us_states <- map_data("state") %>%
  unite(polyname, region) %>%
  left_join(state.fips, by = "polyname") %>%
  rename(name = polyname, state = abb)

state_jail_join <- 
  us_states %>%
  left_join(map_jail, by = "state")

ggplot(state_jail_join) +
  geom_polygon(
    mapping = aes(
      x = long, 
      y = lat, 
      group = group, 
      fill = `Population`
    )
  ) +
  coord_map() +
  scale_fill_continuous(high = "#fff44f", low = "#034746") +
  labs(fill = "Population") +
  facet_grid(Race ~ .)

