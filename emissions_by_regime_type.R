##################################################
#Climate Policy and Regime Type
#Tony Sutton
#November 2021
#
#Compare national greenhouse gas emissions from autocracies and democracies
#Also consider populist-led governments
##################################################


#load libraries
library(tidyverse)
library(knitr)  #for kable function to format regression results
library(stargazer)  #for displaying multiple model results
library(gganimate)
library(gifski)

#load and shape data ----

##emissions, GDP, and population data from World Bank - one row per dyad of country + data_series
wb <- read_csv('./data/greenhouse_gas_emissions_WB.csv') %>%
  rename_all(tolower) %>% janitor::clean_names() %>%
  filter(series_code %in% c('EN.ATM.GHGT.KT.CE', 'SP.POP.TOTL', 'NY.GDP.MKTP.KD'))

#data is from World Bank's World Development Indicators
#Series: Total greenhouse gas emissions (kt of CO2 equivalent);
#        Population, total
#        GDP (constant 2010 US$)
#downloaded 14 September August 2021; data last updated 30 July 2021

###reshape into one row per country + year
ghg <- wb %>% 
  select(-country_name, -series_name) %>%
  pivot_longer(cols = starts_with('x'),
               names_to = 'year',
               names_prefix = 'x',
               values_to = 'value') %>%
  pivot_wider(id_cols = c('country_code', 'year'),
              names_from = 'series_code',
              values_from = 'value')

ghg$year <- str_sub(string = ghg$year, start = 1, end = 4) %>% as.integer()
ghg <- ghg %>% rename('emit' = 'EN.ATM.GHGT.KT.CE',
                      'pop' = 'SP.POP.TOTL',
                      'gdp' = 'NY.GDP.MKTP.KD')

ghg <- ghg %>% mutate(emit = replace(emit, emit == '..', NA),
                      pop = replace(pop, pop == '..', NA),
                      gdp = replace(gdp, gdp == '..', NA))

ghg$emit <- as.numeric(ghg$emit)
ghg$gdp <- as.numeric(ghg$gdp)
ghg$pop <- as.numeric(ghg$pop)

ghg <- ghg %>% mutate(gdp_sq = (gdp^2))

ghg <- ghg %>% filter(year >= 1971, year <= 2018) #World Bank provides emissions data 1970-2018
small_countries <- ghg %>% filter(year == 2018, pop < 1000000) %>% pull(unique(country_code))
ghg <- ghg %>% filter(!country_code %in% small_countries) #remove countries smaller than 1 million people

ghg <- ghg %>% 
  mutate(era = as.factor(case_when(year < 1995 ~ as.character('naive'),
                                   year >= 2000 ~ as.character('aware'))))

###cut countries with no emissions data whatsoever
cut_list <- ghg %>% 
  group_by(country_code) %>%
  summarize(years_with_data = sum(!is.na(emit))) %>%
  filter(years_with_data == 0) %>%
  pull(country_code)
ghg <- ghg %>% filter(!country_code %in% cut_list)

skimr::skim(ghg)

###create alternate df consolidating EU countries into single entity
eu_list <- c('Austria',
               'Italy',
               'Belgium',
               'Latvia',
               'Bulgaria',
               'Lithuania',
               'Croatia',
               'Luxembourg',
               'Cyprus',
               'Malta',
               'Czech Republic',
               'Netherlands',
               'Denmark',
               'Poland',
               'Estonia',
               'Portugal',
               'Finland',
               'Romania',
               'France',
               'Slovakia',
               'Germany',
               'Slovenia',
               'Greece',
               'Spain',
               'Hungary',
               'Sweden',
               'Ireland')
eu <- ghg %>% filter(country_name %in% eu_list) %>%
  group_by(year) %>% 
  summarize(country_name = "European Union",
         country_code = 'EU',
         gdp = sum(gdp, na.rm = TRUE), 
         emit = sum(emit, na.rm = TRUE),
         pop = sum(pop, na.rm = TRUE),
         gdp_sq = gdp^2,
         era = NA,
         polyarchy = mean(polyarchy, na.rm = TRUE),
         libdem = mean(libdem, na.rm = TRUE),
         partipdem = mean(partipdem, na.rm = TRUE),
         delibdem = mean(delibdem, na.rm = TRUE),
         egaldem = mean(egaldem, na.rm = TRUE),
         ideology = NA,
         leader = NA,
         region = NA,
         tenure_aggregate = NA,
         tenure_term = NA,
         term = NA,
         president = NA,
         populism_score = mean(populism_score, na.rm = TRUE),
         in_office = NA,
         regime = NA,
         dem_stock = mean(dem_stock, na.rm = TRUE),
         decades_dem = mean(decades_dem, na.rm = TRUE),
         emit_pergdp = emit / gdp,
         gdp_percap = gdp / pop,
         dem = 'democracy',
         dem_era = NA,
         emit_lag = sum(emit_lag, na.rm = TRUE),
         polyarchy_z = NA,
         gdp_z = NA,
         gdp_sq_z = NA,
         pop_z = NA,
         dem_stock_z = NA,
         emit_z = NA,
         emit_lag_z = NA,
         populism_z = NA)
         
ghg_eu <- ghg %>% filter(!country_name %in% eu_list) %>% rbind(eu)


##regime type data from V-Dem - one row per country year ----
vdem <- read_csv('./data/V_Dem_v11.1_trimmed.csv') %>%
  select(country_name, country_text_id, year, 
         v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem)


##combine data
ghg <- ghg %>% left_join(vdem, by = c('country_code' = 'country_text_id', 'year')) %>% 
  relocate(country_name) %>%
  rename('polyarchy' = 'v2x_polyarchy',
                      'libdem' = 'v2x_libdem',
                      'partipdem' = 'v2x_partipdem',
                      'delibdem' = 'v2x_delibdem',
                      'egaldem' = 'v2x_egaldem')

skimr::skim(ghg)
 #note that missing vdem data is almost entirely countries born with collapse of USSR


##populism data from Team Populism -  one row per leader term ----
## Hawkins, Kirk A., Rosario Aguilar, Erin Jenne, Bojana Kocijan, Cristóbal Rovira Kaltwasser, Bruno Castanho Silva. 2019.
##Global Populism Database: Populism Dataset for Leaders 1.0.
##Available for download at populism.byu.edu
pop <- read_csv('./data/team_populism_prepared.csv', col_types = "ccccccdinfii") %>%
  rename_all(tolower)

pop <- pop %>% rename(populism_score = average_score)

### arbitrarily set threshold to dichotomize "populism"
pop_threshold = 0.75

###rename last year of tenure for leaders still in office
###leaders still in officer are coded as 1899; capture that info in new column
summary(pop$year_end)
pop$in_office <- if_else(pop$year_end == 1899, TRUE, FALSE)
sum(pop$in_office)

###for current leaders, set year_end to latest year of data
pop_last <- max(pop$year_end)
pop$year_end <- as.integer(if_else(pop$year_end == 1899, as.numeric(pop_last), as.numeric(pop$year_end)))
summary(pop$year_end)

###locate one row with impossible data: tenure ends before it begins
pop %>% mutate(time_travel = year_end - year_begin) %>% filter(time_travel < 0) %>% select(year_begin, year_end, iso_code, country, leader)
###manually change value to 2003, per external sources on Noboa's tenure
pop$year_end <- if_else(pop$year_begin == 2000 & pop$iso_code == 'ECU',
                        as.integer(2003),
                        pop$year_end) 
###standardize name used for Morales in Bolivia
pop %>% filter(iso_code == 'BOL' & year_begin > 2005) %>% select(leader)
pop$leader[pop$iso_code == 'BOL' & pop$leader == 'Morales'] <- 'Evo Morales'

###recode into left-right-center
pop <- pop %>% mutate(ideology = fct_recode(as.factor(left_right),
                                            center = '0',
                                            left = '-1',
                                            right = '1'))

### note length of tenure and number of terms
pop <- pop %>% mutate(tenure = year_end - year_begin)

pop <- pop %>% arrange(leader, year_begin)
pop <- pop %>% group_by(leader) %>% mutate(total_terms = n()) %>% ungroup()

pop[1, 'term_number'] <- 1
for (i in 2:nrow(pop)){
  pop[i, 'term_number'] <-
    if_else(as.character(pop[i, 'leader']) == as.character(pop[(i-1), 'leader']),  #if the prior row is the same leader...
            as.integer(pop[(i-1), 'term_number']) + 1,                             #...then count an additional term
            1)
}

pop <- pop %>% arrange(leader, year_begin)
pop[1, 'cumulative_tenure'] <- as.integer(pop[1, 'tenure'])
for (i in 2:nrow(pop)){
  pop[i, 'cumulative_tenure'] <-
    if_else(as.integer(pop[i, 'term_number']) > 1,                                         #if multiple terms...
            as.integer(pop[(i-1), 'cumulative_tenure']) + as.integer(pop[(i), 'tenure']),  #...then add combined tenure
            as.integer(pop[i, 'tenure']))
}

##add populism data to ghg dataframe----

###check country matching
unique(pop$iso_code) %in% unique(ghg$country_code)

### create empty columns for variables from pop df
ghg$region <- ghg$leader <- ghg$ideology <- as.character(NA)
ghg$president <- ghg$term <- ghg$tenure_term <- ghg$tenure_aggregate <- as.integer(NA)
ghg$populism_score <- as.numeric(NA)
ghg$in_office <- as.logical(NA)

### fill project df with matching data from pop df
for (i in 1:nrow(ghg)) {
  ghg_match <- ghg[i,]
  
  pop_match <- pop %>% 
    filter(iso_code == ghg_match$country_code &
             year_begin <= ghg_match$year &
             year_end > ghg_match$year)  #choosing to assign overlapping years to incoming leader
  
  if(nrow(pop_match) == 0) next
  
  ghg[i,]$region <- pop_match$region
  ghg[i,]$leader <- pop_match$leader
  ghg[i,]$ideology <- as.character(pop_match$ideology)
  ghg[i,]$president <- pop_match$president
  ghg[i,]$populism_score <- pop_match$populism_score
  ghg[i,]$in_office <- pop_match$in_office
  ghg[i,]$term <- as.integer(pop_match$term_number)
  ghg[i,]$tenure_term <- as.integer(ghg_match$year - pop_match$year_begin)
  ghg[i,]$tenure_aggregate <- as.integer(pop_match$cumulative_tenure - (pop_match$year_end - ghg_match$year))
}

ghg$ideology <- as.factor(ghg$ideology)

count(ghg, !is.na(populism_score)) #689 usable country-years coded for populism

ghg <- ghg %>% mutate(regime = case_when(polyarchy < 0.5 ~ 'autocracy',
                                         is.na(populism_score) ~ as.character(NA),
                                         populism_score >= pop_threshold ~ 'populist',
                                         polyarchy >= 0.5 ~ 'democracy',
                                         TRUE ~ 'coding error'))
count(ghg, regime)

##calculate democratic heritage ----
 ##note this simply sums all democracy for prior 100 years, whereas template studies apply a 1% annual depreciation
 ##also, I pin democracy around 0.5, such that a country having more years of autocracy on record does not get summed into appearing democratic
ghg$dem_stock <- as.numeric(NA)
ghg$dem_stock_years <- as.numeric(NA)
ghg$decades_dem <- as.logical(NA)
ghg$decades_dem_years <- as.numeric(NA)

for(i in seq_along(ghg$country_name)){
  working_country = ghg$country_code[i]
  working_year = ghg$year[i]
  
  working_dem_stock_df <-
  vdem %>% filter(country_text_id == working_country,
                  year < working_year,
                  year >= (working_year - 100)) %>%
    mutate(polyarchy_relative = (v2x_polyarchy - 0.5)) %>%
    summarize(dem_stock = sum(polyarchy_relative, na.rm = TRUE),
              dem_stock_years = sum(!is.na(polyarchy_relative))) 
  
  ghg$dem_stock[i] <- working_dem_stock_df %>% pull(dem_stock)
  ghg$dem_stock_years[i] <- working_dem_stock_df %>% pull(dem_stock_years)
  
  working_decades_dem_df <- 
    vdem %>% filter(country_text_id == working_country,
                    year <= working_year,
                    year >= (working_year - 19)) %>%
    mutate(dem_binary = (v2x_polyarchy >= 0.5)) %>%
    summarize(decades_dem = (sum(dem_binary == FALSE)) == 0,
              decades_dem_years = sum(!is.na(dem_binary)))
  
  ghg$decades_dem[i] <- working_decades_dem_df %>% pull(decades_dem)
  ghg$decades_dem_years[i] <- working_decades_dem_df %>% pull(decades_dem_years)
}

summary(ghg$dem_stock)
summary(ghg$dem_stock_years)
summary(ghg$decades_dem)
summary(ghg$decades_dem_years)

ghg <- ghg %>% select(-dem_stock_years, -decades_dem_years) #remove checks for completeness of calculations

ghg <- ghg %>%
  mutate(emit_pergdp = emit / gdp,
         gdp_percap = gdp / pop,
         dem = if_else(polyarchy>=0.5, 'democracy', 'autocracy')) %>%
  group_by(dem, era) %>%
  mutate(dem_era = paste(dem, era)) %>%
  ungroup()


##note next year's emissions, for lagged modeling
ghg <- ghg %>% 
  group_by(country_code) %>%
  mutate(emit_lag = lead(emit)) %>%
  ungroup()

##standardize variables by z-score
ghg <- ghg %>% 
  mutate(polyarchy_z = (polyarchy - mean(polyarchy, na.rm = TRUE)) / sd(polyarchy, na.rm = TRUE),
         gdp_z = (gdp - mean(gdp, na.rm = TRUE)) / sd(gdp, na.rm = TRUE),
         gdp_sq_z = (gdp_sq - mean(gdp_sq, na.rm = TRUE)) / sd(gdp_sq, na.rm = TRUE),
         pop_z = (pop - mean(pop, na.rm = TRUE)) / sd(pop, na.rm = TRUE),
         dem_stock_z = (dem_stock - mean(dem_stock, na.rm = TRUE)) / sd(dem_stock, na.rm = TRUE),
         emit_z = (emit - mean(emit, na.rm = TRUE)) / sd(emit, na.rm = TRUE),
         emit_lag_z = (emit_lag - mean(emit_lag, na.rm = TRUE)) / sd(emit_lag, na.rm = TRUE),
         populism_z = (populism_score - mean(populism_score, na.rm = TRUE)) / sd(populism_score, na.rm = TRUE))

## describe data ----
length(unique(pop$country))
summary(pop$year_begin)
summary(pop$year_end)

length(unique(vdem$country_text_id))
summary(vdem$year)

length(unique(wb$country_code))

ghg %>% filter(!is.na(emit)) %>%
  summarize(countries = length(unique(country_code)),
            min(year),
            max(year),
            observations = n())

ghg %>% filter(!is.na(emit_lag), !is.na(polyarchy), !is.na(gdp), !is.na(pop)) %>%
  summarize(countries = length(unique(country_code)),
            min(year),
            max(year),
            observations = n())

ghg %>% filter(!is.na(emit_lag), !is.na(polyarchy), !is.na(gdp), !is.na(pop), year >= 2000) %>%
  summarize(countries = length(unique(country_code)),
            min(year),
            max(year),
            observations = n())

ghg %>% filter(!is.na(emit_lag), !is.na(populism_score), !is.na(polyarchy), !is.na(gdp), !is.na(pop), year >= 2000) %>%
  summarize(countries = length(unique(country_code)),
            min(year),
            max(year),
            observations = n())

ghg %>% filter(!is.na(emit_lag), !is.na(polyarchy), !is.na(gdp), !is.na(pop), !is.na(populism_score)) %>%
  summarize(countries = length(unique(country_code)),
            min(year),
            max(year),
            observations = n())

ghg %>% filter(!is.na(populism_score)) %>% group_by(populism_score >= pop_threshold) %>% summarize(count = n(), unique = length(unique(country_code)))

ggplot(data = ghg, aes(x = polyarchy))+
  geom_histogram(fill = 'dodgerblue', color = 'white')+
  theme_minimal()+
  labs(title = 'Degree of Democracy',
       y = '',
       x = 'V-Dem Polyarchy Score')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/democracy_histogram.jpg",
       width = 10,
       height = 6,
       units = 'in')

ggplot(data = ghg, aes(x = populism_score))+
  geom_histogram(bins = 20, fill = 'darkorange', color = 'white')+
  theme_minimal()+
  labs(title = 'Degree of Populism',
       y = '',
       x = 'Global Populism Database Score')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/populism_histogram.jpg",
       width = 10,
       height = 6,
       units = 'in')

ggplot(data = ghg, aes(x = year, y = (emit/1000000)))+
  geom_point(color = 'forestgreen', alpha = 0.3, position = 'jitter')+
  theme_minimal()+
  coord_cartesian(xlim= c(1970,2021))+
  labs(title = 'Greenhouse Gas Emissions Over Time',
       subtitle = '  in billion metric tons of CO2 equivalent',
       y = 'Emissions',
       x = '')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16))
ggsave(filename = "./visuals/emissions_over_time.jpg",
       width = 10,
       height = 6,
       units = 'in')

ggplot(data = ghg, aes(x = year, y = log(emit)))+
  geom_point(color = 'forestgreen', alpha = 0.3, position = 'jitter')+
  theme_minimal()+
  coord_cartesian(xlim= c(1970,2021))+
  labs(title = 'Greenhouse Gas Emissions Over Time',
       subtitle = '  log-transformed',
       y = 'Emissions, logged',
       x = '')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank())
ggsave(filename = "./visuals/emissions_over_time_logged.jpg",
       width = 10,
       height = 6,
       units = 'in')


#analyze data ----

##linear models ----

#nomenclature:
#m1 is simplest model, with controls for gdp, population, and year
#m2 adds gdp squared to account for possible kuznets curve (acknowledging that cross section is not time series)
#m3 omits China
#m4 omits US
#m5-8 repeat 1-4 but limit time scale to 2000 and after
#m9 repeats m1, but logs gdp and emissions
#m10 repeats m9 with logged variables, but limits time to 2000 and after
#variants of m (reusing number system) include
 #'decades' changing measure of democracy to binary measure of democratic for past 20 years
 #'dem_stock' changing measure of democracy to 100-year stock
 #'dem_stock_flow' including both 100-year stock and current polyarchy to tease out effects
 #several alternate elements of democracy using V-Dem indexes

m0 <- lm(emit_lag ~ gdp + pop + as.factor(year),
         data = ghg)
 summary(m0) #with only control variables, already adjusted R-squared of 0.8262

m1 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
         data = ghg)
summary(m1)

m2 <- lm(emit_lag ~ polyarchy + gdp + gdp_sq + pop + as.factor(year),
         data = ghg)
summary(m2) #apparent evidence of kuznets curve for emissions

m3 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
         data = (ghg %>% filter(country_code != 'CHN')))
summary(m3)

m4 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
         data = (ghg %>% filter(country_code != 'USA')))
summary(m4)

m5 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
         data = (ghg %>% filter(year>= 2000)))
 summary(m5)

m5_inter <- lm(emit_lag ~ (polyarchy * gdp) + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(m5_inter)
 
m6 <- lm(emit_lag ~ polyarchy + gdp + gdp_sq + pop + as.factor(year),
         data = (ghg %>% filter(year>= 2000)))
 summary(m6) #not evidence of kuznets curve for emissions
 
m6_inter <- lm(emit_lag ~ (polyarchy * gdp) + (polyarchy * gdp_sq) + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(m6_inter) 
 
m7 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000& country_code != 'CHN')))
 summary(m7)

m8 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000 & country_code != 'USA')))
 summary(m8)
 
m9 <- lm(log(emit_lag) ~ polyarchy + log(gdp) + pop + as.factor(year),
          data = ghg)
 summary(m9)
 
m10 <- lm(log(emit_lag) ~ polyarchy + log(gdp) + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(m10)

decades_m1 <- lm(emit_lag ~ decades_dem + gdp + pop + as.factor(year),
                  data = ghg)
 summary(decades_m1)

decades_m3 <- lm(emit_lag ~ decades_dem + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(country_code != 'CHN')))
 summary(decades_m3)

decades_m4 <- lm(emit_lag ~ decades_dem + gdp + pop + as.factor(year),
                 data = (ghg %>% filter(country_code != 'USA')))
 summary(decades_m4)
 
decades_m5 <- lm(emit_lag ~ decades_dem + gdp + pop + as.factor(year),
                 data = (ghg %>% filter(year>= 2000)))
 summary(decades_m5)

decades_m7 <- lm(emit_lag ~ decades_dem + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(year>= 2000 & country_code != 'CHN')))
 summary(decades_m7)
 
decades_m8 <- lm(emit_lag ~ decades_dem + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(year>= 2000 & country_code != 'USA')))
 summary(decades_m8)

dem_stock_m1 <- lm(emit_lag ~ dem_stock + gdp + pop + as.factor(year),
                  data = ghg)
 summary(dem_stock_m1)
 
dem_stock_m3 <- lm(emit_lag ~ dem_stock + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(country_code != 'CHN')))
 summary(dem_stock_m3)
 
dem_stock_m4 <- lm(emit_lag ~ dem_stock + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(country_code != 'USA')))
 summary(dem_stock_m4)
 
dem_stock_m5 <- lm(emit_lag ~ dem_stock + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(year>= 2000)))
 summary(dem_stock_m5)
 
dem_stock_m7 <- lm(emit_lag ~ dem_stock + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(year>= 2000 & country_code != 'CHN')))
 summary(dem_stock_m7)
 
dem_stock_m8 <- lm(emit_lag ~ dem_stock + gdp + pop + as.factor(year),
                  data = (ghg %>% filter(year>= 2000 & country_code != 'USA')))
 summary(dem_stock_m8)
 
dem_stock_flow1 <- lm(emit_lag ~ polyarchy + dem_stock + gdp + pop + as.factor(year),
          data = ghg)
 summary(dem_stock_flow1)
 
dem_stock_flow2 <- lm(emit_lag ~ polyarchy + dem_stock + gdp + gdp_sq + pop + as.factor(year),
          data = ghg)
 summary(dem_stock_flow2) #apparent evidence of kuznets curve for emissions
 
dem_stock_flow5 <- lm(emit_lag ~ polyarchy + dem_stock + gdp + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(dem_stock_flow5)

dem_stock_flow6 <- lm(emit_lag ~ polyarchy + dem_stock + gdp + gdp_sq + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(dem_stock_flow6) #not evidence of kuznets curve for emissions
 
dem_stock_flow10 <- lm(log(emit_lag) ~ polyarchy + dem_stock + log(gdp) + pop + as.factor(year),
           data = (ghg %>% filter(year>= 2000)))
 summary(dem_stock_flow10)
 
 
libdem5 <- lm(emit_lag ~ libdem + gdp + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(libdem5)

partipdem5 <- lm(emit_lag ~ libdem + gdp + pop + as.factor(year),
               data = (ghg %>% filter(year>= 2000)))
 summary(partipdem5)
 
delibdem5 <- lm(emit_lag ~ libdem + gdp + pop + as.factor(year),
               data = (ghg %>% filter(year>= 2000)))
 summary(delibdem5)
 
egaldem5 <- lm(emit_lag ~ libdem + gdp + pop + as.factor(year),
               data = (ghg %>% filter(year>= 2000)))
 summary(egaldem5)

richdem1 <- lm(emit_lag ~ (polyarchy * log(gdp_percap)) + pop + as.factor(year),
                           data = ghg)
 summary(richdem1)
 
richdem5 <- lm(emit_lag ~ (polyarchy * log(gdp_percap)) + pop + as.factor(year),
               data = (ghg %>% filter(year>= 2000)))
 summary(richdem5)

popm1 <- lm(emit_lag ~ polyarchy + populism_score + gdp + pop + as.factor(year),
          data = ghg)
 summary(popm1) #populism correlates with lower emissions...
 
popm2 <- lm(emit_lag ~ polyarchy + populism_score + gdp + gdp_sq + pop + as.factor(year),
          data = ghg)
 summary(popm2) 
 
popm4 <- lm(emit_lag ~ polyarchy + populism_score + gdp + pop + as.factor(year),
          data = (ghg %>% filter(country_code != 'USA')))
 summary(popm4)
 
popm5 <- lm(emit_lag ~ polyarchy + populism_score + gdp + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(popm5)
 
popm6 <- lm(emit_lag ~ polyarchy + populism_score + gdp + gdp_sq + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(popm6)
 
popm8 <- lm(emit_lag ~ polyarchy + populism_score + gdp + pop + as.factor(year),
          data = (ghg %>% filter(year>= 2000 & country_code != 'USA')))
 summary(popm8)
 
popm9 <- lm(log(emit_lag) ~ polyarchy + populism_score + log(gdp) + pop + as.factor(year),
          data = ghg)
 summary(popm9) #...except when using logged variables, then no populism effect
 
popm10 <- lm(log(emit_lag) ~ polyarchy + populism_score + log(gdp) + pop + as.factor(year),
           data = (ghg %>% filter(year>= 2000)))
 summary(popm10) #including polyarchy directly probably mediates between damage from populism and effect on emissions 
 
pop_discrete_m <- lm(emit_lag ~ regime + gdp + gdp_sq + pop + as.factor(year),
              data = (ghg %>% filter(year>= 2000)))
 summary(pop_discrete_m) #when treated categorically, populism between democracy and autocracy
 
pop_discrete_pc_inter_m <- lm(emit_lag ~ (regime * gdp) + (regime * gdp_sq) + pop + as.factor(year),
                            data = (ghg %>% filter(year>= 2000)))
 summary(pop_discrete_pc_inter_m)  
  
pop_discrete_log_m <- lm(log(emit_lag) ~ regime + log(gdp) + pop + as.factor(year),
                      data = (ghg %>% filter(year>= 2000)))
 summary(pop_discrete_log_m) #same ordering when using logged variables
 
pop_demonly5 <- lm(emit_lag ~ populism_score + gdp + pop + as.factor(year),
             data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5 & !is.na(populism_score))))
 summary(pop_demonly5) #little point in running versions 1 and 2 for earlier years because missing populism data

pop_demonly6 <- lm(emit_lag ~ populism_score + gdp + gdp_sq + pop + as.factor(year),
                    data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5 & !is.na(populism_score))))
 summary(pop_demonly6)
 
pop_tenure <- lm(emit_lag ~ tenure_aggregate + gdp + gdp_sq + pop + as.factor(year),
                    data = (ghg %>% filter(populism_score >= pop_threshold)))
 summary(pop_tenure) #no discernible effect of tenure among populists
 
eu1 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
          data = ghg_eu)
 summary(eu1)
 
eu2 <- lm(emit_lag ~ polyarchy + gdp + gdp_sq + pop + as.factor(year),
          data = ghg_eu)
 summary(eu2) 
 
eu5 <- lm(emit_lag ~ polyarchy + gdp + pop + as.factor(year),
          data = (ghg_eu %>% filter(year>= 2000)))
 summary(eu5)
 
eu5_inter <- lm(emit_lag ~ (polyarchy * gdp) + pop + as.factor(year),
                data = (ghg_eu %>% filter(year>= 2000)))
 summary(eu5_inter)
 
eu6 <- lm(emit_lag ~ polyarchy + gdp + gdp_sq + pop + as.factor(year),
          data = (ghg_eu %>% filter(year>= 2000)))
 summary(eu6)
 
eu6_inter <- lm(emit_lag ~ (polyarchy * gdp) + (polyarchy * gdp_sq) + pop + as.factor(year),
                data = (ghg_eu %>% filter(year>= 2000)))
 summary(eu6_inter) 
 
eu9 <- lm(log(emit_lag) ~ polyarchy + log(gdp) + pop + as.factor(year),
          data = ghg_eu)
 summary(eu9)

eu10 <- lm(log(emit_lag) ~ polyarchy + log(gdp) + pop + as.factor(year),
           data = (ghg_eu %>% filter(year>= 2000, emit_lag != 0, !is.na(emit_lag), !is.nan(emit_lag))))
 summary(eu10)
 
 
###repeat most useful models with z standardized scores ----
m1z <- lm(emit_lag_z ~ polyarchy_z + gdp_z + pop_z + as.factor(year),
          data = ghg)
 summary(m1z)
 
m2z <- lm(emit_lag_z ~ polyarchy_z + gdp_z + gdp_sq_z + pop_z + as.factor(year),
          data = ghg)
 summary(m2z)
 
m5z <- lm(emit_lag_z ~ polyarchy_z + gdp_z + pop_z + as.factor(year),
           data = (ghg %>% filter(year>= 2000)))
 summary(m5z)
 
m6z <- lm(emit_lag_z ~ polyarchy_z + gdp_z + gdp_sq_z + pop_z + as.factor(year),
          data = (ghg %>% filter(year>= 2000)))
 summary(m6z)
 
m5_inter_z <- lm(emit_lag_z ~ (polyarchy_z * gdp_z) + gdp_sq_z + pop_z + as.factor(year),
                  data = (ghg %>% filter(year>= 2000)))
 summary(m5_inter_z)  
 
m6_inter_z <- lm(emit_lag_z ~ (polyarchy_z * gdp_z) + (polyarchy_z * gdp_sq_z) + pop_z + as.factor(year),
                data = (ghg %>% filter(year>= 2000)))
 summary(m6_inter_z) 
 
dem_stock_flow5z <- lm(emit_lag_z ~ polyarchy_z + dem_stock_z + gdp_z + pop_z + as.factor(year),
                       data = (ghg %>% filter(year>= 2000)))
 summary(dem_stock_flow5z)
 
dem_stock_flow6z <- lm(emit_lag_z ~ polyarchy_z + dem_stock_z + gdp_z + gdp_sq_z + pop_z + as.factor(year),
                        data = (ghg %>% filter(year>= 2000)))
 summary(dem_stock_flow5z)
 
pop5_z <- lm(emit_lag_z ~ populism_z + gdp_z + pop_z + as.factor(year),
                        data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5 & !is.na(populism_z))))
 summary(pop5_z) #among democracies, one sd increase in populism predicts 0.04 sd increase in emissions
 
pop6_z <- lm(emit_lag_z ~ populism_z + gdp_z + gdp_sq_z + pop_z + as.factor(year),
             data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5 & !is.na(populism_z))))
 summary(pop6_z)
 
pop_control_z <- lm(emit_lag_z ~ populism_z + polyarchy_z + gdp_z + gdp_sq_z + pop_z + as.factor(year),
              data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5 & !is.na(populism_z))))
 summary(pop_control_z)

pop_discrete_z <- lm(emit_lag_z ~ regime + gdp_z + pop_z + as.factor(year),
                      data = (ghg %>% filter(year>= 2000)))
 summary(pop_discrete_z) #when treated categorically, populism between democracy and autocracy
 
##display model results in output files
stargazer(m1z, m2z, m6z, dem_stock_flow6z, m6_inter_z, 
           title = 'Democracy and Climate Emissions',
           keep = c('polyarchy_z', 'dem_stock_z', 'gdp_z', 'gdp_sq_z', 'pop_z'),
           dep.var.labels = 'Greenhouse Gas Emissions, lagged one year',
           model.names = TRUE,
           covariate.labels = c('Democracy, current', 'Democracy, stock', 'GDP', 'GDP squared', 'Population', 'Democracy X GDP', 'Democracy X GDP squared'),
           nobs = TRUE,
           type = 'html',
           out = './models/democracy_emissions_model_results.doc')

stargazer(pop5_z, pop6_z, pop_control_z,
          title = 'Populism and Climate Emissions Among Democracies Only',
          keep = c('populism_z', 'polyarchy', 'gdp_z', 'gdp_sq_z', 'pop_z'),
          dep.var.labels = 'Greenhouse Gas Emissions, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'Democracy', 'GDP', 'GDP squared', 'Population'),
          nobs = TRUE,
          type = 'html',
          out = './models/populism_degree_emissions_model_results.doc')

stargazer(pop_discrete_z,
          title = 'Regime Type and Climate Emissions',
          keep = c('regime', 'gdp_z', 'pop_z'),
          dep.var.labels = 'Greenhouse Gas Emissions, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Non-populist Democracy', 'Populist Democracy', 'GDP', 'Population'),
          nobs = TRUE,
          type = 'html',
          out = './models/populism_discrete_emissions_model_results.doc')
 
## case illustrations ----
china_alt <- ghg %>% filter(country_name == 'China' & year == 2017)
china_alt <- china_alt %>% rbind(china_alt[1,])
china_alt[2,'country_name'] <- 'China_as_US'
china_alt[2,'polyarchy'] <- 
  ghg %>% filter(country_code == 'USA' & year == 2017) %>% pull(polyarchy)
china_alt <- china_alt %>%
  mutate(expect_emit = predict(object = m1, newdata = china_alt))
100 * (china_alt[2,'expect_emit'] - china_alt[1,'expect_emit']) / china_alt[1,'expect_emit'] 
 #m1 predicts a 2.0 percent decrease in emissions if China were as democratic as the US

china_alt <- china_alt %>%
  mutate(expect_emit = predict(object = m2, newdata = china_alt))
100 * (china_alt[2,'expect_emit'] - china_alt[1,'expect_emit']) / china_alt[1,'expect_emit'] 
#m2 predicts a 2.4 percent decrease in emissions if China were as democratic as the US

china_alt <- china_alt %>%
  mutate(expect_emit = predict(object = m6, newdata = china_alt))
100 * (china_alt[2,'expect_emit'] - china_alt[1,'expect_emit']) / china_alt[1,'expect_emit'] 
#m6 predicts a 3.1 percent decrease in emissions if China were as democratic as the US

china_alt <- china_alt %>%
  mutate(expect_emit = predict(object = dem_stock_flow6, newdata = china_alt))
100 * (china_alt[2,'expect_emit'] - china_alt[1,'expect_emit']) / china_alt[1,'expect_emit'] 
#sf6 predicts a 2.4 percent decrease in emissions if China were as democratic as the US

china_alt <- china_alt %>%
  mutate(expect_emit = predict(object = m6_inter, newdata = china_alt))
100 * (china_alt[2,'expect_emit'] - china_alt[1,'expect_emit']) / china_alt[1,'expect_emit'] 
 #m6_inter predicts a 58 percent decrease in emissions if China were as democratic as the US

india_alt <- ghg %>% filter(country_code == 'IND' & year == 2017)
india_alt <- india_alt %>% rbind(india_alt[1,])
india_alt[2,'country_name'] <- 'India_as_China'
india_alt[2,'polyarchy'] <- 
  ghg %>% filter(country_code == 'CHN' & year == 2017) %>% pull(polyarchy)
india_alt <- india_alt %>%
  mutate(expect_emit = predict(object = m1, newdata = india_alt))
100 * (india_alt[2,'expect_emit'] - india_alt[1,'expect_emit']) / india_alt[1,'expect_emit'] 
 #m1 predicts a 2.1 percent increase in emissions if India were as autocratic as China

india_alt <- india_alt %>%
  mutate(expect_emit = predict(object = m2, newdata = india_alt))
100 * (india_alt[2,'expect_emit'] - india_alt[1,'expect_emit']) / india_alt[1,'expect_emit'] 
#m2 predicts a 2.5 percent increase in emissions if India were as autocratic as China

india_alt <- india_alt %>%
  mutate(expect_emit = predict(object = m6, newdata = india_alt))
100 * (india_alt[2,'expect_emit'] - india_alt[1,'expect_emit']) / india_alt[1,'expect_emit'] 
#m6 predicts a 2.9 percent increase in emissions if India were as autocratic as China

india_alt <- india_alt %>%
  mutate(expect_emit = predict(object = dem_stock_flow6, newdata = india_alt))
100 * (india_alt[2,'expect_emit'] - india_alt[1,'expect_emit']) / india_alt[1,'expect_emit'] 
#sf6 predicts a 2.3 percent increase in emissions if India were as autocratic as China

india_alt <- india_alt %>%
  mutate(expect_emit = predict(object = m6_inter, newdata = india_alt))
100 * (india_alt[2,'expect_emit'] - india_alt[1,'expect_emit']) / india_alt[1,'expect_emit'] 
 #m6_inter predicts a 53 percent increase in emissions if India were as autocratic as China

### find kuznets turning points
 ### stripped-down model for all democracies in or after 2000  (inelegantly hard-coded)
summary(lm((emit_lag/pop) ~ gdp_percap + I(gdp_percap^2),
         data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5))))

0.0000004343/(2*0.000000000004351) #first derivative of the equation for a line implied by model = inflection point in GDP per capita
#0.002739 + (0.0000004343 * x) - (0.000000000004351 * (x^2)) #equation for the line

turning_point_model <- data.frame(x = seq(from = 0, to = 100000, by = 100))
turning_point_model$y <- 0.002739 + (0.0000004343 * turning_point_model$x) - (0.000000000004351 * (turning_point_model$x^2))
ggplot(data = turning_point_model, aes(x=x, y=y))+
  geom_point() #visual check of the equation for the line

tpm2 <- lm((emit_lag/pop) ~ polyarchy + gdp_percap + I(gdp_percap^2) + as.factor(year),
           data = (ghg %>% filter(year>= 2000))) #mimicking m6, but converted to per capita
summary(tpm2)
(summary(tpm2)$coefficients['gdp_percap','Estimate']) / (-2 * summary(tpm2)$coefficients['I(gdp_percap^2)','Estimate'])  
 #the first derivative (of the regression model's equation for the line) will equal 0 at the value of x equal to
 # the coefficient for gdp per capita divided by negative 2 times the coefficient for gdp per capita squared

tpm3 <- lm((emit_lag/pop) ~ polyarchy + dem_stock + gdp_percap + I(gdp_percap^2) + as.factor(year),
                      data = (ghg %>% filter(year>= 2000))) #mimicking dem_stock_flow6 but converted to per capita
summary(tpm3)
(summary(tpm3)$coefficients['gdp_percap','Estimate']) / (-2 * summary(tpm3)$coefficients['I(gdp_percap^2)','Estimate'])  

tpm4 <- lm((emit_lag/pop) ~ polyarchy + gdp_percap + I(gdp_percap^2) + as.factor(year),
           data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5))) #repeats tpm2 but limited to democracies
summary(tpm4)
(summary(tpm4)$coefficients['gdp_percap','Estimate']) / (-2 * summary(tpm4)$coefficients['I(gdp_percap^2)','Estimate'])  
#the first derivative (of the regression model's equation for the line) will equal 0 at the value of x equal to
# the coefficient for gdp per capita divided by negative 2 times the coefficient for gdp per capita squared

tpm5 <- lm((emit_lag/pop) ~ polyarchy + dem_stock + gdp_percap + I(gdp_percap^2) + as.factor(year),
           data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5))) #repeat tpm3 but limit to democracies
summary(tpm5)
(summary(tpm5)$coefficients['gdp_percap','Estimate']) / (-2 * summary(tpm5)$coefficients['I(gdp_percap^2)','Estimate'])  

tpm6 <- lm((emit_lag/pop) ~ (polyarchy * gdp_percap) + I(gdp_percap^2) + as.factor(year),
           data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5))) #mimic m5_inter
summary(tpm6)
(summary(tpm6)$coefficients['gdp_percap','Estimate']) / (-2 * summary(tpm6)$coefficients['I(gdp_percap^2)','Estimate'])

tpm7 <- lm((emit_lag/pop) ~ (polyarchy * gdp_percap) + (polyarchy * I(gdp_percap^2)) + as.factor(year),
           data = (ghg %>% filter(year>= 2000))) #mimic m6_inter
summary(tpm7)
1/ 
((-2 * summary(tpm7)$coefficients['I(gdp_percap^2)','Estimate']) / (summary(tpm7)$coefficients['gdp_percap','Estimate']) +
(-2 * summary(tpm7)$coefficients['polyarchy:I(gdp_percap^2)','Estimate']) / (summary(tpm7)$coefficients['gdp_percap','Estimate']))

tpm8 <- lm((emit_lag/pop) ~ (polyarchy * gdp_percap) + (polyarchy * I(gdp_percap^2)) + as.factor(year),
           data = (ghg %>% filter(year>= 2000 & polyarchy >= 0.5))) #repeat for only democracies
summary(tpm8)
1/ 
 ((-2 * summary(tpm8)$coefficients['I(gdp_percap^2)','Estimate']) / (summary(tpm8)$coefficients['gdp_percap','Estimate']) +
 (-2 * summary(tpm8)$coefficients['polyarchy:I(gdp_percap^2)','Estimate']) / (summary(tpm8)$coefficients['gdp_percap','Estimate']))

tpm9 <- lm((emit_lag/pop) ~ polyarchy + gdp_percap + I(gdp_percap^2) + as.factor(year),
           data = (ghg)) #mimicking m2, but converted to per capita
summary(tpm9)
(summary(tpm9)$coefficients['gdp_percap','Estimate']) / (-2 * summary(tpm9)$coefficients['I(gdp_percap^2)','Estimate'])  

tpm10 <- lm((emit_lag/pop) ~ polyarchy + gdp_percap + I(gdp_percap^2) + as.factor(year),
           data = (ghg %>% filter(polyarchy >= 0.5))) #repeat for only democracies
summary(tpm10)
(summary(tpm10)$coefficients['gdp_percap','Estimate']) / (-2 * summary(tpm10)$coefficients['I(gdp_percap^2)','Estimate'])  


 
## natural experiment of democratization ----

#first identify episodes of democratization, two definitions
 ## years with polyarchy over 0.5 and immediately prior year under (democratizers)
 ## gains in polyarchy of a fixed amount. one sd? (liberalizers)
#filter to keep
 ## only those where democratization was sustained
 ## only those where prior autocracy lasted at least 10(?) years
# label candidate countries as eventual democratizers or persistent autocracies
# label years relative to democratization year
#dif in dif with 4 linear models splitting by category and by year zero 




#visualize data ----
ad_colors <- c('autocracy' = 'firebrick', 'democracy' = 'dodgerblue')
adp_colors <- c('autocracy' = 'firebrick', 'democracy' = 'dodgerblue', 'populist' = 'darkorange')

vm <- m5 #specify which model to illustrate
polyarchy_scale = seq(from = 0, to = 1, by = 0.05)
year_scale = seq(from = min(ghg$year), to = (max(ghg$year) - 1), by = 1)

mimir <- data.frame(polyarchy = rep(polyarchy_scale, times = length(year_scale)),
                    year = rep(year_scale, each = length(polyarchy_scale)),
                    gdp = median(ghg$gdp, na.rm = TRUE),
                    pop = median(ghg$pop, na.rm = TRUE))
mimir <- mimir %>% mutate(era = as.factor(case_when(year < 1995 ~ as.character('naive'),
                                                    year >= 2000 ~ as.character('aware'))))
mimir <- mimir %>% filter(year >=2000 & year <= 2017) #needed to run m5. not sure how to automate

mimir <- mimir %>% mutate(expect_emit = predict(object = vm, newdata = mimir))


mimir %>% filter(year == 2017) %>%
  ggplot(aes(x = polyarchy, y = expect_emit))+
  geom_line()

#graphing by year is not illuminating because year in linear model simply changes the intercept
p <- mimir %>% 
  ggplot(aes(x = polyarchy, y = expect_emit))+
  geom_line()+
  ggtitle('Democracies Emit Less',
          subtitle = ' Expected emissions in {frame_time}')+
  theme(title = element_text(size = 22),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))+
  transition_time(year)+
  ease_aes('cubic-in-out')
animate(p, fps = 5, end_pause = 30)  

                    
#more democracy predicts fewer emissions per gdp
ghg %>% mutate(emit_pergdp = emit_lag / (gdp)) %>%
  ggplot(aes(x = polyarchy, y = log(emit_pergdp)))+
  geom_point()+
  geom_smooth()

#more democracy predicts U shaped relationship with emissions per capita, probably confounded by wealth
ghg %>% mutate(emit_percap = emit_lag / (pop)) %>%
  ggplot(aes(x = polyarchy, y = log(emit_percap)))+
  geom_point()+
  geom_smooth()

#more democracy predicts fewer emissions per gdp per capita,
 ##all the action happens from middling to strong democracy
median_emit_rel_log <- ghg %>% 
  mutate(emit_rel = emit_lag / (gdp * pop)) %>%
  summarize(median_emit_rel_log = median(log(emit_rel), na.rm = TRUE)) %>%
  pull(median_emit_rel_log)
ghg %>% mutate(emit_rel = emit_lag / (gdp * pop)) %>% 
  ggplot(aes(x = polyarchy, y = log(emit_rel)))+
  geom_point(alpha = 0.2)+
  geom_smooth(size = 3, color = 'forestgreen', fill = 'forestgreen')+
  geom_segment(aes(x=0, xend=1, y=median_emit_rel_log, yend=median_emit_rel_log), linetype = 'dashed')+
  theme_minimal()+
  labs(title = 'More Democracy, Fewer Emissions',
       subtitle = '  Greenhouse Gase Emissions, 1971-2017',
       y = 'Emissions per GDP and Population',
       x = 'Degree of Democracy')+
  theme(title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank()) 
ggsave(filename = "./visuals/emissions_per_democracy.jpg",
       width = 10,
       height = 6,
       units = 'in')

#more democracy predicts fewer emissions per gdp per capita,
 ##relationship existed before awareness of climate change; perhaps explanation is economy's sectoral composition?
 ##although still fewer emissions from middling democracies after awareness, so maybe evidence remains for hypothesis
ghg %>% filter(!is.na(era)) %>%
  mutate(emit_rel = emit_lag / (gdp * pop)) %>%
  ggplot(aes(x = polyarchy, y = log(emit_rel), color = era))+
  geom_smooth()

#democracy breaks relationship between wealth and emissions
ghg %>% filter(year >= 2000, !is.na(polyarchy)) %>%
  mutate(regime = if_else(polyarchy >= 0.5, 'democracy', 'autocracy'),
         emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = gdp_percap, y = (1000 * emit_percap), color = regime, fill = regime))+
  scale_color_manual(values = ad_colors)+
  scale_fill_manual(values = ad_colors)+
  coord_cartesian(ylim = c(0,44))+
  geom_smooth(size = 3, alpha = 0.15)+
  annotate('text', label = 'Autocracy', color = 'firebrick', size = 7, x = 37500, y = 35)+
  annotate('text', label = 'Democracy', color = 'dodgerblue', size = 7, x = 62500, y = 5)+
  geom_rug(sides = 'b', alpha = 0.3)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  labs(title = 'Democracy Escapes Development Dilemma',
       subtitle = '  Greenhouse Gas Emissions, 2000-2017',
       y = 'Emissions per capita (metric tons)',
       x = 'GDP per capita (USD)')+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 
ggsave(filename = "./visuals/development_and_emissions_by_regime.jpg",
       width = 10,
       height = 6,
       units = 'in')

#repeat and add individual country-years
ghg %>% filter(year >= 2000, !is.na(polyarchy)) %>%
  mutate(regime = if_else(polyarchy >= 0.5, 'democracy', 'autocracy'),
         emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = gdp_percap, y = (1000 * emit_percap), color = regime, fill = regime))+
  scale_color_manual(values = ad_colors)+
  scale_fill_manual(values = ad_colors)+
  coord_cartesian(ylim = c(0,44))+
  geom_point(alpha = 0.2)+
  geom_smooth(size = 3, alpha = 0.12)+
  annotate('text', label = 'Autocracy', color = 'firebrick', size = 7, x = 37500, y = 35)+
  annotate('text', label = 'Democracy', color = 'dodgerblue', size = 7, x = 62500, y = 5)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  labs(title = 'Democracy Escapes Development Dilemma',
       subtitle = '  Greenhouse Gas Emissions, 2000-2017',
       y = 'Emissions per capita (metric tons)',
       x = 'GDP per capita (USD)')+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 
ggsave(filename = "./visuals/development_and_emissions_by_regime_dotted.jpg",
       width = 10,
       height = 6,
       units = 'in')


#more democracy predicts fewer emissions per gdp per capita,
 ##relationship changed over time: first high emissions from strong autocracies,
 ##                                second, low emissions from strong democracies
p <- ghg %>%
  filter(year <= 2017) %>% #because 2018 lacks future emissions data from 2019
  mutate(emit_rel = emit_lag / (gdp * pop)) %>%
  ggplot(aes(x = polyarchy, y = log(emit_rel)))+
  geom_point()+
  geom_smooth(size = 3, color = 'forestgreen', fill = 'forestgreen')+
  geom_hline(yintercept = median_emit_rel_log, linetype = 'dashed')+
  theme_minimal()+
  ggtitle('Democracies Emit Less',
          subtitle = ' Relative emissions in {frame_time}')+
  xlab('Degree of Democracy')+
  ylab('Emissions per Population and GDP, logged')+
  scale_x_continuous(breaks = c(0,.25,.5,.75,1),
                     limits = c(0, 1.01))+
  theme(title = element_text(size = 22),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 16),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())+
  transition_time(as.integer(year))+
  ease_aes('cubic-in-out')
animate(p, fps = 4, end_pause = 20, height = 600, width = 1000) 
anim_save('./visuals/anim_democracies_emit_less.gif')

#wealthier countries emit less per unit of gdp
ghg %>%
  mutate(emit_pergdp = emit_lag / gdp,
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = log(gdp_percap), y = log(emit_pergdp)))+
  geom_smooth()

#that relationship is more pronounced among democracies
ghg %>% filter(!is.na(polyarchy)) %>%
  mutate(emit_pergdp = emit_lag / gdp,
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = log(gdp_percap), y = log(emit_pergdp), color = (polyarchy >= 0.5)))+
  geom_smooth(size = 1.5)+
  geom_point(alpha = 0.1)

#wealth in democracies led to lower emissions per GDP in both eras
ghg %>% filter(!is.na(polyarchy), !is.na(era)) %>%
  mutate(emit_pergdp = emit_lag / gdp,
         gdp_percap = gdp / pop,
         dem = if_else(polyarchy>=0.5, 'democracy', 'autocracy')) %>%
  group_by(dem, era) %>%
  mutate(dem_era = paste(dem, era)) %>%
  ungroup() %>%
  ggplot(aes(x = log(gdp_percap), y = log(emit_pergdp), color = dem_era))+
  geom_smooth()

#democracy weakens but still produces the relationship between wealth and absolute emissionss
polyarchy_scale = seq(from = 0, to = 1, by = 0.05)
gdp_scale = seq(from = min(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE),
                to = max(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE),
                by = ((max(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE)) - 
                        (min(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE))) /
                  99)

mimir <- data.frame(polyarchy = rep(polyarchy_scale, times = length(gdp_scale)),
                    gdp = rep(gdp_scale, each = length(polyarchy_scale)),
                    year = 2017,
                    pop = median(ghg$pop, na.rm = TRUE))
mimir <- mimir %>% mutate(gdp_sq = gdp * gdp)
mimir <- mimir %>% mutate(expect_emit = predict(object = m6_inter, newdata = mimir))
mimir %>% mutate(regime = case_when(polyarchy == 0.8 ~ 'democracy',
                                    polyarchy == 0.2 ~ 'autocracy',
                                    TRUE ~ as.character(NA))) %>%
  filter(!is.na(regime))%>%
  ggplot(aes(x = gdp/pop, y = expect_emit, color = regime))+
  geom_point()


#populism as middle state between democracy and autocracy
ghg %>% filter(!is.na(regime), year >= 2000) %>%
  mutate(emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = gdp_percap, y = (1000 * emit_percap), color = regime, fill = regime))+
  scale_color_manual(values = adp_colors)+
  scale_fill_manual(values = adp_colors)+
  geom_smooth(size = 3, alpha = 0.2)+
  geom_rug(sides = 'b', alpha = 0.3)+
  coord_cartesian(ylim = c(0, 44))+
  scale_x_continuous(labels = scales::comma)+
  annotate('text', label = 'Autocracy', color = 'firebrick', size = 7, x = 37500, y = 35)+
  annotate('text', label = 'Populism', color = 'darkorange', size = 7, x = 62500, y = 25)+
  annotate('text', label = 'Democracy', color = 'dodgerblue', size = 7, x = 62500, y = 5)+
  theme_minimal()+
  labs(title = 'Populism: Like Slightly Autocratic Democracy',
       subtitle = '  Greenhouse Gas Emissions, 2000-2017',
       y = 'Emissions per capita (metric tons)',
       x = 'GDP per capita (US$)')+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 
ggsave(filename = "./visuals/populism_emissions.jpg",
       width = 10,
       height = 6,
       units = 'in')


#repeat, animating by year - not very illuminating
p <- ghg %>% filter(!is.na(polyarchy), year <=2017) %>%
  mutate(emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop,
         democracy = if_else(polyarchy >= 0.5, TRUE, FALSE)) %>% #coded populists too few to model with loess 
  ggplot(aes(x = gdp_percap, y = (1000 * emit_percap), color = democracy))+
  geom_smooth()+
  transition_time(as.integer(year))+
  ease_aes('cubic-in-out')
animate(p)


ghg %>% filter(!is.na(regime)) %>%
  mutate(emit_pergdp = emit_lag / gdp,
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = gdp_percap, y = emit_pergdp, color = regime))+
  coord_cartesian(ylim = c(0,0.000025))+
  geom_smooth() #poor autocracies dominate relationship with emissions per gdp

ghg %>% filter(!is.na(regime)) %>%
  mutate(emit_rel = emit_lag / (gdp * pop),
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = tenure_aggregate, y = emit_rel, color = regime))+
  geom_smooth() #maybe middle-state phenomenon over leader tenure, but lose most autocratic codings by relying on tenure column

ghg %>% filter(!is.na(regime)) %>%
  mutate(emit_pergdp = emit_lag / gdp,
         gdp_percap = gdp / pop) %>%
  ggplot(aes(x = tenure_aggregate, y = emit_pergdp, color = regime))+
  geom_smooth() #clearer middle-state phenomenon over leader tenure vs emissions per capita
                #but tenure not showing anything interesting, so why break out this way?

#plot country emissions over time
ggplot(data = ghg, aes(x = year, y = (1000 * emit) / pop, color = country_code))+
  geom_line()+
  theme(legend.position = "none")
  


##simulate gdp per capita vs emissions, separated by regime type
gdp_scale = seq(from = min(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE),
                to = max(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE),
                by = ((max(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE)) - 
                        (min(ghg$gdp_percap, na.rm = TRUE) * median(ghg$pop, na.rm = TRUE))) /
                  99)
mimir <- data.frame(regime = rep(unique(ghg$regime[!is.na(ghg$regime)]), each = length(gdp_scale)),
                    gdp = rep(gdp_scale, times = length(unique(ghg$regime[!is.na(ghg$regime)]))),
                    pop = median(ghg$pop, na.rm = TRUE),
                    year = 2017)
mimir$gdp_sq <- mimir$gdp * mimir$gdp
  
mimir <- mimir %>% mutate(expect_emit = predict(object = pop_discrete_inter_m, newdata = mimir))

ggplot(data = mimir, aes(x = gdp/pop, y = expect_emit, color = regime))+
  geom_point()

##illustrate models' expected kuznets curve for democracies and autocracies, by era----
m2pc_dems <- lm((emit_lag / pop) ~ gdp_percap + I(gdp_percap^2) + as.factor(year),
                data = ghg %>% filter(polyarchy >= 0.5))
summary(m2pc_dems) 

m2pc_autos <- lm((emit_lag / pop) ~ gdp_percap + I(gdp_percap^2) + as.factor(year),
                 data = ghg %>% filter(polyarchy < 0.5))
summary(m2pc_autos) 

naive_pc_dems <- lm((emit_lag / pop) ~ gdp_percap + I(gdp_percap^2) + as.factor(year),
                    data = ghg %>% filter(year < 2000, polyarchy >= 0.5))
summary(naive_pc_dems) 

naive_pc_autos <- lm((emit_lag / pop) ~ gdp_percap + I(gdp_percap^2) + as.factor(year),
                     data = ghg %>% filter(year < 2000, polyarchy < 0.5))
summary(naive_pc_autos) 

m6pc_dems <- lm((emit_lag / pop) ~ gdp_percap + I(gdp_percap^2) + as.factor(year),
                data = ghg %>% filter(year >= 2000, polyarchy >= 0.5))
summary(m6pc_dems) 

m6pc_autos <- lm((emit_lag / pop) ~ gdp_percap + I(gdp_percap^2) + as.factor(year),
                 data = ghg %>% filter(year >= 2000, polyarchy < 0.5))
summary(m6pc_autos) 

kc <- data.frame(gdp_percap = seq(from = 0, to = 120000, by = 1000))
kc <- kc %>% mutate(gdp_percap_sq = gdp_percap^2,
                    emit_auto = summary(m2pc_autos)$coefficients['(Intercept)',1] +
                      (gdp_percap * summary(m2pc_autos)$coefficients['gdp_percap',1]) + 
                      (gdp_percap_sq * summary(m2pc_autos)$coefficients['I(gdp_percap^2)',1]),
                    emit_dem = summary(m2pc_dems)$coefficients['(Intercept)',1] +
                      (gdp_percap * summary(m2pc_dems)$coefficients['gdp_percap',1]) + 
                      (gdp_percap_sq * summary(m2pc_dems)$coefficients['I(gdp_percap^2)',1]))
ggplot(data = kc, aes(x = gdp_percap))+
  geom_point(aes(y = emit_auto), color = 'firebrick')+
  geom_point(aes(y = emit_dem), color = 'dodgerblue')

kc <- data.frame(gdp_percap = seq(from = 0, to = 120000, by = 1000))
kc <- kc %>% mutate(gdp_percap_sq = gdp_percap^2,
                    emit_auto = summary(naive_pc_autos)$coefficients['(Intercept)',1] +
                      (gdp_percap * summary(naive_pc_autos)$coefficients['gdp_percap',1]) + 
                      (gdp_percap_sq * summary(naive_pc_autos)$coefficients['I(gdp_percap^2)',1]),
                    emit_dem = summary(naive_pc_dems)$coefficients['(Intercept)',1] +
                      (gdp_percap * summary(naive_pc_dems)$coefficients['gdp_percap',1]) + 
                      (gdp_percap_sq * summary(naive_pc_dems)$coefficients['I(gdp_percap^2)',1]))
ggplot(data = kc, aes(x = gdp_percap))+
  geom_point(aes(y = emit_auto), color = 'firebrick')+
  geom_point(aes(y = emit_dem), color = 'dodgerblue')


kc <- data.frame(gdp_percap = seq(from = 0, to = 120000, by = 1000))
kc <- kc %>% mutate(gdp_percap_sq = gdp_percap^2,
                    emit_auto = summary(m6pc_autos)$coefficients['(Intercept)',1] +
                      (gdp_percap * summary(m6pc_autos)$coefficients['gdp_percap',1]) + 
                      (gdp_percap_sq * summary(m6pc_autos)$coefficients['I(gdp_percap^2)',1]),
                    emit_dem = summary(m6pc_dems)$coefficients['(Intercept)',1] +
                      (gdp_percap * summary(m6pc_dems)$coefficients['gdp_percap',1]) + 
                      (gdp_percap_sq * summary(m6pc_dems)$coefficients['I(gdp_percap^2)',1]))
ggplot(data = kc, aes(x = gdp_percap))+
  geom_point(aes(y = emit_auto), color = 'firebrick')+
  geom_point(aes(y = emit_dem), color = 'dodgerblue')

##illustrate kuznets curve by regime type, iterated by era (straight data, not models)
ghg %>% 
  filter(polyarchy >= 0.5) %>%
  mutate(emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop,
         decade = case_when(year >= 2010 ~ as.character('d2010'),
                             (year >= 2000 ~ 'd2000'),
                             (year >= 1990 ~ 'd1990'),
                             (year >= 1980 ~ 'd1980'),
                             (year >= 1970 ~ 'd1970')) ) %>%
  ggplot(aes(x = gdp_percap, y = (1000 * emit_percap), color = decade, linetype = decade))+
  coord_cartesian(ylim = c(0,44))+
  geom_smooth(size = 3, alpha = 0.2, se = FALSE)+
#  annotate('text', label = 'Autocracy', color = 'firebrick', size = 7, x = 37500, y = 35)+
#  annotate('text', label = 'Democracy', color = 'dodgerblue', size = 7, x = 62500, y = 5)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  labs(title = 'Democracies Improved Growth-Emissions Tradeoff',
       subtitle = '  Greenhouse Gas Emissions, by decade',
       y = 'Emissions per capita (metric tons)',
       x = 'GDP per capita (USD)')+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 

ghg %>% 
  filter(polyarchy >= 0.5) %>%
  mutate(emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop,
         decade = case_when(year >= 2010 ~ as.character('d2010'),
                            (year >= 2000 ~ 'd2000'),
                            (year >= 1990 ~ 'd1990'),
                            (year >= 1980 ~ 'd1980'),
                            (year >= 1970 ~ 'd1970')) ) %>%
  ggplot()+
  geom_smooth(data = . %>% filter(decade == 'd1970'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'dotted', size = 1, alpha = 0.2, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1980'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'dashed', size = 1.2, alpha = 0.4, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1990'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'dotdash', size = 1.4, alpha = 0.6, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2000'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'longdash', size = 1.6, alpha = 0.8, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2010'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'solid', size = 2, alpha = 1, se = FALSE)+
  coord_cartesian(ylim = c(0,60), xlim = c(0,90000))+
  #  annotate('text', label = 'Autocracy', color = 'firebrick', size = 7, x = 37500, y = 35)+
  #  annotate('text', label = 'Democracy', color = 'dodgerblue', size = 7, x = 62500, y = 5)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  labs(title = 'Democracies Improved Growth-Emissions Tradeoff',
       subtitle = '  Greenhouse Gas Emissions, by decade',
       y = 'Emissions per capita (metric tons)',
       x = 'GDP per capita (USD)')+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 



ghg %>% 
  filter(polyarchy < 0.5) %>%
  mutate(emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop,
         decade = case_when(year >= 2010 ~ as.character('d2010'),
                            (year >= 2000 ~ 'd2000'),
                            (year >= 1990 ~ 'd1990'),
                            (year >= 1980 ~ 'd1980'),
                            (year >= 1970 ~ 'd1970')) ) %>%
  ggplot()+
  geom_smooth(data = . %>% filter(decade == 'd1970'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'dotted', size = 1, alpha = 0.2, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1980'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'dashed', size = 1.2, alpha = 0.4, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1990'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'dotdash', size = 1.4, alpha = 0.6, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2000'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'longdash', size = 1.6, alpha = 0.8, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2010'),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'solid', size = 2, alpha = 1, se = FALSE)+
  coord_cartesian(ylim = c(0,60), xlim = c(0,90000))+
  #  annotate('text', label = 'Autocracy', color = 'firebrick', size = 7, x = 37500, y = 35)+
  #  annotate('text', label = 'Democracy', color = 'dodgerblue', size = 7, x = 62500, y = 5)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  labs(title = 'Autocracies Face Growth-Emissions Tradeoff',
       subtitle = '  Greenhouse Gas Emissions, by decade',
       y = 'Emissions per capita (metric tons)',
       x = 'GDP per capita (USD)')+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 

ghg %>% 
  filter(!is.na(polyarchy)) %>%
  mutate(emit_percap = emit_lag / pop,
         gdp_percap = gdp / pop,
         decade = case_when(year >= 2010 ~ as.character('d2010'),
                            (year >= 2000 ~ 'd2000'),
                            (year >= 1990 ~ 'd1990'),
                            (year >= 1980 ~ 'd1980'),
                            (year >= 1970 ~ 'd1970')) ) %>%
  ggplot()+
  geom_smooth(data = . %>% filter(decade == 'd1970', polyarchy < 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'dotted', size = 1, alpha = 0.2, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1980', polyarchy < 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'dotdash', size = 1.2, alpha = 0.4, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1990', polyarchy < 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'dashed', size = 1.4, alpha = 0.6, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2000', polyarchy < 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'longdash', size = 1.6, alpha = 0.8, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2010', polyarchy < 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'firebrick', linetype = 'solid', size = 2, alpha = 1, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1970', polyarchy >= 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'dotted', size = 1, alpha = 0.2, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1980', polyarchy >= 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'dotdash', size = 1.2, alpha = 0.4, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd1990', polyarchy >= 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'dashed', size = 1.4, alpha = 0.6, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2000', polyarchy >= 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'longdash', size = 1.6, alpha = 0.8, se = FALSE)+
  geom_smooth(data = . %>% filter(decade == 'd2010', polyarchy >= 0.5),
              aes(x = gdp_percap, y = (1000 * emit_percap)),
              color = 'dodgerblue', linetype = 'solid', size = 2, alpha = 1, se = FALSE)+
  coord_cartesian(ylim = c(0,60), xlim = c(0,90000))+
  annotate('text', label = 'Autocracy', color = 'firebrick', size = 7, x = 37500, y = 53)+
  annotate('text', label = 'Democracy', color = 'dodgerblue', size = 7, x = 62500, y = 5)+
  annotate('text', label = '1970s', color = 'gray40', size = 5, x = 75000, y = 62)+
  annotate('text', label = '1980s', color = 'gray40', size = 5, x = 87500, y = 42)+
  annotate('text', label = '1990s', color = 'gray40', size = 5, x = 72000, y = 44)+
  annotate('text', label = '2000s', color = 'gray40', size = 5, x = 70000, y = 52)+
  annotate('text', label = '2010s', color = 'gray40', size = 5, x = 73500, y = 34)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  labs(title = 'Democracies Improving, Autocracies Not',
       subtitle = '  Greenhouse Gas Emissions, by decade',
       y = 'Emissions per capita (metric tons)',
       x = 'GDP per capita (USD)')+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 8))) 
ggsave(filename = "./visuals/development_dilemma_by_decade_and_regime.jpg",
       width = 10,
       height = 6,
       units = 'in')
