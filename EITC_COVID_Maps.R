##-----------------------------------------------------------------------------------------------#
## Date:         06-15-2020
## Author:       Alice Milivinti
## Institution:  PHS, Stanford
##
## Project: 'EICT & COVID-19'
## R-codes: 'MAPS'
##-----------------------------------------------------------------------------------------------#

rm(list = ls(all = TRUE)) 
options('scipen' = 999, 'digits' = 20)

##-----------------------------------------------------------------------------------------------#
## Load required packages
##-----------------------------------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(data.table)
library(ipumsr)
library(lubridate)

##-----------------------------------------------------------------------------------------------#
## Load data and metadata
##-----------------------------------------------------------------------------------------------#
# 1. Core Monthly Survey & Outgoing Rotation Groups (Earner Study) Variables 2020
cps_ddi_core <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/Core_2020.xml' # Metadata
cps_data_core <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/Core_2020.dat' # Data
cps_ddi_core <- read_ipums_ddi(cps_ddi_core)
cps_core <- read_ipums_micro(cps_ddi_core, data_file = cps_data_core)

# 2. Annual Socio-Economic Supplement (ASEC) for 2019 
cps_ddi_asec <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/ASEC_2019.xml'
cps_data_asec <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/ASEC_2019.dat'
cps_ddi_asec <- read_ipums_ddi(cps_ddi_asec) 
cps_asec <- read_ipums_micro(cps_ddi_asec, data_file = cps_data_asec)

# 3. Google Community Mobility Reports: https://www.google.com/covid19/mobility/
# Data for US counties, daily frequency
mob_cou_day <- read.csv('/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/Google/Mobility_Report_US_County.csv')

# 4. Geo-spatial Data 
map.county <- data.table(map_data('county'))

##-----------------------------------------------------------------------------------------------#
## Data Management
##-----------------------------------------------------------------------------------------------#
cps_core <- cps_core %>%
  mutate(# Core Monthly Survey Working Hours Variables
    # How do we deal with varying working hours? I temporarly set UHRSWORKT == 997 & UHRSWORK1 == 997 as NAs.
    UHRSWORK1   = ifelse(UHRSWORK1 == 997 | UHRSWORK1 == 999, NA, UHRSWORK1), # Hours usually worked per week at main job (self-employed incl.)
    AHRSWORK1   = ifelse(AHRSWORK1 == 999, NA, AHRSWORK1), # Hours worked last week main job (self-employed incl., housework incl.)
    # Outgoing Rotation/Earner Study Variables
    HOURWAGE    = ifelse(HOURWAGE == 999.99, NA, HOURWAGE), # Hourly wage (self-employed excl.)
    EARNWEEK    = ifelse(EARNWEEK == 9999.99, NA, EARNWEEK), # Weekly Earnings (self-employed excl.)
    UHRSWORKORG = ifelse(UHRSWORKORG == 998 | UHRSWORKORG == 999, NA, UHRSWORKORG), # Usual hours worked per week (self-employed excl.)
    # Occupational Categories
    OCC_CAT     = ifelse(OCC >= 10   & OCC <= 430, 'Management',
                  ifelse(OCC >= 500  & OCC <= 730, 'Business Operations',
                  ifelse(OCC >= 800  & OCC <= 950, 'Financial Specialists',
                  ifelse(OCC >= 1000 & OCC <= 1240, 'Computer, Mathematical', 
                  ifelse(OCC >= 1300 & OCC <= 1540, 'Architecture, Engineering',
                  ifelse(OCC >= 1550 & OCC <= 1556, 'Technicians',
                  ifelse(OCC >= 1600 & OCC <= 1980, 'Life, Physical, Social Science',
                  ifelse(OCC >= 2000 & OCC <= 2060, 'Community, Social Services',
                  ifelse(OCC >= 2100 & OCC <= 2150, 'Legal',
                  ifelse(OCC >= 2200 & OCC <= 2550, 'Education, Training, Library',
                  ifelse(OCC >= 2600 & OCC <= 2920, 'Arts, Entertainment, Sports, Media',
                  ifelse(OCC >= 3000 & OCC <= 3540, 'Healthcare Practitioners, Technicians', 
                  ifelse(OCC >= 3600 & OCC <= 3650, 'Healthcare Support', 
                  ifelse(OCC >= 3700 & OCC <= 3950, 'Protective Service', 
                  ifelse(OCC >= 4000 & OCC <= 4150, 'Food Preparation, Serving',
                  ifelse(OCC >= 4200 & OCC <= 4250, 'Building, Grounds Cleaning, Maintenance',
                  ifelse(OCC >= 4300 & OCC <= 4650, 'Personal Care',
                  ifelse(OCC >= 4700 & OCC <= 4865, 'Sales & Related',
                  ifelse(OCC >= 5000 & OCC <= 5940, 'Office, Administrative Support',
                  ifelse(OCC >= 6005 & OCC <= 6130, 'Farming, Fisheries, Forestry',
                  ifelse(OCC >= 6200 & OCC <= 6765, 'Construction',
                  ifelse(OCC >= 6800 & OCC <= 6940, 'Extraction',
                  ifelse(OCC >= 7000 & OCC <= 7630, 'Installation, Maintenance, Repair',
                  ifelse(OCC >= 7700 & OCC <= 8965, 'Production',
                  ifelse(OCC >= 9000 & OCC <= 9750, 'Transportation, Material Moving',
                  ifelse(OCC >= 9800 & OCC <= 9830, 'Military', 'No Occupation')))))))))))))))))))))))))),
    # Occupation aggregation by type of tasks         
    OCC_ROUT   = ifelse(OCC >= 10 & OCC <= 3540, 'Non-Routine Cognitive', 
                 ifelse(OCC >= 3600 & OCC <= 4549 | OCC >= 4551 & OCC <= 4865, 'Non-Routine Manual',
                 ifelse(OCC >= 4700 & OCC <= 5940, 'Routine Cognitive',
                 ifelse(OCC >= 6005 & OCC <= 9750 | OCC == 4550, 'Routine Manual', 
                 ifelse(OCC == 9840, 'Armed Forces', 'No Occupation'))))),
    OCC_CAT = as.factor(OCC_CAT),
    OCC_ROUT = as.factor(OCC_ROUT)) 

# Aggregate the CPS Core data by county
cps_core_cou <- cps_core %>%
  mutate(DATE = make_date(YEAR, MONTH)) %>%
  filter(LABFORCE == 2) %>% # Filter only people in the labour force 
  filter(COUNTY != 0) %>% # Remove Counties not identified
  filter(OCC_ROUT != 'Armed Forces' & OCC_ROUT != 'No Occupation') %>% # filter out Armed Forces and No Occupation
  group_by(YEAR, MONTH, DATE, STATEFIP, STATECENSUS, COUNTY, OCC_ROUT) %>%
  mutate(EMPL_PERC = weighted.mean(EMPSTAT == 10 | EMPSTAT == 12, w = WTFINL, na.rm = TRUE), # At work & Has job, not at work last week
            EARNWEEK  = weighted.mean(EARNWEEK, w = EARNWT, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by( STATEFIP, STATECENSUS, COUNTY, OCC_ROUT) %>%
  mutate(DELTA_EMPL_PERC = EMPL_PERC - dplyr::lag(EMPL_PERC, 2, order_by = DATE), # create delta variation in percentage of people employed vs unemployed
         DELTA_EMPL_OCC_PERC = scales::percent(DELTA_EMPL_PERC), # Graphical reasons
         DELTA_EAR_OCC = round(EARNWEEK - dplyr::lag(EARNWEEK, 2, order_by = DATE))) %>%
  ungroup()


# Aggregate the CPS ASEC data by county
cps_asec_cou <- cps_asec %>%
  filter(COUNTY != 0) %>% # Remove Counties not identified
  mutate(EITCRED  = ifelse(EITCRED == 9999, NA, EITCRED)) %>%
  group_by(STATEFIP, STATECENSUS, COUNTY) %>%
  summarise(EITCRED = weighted.mean(EITCRED, w = ASECWT, na.rm = T)) %>%
  ungroup() %>%
  mutate(EITC_COU_PERC = EITCRED / mean(EITCRED),
         NAT_AVE = ifelse(EITC_COU_PERC < 1, 'EITC below the Natioanl Average', 'EITC above the Natioanl Average'))

# Aggregate the Google Mobility data at the monthly level for each county
mob_cou_mon <- mob_cou_day %>%
  rename(State_name = sub_region_1,
         County_name = sub_region_2,
         COUNTY = census_fips_code) %>%
  separate(County_name, c('County_name','C'), sep = "County") %>%
  mutate(MONTH = month(date),) %>%
  group_by(MONTH, State_name, COUNTY, County_name) %>%
# We are interested in the workplaces_percent_change_from_baseline: Mobility trends for places of work.
  summarise(Work_Mob = mean(workplaces_percent_change_from_baseline, na.rm = T))  %>%
  mutate(Work_Mob = Work_Mob / 100) %>%
  ungroup()


# Merge CPS Core Data with Google Mobility 
mob_cou_mon_core <- left_join(cps_core_cou, mob_cou_mon, by = c('COUNTY', 'MONTH')) %>%
  filter(!is.na(MONTH)) %>%
  filter(DATE == '2020-04-01')
 

# Merge CPS Asec Data with Google Mobility 
mob_cou_mon_eitc <- left_join(cps_asec_cou, mob_cou_mon, by = c('COUNTY')) %>%
  filter(!is.na(MONTH)) %>%
  mutate(Month_name = ifelse(MONTH == 2, 'February',
                 ifelse(MONTH == 3, 'March',
                 ifelse(MONTH == 4, 'April',
                 ifelse(MONTH == 5, 'May', 'June')))),
         Month_name = factor(Month_name, levels=c('February','March','April','May', 'June')))


##-----------------------------------------------------------------------------------------------#
## Data Visualization
##-----------------------------------------------------------------------------------------------#

mob_core_p <- ggplot(data = filter(mob_cou_mon_core, DELTA_EMPL_PERC < 0 & DELTA_EMPL_PERC > -1), aes(x = Work_Mob, y = DELTA_EMPL_PERC)) +
  geom_point(aes(color = OCC_ROUT)) +
  geom_smooth(aes(color = OCC_ROUT)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = '% Difference in Employment Feb-Apr', x = '% Change Mobility to Work', title = 'County Level Analysis') + 
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = 'bottom', legend.title = element_blank())
mob_core_p

#ggsave('Empl_Mob.png', plot = mob_core_p, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 25, height = 18, units = c('cm'))


mob_eitc_p <- ggplot(data = mob_cou_mon_eitc, aes(x = Work_Mob, y = EITC_COU_PERC)) +
  geom_point(aes( color = EITC_COU_PERC)) +
  geom_smooth() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = '#2F2424') +
  facet_wrap(~ Month_name, ncol = 1) + 
  labs(y = 'Normalized EITC 2019', x = '% Change Mobility to Work', title = 'County Level Analysis') + 
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = 'bottom', legend.title = element_blank())
mob_eitc_p

#ggsave('Empl_Mob1.png', plot = mob_eitc_p, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 25, height = 18, units = c('cm'))

# Aggregate EITC categories: Above & Equal vs Below to the federal average
eitc_agg <- mob_cou_mon_eitc %>%
  group_by(Month_name, NAT_AVE) %>%
  summarise(Work_Mob = mean(Work_Mob)) %>%
  ungroup()

mob_eitc_agg <- ggplot(data = eitc_agg, aes(x = Month_name, y = Work_Mob, fill = NAT_AVE)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = '', y = '% Change Mobility to Work', title = '') + 
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = 'bottom', legend.title = element_blank())
  mob_eitc_agg

# County Map
cou_map <- map_data('county') %>%
  mutate(region = paste(toupper(substr(region, 1, 1)), substr(region, 2, nchar(region)), sep = ''),
         subregion = paste(toupper(substr(subregion, 1, 1)), substr(subregion, 2, nchar(subregion)), sep = '')) %>%
  left_join(., mob_cou_mon_eitc, by = c('subregion' = 'County_name', region = 'State_name')) %>%
  filter(!is.na(Month_name))

map_mob <- ggplot(data = filter(cou_map, Month_name == 'April')) + 
  geom_polygon(aes(x = long, y = lat, group = group)) 
map_mob





#######################################################################


# Aggregate the CPS Core data by county
cps_rou <- cps_core_cou %>%
  filter(DATE != '2020-01-01') %>%
  # % OCC_ROUT by COUNTY
  group_by(YEAR, MONTH, DATE, STATEFIP, STATECENSUS, COUNTY) %>%
  summarise(ROUT_COG_PERC = weighted.mean(OCC_ROUT == 'Non-Routine Cognitive', w = WTFINL, na.rm = TRUE), # % Non-Routine Cognitive jobs by date and county
            NON_ROUT_MAN_UNEMP = weighted.mean(OCC_ROUT == 'Non-Routine Manual' & EMPSTAT == 10 | 
                                               OCC_ROUT == 'Non-Routine Manual' & EMPSTAT == 12, 
                                               w = WTFINL, na.rm = TRUE)) %>% # % Non-Routine Manual jobs unemployment by date and county
  ungroup() %>%
  mutate(DELTA_NON_ROUT_MAN_UNEMP = NON_ROUT_MAN_UNEMP - dplyr::lag(NON_ROUT_MAN_UNEMP, 2, order_by = DATE)) 


non_rout_p <- ggplot(data = filter(cps_rou, DATE == '2020-04-01'), aes(x = DELTA_NON_ROUT_MAN_UNEMP, y = ROUT_COG_PERC)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = '% Non-Routine Cognitive Jobs', x = '% Difference Employment among Non-Routine Manual Jobs', title = 'County Level Analysis',
       caption = 'Each dot represent a county. The horizontal axis represent the difference in employment between Feb and Apr. \n The vertical axis represent the % of Non-Routine Cognitive jobs in Apr.') 
non_rout_p

#ggsave('Empl_county.png', plot = non_rout_p, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 25, height = 18, units = c('cm'))
