##-----------------------------------------------------------------------------------------------#
## Date:         05-29-2020
## Author:       Alice Milivinti
## Institution:  PHS, Stanford
##
## Project: 'EICT & COVID-19'
## R-codes: 'Aggregated Analysis'
##-----------------------------------------------------------------------------------------------#

rm(list = ls(all = TRUE)) 
options('scipen' = 999, 'digits' = 10)

##-----------------------------------------------------------------------------------------------#
## Load required packages
##-----------------------------------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(data.table)
library(ipumsr)
library(ggrepel)


# Set the Stanford Color Palette
stanford_colors <- c(
  `black`           = '#2e2d29',
  `cool grey`       = '#4D4F53',
  `chocolate`       = '#2F2424',
  `warm grey`       = '#3f3c30',
  `stone`           = '#544948',
  `driftwood`       = '#b6b1a9',
  `clay`            = '#5f574f',
  `brown`           = '#5e3032',
  `redwood`         = '#8d3c1e',
  `light red`       = '#dfa7a7',
  `bright red`      = '#B1040E',
  `cardinal red`    = '#8C1515',
  `dark red`        = '#820000',
  `teal`            = '#00505c',
  `green`           = '#175e54',
  `light sage`      = '#c7d1c5',
  `sandhill`        = '#b3995d',
  `beige`           = '#9d9573',
  `sandstone`       = '#D8CCB7',
  `light sandstone` = '#F4F4F4',
  `cloud`           = '#dad7cb',
  `fog`             = '#F4F4F4',
  `white`           = '#FFFFFF')

stanford_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (stanford_colors)
  
  stanford_colors[cols]
}

stanford_palettes <- list(`main` = stanford_cols('black', 'cool grey', 'chocolate', 'warm grey', 'stone', 'driftwood', 'clay', 'brown',
                                                 'redwood', 'bright red', 'cardinal red', 'dark red', 'teal', 'green', 'light sage',
                                                 'sandhill', 'beige', 'sandstone', 'light sandstone', 'cloud', 'fog', 'white'),
                          `blind` = stanford_cols('cardinal red', 'cloud', 'green'),
                          `blind reverse` = stanford_cols('green', 'could', 'cardinal red'),
                          `ground` = stanford_cols('chocolate',  'sandstone'),
                          `ground reverse` = stanford_cols('sandstone', 'chocolate'),
                          `earth` = stanford_cols('dark red', 'light sandstone'),
                          `earth reverse` = stanford_cols('light sandstone', 'dark red'),
                          `cool` = stanford_cols('dark red', 'sandstone'),
                          `cool reverse` = stanford_cols('sandstone', 'dark red'),
                          `all red` = stanford_cols('dark red', 'light red'),
                          `all red reverse` = stanford_cols('light red', 'dark red'))

stanford_pal <- function(palette = 'main', reverse = FALSE, ...) {
  pal <- stanford_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#stanford_pal('cool') 
#stanford_pal('cool')(10)

# Scale color
scale_color_stanford <- function(palette = 'main', discrete = TRUE, reverse = FALSE, ...) {
  pal <- stanford_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale('colour', paste0('stanford_', palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale fill
scale_fill_stanford <- function(palette = 'main', discrete = TRUE, reverse = FALSE, ...) {
  pal <- stanford_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale('fill', paste0('stanford_', palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


##-----------------------------------------------------------------------------------------------#
## Load data and metadata
##-----------------------------------------------------------------------------------------------#
# CPS Wave April 2020 
# For those who did not work at all during the survey reference week of April 12–18, if a person indicated they were under quarantine or self-isolating due to health concerns, the interviewer should select “own illness, injury, or medical problem.”
# To be classified as unemployed on temporary layoff, a person has either been given a date to return to work by their employer or expects   to be recalled to their job within 6 months. 

# 1. Core Monthly Survey & Outgoing Rotation Groups (Earner Study) Variables 2020
cps_ddi_core <- '/media/alice/TOSHIBA EXT/CPS/Data/Core_2020.xml' # Metadata
cps_data_core <- '/media/alice/TOSHIBA EXT/CPS/Data/Core_2020.dat' # Data
cps_ddi_core <- read_ipums_ddi(cps_ddi_core)
cps_core <- read_ipums_micro(cps_ddi_core, data_file = cps_data_core)

#ipums_var_desc(cps_ddi_core, EMPSTAT) # Variable Description
#ipums_var_label(cps_ddi_core, UHRSWORKORG) # Variable Label
#ipums_val_labels(cps_ddi_core, UHRSWORKORG) # Variable Labels

# 2. Annual Socio-Economic Supplement (ASEC) for 2019 
cps_ddi_asec <- '/media/alice/TOSHIBA EXT/CPS/Data/ASEC_2019.xml'
cps_data_asec <- '/media/alice/TOSHIBA EXT/CPS/Data/ASEC_2019.dat'
cps_ddi_asec <- read_ipums_ddi(cps_ddi_asec) 
cps_asec <- read_ipums_micro(cps_ddi_asec, data_file = cps_data_asec)

##-----------------------------------------------------------------------------------------------#
## Data Management
##-----------------------------------------------------------------------------------------------#

###############################################################
# 1. Core Monthly Survey
###############################################################

cps_core <- cps_core %>%
  mutate(DATE = make_date(YEAR, MONTH)) %>%
  mutate(OCC = as.integer(OCC))

# better to categorize the Occupations before the merging of CPS Core and ASEC
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
          OCC_ROUT = as.factor(OCC_ROUT)) %>%
  select(-OCC) %>%
  filter(DATE != '2020-01-01')  # Drop January from the sample

##########################################
# Aggregate CPS Core & Earner by OCC_CAT
##########################################

count_occ <- count(cps_core, OCC_CAT, wt = WTFINL) %>%
  rename(OCC_CAT_SIZE = n)

cps_core_occ <- cps_core %>%
  filter(LABFORCE == 2) %>% # Filter only people in the labour force 
  filter(OCC_ROUT != 'Armed Forces' & OCC_ROUT != 'No Occupation') %>% # filter out Armed Forces and No Occupation
  group_by(DATE, OCC_CAT) %>%
  summarize(EMPL_PERC = weighted.mean(EMPSTAT == 10 | EMPSTAT == 12, w = WTFINL, na.rm = TRUE), # At work & Has job, not at work last week
            UHRSWORK1 = weighted.mean(UHRSWORK1, w = WTFINL, na.rm = TRUE),
            AHRSWORK1 = weighted.mean(AHRSWORK1, w = WTFINL, na.rm = TRUE), 
            HOURWAGE  = weighted.mean(HOURWAGE, w = EARNWT, na.rm = TRUE), 
            EARNWEEK  = weighted.mean(EARNWEEK, w = EARNWT, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(OCC_CAT) %>%
  mutate(DELTA_EMPL_PERC = EMPL_PERC - dplyr::lag(EMPL_PERC, 2, order_by = DATE), # create delta variation in percentage of people employed vs unemployed
         DELTA_EMPL_OCC_PERC = scales::percent(DELTA_EMPL_PERC), # Graphical reasons
         DELTA_HRS_OCC = round(UHRSWORK1 - dplyr::lag(UHRSWORK1, 2, order_by = DATE), digits = 1),
         DELTA_EAR_OCC = round(EARNWEEK - dplyr::lag(EARNWEEK, 2, order_by = DATE), digits = 1)) %>%
  ungroup() %>%
  left_join(., count_occ, by = "OCC_CAT")

###########################################
# Aggregate CPS Core & Earner by OCC_ROUT
###########################################

cps_core_rout <- cps_core %>%
  filter(LABFORCE == 2) %>% # Filter only people in the labour force 
  filter(OCC_ROUT != 'Armed Forces' & OCC_ROUT != 'No Occupation') %>% # filter out Armed Forces and No Occupation
  group_by(DATE, OCC_ROUT) %>%
  summarize(EMPL_PERC_ROUT = weighted.mean(EMPSTAT == 10 | EMPSTAT == 12, w = WTFINL, na.rm = TRUE), # At work & Has job, not at work last week
            HOURWAGE_ROUT  = weighted.mean(HOURWAGE, w = EARNWT, na.rm = TRUE), 
            EARNWEEK_ROUT  = weighted.mean(EARNWEEK, w = EARNWT, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(OCC_ROUT) %>%
  mutate(DELTA_ROUT = EMPL_PERC_ROUT - dplyr::lag(EMPL_PERC_ROUT, 2, order_by = DATE), # create delta variation in percentage of people employed vs unemployed
         DELTA_PERC_ROUT = scales::percent(DELTA_ROUT)) # Variable used for graphical reasons 

############################################################
# Aggregate CPS Core & Earner by OCC_ROUT & EARNWEEK_QT
############################################################

count_core_rout <- cps_core %>%
  filter(LABFORCE == 2) %>% # Filter only people in the labour force 
  filter(OCC_ROUT != 'Armed Forces' & OCC_ROUT != 'No Occupation') %>% # filter out Armed Forces and No Occupation
  filter(!is.na(EARNWEEK)) %>% # filter out NA weekly earnings 
  group_by(DATE) %>%
  # Create quantiles for Earnings form the longest job by OCC_ROUT 
  mutate(EARNWEEK_QT = ifelse(EARNWEEK <  quantile(EARNWEEK, probs = c(.25), na.rm = TRUE), round(quantile(EARNWEEK, probs = c(.25), na.rm = TRUE)),
                          ifelse(EARNWEEK >= quantile(EARNWEEK, probs = c(.25), na.rm = TRUE) & EARNWEEK < quantile(EARNWEEK, probs = c(.50), na.rm = TRUE), round(quantile(EARNWEEK, probs = c(.5), na.rm = TRUE)),
                          ifelse(EARNWEEK >= quantile(EARNWEEK, probs = c(.50), na.rm = TRUE) & EARNWEEK < quantile(EARNWEEK, probs = c(.75), na.rm = TRUE), round(quantile(EARNWEEK, probs = c(.75), na.rm = TRUE)),
                                 round(quantile(EARNWEEK, probs = c(.95), na.rm = TRUE))))),
         EARNWEEK_QT_N  = ifelse(EARNWEEK <  quantile(EARNWEEK, probs = c(.25), na.rm = TRUE), '4th',
                          ifelse(EARNWEEK >= quantile(EARNWEEK, probs = c(.25), na.rm = TRUE) & EARNWEEK < quantile(EARNWEEK, probs = c(.50), na.rm = TRUE), '3rd',
                          ifelse(EARNWEEK >= quantile(EARNWEEK, probs = c(.50), na.rm = TRUE) & EARNWEEK < quantile(EARNWEEK, probs = c(.75), na.rm = TRUE), '2nd', '1st')))) %>%
  ungroup() %>%
  group_by(DATE, OCC_ROUT, EARNWEEK_QT, EARNWEEK_QT_N) %>%
  count(EMPSTAT, wt = WTFINL) %>% # Weighted # of people by EMPSTAT, OCC_ROUT and EARNWEEK_QT 
  summarize(ROUT_QT_N = sum(n)) %>% 
  ungroup() %>%
  group_by(OCC_ROUT, EARNWEEK_QT_N) %>% 
  mutate(DELTA_ROUT_QT_N = ROUT_QT_N - dplyr::lag(ROUT_QT_N, 2, order_by = DATE)) %>% # Absolute difference between Feb and May in the Weighted # of people by EMPSTAT, OCC_ROUT and EARNWEEK_QT 
  ungroup() %>%
  group_by(OCC_ROUT) %>% 
  mutate(DELTA_ROUT_QT_PERC = (DELTA_ROUT_QT_N / sum(DELTA_ROUT_QT_N, na.rm = TRUE))) %>% # Relative difference between Feb and May in the Weighted # of people by EMPSTAT, OCC_ROUT and EARNWEEK_QT 
  ungroup() %>%
  left_join(cps_core_rout, ., by = c("DATE", "OCC_ROUT")) %>% # Join the aggergated cps_core_rout (DATE, OCC_ROUT)
  filter(DATE == '2020-04-01') %>%
  mutate(DELTA_EMP_ROUT_QT_REL = DELTA_ROUT * DELTA_ROUT_QT_PERC) # Cumpute the relative % of EMPSTAT by EARNWEEK_QT within OCC_ROUT

###############################################################
# 2. ASEC 2019 Annual Socio-Economic Supplement
###############################################################
# Since ASEC is one Wave (March 2019) I remove the time variables  
# I will merge only based on occupation, but to do that I need to aggregate

cps_asec <- cps_asec %>%
  select(-YEAR, -MONTH) %>%
  rename(OCC = OCCLY) %>%
  # I recode NAs to avoid problems when averaging over a grouping variable
  mutate(INCTOT   = ifelse(INCTOT == 999999999 | INCTOT == 999999998, NA, INCTOT), # Total personal income
         INCWAGE  = ifelse(INCWAGE == 99999999 | INCWAGE == 99999998, NA, INCWAGE), # Wage and salary income
         INCLONGJ = ifelse(INCLONGJ == 99999999, NA, INCLONGJ), # Earnings form the longest job
         OINCBUS  = ifelse(OINCBUS == 9999999, NA, OINCBUS), # Earnings from other work included business self-employment earnings
         EITCRED  = ifelse(EITCRED == 9999, 0, EITCRED), # Earned income tax credit
         OCC_CAT  = ifelse(OCC >= 10   & OCC <= 430, 'Management',
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
         OCC_ROUT = ifelse(OCC >= 10 & OCC <= 3540, 'Non-Routine Cognitive', 
                    ifelse(OCC >= 3600 & OCC <= 4549 | OCC >= 4551 & OCC <= 4865, 'Non-Routine Manual',
                    ifelse(OCC >= 4700 & OCC <= 5940, 'Routine Cognitive',
                    ifelse(OCC >= 6005 & OCC <= 9750 | OCC == 4550, 'Routine Manual', 
                    ifelse(OCC == 9840, 'Armed Forces', 'No Occupation'))))),
         OCC_CAT = as.factor(OCC_CAT),
         OCC_ROUT = as.factor(OCC_ROUT)) %>%
  group_by(OCC_ROUT) %>%
  # Create Earnings form the longest job quantiles by OCC_ROUT 
  mutate(INCLONGJ_QUANT = ifelse(INCLONGJ <  quantile(INCLONGJ, probs = c(.25), na.rm = TRUE), round(quantile(INCLONGJ, probs = c(.25), na.rm = TRUE)),
                    ifelse(INCLONGJ >= quantile(INCLONGJ, probs = c(.25), na.rm = TRUE) & INCLONGJ < quantile(INCLONGJ, probs = c(.50), na.rm = TRUE), round(quantile(INCLONGJ, probs = c(.5), na.rm = TRUE)),
                    ifelse(INCLONGJ >= quantile(INCLONGJ, probs = c(.50), na.rm = TRUE) & INCLONGJ < quantile(INCLONGJ, probs = c(.75), na.rm = TRUE), round(quantile(INCLONGJ, probs = c(.75), na.rm = TRUE)),
                    ifelse(INCLONGJ >= quantile(INCLONGJ, probs = c(.75), na.rm = TRUE), round(quantile(INCLONGJ, probs = c(.95), na.rm = TRUE)), NA))))) %>%
  ungroup() %>%
  select(-OCC) 
  
# Quick EITC Sample Analysis
cps_asec %>%
  filter(WORKLY == 2) %>% # Filter people who worked last year
  summarize(eitc_pct = weighted.mean(EITCRED > 0, ASECWT))
# ~ 10.6% population is EITC recipients

cps_asec %>%
  filter(CLASSWLY == 10 | CLASSWLY == 13 | CLASSWLY == 14) %>% # Filter for self-employed categories
  summarize(eitc_pct = weighted.mean(EITCRED > 0, ASECWT))
# ~ 11.7 % of EITC recipients are self-employed

##########################################
# Aggregate ASEC by OCC_CAT
##########################################
cps_asec_occ <- cps_asec %>%
  filter(OCC_ROUT != 'Armed Forces' & OCC_ROUT != 'No Occupation') %>% # filter out Armed Forces and No Occupation
  group_by(OCC_CAT) %>%
  summarize(INCTOT   = weighted.mean(INCTOT, w = ASECWT, na.rm = TRUE), # Individual level weight
            INCWAGE  = weighted.mean(INCWAGE, w = ASECWT, na.rm = TRUE), # Individual level weight
            INCLONGJ = weighted.mean(INCLONGJ, w = ASECWT, na.rm = TRUE), # Individual level weight
            OINCBUS  = weighted.mean(OINCBUS, w = ASECWT, na.rm = TRUE), # Individual level weight
            EITCRED  = weighted.mean(EITCRED, w = ASECWT, na.rm = TRUE)) # Household level weight

###########################################
# Aggregate ASEC by OCC_ROUT
###########################################
cps_asec_rout <- cps_asec %>%
  filter(OCC_ROUT != 'Armed Forces' & OCC_ROUT != 'No Occupation') %>% # filter out Armed Forces and No Occupation
  group_by(OCC_ROUT) %>%
  summarize(INCTOT   = weighted.mean(INCTOT, w = ASECWT, na.rm = TRUE), # Individual level weight
            INCWAGE  = weighted.mean(INCWAGE, w = ASECWT, na.rm = TRUE), # Individual level weight
            INCLONGJ = weighted.mean(INCLONGJ, w = ASECWT, na.rm = TRUE), # Individual level weight
            OINCBUS  = weighted.mean(OINCBUS, w = ASECWT, na.rm = TRUE), # Individual level weight
            EITCRED  = weighted.mean(EITCRED, w = ASECWT, na.rm = TRUE)) # Household level weight


# Merge Monthly CPS Core & Earner with Yearly ASEC  
# OCC_CAT
cps_occ <- left_join(cps_core_occ, cps_asec_occ, by = "OCC_CAT") 
# OCC_ROUT
cps_rout <- left_join(count_core_rout, cps_asec_rout, by = "OCC_ROUT")

remove(cps_core_occ, cps_asec_occ, cps_core_rout, cps_asec_rout, count_core_rout, count_occ)

##-----------------------------------------------------------------------------------------------#
## Descriptive Statistics
##-----------------------------------------------------------------------------------------------#

##------------------------------------------------------------#
# 1. Employment status (EMPSTAT) in Feb/Apr 2020 by OCC_CAT 
##------------------------------------------------------------#

cps_occ <- cps_occ %>%
  filter(OCC_CAT != 'No Occupation') %>%
  filter(DATE == '2020-04-01')

# Plot
plot_occ <- ggplot(data = cps_occ) + 
  geom_bar(aes(x = reorder(OCC_CAT, DELTA_EMPL_PERC), y = DELTA_EMPL_PERC, fill = EARNWEEK), 
           stat = 'identity', position = 'stack', color = '#dad7cb', width = 0.4) +
  labs(y = '% Difference in Employment', x = '', fill = '') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(position = 'top') + 
  geom_text(aes(x = reorder(OCC_CAT, DELTA_EMPL_PERC), y = (DELTA_EMPL_PERC + .015 * sign(DELTA_EMPL_PERC)), label = DELTA_EMPL_OCC_PERC, 
                fontface = 2), position = position_dodge(width = 1), size = 3.5, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool reverse', discrete = FALSE, name = 'Weekly Earnings (USD)') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.4, 0.9), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Roboto Sans Serif'),
        panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5, label.position = 'bottom')) +
  coord_flip()
plot_occ

ggsave('Empl_Occ.png', plot = plot_occ, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 25, height = 18, units = c('cm'))


#############################
#          EITC
#############################

plot_eitc <- ggplot(data = cps_occ, aes(x = EITCRED, y = DELTA_EMPL_PERC, color = reorder(OCC_CAT, EARNWEEK), size = OCC_CAT_SIZE)) + 
  geom_point() +
  labs(y = '% Difference in Employment', x = 'Average 2019 EITC (USD)', color = '', size = '# of People Employed') + 
  scale_size(range = c(3,15)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text_repel(aes(label = OCC_CAT),  fontface = 'bold',  size = 3.5, lineheight = .7, color = '#2F2424', box.padding = 1) +
  scale_color_stanford(palette = 'cool reverse') +
  geom_hline(yintercept = 0, linetype = 'dotted', color = '#2F2424') +
  theme_minimal() +
  xlim(0, 810) +
  theme(legend.direction = "vertical",  legend.position = c(0.1, 0.2), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Roboto Sans Serif'),
        panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(color = FALSE )
plot_eitc

ggsave('EITC_Occ.png', plot = plot_eitc, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 35, height = 20,  units = c('cm'))


##------------------------------------------------------------#
# 2. Employment status (EMPSTAT) in Feb/Apr 2020 by OCC_ROUT 
##------------------------------------------------------------#

##############################################
# Distribution Routine jobs by wage quantile
##############################################

cps_dist <- cps_core  %>%
  filter(OCC_ROUT != 'No Occupation' & OCC_ROUT != 'Armed Forces') %>%
  filter(!is.na(EARNWEEK) & EARNWEEK > 0) %>%
  group_by(OCC_ROUT) %>%
  mutate(W_AVE = ifelse(OCC_ROUT == 'Non-Routine Cognitive', 1,
                 ifelse(OCC_ROUT == 'Routine Cognitive', 2,
                 ifelse(OCC_ROUT == 'Routine Manual', 3, 4)))) %>%
  ungroup()

# Density Plot
dist_rout <- ggplot(data = cps_dist, aes(x = EARNWEEK, color = reorder(OCC_ROUT, W_AVE), fill = reorder(OCC_ROUT, W_AVE), weight = EARNWT)) + 
  geom_density(alpha = 0.2) +
  #xlim(0, 200000) +
  labs(y = 'Density', x = 'Weekly Earnings (USD)', color = 'Occupational Categories', fill = 'Occupational Categories') + 
  scale_color_stanford(palette = 'cool') +
  scale_fill_stanford(palette = 'cool') +
  theme_minimal() +
  theme(legend.direction = 'vertical', legend.position = c(0.7, 0.8), 
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Roboto Sans Serif'),
        panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) 
dist_rout

ggsave('W_Dist_Rout.png', plot = dist_rout, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 20, height = 15,  units = c('cm'))

##############################################
# % Employment Difference by wage quantile
##############################################

cps_rout <- cps_rout %>%
  mutate(WHITE = ifelse(EARNWEEK_QT_N == '1st', 1, 0)) # Variable Created for graphica reasons

# Plot
plot_rout <- ggplot(data = arrange(cps_rout, DELTA_EMP_ROUT_QT_REL) , aes(x = reorder(OCC_ROUT, DELTA_ROUT), y = DELTA_EMP_ROUT_QT_REL, fill = EARNWEEK_QT_N)) + 
  geom_bar(stat = 'identity', width = 0.4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(position = 'top') + 
  geom_text(aes(label = scales::percent(DELTA_EMP_ROUT_QT_REL, accuracy = .1), fontface = 2, color = WHITE), 
            position = position_stack(0.5), size = 3.5, angle = 90) +
  scale_fill_stanford(palette = 'cool') +
  scale_color_stanford(palette = 'ground', discrete = FALSE) +
  labs(y = '% Total Difference in Employment', x = '', fill = 'Weekly Earnings Quartiles (USD)', color = '') + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "vertical", legend.position = c(0.2, 1.02),
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  coord_flip()
plot_rout

ggsave('Empl_Rout.png', plot = plot_rout, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 25, height = 18,  units = c('cm'))


###############################################################
# Hours Worked status (EMPSTAT) in Feb/Apr 2020 by OCC_CAT
###############################################################

# Average working hrs among employed

plot_hrs_emp <- ggplot(data = cps_occ) + 
  geom_bar(aes(x = reorder(OCC_CAT, DELTA_HRS_OCC), y = DELTA_HRS_OCC, fill = EARNWEEK), 
           stat = 'identity', position = 'dodge', color = '#dad7cb', width = 0.4) +
  labs(y = 'Difference in hours usually worked per week at the main job', x = '', fill = '') + 
  scale_x_discrete(position = 'top') + 
  geom_text(aes(x = reorder(OCC_CAT, DELTA_HRS_OCC), y = (DELTA_HRS_OCC + .08 * sign(DELTA_HRS_OCC)), label = DELTA_HRS_OCC, fontface = 2),
            position = position_dodge(width = 1), size = 4, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool', discrete = FALSE, name = 'Weekly Earnings (USD)') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.3, 0.9), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5)) +
  coord_flip()
plot_hrs_emp

ggsave('Hrs_Occ_Emp.png', plot = plot_hrs_emp, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 32, height = 17,  units = c('cm'))


# Employed & Unemployed average working hrs

# Create new dataset
cps_hrs <- cps_core %>%
  filter(LABFORCE == 2) %>% # Filter only people in the labour force 
  filter(OCC_ROUT != 'Armed Forces' & OCC_ROUT != 'No Occupation') %>% # filter out Armed Forces and No Occupation
  group_by(DATE, OCC_CAT) %>%
  mutate(UHRSWORK1 = ifelse(is.na(UHRSWORK1), 0 , UHRSWORK1), # Re-code NA working hrs in 0 
         AHRSWORK1 = ifelse(is.na(AHRSWORK1), 0 , AHRSWORK1),) %>%     
  group_by(DATE, OCC_CAT) %>%
  summarize(UHRSWORK1 = weighted.mean(UHRSWORK1, w = WTFINL, na.rm = TRUE),
            AHRSWORK1 = weighted.mean(AHRSWORK1, w = WTFINL, na.rm = TRUE), 
            HOURWAGE  = weighted.mean(HOURWAGE, w = EARNWT, na.rm = TRUE), 
            EARNWEEK  = weighted.mean(EARNWEEK, w = EARNWT, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(OCC_CAT) %>%
  mutate(DELTA_HRS_OCC = round(UHRSWORK1 - dplyr::lag(UHRSWORK1, 2, order_by = DATE), digits = 1)) %>%
  ungroup() %>%
  filter(DATE ==  '2020-04-01') %>%
  filter(OCC_CAT != 'No Occupation')

# Plot
plot_hrs <- ggplot(data = cps_hrs) + 
  geom_bar( aes(x = reorder(OCC_CAT, DELTA_HRS_OCC), y = DELTA_HRS_OCC, fill = EARNWEEK),
            stat = 'identity', position = 'dodge', color = '#dad7cb', width = 0.4) +
  labs(y = 'Difference in hours usually worked per week at the main job', x = '', fill = '') + 
  scale_x_discrete(position = 'top') + 
  geom_text(aes(x = reorder(OCC_CAT, DELTA_HRS_OCC), y = (DELTA_HRS_OCC + .2 * sign(DELTA_HRS_OCC)), label = DELTA_HRS_OCC, fontface = 2),
            position = position_dodge(width = 1), size = 4, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool', discrete = FALSE, name = 'Weekly Earnings (USD)') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.3, 0.9), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5)) +
  coord_flip()
plot_hrs

ggsave('Hrs_Occ.png', plot = plot_hrs, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 32, height = 17,  units = c('cm'))



###############################################################
# Hours Worked status (EMPSTAT) in Feb/Apr 2020 by OCC_CAT
###############################################################

# Average weekly earnings among employed
plot_earn <- ggplot(data = cps_occ) + 
  geom_bar(aes(x = reorder(OCC_CAT, DELTA_EAR_OCC), y = DELTA_EAR_OCC, fill = EARNWEEK), 
           stat = 'identity', position = 'dodge', color = '#dad7cb', width = 0.4) +
  labs(y = 'Difference in weekly earnings at alhe main job', x = '', fill = '') + 
  scale_x_discrete(position = 'top') + 
  geom_text(aes(x = reorder(OCC_CAT, DELTA_EAR_OCC), y = (DELTA_EAR_OCC + 8 * sign(DELTA_EAR_OCC)), label = DELTA_EAR_OCC, fontface = 2),
            position = position_dodge(width = 1), size = 4, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool', discrete = FALSE, name = 'Weekly Earnings (USD)') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.3, 0.9), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5)) +
  coord_flip()
plot_earn

ggsave('Earn_Occ_Emp.png', plot = plot_earn, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 32, height = 17,  units = c('cm'))

