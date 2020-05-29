##-----------------------------------------------------------------------------------------------#
## Date:         05-29-2020
## Author:       Alice Milivinti
## Institution:  PHS, Stanford
##
## Project: 'EICT & COVID-19'
##-----------------------------------------------------------------------------------------------#

rm(list = ls(all = TRUE)) 
options('scipen' = 999, 'digits' = 10)

##-----------------------------------------------------------------------------------------------#
## Load required packages
##-----------------------------------------------------------------------------------------------#

#library(haven)
library(tidyverse)
library(foreign)
library(lubridate)
library(data.table)
library(ipumsr)
library(rgdal) 
library(rgeos) 
library(maptools)
library(viridis)
library(maps)
library(ggrepel)
library(RColorBrewer)
library(gridExtra)
library(CoordinateCleaner)
library(mapproj)
library(wesanderson)


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
  `bright red`      = '#B1040E',
  `cardinal red`    = '#8C1515',
  `dark red`        = '#820000',
  `teal`            = '#00505c',
  `green`           = '#175e54',
  `light sage`      = '#c7d1c5',
  `sandhill`        = '#b3995d',
  `beige`           = '#9d9573',
  `sandstone`       = '#D8CCB7',
  `light sandstone` = '#F9F6EF',
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
                          `cool` = stanford_cols('cardinal red', 'cloud', 'green'),
                          `cool reverse` = stanford_cols('green', 'could', 'cardinal red'),
                          `ground` = stanford_cols('chocolate',  'sandstone'),
                          `earth` = stanford_cols('dark red', 'sandstone'),
                          `earth reverse` = stanford_cols('sandstone', 'dark red'))

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
cps_ddi_ind <- '/media/alice/TOSHIBA EXT/CPS/Data/CPS_19_20.xml' # Metadata
cps_data_ind <- '/media/alice/TOSHIBA EXT/CPS/Data/CPS_19_20.dat' # Data
cps_ddi_ind <- read_ipums_ddi(cps_ddi_ind)
cps_ind <- read_ipums_micro(cps_ddi_ind, data_file = cps_data_ind)

#ipums_var_desc(cps_ddi_ind, EMPSTAT) # Variable Description
#ipums_var_label(cps_ddi_ind, EMPSTAT) # Variable Label
#ipums_val_labels(cps_ddi_ind, UHRSWORKT) # Variable Labels

# 1. Annual Socio-Economic Supplement (ASEC) for 2019 (earnings, eitc, etc)
cps_ddi_asec <- '/media/alice/TOSHIBA EXT/CPS/Data/ASEC_2019.xml'
cps_data_asec <- '/media/alice/TOSHIBA EXT/CPS/Data/ASEC_2019.dat'
cps_ddi_asec <- read_ipums_ddi(cps_ddi_asec) 
cps_asec <- read_ipums_micro(cps_ddi_asec, data_file = cps_data_asec)

##-----------------------------------------------------------------------------------------------#
## Data Management
##-----------------------------------------------------------------------------------------------#

cps_ind <- cps_ind %>%
  mutate(DATE = make_date(YEAR, MONTH)) %>%
  select(DATE, COUNTY, STATECENSUS, STATEFIP, REGION, CBSASZ, LABFORCE, OCC, EDUC99, FAMINC, EMPSTAT, UHRSWORKT) %>%
  mutate(OCC = as.integer(OCC))


# better to categorize the Occupations before the merginf of CPS and ASEC
cps_ind <- cps_ind %>%
  # Attemt to aggregate Occupational Categories
  mutate(UHRSWORKT = ifelse(UHRSWORKT == 997 | UHRSWORKT == 999, NA, UHRSWORKT), # Hours usually worked per week at all jobs
         OCC_CAT  = ifelse(OCC >= 10   & OCC <= 430, 'Management in Business, Science, Arts',
                           ifelse(OCC >= 500  & OCC <= 730, 'Business Operations Specialists',
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
                                                                                                                                                         ifelse(OCC >= 6005 & OCC <= 6130, 'Farming, Fisheries,, Forestry',
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
         EDUC = ifelse(EDUC99 < 10, 'Less than High School',
                       ifelse(EDUC99 >= 10 & EDUC99 <= 14, 'High School & Some College & Associate Degree',
                              ifelse(EDUC99 == 15, 'Bachelor',
                                     ifelse(EDUC99 >=16, 'Master +', 'NIU')))),
         EDUC_CODE = ifelse(EDUC == 'Less than High School', 1,
                            ifelse(EDUC == 'High School & Some College & Associate Degree', 2,
                                   ifelse(EDUC == 'Bachelor', 3, 
                                          ifelse(EDUC == 'Master +', 4, NA))))) %>%
  select(-OCC, -EDUC99)


# Since ASEC is one Wave (March 2019) I remove the time variables  
# I will merge only based on occupation, but to do that I need to aggregate
cps_asec <- cps_asec %>%
  select(COUNTY, STATECENSUS, STATEFIP, REGION, CBSASZ, OCCLY, UHRSWORKLY, INCTOT, INCWAGE, INCLONGJ, OINCBUS, OINCFARM, OINCWAGE, EITCRED) %>%
  rename(OCC = OCCLY) %>%
  # I recode NAs to avoid problems when averaging over CBSASZ
  mutate(INCTOT   = ifelse(INCTOT == 999999999 | INCTOT == 999999998, 0, INCTOT), # 'Total personal income'
         INCWAGE  = ifelse(INCWAGE == 99999999 | INCWAGE == 99999998, 0, INCWAGE), # Wage and salary income
         INCLONGJ = ifelse(INCLONGJ == 999999999, 0, INCLONGJ), # Earnings from longest job
         OINCBUS  = ifelse(OINCBUS == 999999999, 0, OINCBUS), # Earnings from other work included business self-employment earnings
         OINCFARM = ifelse(OINCFARM == 999999999, 0, OINCFARM), # Earnings from other work included farm self-employment earnings
         OINCWAGE = ifelse(OINCWAGE == 999999999, 0, OINCWAGE), # Earnings from other work included wage and salary earnings
         EITCRED  = ifelse(EITCRED == 9999, 0, EITCRED), # Earned income tax credit
         OCC_CAT  = ifelse(OCC >= 10   & OCC <= 430, 'Management in Business, Science, Arts',
                           ifelse(OCC >= 500  & OCC <= 730, 'Business Operations Specialists',
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
                                                                                                                                                         ifelse(OCC >= 6005 & OCC <= 6130, 'Farming, Fisheries,, Forestry',
                                                                                                                                                                ifelse(OCC >= 6200 & OCC <= 6765, 'Construction',
                                                                                                                                                                       ifelse(OCC >= 6800 & OCC <= 6940, 'Extraction',
                                                                                                                                                                              ifelse(OCC >= 7000 & OCC <= 7630, 'Installation, Maintenance, Repair',
                                                                                                                                                                                     ifelse(OCC >= 7700 & OCC <= 8965, 'Production',
                                                                                                                                                                                            ifelse(OCC >= 9000 & OCC <= 9750, 'Transportation, Material Moving',
                                                                                                                                                                                                   ifelse(OCC >= 9800 & OCC <= 9830, 'Military', 'No Occupation'))))))))))))))))))))))))))) %>%
  select(-OCC) %>%
  # Aggregation by Geographical Area and Occupation
  group_by(COUNTY, STATECENSUS, STATEFIP, REGION, CBSASZ, OCC_CAT) %>%
  summarize(INCTOT = mean(INCTOT, na.rm = TRUE),
            INCWAGE = mean(INCWAGE, na.rm = TRUE),
            INCLONGJ = mean(INCLONGJ, na.rm = TRUE),
            OINCBUS = mean(OINCBUS, na.rm = TRUE),
            OINCFARM = mean(OINCFARM, na.rm = TRUE),
            OINCWAGE = mean(OINCWAGE, na.rm = TRUE),
            EITCRED = mean(EITCRED, na.rm = TRUE)) 


# Merge Monthly CPS and Yearly ASEC  
cps <- left_join(cps_ind, cps_asec, by = c('COUNTY', 'STATECENSUS', 'STATEFIP', 'REGION', 'CBSASZ', 'OCC_CAT')) 

##-----------------------------------------------------------------------------------------------#
## Descriptive Statistics
##-----------------------------------------------------------------------------------------------#

##------------------------------------------------------------#
# 1. Employment status (EMPSTAT) in March/Apr 2020 by OCC_CAT 
##------------------------------------------------------------#

cps_emp_occ <- cps %>%
  filter(DATE == '2020-02-01' | DATE == '2020-03-01' | DATE == '2020-04-01') %>%
  filter(LABFORCE == 2) %>% # only people in the labour force P.S. Are we interested in Armed Forces???
  # Re-code Employment status in order to have only two categories (Emp vs Unemp)
  mutate(EMPSTAT = ifelse(EMPSTAT == 10 | EMPSTAT == 12, 'At work & Has job, not at work last week', 'Unemployed')) %>% 
  group_by(DATE, OCC_CAT) %>%
  mutate(TOT_EMPSTAT_OCC = length(EMPSTAT),
         OCC_CAT_N = length(OCC_CAT),
         EITCRED = ifelse(EITCRED == 0, NA, EITCRED)) %>% # We consider only EITC > 0
  ungroup() %>%
  group_by(DATE, EMPSTAT, OCC_CAT, OCC_CAT_N) %>%
  summarize(N_EMPSTAT_OCC    = length(EMPSTAT),
            PERC_EMPSTAT_OCC = round(mean(N_EMPSTAT_OCC / TOT_EMPSTAT_OCC), digits = 2),
            INCWAGE = mean(INCWAGE, na.rm = TRUE),
            EITCRED = mean(EITCRED, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(EMPSTAT, OCC_CAT) %>%
  mutate(DELTA_EMPSTAT_OCC = PERC_EMPSTAT_OCC - dplyr::lag(PERC_EMPSTAT_OCC, 2, order_by = DATE), # create delta variation in percentage of people employed vs unemployed
         DATE_DIFF = ifelse(DATE == '2020-04-01', '20.03 -> 20.04', NA),
         WHITE = ifelse(DELTA_EMPSTAT_OCC < -0.15, 0,1),
         DELTA_EMPSTAT_OCC_PERC = scales::percent(DELTA_EMPSTAT_OCC),
         OCC_PERC = paste0(OCC_CAT, "\n", DELTA_EMPSTAT_OCC_PERC)) %>%
  ungroup() %>%
  filter(EMPSTAT == 'At work & Has job, not at work last week' & DATE_DIFF == '20.03 -> 20.04') %>%
  filter(OCC_CAT != 'No Occupation')

# Plot
plot_occ <- ggplot(data = cps_emp_occ, aes(x = reorder(OCC_CAT, DELTA_EMPSTAT_OCC), y = DELTA_EMPSTAT_OCC, fill = INCWAGE)) + 
  geom_bar(stat = 'identity', position = 'stack', color = '#dad7cb', width = 0.4) +
  labs(y = '% Difference in Employment', x = '', fill = '', 
       title = '% Difference in employment between February and April 2020 by occupation') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(position = 'top') + 
  geom_text(aes(label = scales::percent(DELTA_EMPSTAT_OCC), fontface = 2), position = position_dodge(width = 1), hjust = 1.5, size = 3, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool', discrete = FALSE, name = 'Ave. Wage 2018') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.2, 0.9), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text( size = 10, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#F9F6EF', color = '#F9F6EF'),
        plot.background = element_rect(fill = '#F9F6EF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5, label.position = 'bottom')) +
  coord_flip()
plot_occ

#ggsave('Empl_Occ.png', plot = plot_occ, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 35, height = 25,  units = c('cm'))


#############################
#          EITC
#############################

plot_eitc <- ggplot(data = cps_emp_occ, aes(x = EITCRED, y = DELTA_EMPSTAT_OCC, color = reorder(OCC_CAT, INCWAGE), size = OCC_CAT_N)) + 
  geom_point() +
  labs(y = '% Difference in Employment', x = 'Ave. EITC in USD', color = '', size = '# of People Employed',
       title = 'EITC 2019 vs % Difference in employment between February and April 2020 by occupation') + 
  scale_size(range = c(3,15)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text_repel(aes(label = OCC_PERC),  fontface = 'bold',  size = 3, lineheight = .7, color = '#2F2424', vjust = -.1, hjust = .1) +
  scale_color_stanford(palette = 'cool') +
  geom_hline(yintercept = 0, linetype = 'dotted', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "vertical",  legend.position = c(0.1, 0.15), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text( size = 10, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#F9F6EF', color = '#F9F6EF'),
        plot.background = element_rect(fill = '#F9F6EF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(color = FALSE )
plot_eitc

#ggsave('EITC_Occ.png', plot = plot_eitc, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 35, height = 20,  units = c('cm'))



##------------------------------------------------------------#
# 2. Employment status (EMPSTAT) in March/Apr 2020 by OCC_ROUT 
##------------------------------------------------------------#

##############################################
# Distribution Routine jobs by wage quantile
##############################################

cps_dist <- cps  %>%
  filter(OCC_CAT != 'No Occupation') %>%
  filter(EMPSTAT == 10 | EMPSTAT == 12) %>%
  group_by(OCC_ROUT) %>%
  #  mutate(W_AVE = mean(INCWAGE, na.rm = TRUE)) %>%
  mutate(W_AVE = ifelse(OCC_ROUT == 'Non-Routine Cognitive', 1,
                        ifelse(OCC_ROUT == 'Routine Cognitive', 2,
                               ifelse(OCC_ROUT == 'Routine Manual', 3, 4)))) %>%
  ungroup()

dist_rout <- ggplot(data = cps_dist, aes(x = INCWAGE, color = reorder(OCC_ROUT, W_AVE), fill = reorder(OCC_ROUT, W_AVE))) + 
  geom_density(alpha = 0.2) +
  xlim(0, 200000) +
  labs(y = 'Density', x = 'Annual Wage USD', color = 'Occupational Categories', fill = 'Occupational Categories',
       title = 'Wage Distribution by Occupation') + 
  scale_color_stanford(palette = 'cool reverse') +
  scale_fill_stanford(palette = 'cool reverse') +
  theme_minimal() +
  theme(legend.direction = 'vertical', legend.position = c(0.9, 0.9), 
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text( size = 10, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#F9F6EF', color = '#F9F6EF'),
        plot.background = element_rect(fill = '#F9F6EF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) 
dist_rout

#ggsave('W_Dist_Rout.png', plot = dist_rout, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 20, height = 15,  units = c('cm'))

##############################################
# % Employment Difference by wage quantile
##############################################

cps_emp <- cps %>%
  filter(DATE == '2020-02-01' | DATE == '2020-03-01' | DATE == '2020-04-01') %>%
  filter(LABFORCE == 2) %>% # only people in the labour force P.S. Are we interested in Armed Forces???
  # Re-code Employment status in order to have only two categories (Emp vs Unemp)
  mutate(EMPSTAT = ifelse(EMPSTAT == 10 | EMPSTAT == 12, 'At work & Has job, not at work last week', 'Unemployed')) %>% 
  group_by(DATE) %>%
  mutate(TOT_EMPSTAT = length(EMPSTAT)) %>%
  ungroup() %>%
  group_by(DATE, OCC_ROUT, EMPSTAT) %>%
  summarize(N_EMPSTAT    = length(EMPSTAT),
            PERC_EMPSTAT = round(mean(N_EMPSTAT / TOT_EMPSTAT), digits = 2)) %>%
  ungroup() %>%
  group_by(OCC_ROUT, EMPSTAT) %>%
  mutate(DELTA_EMPSTAT = PERC_EMPSTAT - dplyr::lag(PERC_EMPSTAT, 2, order_by = DATE)) %>% # create delta variation in percentage of people employed vs unemployed
  filter(DATE == '2020-04-01') %>%
  ungroup()


cps_emp_rout <- cps %>%
  filter(DATE == '2020-02-01' | DATE == '2020-03-01' | DATE == '2020-04-01') %>%
  filter(LABFORCE == 2) %>% # only people in the labour force P.S. Are we interested in Armed Forces???
  # Re-code Employment status in order to have only two categories (Emp vs Unemp)
  mutate(EMPSTAT = ifelse(EMPSTAT == 10 | EMPSTAT == 12, 'At work & Has job, not at work last week', 'Unemployed')) %>% 
  group_by(OCC_ROUT) %>%
  mutate(INCWAGEQUANT = ifelse(INCWAGE <= quantile(INCWAGE, probs = c(.25), na.rm = TRUE), round(quantile(INCWAGE, probs = c(.25), na.rm = TRUE)),
                               ifelse(INCWAGE >= quantile(INCWAGE, probs = c(.25), na.rm = TRUE) & INCWAGE <= quantile(INCWAGE, probs = c(.5), na.rm = TRUE), round(quantile(INCWAGE, probs = c(.5), na.rm = TRUE)),
                                      ifelse(INCWAGE >= quantile(INCWAGE, probs = c(.5), na.rm = TRUE) & INCWAGE <= quantile(INCWAGE, probs = c(.75), na.rm = TRUE), round(quantile(INCWAGE, probs = c(.75), na.rm = TRUE)),
                                             ifelse(INCWAGE >= quantile(INCWAGE, probs = c(.75), na.rm = TRUE), round(quantile(INCWAGE, probs = c(.95), na.rm = TRUE)), NA)))), 
         INCWAGEQUANT_CODE = ifelse(INCWAGE <= quantile(INCWAGE, probs = c(.25), na.rm = TRUE), '4th',
                                    ifelse(INCWAGE >= quantile(INCWAGE, probs = c(.25), na.rm = TRUE) & INCWAGE <= quantile(INCWAGE, probs = c(.5), na.rm = TRUE), '3rd',
                                           ifelse(INCWAGE >= quantile(INCWAGE, probs = c(.5), na.rm = TRUE) & INCWAGE <= quantile(INCWAGE, probs = c(.75), na.rm = TRUE), '2nd',
                                                  ifelse(INCWAGE >= quantile(INCWAGE, probs = c(.75), na.rm = TRUE), '1st', NA))))) %>%
  ungroup() %>%
  group_by(DATE, OCC_ROUT) %>%
  mutate(TOT_EMPSTAT_OCC = length(EMPSTAT))  %>%
  ungroup() %>%
  group_by(DATE, EMPSTAT, OCC_ROUT, INCWAGEQUANT, INCWAGEQUANT_CODE) %>%
  summarize(N_EMPSTAT_OCC    = length(EMPSTAT),
            PERC_EMPSTAT_OCC = round(mean(N_EMPSTAT_OCC / TOT_EMPSTAT_OCC), digits = 3),
            INCWAGE = mean(INCWAGE, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(EMPSTAT, OCC_ROUT, INCWAGEQUANT_CODE) %>%
  mutate(DELTA_EMPSTAT_OCC = PERC_EMPSTAT_OCC - dplyr::lag(PERC_EMPSTAT_OCC, 2, order_by = DATE), # create delta variation in percentage of people employed vs unemployed
         DATE_DIFF = ifelse(DATE == '2020-04-01', '20.02 -> 20.04', NA)) %>%
  ungroup() %>%
  filter(EMPSTAT == 'At work & Has job, not at work last week' & DATE_DIFF == '20.02 -> 20.04') %>%
  filter(OCC_ROUT != 'No Occupation') %>%
  filter(!is.na(INCWAGEQUANT))

cps_emp_rout <- filter(cps_emp_rout, EMPSTAT == 'At work & Has job, not at work last week' & DATE_DIFF == '20.02 -> 20.04') %>%
  left_join(.,cps_emp) %>%
  group_by(OCC_ROUT) %>%
  mutate(DELTA_EMPSTAT2 = sum(DELTA_EMPSTAT, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(DELTA_EMPSTAT_OCC1 = ((DELTA_EMPSTAT_OCC * DELTA_EMPSTAT) / DELTA_EMPSTAT2)) 

plot_rout <- ggplot(data = arrange(cps_emp_rout, -DELTA_EMPSTAT_OCC) , aes(x = reorder(OCC_ROUT, DELTA_EMPSTAT_OCC), y = DELTA_EMPSTAT_OCC1, fill = INCWAGEQUANT)) + 
  geom_bar(stat = 'identity',width = 0.4) +
  labs(y = '% Total Difference in Employment', x = 'Occupation Category', fill = 'Income Quartiles', 
       title = '% Difference in employment between February and April 2020 by wage quartiles and occupational category') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(position = 'top') + 
  geom_text(aes(label = scales::percent(DELTA_EMPSTAT_OCC, accuracy = .1), fontface = 2), position = position_stack(0.5), size = 3, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool', discrete = FALSE) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.2, 0.9), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text( size = 10, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#F9F6EF', color = '#F9F6EF'),
        plot.background = element_rect(fill = '#F9F6EF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5, label.position = 'bottom', reverse = FALSE)) +
  coord_flip()
plot_rout

#ggsave('Empl_Rout.png', plot = plot_rout, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 20, height = 15,  units = c('cm'))


##############################################
# Hours Worked status (EMPSTAT) in March/Apr 2020 by OCC_CAT
##############################################

# Average Employed working hrs & unemployed working hrs
cps_hrs_occ <- cps %>%
  filter(DATE == '2020-02-01' | DATE == '2020-03-01' | DATE == '2020-04-01') %>%
  filter(LABFORCE == 2) %>% # only people in the labour force P.S. Are we interested in Armed Forces???
  mutate(UHRSWORKT = ifelse(is.na(UHRSWORKT), 0 , UHRSWORKT)) %>% # 10 At work, 12 Has job, not at work last week,  21 Unemployed, experienced worker, 22 Unemployed, new worker    
  group_by(DATE, OCC_CAT) %>%
  summarize(TOT_HRS_OCC = mean(UHRSWORKT, na.rm = TRUE),
            INCWAGE = mean(INCWAGE, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(OCC_CAT) %>%
  mutate(DELTA_HRS_OCC = round(TOT_HRS_OCC - dplyr::lag(TOT_HRS_OCC, 2, order_by = DATE), digits = 1)) %>%
  ungroup() %>%
  filter(DATE ==  '2020-04-01') %>%
  filter(OCC_CAT != 'No Occupation')

plot_hrs <- ggplot(data = cps_hrs_occ, aes(x = reorder(OCC_CAT, DELTA_HRS_OCC), y = DELTA_HRS_OCC, fill = INCWAGE)) + 
  geom_bar(stat = 'identity', position = 'dodge', color = '#dad7cb', width = 0.4) +
  labs(y = 'Difference in hours usually worked per week at all jobs', x = '', fill = '', 
       title = 'Difference in Hours usually worked per week at all jobs between February and April 2020 by occupation (incl. unemployed with hrs = 0)') + 
  scale_x_discrete(position = 'top') + 
  geom_text(aes(label = DELTA_HRS_OCC, fontface = 2), position = position_dodge(width = 1), hjust = 1.5, size = 3, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool', discrete = FALSE, name = 'Ave. Wage 2018') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.2, 0.9), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text( size = 10, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#F9F6EF', color = '#F9F6EF'),
        plot.background = element_rect(fill = '#F9F6EF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5)) +
  coord_flip()
plot_hrs

#ggsave('Hrs_Occ.png', plot = plot_hrs, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 35, height = 25,  units = c('cm'))



# Average Employed working hrs & unemployed working hrs
cps_hrs_occ1 <- cps %>%
  filter(DATE == '2020-02-01' | DATE == '2020-03-01' | DATE == '2020-04-01') %>%
  filter(LABFORCE == 2) %>% # only people in the labour force 
  mutate(EMPSTAT = ifelse(EMPSTAT == 10 | EMPSTAT == 12, 'At work & Has job, not at work last week', 'Unemployed')) %>% 
  filter(!(DATE == '2020-04-01' & EMPSTAT == 'Unemployed')) %>%
  mutate(UHRSWORKT = ifelse(is.na(UHRSWORKT), 0 , UHRSWORKT)) %>%     
  group_by(DATE, OCC_CAT) %>%
  summarize(TOT_HRS_OCC = mean(UHRSWORKT, na.rm = TRUE),
            INCWAGE = mean(INCWAGE, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(OCC_CAT) %>%
  mutate(DELTA_HRS_OCC = round(TOT_HRS_OCC - dplyr::lag(TOT_HRS_OCC, 2, order_by = DATE), digits = 1)) %>%
  ungroup() %>%
  filter(DATE ==  '2020-04-01') %>%
  filter(OCC_CAT != 'No Occupation')

plot_hrs_emp <- ggplot(data = cps_hrs_occ1, aes(x = reorder(OCC_CAT, DELTA_HRS_OCC), y = DELTA_HRS_OCC, fill = INCWAGE)) + 
  geom_bar(stat = 'identity', position = 'dodge', color = '#dad7cb', width = 0.4) +
  labs(y = 'Difference in hours usually worked per week at all jobs', x = '', fill = '', 
       title = 'Difference in Hours usually worked per week at all jobs between February and April 2020 by occupation among employed only') + 
  scale_x_discrete(position = 'top') + 
  geom_text(aes(label = DELTA_HRS_OCC, fontface = 2), position = position_dodge(width = 1), hjust = 1.5, size = 3, color = '#2F2424') +
  scale_fill_stanford(palette = 'cool', discrete = FALSE, name = 'Ave. Wage 2018') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#2F2424') +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = c(0.7, 0.1), legend.box = "vertical",
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text( size = 10, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#F9F6EF', color = '#F9F6EF'),
        plot.background = element_rect(fill = '#F9F6EF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = 'bottom', title.hjust = 0.5)) +
  coord_flip()
plot_hrs_emp

#ggsave('Hrs_Occ_Emp.png', plot = plot_hrs_emp, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 35, height = 25,  units = c('cm'))

