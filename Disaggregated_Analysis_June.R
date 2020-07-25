##-----------------------------------------------------------------------------------------------#
## Date:         05-17-2020
## Author:       Alice Milivinti
## Institution:  PHS, Stanford
##
## Project: 'EICT & COVID-19'
## R-codes: 'Disaggregated Analysis'
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
library(TraMineR)
library(srvyr)

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
                          `ground reverse` = stanford_cols('sandstone', 'chocolate'),
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
# Due to the 4-8-4 design we have longitudinal information for people who were first surveyed in 2018/12 & 2019/01 & 2019/02 & 2019/03
# 1. Annual Socio-Economic Supplement (ASEC) for 2019 (earnings, eitc, etc)
# 2. CPS for Jan-Apr 2019 & Jan-Apr 2020 for individual (work) and household information (geo)
# HWTFINL is a household-level weight that should be used to generate statistics about households. 
# The CPS uses a complex stratified sampling scheme, and HWTFINL must be used to produce unbiased household-level statistics
# from IPUMS-CPS basic monthly samples. For analyses of March Annual Social and Economic (ASEC) data, researchers should use HWTSUPP.
# For individual-level analyses, researchers should use WTFINL, WTSUPP, or EARNWT.

# Data: 20192020 
#cps_ddi_ind <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/Core_2019_2020.xml' # Metadata
#cps_data_ind <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/Core_2019_2020.dat' # Data
cps_ddi_ind <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/Core_01.2019_06.2020.xml' # Metadata
cps_data_ind <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/Core_01.2019_06.2020.dat' # Data
cps_ddi_ind <- read_ipums_ddi(cps_ddi_ind)
cps_ind <- read_ipums_micro(cps_ddi_ind, data_file = cps_data_ind)
cps_ind <- cps_ind %>%
#  filter(ASECFLAG == 2) %>% # filter only respondants who are also part of the ASEC
  select(-ASECFLAG, -SERIAL, -PERNUM) %>%
  mutate(DATE = make_date(YEAR, MONTH))

# How to use the metadata
#ipums_var_desc(cps_ddi_ind, EMPSTAT) # Variable Description
#ipums_var_label(cps_ddi_ind, EMPSTAT) # Variable Label
#ipums_val_labels(cps_ddi_ind, UHRSWORKT) # Variable Labels

# Annual Socio-Economic Supplement (ASEC) 2019
cps_ddi_asec <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/ASEC_2019.xml'
cps_data_asec <- '/media/alice/TOSHIBA EXT/PostDoc/UBI_EITC/Data/CPS/Data/ASEC_2019.dat'
cps_ddi_asec <- read_ipums_ddi(cps_ddi_asec) 
cps_asec <- read_ipums_micro(cps_ddi_asec, data_file = cps_data_asec)
cps_asec <- cps_asec %>%
  filter(ASECOVERH != 1) %>%
  select(-YEAR, -MONTH, -REGION, -STATEFIP, -METRO, -METAREA, -COUNTY, -STATECENSUS, -CBSASZ, -METFIPS)

##-----------------------------------------------------------------------------------------------#
## Data Management
##-----------------------------------------------------------------------------------------------#
# Wave April 2020 
# For those who did not work at all during the survey reference week of April 12–18, if a person indicated they were under quarantine or self-isolating due to health concerns, the interviewer should select “own illness, injury, or medical problem.”
# To be classified as unemployed on temporary layoff, a person has either been given a date to return to work by their employer or expects   to be recalled to their job within 6 months. 

# Merge Monthly CPS and ASEC  
cps <- cps_ind %>%
  left_join(., cps_asec, by = c("CPSID", "CPSIDP")) %>%
  filter(!is.na(ASECOVERH))

# Filter a subsample of individuals in the labor force observed in (ASEC) March 2019 & Apr 2020
cps_20_06 <- cps %>%
  filter(DATE == '2020-06-01') 

# Subsample of individuals and households in observed in (ASEC) March 2019 & June 2020
length(unique(cps_20_06$CPSIDP)) # 10,141
length(unique(cps_20_06$CPSID)) # 4,327

cps_20_06 <- cps_20_06 %>%
  filter(LABFORCE == 2) %>% # filter only people in the labour force
  mutate(EMPSTAT = ifelse(EMPSTAT == 10 | EMPSTAT == 12, 'At work & Has job, not at work last week', 'Unemployed ~16.4%'), # re-code the employment status variable into a dicot.
         EITCRED = ifelse(EITCRED == 9999, NA, EITCRED)) 

# Subsample of individuals and households in the labor force observed in (ASEC) March 2019 & June 2020
length(unique(cps_20_06$CPSIDP)) # 4,797
length(unique(cps_20_06$CPSID)) # 2,947

# Quick EITC Sample Analysis
cps_20_06 %>%
  summarize(eitc_pct = weighted.mean(EITCRED > 0, ASECWTH, na.rm = TRUE))
# ~ 7% of the sample is EITC recipients

cps_20_06 <- filter(cps_20_06, EITCRED > 0) 
# Subsample of individuals and households in the labor force observed in (ASEC) March 2019 & June 2020
length(unique(cps_20_06$CPSIDP)) # 401
length(unique(cps_20_06$CPSID)) # 383

# Histogram 
options('scipen' = 999, 'digits' = 10)

hist_eitc <- ggplot(data =  cps_20_06,  # Filter for individuals whi have receiver EITC > 0
                    aes(x = EITCRED, fill = EMPSTAT, weight = ASECWTH)) + 
  geom_histogram(bins = 15) + 
  labs(y = '', x = '', fill = 'Employment Status',
       caption = 'Households Sample Size: 383 Weight adjusted.') + 
  scale_fill_stanford(palette = 'earth reverse') +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.9), 
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#ffffff', color = '#ffffff'),
        plot.background = element_rect(fill = '#ffffff'), 
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) 
hist_eitc

ggsave('EITC_Emp_Sub_W.png', plot = hist_eitc, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 25, height = 18,  units = c('cm'))

# Quick EITC Sample Analysis
filter(cps_20_06, EITCRED > 0) %>%
  summarize(emp_pct = weighted.mean(EMPSTAT == 'Unemployed ~16.4%', ASECWTH))
# ~16.4% individuals who received EITC in 2019 are unemployed in June 2020.
filter(cps_20_06, EITCRED > 0 & EMPSTAT == 'Unemployed ~16.4%') %>%
  summarize(n_eitc = sum(ASECWTH))
# 110,931

# Compare with 2019-06
cps %>%
  filter(DATE == '2019-06-01') %>%  # Filter a subsample of individuals in the labor force observed in (ASEC) March 2019 & Apr 2019
  filter(EITCRED > 0) %>%
  summarize(emp_pct = weighted.mean(EMPSTAT == 'Unemployed ~16.4%', ASECWTH))
# ~4.7% individuals who received EITC in 2019 were unemployed in June 2019.

# Representiative Household
cps_eitc_2c <- cps %>%
 # filter(DATE == '2019-04-01') %>%  # Filter a subsample of individuals in the labor force observed in (ASEC) March 2019 & Apr 2019
  filter(EITCRED > 0 & NCHILD == 2) %>%
  summarize(emp_pct = weighted.mean(EITCRED, ASECWTH, na.rm = T))


filter(cps, EITCRED > 0) %>%
  summarize(n_eitc = sum(ASECWTH))

filter(cps, EITCRED > 0 & EMPSTAT == 'Unemployed ~16.4%') %>%
  summarize(n_eitc = sum(ASECWTH))

  
# Analysis of childless lower income workers

cps_childless <- cps %>%
  filter(NCHILD == 0 & FAMSIZE == 1 & EITCRED > 0) %>%
  summarize(inc = weighted.mean(INCTOT, ASECWT, na.rm = T),
            eitc = weighted.mean(EITCRED, ASECWT, na.rm = T))



