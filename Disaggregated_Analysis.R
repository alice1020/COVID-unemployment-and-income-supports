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
# Due to the 4-8-4 design we have longitudinal information for people who were first surveyed in 2018/12 & 2019/01 & 2019/02 & 2019/03
# 1. Annual Socio-Economic Supplement (ASEC) for 2019 (earnings, eitc, etc)
# 2. CPS for Jan-Apr 2019 & Jan-Apr 2020 for individual (work) and household information (geo)

# Data: 20192020 
cps_ddi_ind <- '/media/alice/TOSHIBA EXT/CPS/Data/CPS_19_20.xml' # Metadata
cps_data_ind <- '/media/alice/TOSHIBA EXT/CPS/Data/CPS_19_20.dat' # Data
cps_ddi_ind <- read_ipums_ddi(cps_ddi_ind)
cps_ind <- read_ipums_micro(cps_ddi_ind, data_file = cps_data_ind)
cps_ind <- cps_ind %>%
  mutate(DATE = make_date(YEAR, MONTH)) %>%
  select(DATE, CPSIDP, CPSID, COUNTY, STATECENSUS, STATEFIP, REGION, CBSASZ, LABFORCE, OCC, EDUC99, FAMINC, EMPSTAT, UHRSWORKT) 

# How to use the metadata
#ipums_var_desc(cps_ddi_ind, EMPSTAT) # Variable Description
#ipums_var_label(cps_ddi_ind, EMPSTAT) # Variable Label
#ipums_val_labels(cps_ddi_ind, UHRSWORKT) # Variable Labels

# Annual Socio-Economic Supplement (ASEC) 2019
cps_ddi_asec <- '/media/alice/TOSHIBA EXT/CPS/Data/ASEC_2019.xml'
cps_data_asec <- '/media/alice/TOSHIBA EXT/CPS/Data/ASEC_2019.dat'
cps_ddi_asec <- read_ipums_ddi(cps_ddi_asec) 
cps_asec <- read_ipums_micro(cps_ddi_asec, data_file = cps_data_asec)
cps_asec <- cps_asec %>%
  select(CPSIDP, CPSID, COUNTY, STATECENSUS, STATEFIP, REGION, CBSASZ, OCCLY, UHRSWORKLY, INCTOT, INCWAGE, INCLONGJ, OINCBUS, OINCFARM, OINCWAGE, EITCRED) %>%
  rename(OCC = OCCLY)

# Merge Monthly CPS and ASEC  
cps <- left_join(cps_ind, cps_asec, by = c("CPSIDP", "CPSID", "COUNTY", "STATECENSUS", "STATEFIP", "REGION", "CBSASZ", "OCC")) %>%
  separate(CPSID, into = c('FIRST_CPS', 'REST_ID'), sep = 6, remove = F) # Separate the first time interviewed

##-----------------------------------------------------------------------------------------------#
## Data Management
##-----------------------------------------------------------------------------------------------#
# Wave April 2020 
# For those who did not work at all during the survey reference week of April 12–18, if a person indicated they were under quarantine or self-isolating due to health concerns, the interviewer should select “own illness, injury, or medical problem.”
# To be classified as unemployed on temporary layoff, a person has either been given a date to return to work by their employer or expects   to be recalled to their job within 6 months. 

cps <- cps %>%
  filter(FIRST_CPS != 201804 & FIRST_CPS != 201805 & FIRST_CPS != 201806)  # remove if first wave survey 2018-04 2018-05 2018-06

##-----------------------------------------------------------------------------------------------#
## Descriptive Statistics
##-----------------------------------------------------------------------------------------------#

#########################################
# Sequence Analysis Individual Level
#########################################

# Structure the data for sequence visualization 
cps_wide <- cps %>%
  select(DATE, CPSIDP, EMPSTAT) %>%
  spread(DATE, EMPSTAT) %>%
  mutate(`2019-07-01` = NA,
         `2019-08-01` = NA,
         `2019-09-01` = NA,
         `2019-10-01` = NA,
         `2019-11-01` = NA) %>%
  select(CPSIDP, `2019-01-01`, `2019-02-01`, `2019-03-01`, `2019-04-01`, `2019-05-01`, `2019-06-01`, `2019-07-01`, `2019-08-01`, `2019-09-01`,
         `2019-10-01`, `2019-11-01`, `2019-12-01`, `2020-01-01`, `2020-02-01`, `2020-03-01`, `2020-04-01`)

cps_seq <- seqdef(cps_wide, 2:17, void = '*')

sequi_empstat <- seqiplot(cps_seq, idxs = 0, border = NA, space = 0, with.legend = FALSE) 
#Draw the sequence frequency plot of the 10 most frequent sequences with bar width proportional to the frequencies. Result in Fig. 1(c).
seqf_empstat <- seqfplot(cps_seq, pbarw = T, with.legend = F) 
#Plot the state distribution by time points. Result in Fig. 1(d).
seqd_empstat <- seqdplot(cps_seq, with.legend = F)
seqlegend(cps_seq, cex = 0.8) 


cps_sub <- select(cps, DATE, YEAR, COUNTY, STATECENSUS, STATEFIP, EMPSTAT, OCC, OCC2010, UHRSWORKT, INCTOT, INCWAGE, INCLONGJ, OINCBUS, OINCFARM, OINCWAGE, EITCRED) %>%
  filter(UHRSWORKT == 997 | UHRSWORKT == 999)
table(cps_sub$DATE)
cps_sub_20 <- filter(cps_sub, YEAR == 2020)
table(cps_sub_20$OCC2010)
table(cps_sub_20$EMPSTAT)


#########################################
# Londitudinal Analysis Individual Level
#########################################

# Merge Monthly CPS and ASEC  
cps_20_04 <- left_join(cps_asec, cps_ind,  by = c("CPSIDP", "CPSID", "COUNTY", "STATECENSUS", "STATEFIP", "REGION", "CBSASZ", "OCC")) %>%
  separate(CPSID, into = c('FIRST_CPS', 'REST_ID'), sep = 6, remove = F) %>% # Separate the first time interviewed
  rename(UHRSWORKLY2019 = UHRSWORKLY) %>%
  select(DATE, CPSIDP, CPSID, FIRST_CPS, REST_ID, COUNTY, STATECENSUS, STATEFIP, REGION, CBSASZ, EDUC99, OCC, LABFORCE, EMPSTAT, UHRSWORKT,
         FAMINC, UHRSWORKLY2019, INCTOT, INCWAGE, INCLONGJ, OINCBUS, OINCFARM, OINCWAGE, EITCRED)

cps_20_04 <- cps_20_04 %>%
  # Filter a subsample of individuals in the labor force observed in (ASEC) March 2019 & Apr 2020
  filter(DATE == '2020-04-01') %>%
  filter(LABFORCE == 2) %>%
  mutate(EMPSTAT = ifelse(EMPSTAT == 10 | EMPSTAT == 12, 'At work & Has job, not at work last week', 'Unemployed, ~17%'),
         EITCRED = ifelse(EITCRED == 9999, 0, EITCRED)) 
# Sample Size Household CPSID: 3,984

# Filter for individuals whi have receiver EITC > 0
cps_20_04_sub <- filter(cps_20_04, EITCRED > 0)
# ~9.3% of the sample households have received EITC

options('scipen' = 999, 'digits' = 10)

hist_eitc <- ggplot(data =  cps_20_04_sub, 
                    aes(x = EITCRED, fill = EMPSTAT)) + 
  geom_histogram() + 
  labs(y = '', x = '', fill = 'Employment Status',
       caption = 'Individual Sample Size: 370. Households Sample Size: 363') + 
  scale_fill_stanford(palette = 'cool reverse') +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.9), 
        axis.ticks = element_blank(),
        axis.text = element_text(color = '#2F2424'),
        text = element_text(size = 14, color = '#2F2424', family = 'Source Sans Pro'),
        panel.background = element_rect(fill = '#F9F6EF', color = '#F9F6EF'),
        plot.background = element_rect(fill = '#F9F6EF'),
        legend.background = element_blank(),
        panel.grid = element_line(colour = '#dad7cb')) 
hist_eitc

ggsave('EITC_Emp_Sub.png', plot = hist_eitc, path = '/home/alice/Dropbox/PostDoc/UBI_EITC/', width = 25, height = 18,  units = c('cm'))


nrow(filter(cps_20_04_sub, EMPSTAT == 'Unemployed')) / nrow(cps_20_04_sub)
# 62 out of 370 (~17%) individuals who received EITC in 2019 is unemployed in April 2020.

