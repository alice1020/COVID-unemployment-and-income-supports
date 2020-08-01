
# Create Dataframe for EITC

library(tidyverse)
library(rjson)

eitc_param <- fromJSON(file = "/home/alice/Dropbox/PostDoc/UBI_EITC/COVID-unemployment-and-income-supports/Value_of_Federal_Earned_Income_Tax_Credit_2019.json")
eitc_param <- as.data.frame(eitc_param)

Income_level <- 1:60000
data <- as.data.frame(Income_level)
data <- data %>%
  mutate(Single_no_children = ifelse(Income_level > 15570, 0,
                              ifelse(Income_level <= 15570 & Income_level > eitc_param$baseline.0.begin_phaseout,
                                    (Income_level * (-eitc_param$baseline.0.phase_out_rate)) + ((eitc_param$baseline.0.earned_income_base_amount*eitc_param$baseline.0.phase_in_rate) + (eitc_param$baseline.0.phase_out_rate * eitc_param$baseline.0.begin_phaseout)), 
                              ifelse(Income_level <= eitc_param$baseline.0.begin_phaseout & Income_level > eitc_param$baseline.0.earned_income_base_amount, eitc_param$baseline.0.earned_income_base_amount*eitc_param$baseline.0.phase_in_rate,
                                     Income_level*eitc_param$baseline.0.phase_in_rate))),
         Married_no_children = ifelse(Income_level > (15570 + eitc_param$baseline.0.marriage_penalty_relief), 0,
                               ifelse(Income_level <= (15570 + eitc_param$baseline.0.marriage_penalty_relief) & Income_level > (eitc_param$baseline.0.begin_phaseout + eitc_param$baseline.0.marriage_penalty_relief),
                                     (Income_level * (-eitc_param$baseline.0.phase_out_rate)) + ((eitc_param$baseline.0.earned_income_base_amount*eitc_param$baseline.0.phase_in_rate) + (eitc_param$baseline.0.phase_out_rate * (eitc_param$baseline.0.begin_phaseout + eitc_param$baseline.0.marriage_penalty_relief))), 
                               ifelse(Income_level <= (eitc_param$baseline.0.begin_phaseout + eitc_param$baseline.0.marriage_penalty_relief ) & Income_level > eitc_param$baseline.0.earned_income_base_amount, eitc_param$baseline.0.earned_income_base_amount*eitc_param$baseline.0.phase_in_rate,
                                      Income_level*eitc_param$baseline.0.phase_in_rate))),
         Single_one_child = ifelse(Income_level > 41094, 0,
                            ifelse(Income_level <= 41094 & Income_level > eitc_param$baseline.1.begin_phaseout,
                                   (Income_level * (-eitc_param$baseline.1.phase_out_rate)) + ((eitc_param$baseline.1.earned_income_base_amount*eitc_param$baseline.1.phase_in_rate) + (eitc_param$baseline.1.phase_out_rate * eitc_param$baseline.1.begin_phaseout)), 
                             ifelse(Income_level <= eitc_param$baseline.1.begin_phaseout & Income_level > eitc_param$baseline.1.earned_income_base_amount, eitc_param$baseline.1.earned_income_base_amount*eitc_param$baseline.1.phase_in_rate,
                             Income_level*eitc_param$baseline.1.phase_in_rate))),
         Married_one_child = ifelse(Income_level > (41094 + eitc_param$baseline.1.marriage_penalty_relief), 0,
                             ifelse(Income_level <= (41094 + eitc_param$baseline.1.marriage_penalty_relief) & Income_level > (eitc_param$baseline.1.begin_phaseout + eitc_param$baseline.1.marriage_penalty_relief),
                                    (Income_level * (-eitc_param$baseline.1.phase_out_rate)) + ((eitc_param$baseline.1.earned_income_base_amount*eitc_param$baseline.1.phase_in_rate) + (eitc_param$baseline.1.phase_out_rate * (eitc_param$baseline.1.begin_phaseout + eitc_param$baseline.1.marriage_penalty_relief))), 
                             ifelse(Income_level <= (eitc_param$baseline.1.begin_phaseout + eitc_param$baseline.1.marriage_penalty_relief ) & Income_level > eitc_param$baseline.1.earned_income_base_amount, eitc_param$baseline.1.earned_income_base_amount*eitc_param$baseline.1.phase_in_rate,
                             Income_level*eitc_param$baseline.1.phase_in_rate))),
         Single_two_children = ifelse(Income_level > 46703, 0,
                               ifelse(Income_level <= 46703 & Income_level > eitc_param$baseline.2.begin_phaseout,
                                     (Income_level * (-eitc_param$baseline.2.phase_out_rate)) + ((eitc_param$baseline.2.earned_income_base_amount*eitc_param$baseline.2.phase_in_rate) + (eitc_param$baseline.2.phase_out_rate * eitc_param$baseline.2.begin_phaseout)), 
                               ifelse(Income_level <= eitc_param$baseline.2.begin_phaseout & Income_level > eitc_param$baseline.2.earned_income_base_amount, eitc_param$baseline.2.earned_income_base_amount*eitc_param$baseline.2.phase_in_rate,
                               Income_level*eitc_param$baseline.2.phase_in_rate))),
         Married_two_children = ifelse(Income_level > (46703 + eitc_param$baseline.2.marriage_penalty_relief), 0,
                                ifelse(Income_level <= (46703 + eitc_param$baseline.2.marriage_penalty_relief) & Income_level > (eitc_param$baseline.2.begin_phaseout + eitc_param$baseline.2.marriage_penalty_relief),
                                     (Income_level * (-eitc_param$baseline.2.phase_out_rate)) + ((eitc_param$baseline.2.earned_income_base_amount*eitc_param$baseline.2.phase_in_rate) + (eitc_param$baseline.2.phase_out_rate * (eitc_param$baseline.2.begin_phaseout + eitc_param$baseline.2.marriage_penalty_relief))), 
                                ifelse(Income_level <= (eitc_param$baseline.2.begin_phaseout + eitc_param$baseline.2.marriage_penalty_relief ) & Income_level > eitc_param$baseline.2.earned_income_base_amount, eitc_param$baseline.2.earned_income_base_amount*eitc_param$baseline.2.phase_in_rate,
                                Income_level*eitc_param$baseline.2.phase_in_rate))),
         Single_three_children = ifelse(Income_level > 50162, 0,
                                 ifelse(Income_level <= 50162 & Income_level > eitc_param$baseline.3.begin_phaseout,
                                 (Income_level * (-eitc_param$baseline.3.phase_out_rate)) + ((eitc_param$baseline.3.earned_income_base_amount*eitc_param$baseline.3.phase_in_rate) + (eitc_param$baseline.3.phase_out_rate * eitc_param$baseline.3.begin_phaseout)), 
                                 ifelse(Income_level <= eitc_param$baseline.3.begin_phaseout & Income_level > eitc_param$baseline.3.earned_income_base_amount, eitc_param$baseline.3.earned_income_base_amount*eitc_param$baseline.3.phase_in_rate,
                                 Income_level*eitc_param$baseline.3.phase_in_rate))),
         Married_three_children = ifelse(Income_level > (50162 + eitc_param$baseline.3.marriage_penalty_relief), 0,
                                  ifelse(Income_level <= (50162 + eitc_param$baseline.3.marriage_penalty_relief) & Income_level > (eitc_param$baseline.3.begin_phaseout + eitc_param$baseline.3.marriage_penalty_relief),
                                        (Income_level * (-eitc_param$baseline.3.phase_out_rate)) + ((eitc_param$baseline.3.earned_income_base_amount*eitc_param$baseline.3.phase_in_rate) + (eitc_param$baseline.3.phase_out_rate * (eitc_param$baseline.3.begin_phaseout + eitc_param$baseline.3.marriage_penalty_relief))), 
                                  ifelse(Income_level <= (eitc_param$baseline.3.begin_phaseout + eitc_param$baseline.3.marriage_penalty_relief ) & Income_level > eitc_param$baseline.3.earned_income_base_amount, eitc_param$baseline.3.earned_income_base_amount*eitc_param$baseline.3.phase_in_rate,
                                  Income_level*eitc_param$baseline.3.phase_in_rate))))




ggplot(data = data) +
  geom_line(aes(x = Income_level, y = Single_no_children)) +
  geom_line(aes(x = Income_level, y = Married_no_children)) +
  geom_line(aes(x = Income_level, y = Single_one_child)) +
  geom_line(aes(x = Income_level, y = Married_one_child)) +
  geom_line(aes(x = Income_level, y = Single_two_children)) +
  geom_line(aes(x = Income_level, y = Married_two_children)) +
  geom_line(aes(x = Income_level, y = Single_three_children)) +
  geom_line(aes(x = Income_level, y = Married_three_children)) 

  
write.csv(data, '/home/alice/Dropbox/PostDoc/UBI_EITC/COVID-unemployment-and-income-supports/Value_of_Federal_Earned_Income_Tax_Credit_2019.csv')

