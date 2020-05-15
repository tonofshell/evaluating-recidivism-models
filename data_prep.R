## DATA PREP ##

library(tidyverse)
library(cowpoke)
library(lubridate)
library(here)

raw_data = read_csv(here("compas-scores-two-years.csv"))

can_be_logical = function(x) {
  x = as.character(x) %>% str_to_lower()
  unique(x) %in% c("0", "1", "t", "f", "true", "false") %>% all()
}

analysis_data = raw_data %>% 
  select(c(id, sex, age, age_cat, race, juv_fel_count, juv_misd_count, juv_other_count, priors_count, days_b_screening_arrest, c_offense_date, c_charge_degree, c_charge_desc, is_recid, is_violent_recid, decile_score_1, score_text, v_decile_score, v_score_text, two_year_recid)) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  select(-days_b_screening_arrest) %>% 
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A') %>% 
  mutate(c_offense_date = ymd(c_offense_date), 
         offense_year = year(c_offense_date), 
         offense_month = month(c_offense_date), 
         offense_day_week = wday(c_offense_date), 
         offense_day_month = day(c_offense_date), 
         offense_day_year = yday(c_offense_date),
         age_cat = factor(age_cat, c("Less than 25", "25 - 45", "Greater than 45"), ordered = TRUE),
         score_text = factor(score_text, c("Low", "Medium", "High"), ordered = TRUE), 
         v_score_text = factor(v_score_text, c("Low", "Medium", "High"), ordered = TRUE)) %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(can_be_logical, as.logical)

saveRDS(analysis_data, here("cleaned_data.rds"))
