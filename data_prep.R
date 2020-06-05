## DATA PREP ##

library(tidyverse)
library(cowpoke)
library(lubridate)
library(haven)
library(here)

rep_nums = function(num, times_seq) {
  sapply(times_seq, (function(x) rep(num, x) %>% paste(collapse = "") %>% as.numeric()))
}

fix_num_nas = function(vect) {
  max_digits = suppressWarnings(vect %>% unique() %>% as.numeric() %>% max() %>% (function(x) floor(log10(x)) + 1))
  if (is.finite(max_digits) & max_digits > 0) {
    na_vals = vect %in% c(rep_nums(9, max_digits), rep_nums(8, max_digits))
    vect[na_vals] = NA
  }
  vect
}

raw_data = read_dta(here("us_pretrial_release_data.dta"))

processed_data = raw_data %>% mutate_all(fix_num_nas) %>% mutate_if(is.labelled, as_factor) %>% mutate_all(fix_nas, na_strs = c("", " ", ".", "Not applicable", "Blank", "-9"))

saveRDS(processed_data, here("cleaned_data.rds"))