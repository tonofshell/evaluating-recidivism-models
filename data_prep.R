## DATA PREP ##

library(tidyverse)
library(cowpoke)
library(lubridate)
library(haven)
library(here)

rep_nums = function(num, times_seq) {
  sapply(times_seq, (function(x) rep(num, x) %>% paste(collapse = "") %>% as.numeric()))
}

fix_num_nas = function(vect, rep_digit = 9, na_val = NA) {
  unique_vals = vect %>% unique()
  if (is.factor(vect)) {
    unique_vals = levels(unique_vals)
  }
  max_digits = suppressWarnings(unique_vals %>% as.numeric() %>% max(na.rm = TRUE) %>% (function(x) floor(log10(x)) + 1))
  # message(paste('max digits', max_digits))
  if (is.finite(max_digits) & max_digits > 0) {
    val_to_replace = rep_nums(rep_digit, max_digits)
    # message(val_to_replace)
    na_vals = vect == val_to_replace
    #na_vals_char = vect == as.character(val_to_replace)
    vect[na_vals] = na_val
  }
  vect
}

can_be_numeric = function(x) {
  x[x == "Not applicable"] = NA
  start_nas = x %>% is.na() %>% sum()
  end_nas = suppressWarnings(x %>% as.character() %>% as.numeric() %>% is.na() %>% sum())
  start_nas == end_nas
}

as_labelled_func = function(x, func) {
  label = attributes(x)$label
  new_x = func(x)
  attributes(new_x)$label = label
  new_x
}

raw_data = read_dta(here("us_pretrial_release_data.dta"))

processed_data = raw_data %>% mutate_all(fix_num_nas) %>% mutate_if(is.labelled, as_factor) %>% mutate_all(fix_nas, na_strs = c("", " ", ".", "Blank", "-9")) %>% mutate_all(fix_num_nas, rep_digit = 8, na_val = "Not applicable") %>% mutate_if(can_be_numeric, as_labelled_func, func = as.numeric) %>% mutate_if(is.character, as_labelled_func, func = factor) 

processed_data %>% names() %>% enframe() %>% write_csv(here("names.csv"))

vars_to_keep = read_csv(here("names_to_keep.csv")) %>% filter(keep) %>% .$value

selected_data = processed_data %>% select(all_of(vars_to_keep))

missing_by_county = selected_data %>% group_by(COUNTY) %>% summarise_all(function(x) mean(is.na(x)) * 100) %>% pivot_longer(-COUNTY) %>% select(-name) %>% group_by(COUNTY) %>% summarise_all(mean)

missing_by_variable = selected_data %>% summarise_all(function(x) mean(is.na(x)) * 100) %>% pivot_longer(everything())

selected_data %>% na.omit() %>% nrow()
selected_data %>% select(-BAILAMT, -BAILLN) %>% na.omit() %>% nrow()


saveRDS(selected_data, here("cleaned_data.rds"))

