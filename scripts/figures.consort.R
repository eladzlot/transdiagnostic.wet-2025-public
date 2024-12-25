library(tidyverse)
library(consort)
library(here)
if (!exists('data_long')) source(here('scripts','load_data.R'))

##########################
# Create CONSORT plot
##########################

# First we compose the input df for consort_plot
# Essentially this means getting a row for each subject
# And a column for each step in the study.
# See documentation for details

participation = data_long %>%
  select(prolific_pid = id, group = condition, wave) %>%
  filter(group != 'waitlist') %>%
  distinct() %>%
  mutate(group = if_else(group == 'treatment', 'WET', 'NIW')) %>%
  group_by(prolific_pid, group) %>%
  summarise(
    wv1 = if_else(any(wave == 1), TRUE, NA),
    wv2 = if_else(any(wave == 2), TRUE, NA),
    wv3 = if_else(any(wave == 3), TRUE, NA),
    wv4 = if_else(any(wave == 4), TRUE, NA),
    fu = if_else(any(wave == 5), TRUE, NA),
    fu2 = if_else(any(wave == 6), TRUE, NA)
  )

d = screening %>% 
  # Name the reasons for exclusion (used in the side box)
  mutate(screen_reason = case_when(
    sc0 >= 2 ~ 'Failed attention', # attention_check_sum
    sc1 <= 4 ~ 'Low anxiety', # oasis
    sc2 > 15 ~ 'Depression', # phq
    a & (sc3 > 6) ~ 'Trauma', # has trauma & pcl
    rowSums(across(starts_with("wsas")& !starts_with("wsas_4")) > 2) == 0 ~ 'No impairment',
    hallucination == 1 ~ 'Hallucinations',
    delusion == 1 ~ 'Delusions'
  )) %>%
  select(prolific_pid,screen_reason) %>%
  mutate(accepted = !is.na(screen_reason)) %>%
  left_join(participation, by = 'prolific_pid')
  # join columns frome each intervention wave
  #left_join(rbind(raw.treatment.1 %>% select(prolific_pid) %>% mutate(group = 'WET'), raw.control.1 %>% select(prolific_pid) %>% mutate(group = 'NIW')) %>% mutate(wv1 = TRUE)) %>%
  #left_join(rbind(raw.treatment.2 %>% select(prolific_pid), raw.control.2 %>% select(prolific_pid)) %>% mutate(wv2 = TRUE)) %>%
  #left_join(rbind(raw.treatment.3 %>% select(prolific_pid), raw.control.3 %>% select(prolific_pid)) %>% mutate(wv3 = TRUE)) %>%
  #left_join(rbind(raw.treatment.4 %>% select(prolific_pid), raw.control.4 %>% select(prolific_pid)) %>% mutate(wv4 = TRUE)) %>%
  #left_join(rbind(raw.treatment.5 %>% select(prolific_pid), raw.control.5 %>% select(prolific_pid)) %>% mutate(fu = TRUE)) %>%
  #left_join(raw.followup %>% select(prolific_pid) %>% mutate(fu2 = TRUE))

# plot away
consort_plot(data = d,
             order = c(
               prolific_pid = 'Screened',
               screen_reason = 'Excluded',
               accepted = 'Accepted',
               group = 'Randomized',
               wv1 = 'Wave 1',
               wv2 = 'Wave 2',
               wv3 = 'Wave 3',
               wv4 = 'Wave 4',
               fu = 'Follow up 1w',
               fu2 = 'Follow up 2m'
             ),
             side_box = 'screen_reason',
             allocation = 'group',
             labels = c('1' = 'Screening', '3' = 'Randomization', '9' = 'Followup')) %>% print

