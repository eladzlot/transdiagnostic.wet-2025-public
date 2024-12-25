library(tidyverse)
library(here)

get_qualtrics = function(file){
  read_csv(
    here(file),skip = 3,
    col_names = read_csv(here(file),col_names = FALSE, n_max = 1) %>% as.character() %>% tolower()
  )
}

##################################
# Load data
##################################


get_valid_pid = function(df){
  clean = df %>% 
    filter(prolific_pid != '620a6aa34b2bbf5f1b3d1f5c') %>%  # remove testing pid
    filter(grepl("^[A-Fa-f0-9]{24}$", prolific_pid))        # remove non prolific pid
  
  ids = clean$prolific_pid
  if (length(ids) != length(unique(ids))) {
    duplicate_values = ids[duplicated(ids)]
    stop("Duplicate prolific_pid found: ", paste(duplicate_values, collapse = ", "))
  } else {
    return(clean)
  }
}


# read all data files
raw.screening = get_qualtrics('raw/wet.screening.csv') %>%
    # we filter by finished because some did not finish and restarted
    filter(finished==1) %>% get_valid_pid
raw.treatment.1 = get_qualtrics('raw/wet.treatment.1.csv') %>% get_valid_pid
raw.treatment.2 = get_qualtrics('raw/wet.treatment.2.csv') %>% get_valid_pid
raw.treatment.3 = get_qualtrics('raw/wet.treatment.3.csv') %>% get_valid_pid
raw.treatment.4 = get_qualtrics('raw/wet.treatment.4.csv') %>% 
    # We had a technical problem that didn't allow completion of the task
    # one participant gave three responses, we keep only the first which was fullest
    filter(responseid != 'R_UXBeIQl9Mpy5ik9',responseid != 'R_3CNJbk19Iwq8JFo') %>% get_valid_pid 
raw.treatment.5 = get_qualtrics('raw/wet.treatment.5.csv') %>% get_valid_pid

raw.control.1 = get_qualtrics('raw/wet.control.1.csv') %>%
    # one participant restarted but gave no responses, we removed the empty response
    filter(responseid != 'R_OJSmJ4pvO6bGD9n') %>% get_valid_pid 
raw.control.2 = get_qualtrics('raw/wet.control.2.csv') %>% get_valid_pid
raw.control.3 = get_qualtrics('raw/wet.control.3.csv') %>% get_valid_pid
raw.control.4 = get_qualtrics('raw/wet.control.4.csv') %>% get_valid_pid
raw.control.5 = get_qualtrics('raw/wet.control.5.csv') %>% get_valid_pid

raw.waitlist.1 = get_qualtrics('raw/waitlist.1.csv') %>% get_valid_pid() %>% rename_at(vars(q2:q6), ~paste0("oasis_", 1:5)) %>% filter(wsas_6==8)
raw.waitlist.2 = get_qualtrics('raw/waitlist.2.csv') %>% get_valid_pid() %>% rename_at(vars(q2:q6), ~paste0("oasis_", 1:5)) %>% filter(wsas_6==8)
raw.waitlist.3 = get_qualtrics('raw/waitlist.3.csv') %>% get_valid_pid() %>% rename_at(vars(q2:q6), ~paste0("oasis_", 1:5)) %>% filter(wsas_6==8)

# for convenience: a df of pid condition
pid.condition = bind_rows(
  raw.treatment.1 %>% select(prolific_pid) %>% mutate(condition = 'treatment'),
  raw.control.1 %>% select(prolific_pid) %>% mutate(condition = 'control'),
  raw.waitlist.1 %>% select(prolific_pid) %>% mutate(condition = 'waitlist')
)

raw.followup = get_qualtrics('raw/wet.followup.csv') %>% get_valid_pid %>% left_join(pid.condition)

raw.demographics = rbind(
  read_csv(here('raw','demog.csv')) %>% select(id = `Participant id`, Age, Sex, Ethnicity = `Ethnicity simplified`),
  read_csv(here('raw','waitlist.demog.csv')) %>% select(id = `Participant id`, Age, Sex, Ethnicity = `Ethnicity simplified`)
) %>%
  inner_join(pid.condition, by=join_by(id == prolific_pid)) %>%
  mutate(Age = as.numeric(Age))


##################################
# Create long form data
##################################

# We transfer all data into a data frame with the following form:
# id: <string> prolific_id
# condition: <factor>treatment|control
# wave: <int> 1:5
# measure: <string> e.g. 'oasis'
# question: <int> question number 1:j where j is the number of questions
# value: <int> question response
to_int = function(i) as.numeric(gsub('[^0-9]','',i))
get_questionnaires_long = function(df, condition, wave){
  df %>% 
    # Clean up BSAM
    rename_with(~ gsub("bsam_", "bsam.", .x), starts_with("bsam")) %>% # change the name of the bsam columns so they dont cause problems with pivot longer
    mutate(across(starts_with("bsam"), ~ .x -13 )) %>% # fix recode problem with the question (values where set to 14:20 instead of 1:7 
    mutate(across(matches("^bsam.*[124]$"), ~ 8-.x)) %>% # reverse positive items from bsame
    rename(any_of(c(commitment_1 = "commitment"))) %>%
  pivot_longer(
    cols = starts_with(c('oasis_','dts','wsas','tiii','taf','bcss','ifs', 'bsam', 'mcq', 'scs', 'commitment', 'credibility')),
    names_to = c('measure','question'),
    names_sep = '_',
    values_to = 'value',
    values_transform = to_int,
    names_transform = list(question=to_int)
  ) %>%
    select(id = prolific_pid, measure, question,value) %>%
    mutate(condition = condition, wave = wave) %>%
    select(id, condition, wave, everything())
}

data_long = bind_rows(
    left_join(raw.treatment.1 %>% select(prolific_pid), raw.screening) %>% get_questionnaires_long(condition = 'treatment', wave = 0),
    left_join(raw.control.1 %>% select(prolific_pid), raw.screening) %>% get_questionnaires_long(condition = 'control', wave = 0),
    raw.treatment.1 %>% get_questionnaires_long(condition = 'treatment',wave = 1),
    raw.treatment.2 %>% get_questionnaires_long(condition = 'treatment',wave = 2),
    raw.treatment.3 %>% get_questionnaires_long(condition = 'treatment',wave = 3),
    raw.treatment.4 %>% get_questionnaires_long(condition = 'treatment',wave = 4),
    raw.treatment.5 %>% get_questionnaires_long(condition = 'treatment',wave = 5),
    raw.control.1 %>% get_questionnaires_long(condition = 'control',wave = 1),
    raw.control.2 %>% get_questionnaires_long(condition = 'control',wave = 2),
    raw.control.3 %>% get_questionnaires_long(condition = 'control',wave = 3),
    raw.control.4 %>% get_questionnaires_long(condition = 'control',wave = 4),
    raw.control.5 %>% get_questionnaires_long(condition = 'control',wave = 5),
    raw.waitlist.1 %>% get_questionnaires_long(condition = 'waitlist',wave = 1),
    raw.waitlist.2 %>% get_questionnaires_long(condition = 'waitlist',wave = 4),
    raw.waitlist.3 %>% get_questionnaires_long(condition = 'waitlist',wave = 5),
    raw.followup %>% filter(condition == 'control') %>% get_questionnaires_long(condition = 'control' ,wave = 6),
    raw.followup %>% filter(condition == 'treatment') %>% get_questionnaires_long(condition = 'treatment' ,wave = 6)
) %>% mutate(measure = case_when(
  measure == 'bcss#1' & question %in% 1:6 ~ 'bcss_self_neg',
  measure == 'bcss#1' & question %in% 7:12 ~ 'bcss_self_pos',
  measure == 'bcss#1' & question %in% 13:18 ~ 'bcss_other_neg',
  measure == 'bcss#1' & question %in% 19:24 ~ 'bcss_other_pos',
  .default = measure
)) %>% mutate(measure = case_when(
  measure == 'mcq' & question %in% c(8, 14, 17, 24, 26 , 29) ~ 'mcq_confidence',
  measure == 'mcq' & question %in% c(1, 7, 10, 19, 23 , 28) ~ 'mcq_worry',
  measure == 'mcq' & question %in% c(3, 5, 12, 16, 18 , 30) ~ 'mcq_self',
  measure == 'mcq' & question %in% c(2, 4, 9, 11, 15 , 21) ~ 'mcq_danger',
  measure == 'mcq' & question %in% c(6, 13, 20, 22, 25 , 27) ~ 'mcq_control',
  .default = measure
)) %>%
  # reverse score SCS
  mutate(value = if_else(measure=='scs' & question %in% c(11,12,4,8,1,9), 6-value, value)) %>%
  # Remove attention question from taf
  filter(!(measure == 'taf' & question==6)) %>%
  mutate(question = if_else(measure == 'taf' & question>6, question-1, question)) %>%
  # break taf into subscales
  mutate(measure = case_when(
    measure == 'taf' & question %in% c(1, 3, 4, 6, 8, 10, 11, 13, 15, 17, 18, 19) ~ 'taf_m',
    measure == 'taf' & question %in% c(2, 5, 7, 9, 12, 14, 16) ~ 'taf_l',
    .default = measure
  )) %>%
  
  # remove attention check from wsas (fake question 4)
  filter(!(measure == 'wsas' & question==6 & condition == 'waitlist')) %>%
  filter(!(measure == 'wsas' & (wave %in% c(0,6)) & question==4)) %>%
  mutate(question = if_else(measure == 'wsas' & wave %in% c(0,6) & question>4, question-1, question)) %>%
  
  # We had a problem with the DTS where question 6 repeated question 7, thus we're missing the reversed coded question
  # mutate(value = if_else(measure == 'dts' & question==6, 6-value, value)) %>%
  filter(!(measure == 'dts' & question==6)) %>%
  mutate(question = if_else(measure == 'dts' & question>6, question-1, question)) %>%
  
  mutate(condition = factor(condition, levels = c('treatment','control','waitlist'))) %>%
  
  arrange(id) # so that when transforming to factor we have constant labeling

demographics.data = raw.demographics %>%
  mutate(completed = id %in% raw.followup$prolific_pid) %>%
  left_join(data_long %>%
    filter(measure %in% c('wsas','oasis'), wave == 1) %>%
    group_by(measure, id) %>%
    summarise(value = sum(value), .groups = 'drop_last') %>%
    pivot_wider(names_from = measure, values_from=value)
  ) %>%
  mutate(Age = as.numeric(Age))

screening = raw.screening %>% select(!(status:consent), -prolific_id)

#################################
# Deidentify and save to data
#################################
# Combine IDs from both data frames to ensure unique integers across both
all_ids <- unique(c(data_long$id, demographics.data$id, screening$prolific_pid))

# Create a mapping of IDs to integers
id_mapping <- setNames(seq_along(all_ids), all_ids)

# Transform IDs in both data frames
data_long$id <- id_mapping[data_long$id]
demographics.data$id <- id_mapping[demographics.data$id]
screening$prolific_pid <- id_mapping[screening$prolific_pid]
pid.condition$prolific_pid <- id_mapping[pid.condition$prolific_pid]

write_csv(data_long, here('data', 'data_long.csv'))
write_csv(demographics.data, here('data', 'demographics.data.csv'))
write_csv(screening, here('data', 'screening.csv'))
write_csv(pid.condition, here('data', 'pid.condition.csv'))

