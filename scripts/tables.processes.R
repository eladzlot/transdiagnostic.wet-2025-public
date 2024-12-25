library(tidyverse)
library(here)
source(here('scripts','model.change.R'))
if (!exists('data_long')) source(here('scripts','load_data.R'))
if (!exists('process_models')) source(here('scripts','run.processes.R'))

models_list = c('dts','scs','ifs', 'taf_m','taf_l', 'bcss_self_neg', 'bcss_other_neg','bcss_self_pos', 'bcss_other_pos', 'mcq_confidence','mcq_worry','mcq_self','mcq_danger','mcq_control')
table_data = purrr::map(models_list, function(mname){
  proc = process_models[[mname]]
  
  delta.wet = (proc$wet[,1]-proc$wet[,2])/proc$sd.wet

  return(data.frame(
    name = mname,
    mean.pre = mean(proc$wet[,1]),
    mean.pre = mean(proc$wet[,2]),
    WET.E = mean(delta.wet),
    WET.CI = sprintf("[%.2f,%.2f]", quantile(delta.wet, .055), quantile(delta.wet, .945)),
    WET.p = mean(delta.wet>0) %>% p_star()
  ))
}) %>% bind_rows

table_data %>%
  mutate(name = case_when(
            name == 'dts' ~ 'DTS †',
            name == 'scs' ~ 'SCS †', 
            name == 'ifs' ~ 'IFES',
            name == 'taf_m' ~ 'Morality',
            name == 'taf_l' ~ 'Likelihood',
            name == 'bcss_self_neg' ~ 'Self negative',
            name == 'bcss_other_neg' ~ 'Other negative',
            name == 'bcss_self_pos' ~ 'Self positive',
            name == 'bcss_other_pos' ~ 'Other positive',
            name == 'mcq_confidence' ~ '(Lack of) Cognitive Confidence',
            name == 'mcq_self' ~ 'Cognitive Self-Consciousness',
            name == 'mcq_worry' ~ 'Positive Beliefs about Worry',
            name == 'mcq_danger' ~ 'Uncontrollability and Danger',
            name == 'mcq_control' ~ 'Need to Control Thoughts',
            .default = str_to_upper(name)
  )) %>%
  apa_table(
    caption = 'Process estimates for each of the proposed mechanisms in WET.',
    col.names = c('Measure', 'Pre', 'Post','$d$','89\\% CI', 'P(>0)'),
    col_spanners = list(Mean = 2:3, Change = c(4,6) ),
    stub_indents = list(TAF = 4:5, BCSS = 6:9, MCQ = 10:14),
    span_text_columns = TRUE,
    note = " † Indicates a measure of strength or positive attributes and negative change should be interpreted as improvement",
    escape = TRUE
  )
