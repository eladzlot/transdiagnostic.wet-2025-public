library(tidyverse)
library(here)
source(here('scripts','model.mechanisms.R'))
if (!exists('data_long')) source(here('scripts','load_data.R'))
if (!exists('mechanism_models')) source(here('scripts','run.mechanisms.R'))

models_list = c('dts', 'bcss_self_pos','ifs', 'mcq_confidence', 'mcq_danger')

table_data = purrr::map(models_list, function(mname){
  m = mechanism_models[[mname]]
  post = extract.samples(m)
  r.treat = post$Rho[,5,2]
  r.fu = post$Rho[,5,3]
  
  return(data.frame(
    name = mname,
    tre = median(r.treat),
    tre.ci = format_ci(r.treat),
    tre.p = mean(r.treat>0) %>% p_star(),
    fu = median(r.fu),
    fu.ci = format_ci(r.fu),
    fu.p = mean(r.fu>0) %>% p_star()
    #maxRhat = max(prec$rhat, na.rm = TRUE),
    #minESS = min(prec$ess_bulk, na.rm = TRUE)
  )) 

}) %>% bind_rows

table_data %>%
  mutate(name = case_when(
            name == 'dts' ~ 'DTS †',
            name == 'bcss_self_pos' ~ 'BCSS Self positive',
            name == 'ifs' ~ 'IFES',
            name == 'mcq_confidence' ~ '(Lack of) Cognitive Confidence',
            name == 'mcq_danger' ~ 'Uncontrollability and Danger',
            .default = str_to_upper(name)
  )) %>%
  apa_table(
    caption = 'Proposed mechanisms of change. Correlations between symptom change (during treatment and follow-up) and processes that have been found to change in WET.',
    col.names = c(
                  'Measure',
                  '$r$',
                  '89\\% CI',
                  'P(<0)',
                  '$r$',
                  '89\\% CI',
                  'P(>0)'),
    col_spanners = list(Treatment = c(2,4), Followup = c(5,7)),
    note = " † Indicates a measure of strength or positive attributes, and negative correlation should be interpreted as alignment",
    escape = TRUE
  )
