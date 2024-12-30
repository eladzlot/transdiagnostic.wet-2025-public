library(rethinking)
library(tidyverse)
library(here)
source(here('scripts','helpers.R'))
source(here('scripts','model.change.R'))
if (!exists('data_long')) source(here('scripts','load_data.R'))

process_models = list()

models_list = c('dts','scs','ifs', 'taf_m','taf_l', 'bcss_self_neg', 'bcss_other_neg','bcss_self_pos', 'bcss_other_pos', 'mcq_confidence','mcq_worry','mcq_self','mcq_danger','mcq_control')
for (mname in models_list){
  d = data_long %>% filter(measure == mname, !is.na(value),condition != 'waitlist', wave>0)
  m = model.change(d, model_name = mname, force=FALSE)
  post = extract.samples(m)
  process_models[[mname]] = list(
    m = m,
    post = post,
    wet = change_link(post,1),
    ew = change_link(post,2),
    sd.wet = d %>% filter(wave == 1, condition == 'treatment') %>% group_by(id) %>% summarize(v = sum(value)) %>% pull(v) %>% sd,
    sd.ew = d %>% filter(wave == 1, condition == 'control') %>% group_by(id) %>% summarize(v = sum(value)) %>% pull(v) %>% sd
  )
}