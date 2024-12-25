library(rethinking)
library(tidyverse)
library(here)
source(here('scripts','helpers.R'))
source(here('scripts','model.mechanisms.R'))
if (!exists('data_long')) source(here('scripts','load_data.R'))

mechanism_models = list()

d_x = data_long %>%
  filter(measure == 'oasis', !is.na(value), condition == 'treatment', wave>0) %>%
  left_join(demographics.data, by = "id")  # Ensure 'id' is the key for joining


models_list = c('dts', 'bcss_self_pos','ifs', 'tiii', 'mcq_confidence', 'mcq_danger')
for (mname in models_list){
  d_m = data_long %>% filter(measure == mname, !is.na(value),condition == 'treatment', wave>0)
  m = model.mechanism(d_x, d_m, model_name = mname, force=FALSE)
  mechanism_models[[mname]] = m
}

#######################
# BSAM
#######################

# Between session 1 and 2
d_m = data_long %>%
  filter(measure == 'bsam.pre', condition=='treatment', wave %in% 1:2, !is.na(value)) %>%
  mutate(wave = if_else(wave == 1, 1, 4))
mname = "bsam.change.all"
m = model.mechanism(d_x, d_m, model_name = mname, force=FALSE)
mechanism_models[[mname]] = m

d_x = data_long %>%
  filter(measure == 'oasis', !is.na(value), condition == 'control', wave>0) %>%
  left_join(demographics.data, by = "id")  # Ensure 'id' is the key for joining
d_m = data_long %>%
  filter(measure == 'bsam.pre', condition=='control', wave %in% 1:2, !is.na(value)) %>%
  mutate(wave = if_else(wave == 1, 1, 4))
mname = "bsam.change.all.ew"
m = model.mechanism(d_x, d_m, model_name = mname, force=FALSE)
mechanism_models[[mname]] = m

#######################
# DTS EW
#######################
d_x = data_long %>%
  filter(measure == 'oasis', !is.na(value), condition == 'control', wave>0) %>%
  left_join(demographics.data, by = "id")  # Ensure 'id' is the key for joining
d_m = data_long %>% filter(measure == 'dts', !is.na(value),condition == 'control', wave>0)
m = model.mechanism(d_x, d_m, model_name = 'dts.ew', force=FALSE)
mechanism_models[['dts.ew']]= m

play_beep_if_installed(3)


#######################
# DTS * BSAM
#######################
d_x = data_long %>%
  filter(measure == 'dts', !is.na(value), condition == 'control', wave>0) %>%
  left_join(demographics.data, by = "id")  # Ensure 'id' is the key for joining
d_m = data_long %>%
  filter(measure == 'bsam.pre', condition=='control', wave %in% 1:2, !is.na(value)) %>%
  mutate(wave = if_else(wave == 1, 1, 4))
mname = "dts.bsam.wet"
m = model.mechanism(d_x, d_m, model_name = mname, force=FALSE)
mechanism_models[[mname]] = m
