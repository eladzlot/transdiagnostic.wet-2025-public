library(tidyverse)
library(here)

data_long = read_csv(here('data', 'data_long.csv'))
demographics.data = read_csv(here('data', 'demographics.data.csv'))
screening = read_csv(here('data', 'screening.csv'))
pid.condition = read_csv(here('data', 'pid.condition.csv'))
