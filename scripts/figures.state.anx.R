library(tidyverse)
library(here)
if (!exists('data_long')) source(here('scripts','load_data.R'))

colors <- c("#E69F00", "#56B4E9")  # Orange, Blue
d = data_long %>% 
  filter(measure %in% c('bsam.pre', 'bsam.post')) %>%
  mutate(measure = if_else(measure == 'bsam.pre', 'pre','post')) %>%
  group_by(wave,condition,measure,id) %>% 
  summarise(value = sum(value), .groups = 'drop_last') %>% 
  summarize(m = mean(value, na.rm=TRUE), se = sd(value, na.rm = TRUE)/sqrt(n()), .group = 'drop_last') %>% 
  pivot_wider(names_from = measure, values_from = m:se) %>%
  mutate(color = if_else(condition == 'treatment', colors[1], colors[2])) %>%
  mutate(hi = m_pre + se_pre, low = m_pre - se_pre) %>%
  ungroup()

# Create an empty plot with specified y and x ranges
plot(x = c(1, 6), y = c(18, 34), type = "n", xlab = "Wave", ylab = "BSAM", main = "Changes in State Anxiety")

shift = .4
points(x = d$wave, y=d$m_pre, col = d$color, lwd=2)
#points(x = d$wave+shift, y=d$m_post, col = d$color, pch=19)
lines(1:6, d %>% filter(condition=='treatment') %>% pull(m_pre), col=colors[1], lty=2, lwd=2)    
lines(1:6, d %>% filter(condition=='control') %>% pull(m_pre), col=colors[2], lty=2, lwd=2)    
shade(d %>% filter(condition=='control') %>% select(hi,low) %>% t, 1:6 )
shade(d %>% filter(condition=='treatment') %>% select(hi,low) %>% t, 1:6 )

for (i in 1:nrow(d)){
  if (!is.na(d$m_post[i])) with(d[i,], arrows(
    x0=wave, x1=wave+shift, y0=m_pre, y1=m_post,
    col = color,
    lwd = 2,
    length = .10
  ))
}

legend("top", legend = c("WET", "NIW"), col = colors, horiz = TRUE, pch = 16, lty = 2, bty='n')
