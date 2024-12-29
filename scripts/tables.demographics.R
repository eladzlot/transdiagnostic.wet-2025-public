df = demographics.data
# Total N for each condition
n_summary <- df %>%
  group_by(condition) %>%
  summarise(N = n()) %>%
  bind_rows(df %>%
              summarise(condition = "Total", N = n()))

# Age summary
age_summary <- df %>%
  group_by(condition) %>%
  summarise(mean_age = mean(Age, na.rm = TRUE),
            sd_age = sd(Age, na.rm = TRUE)) %>%
  bind_rows(df %>%
              summarise(condition = "Total",
                        mean_age = mean(Age, na.rm = TRUE),
                        sd_age = sd(Age, na.rm = TRUE))) %>%
  mutate(Age = paste0(round(mean_age, 1), " (", round(sd_age, 1), ")")) %>%
  select(condition, `Age M (SD)` = Age)

# Gender summary
gender_summary <- df %>%
  group_by(condition, Sex) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = Sex, values_from = count, values_fill = 0) %>%
  bind_rows(df %>%
              group_by(Sex) %>%
              summarise(count = n()) %>%
              pivot_wider(names_from = Sex, values_from = count, values_fill = 0) %>%
              mutate(condition = "Total")) %>%
  mutate(female_percent = round(Female / (Female + Male) * 100, 1)) %>%
  mutate(male_percent = round(Male / (Female + Male) * 100, 1)) %>%
  mutate(GenderF = paste0(Female, " (", female_percent, "%)")) %>%
  mutate(GenderM = paste0(Male, " (", male_percent, "%)")) %>%
  select(condition, `Gender Female` = GenderF, `Gender Male` = GenderM) 

# Ethnicity summary
ethnicity_summary <- df %>%
  group_by(condition, Ethnicity) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  # Add the total count for each ethnicity across all conditions
  bind_rows(df %>%
              group_by(Ethnicity) %>%
              summarise(count = n()) %>%
              mutate(condition = "Total")) %>%
  # Calculate percentages within each condition
  group_by(condition) %>%
  mutate(percentage = round(count / sum(count) * 100, 1)) %>%
  # Create a combined "count (percentage)" column
  mutate(ethnicity_count_percentage = paste0(count, " (", percentage, "%)")) %>%
  # Select relevant columns
  select(condition, Ethnicity, ethnicity_count_percentage) %>%
  pivot_wider(names_from = Ethnicity, values_from = ethnicity_count_percentage)
  


outcome_summary <- data_long %>%
  filter(measure %in% c('wsas','oasis'), wave == 1) %>%
  group_by(condition, measure, id) %>%
  summarise(value = sum(value), .groups = 'drop_last') %>%
  summarise(mean = mean(value), sd = sd(value), .groups = 'drop_last') %>%
  bind_rows(
    data_long %>%
      filter(measure %in% c('wsas','oasis'), wave == 1) %>%
      group_by(measure, id) %>%
      summarise(value = sum(value), .groups = 'drop_last') %>%
      summarise(mean = mean(value), sd = sd(value), .groups = 'drop_last') %>%
      mutate(condition = 'Total')
  ) %>%
  mutate(outcome = paste0(round(mean,1), " (", round(sd,1), ")")) %>%
  select(-mean,-sd) %>%
  pivot_wider(names_from = measure, values_from = outcome) %>%
  rename(`OASIS M (SD)` = oasis, `WSAS M (SD)` = wsas)

 # Merge all summaries
final_summary <- n_summary %>%
  left_join(age_summary, by = "condition") %>%
  left_join(gender_summary, by = "condition") %>%
  left_join(ethnicity_summary, by = "condition") %>%
  left_join(outcome_summary, by = "condition")

# Transpose the final summary
final_table <- final_summary %>%
  mutate(N = as.character(N)) %>%
  pivot_longer(cols = -condition, names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = condition, values_from = Value)

final_table %>%
  apa_table(
    caption = '(ref:demographics)',
    col.names = c('Characteristics', 'NIW', 'WET', 'Waitlist', 'Total'),
    col_spanners = list(Conditions = 2:4),
    note = 'Data are presented as number (percentage) of patients unless otherwise indicated.',
    stub_indents = list(Ethnicity = 5:7, `Pretreatment measures` = 8:9)
  )
