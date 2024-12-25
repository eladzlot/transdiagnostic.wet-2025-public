format_perc <- function(count, N = NULL) sprintf("%.1f%%", if (!is.null(N)) (count / N) * 100 else count * 100)
format_count_perc <- function(count, N) { sprintf("(n = %.f, %.1f%%)", count, (count / N) * 100) }
format_mean_sd <- function(vec) {sprintf("%.1f (%.1f)", mean(vec, na.rm=TRUE),  sd(vec, na.rm=TRUE))}
format_ci <- function(x, p = .89) sprintf("[%.2f, %.2f]", quantile(x, p / 2), quantile(x, 1 - p / 2))
table_estimate = function(vec) {sprintf("%.2f [%.2f,%.2f], PD = %.0f%%", mean(vec), quantile(vec, 0.055), quantile(vec, 0.945), 100*mean(vec<0))}
format_dscore = function(vec, p = .89) {sprintf("*d* = %.2f, 89%% CI [%.2f, %.2f]", mean(vec), quantile(vec, (1-p)/2), quantile(vec, (1+p)/2))}
format_estimate <- function(vec, p = 0.89, estimand = "*d*") {
  sprintf(
    "%s = %.2f, %.0f%% CI [%.2f, %.2f], P(>0) = %.0f%%",
    estimand,
    mean(vec),
    100 * p,
    quantile(vec, (1 - p) / 2),
    quantile(vec, (1 + p) / 2),
    100 * mean(vec > 0)
  )
}

p_star = function(p, precision = .11) { paste0(sprintf('%.2f', p), ifelse(p<precision/2 | p > 1-precision/2, '*', '')) }
bayesep = function(x, precision = .11){sprintf("%.2f (%s)", mean(x), p_star(mean(x > 0)))}

# Custom function to handle Fisher's exact test with correct p-value formatting
apa_print.fisher <- function(x, ...) {
  # Extract p-value from Fisher's test result
  p_value <- x$p.value
  
  # Format the p-value for APA style
  if (p_value < .001) { p_value_formatted <- "$p$ < .001" }
  else if (p_value > .999) { p_value_formatted <- "$p$ > .999" }
  else { p_value_formatted <- sprintf("$p$ = %.3f", p_value) }
  
  paste("Fisher's exact test,", p_value_formatted) # Formulate the full result string
}


# Function to calculate omega.tot from long-format data
library(psych)
omega_from_long <- function(data, 
                                 id_col = "id", 
                                 wave_col = "wave", 
                                 question_col = "question", 
                                 value_col = "value", 
                                 exclude_cols = c("condition")) {
  # Ensure required columns exist
  required_cols <- c(id_col, wave_col, question_col, value_col)
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain the following columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Combine id and wave columns, pivot to wide format, exclude columns, and calculate omega
  tryCatch({
    omega_tot <- data %>%
      unite("id", all_of(c(id_col, wave_col, 'measure')), remove = TRUE) %>%  # Combine id and wave
      pivot_wider(names_from = {{ question_col }}, values_from = {{ value_col }}) %>% 
      select(-all_of(c("id", exclude_cols))) %>%  # Remove ID and specified columns
      omega(1) %>% 
      .$omega.tot  # Extract omega.tot
    
    return(omega_tot)
  }, error = function(e) {
    stop("Error calculating omega.tot: ", e$message)
  })
}

play_beep_if_installed <- function(sound = 2) {
  if (requireNamespace("beepr", quietly = TRUE)) {
    library(beepr)
    #beep(sound)  # Default beep sound
  } else {
    warning("The 'beepr' package is not installed. Please install it to enable sound notifications.")
  }
}
