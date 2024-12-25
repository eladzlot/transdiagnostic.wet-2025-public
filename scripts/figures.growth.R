# =============================================================================
# Script: Fit Ordinal Regression Model with Longitudinal Effects and Plot Results
# Dependencies: load_data.R, model.helpers.R
# =============================================================================

library(tidyverse)
library(rethinking)  # For Bayesian modeling functions like 'ulam' and 'extract.samples'
library(here)

# -------------------------------
# Source External Scripts
# -------------------------------

# Load external R scripts containing data loading and helper functions
# Uncomment the lines below if these scripts are available in your working directory
if (!exists('data_long')) source(here('scripts','load_data.R'))
source(here('scripts','model.growth.R'))

# -------------------------------
# Data Preparation
# -------------------------------

# Filter and merge data for analysis
# - Select rows where 'measure' is 'oasis', 'value' is not missing, and 'wave' is greater than 0
# - Merge with demographic data based on 'id'
d <- data_long %>%
  filter(measure == 'oasis', !is.na(value), wave > 0) %>%
  left_join(demographics.data, by = c("id",'condition'))  # Ensure 'id' is the key for joining

# -------------------------------
# Model Fitting
# -------------------------------

# Fit the longitudinal ordinal regression model using the 'model.growth' function
# - 'model.growth' is defined in 'model.helpers.R'
model_name <- 'oasis'
m <- model.growth(d, model_name, force=FALSE)

# Extract posterior samples from the fitted model for further analysis
post <- extract.samples(m)

# -------------------------------
# Define Plot Parameters
# -------------------------------

# Identify unique waves (time points) and experimental conditions
waves <- sort(unique(d$wave))
waves <- c(1:4, 7, 9)
conditions <- c('WET', 'NIW', 'Waitlist')

# Assign colors to each condition for plotting
colors <- c("#E69F00", "#56B4E9", "#009E73")  # Orange, Blue, Green

# -------------------------------
# Compute Outcomes for Each Condition
# -------------------------------

# For each condition, compute the mean and posterior interval of raw OASIS scores across waves
outcomes <- lapply(1:length(conditions), function(cond) {
  # Compute raw scores matrix for the current condition
  mat <- growth_link(post, cond)
  
  # Calculate mean raw score and 89% posterior interval (default in 'rethinking')
  list(
    means = apply(mat, 2, mean),
    PI = apply(mat, 2, PI),  # 'PI' function from 'rethinking' computes credible intervals
    col = colors[cond]
  )
})

# -------------------------------
# Plotting the Results
# -------------------------------

# Initialize an empty plot with specified limits and labels
plot(
  NULL,
  type = "n",
  ylim = c(0, 10),  # Adjust based on expected OASIS score range
  xlim = range(waves),
  xaxt = 'n',
  xlab = "Wave",
  ylab = "Expected OASIS",
  main = "Growth Model"
)

# Add custom x-axis with non-equally spaced labels
axis(1, at = c(1,4,7,9), labels = c('Pre','Post', 'FU1', 'FU2'))

# Add lines and shaded credible intervals for each condition
for (outcome in outcomes) {
  # Plot the mean raw OASIS scores across waves
  points(waves, outcome$means, col=outcome$col, pch=16)
  lines( waves[waves<=7], outcome$means[waves<=7],  col = outcome$col, lwd=2, lty = 1)
  lines( waves[waves>=7], outcome$means[waves>=7],  col = outcome$col, lwd=2, lty = 3)
  
  # Add shaded credible intervals around the mean lines
  shade(outcome$PI, waves)
}

# -------------------------------
# Add Legend to the Plot
# -------------------------------

legend(
  "top",
  legend = conditions,
  col = colors,
  pch = 16,         # Solid circle to match plot points
  lty = 1,          # Dashed line to match plot lines
  lwd = 2,
  horiz = TRUE,
  bty = "n",        # No box around the legend
  xpd = TRUE         # Allow legend to be drawn outside plot region if necessary
)

# =============================================================================
# End of Script
# =============================================================================
