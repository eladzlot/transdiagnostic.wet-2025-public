#' Plot Growth Model Outcomes for Treatment and Control Groups
#'
#' @param mname A string specifying the name of the model, used in the plot title.
#' @param post A matrix or data structure containing posterior samples for outcomes.
#' @return A plot of the growth model outcomes.
#' @examples
#' plot_growth_model("Model Name", posterior_samples)
plot_change_model <- function(mname, proc) {
  # Define constants
  waves <- c(1, 4)
  conditions <- c("WET", "NIW")
  colors <- c("#E69F00", "#56B4E9")  # Orange, Blue

  # -------------------------------
  # Compute Outcomes for Each Condition
  # -------------------------------

  outcomes <- lapply(1:length(conditions), function(cond) {
    if (cond == 1) { mat = proc$wet  } else { mat = proc$ew }
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

  plot(
    NULL,
    type = "n",
    ylim = c(min(c(proc$wet,proc$ew)), max(c(proc$wet,proc$ew))),
    xlim = range(waves),
    xaxt = 'n',
    xlab = "Wave",
    ylab = "Effect size",
    main = paste0("Growth Model ", mname)
  )

  # Add custom x-axis with non-equally spaced labels
  axis(1, at = c(1, 4), labels = c('Pre', 'Post'))

  # Add lines and shaded credible intervals for each condition
  for (outcome in outcomes) {
    # Plot the mean raw OASIS scores across waves
    points(waves, outcome$means, col = outcome$col, pch = 16)
    lines(waves, outcome$means, col = outcome$col, lwd = 2, lty = 1)

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
    lty = 1,          # Solid line to match plot lines
    lwd = 2,
    horiz = TRUE,
    bty = "n",        # No box around the legend
    xpd = TRUE         # Allow legend to be drawn outside plot region if necessary
  )
}

#' Map names to their corresponding string representations
#'
#' @param name A character vector of names.
#' @return A character vector with transformed names according to the specified rules.
map_name <- function(name) {
  dplyr::case_when(
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
    TRUE ~ stringr::str_to_upper(name)  # Default case: transform to uppercase
  )
}
