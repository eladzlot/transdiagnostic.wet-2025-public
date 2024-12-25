source(here('scripts','helpers.R'))

as.ordinal = function(x) {x %>% as.factor %>% as.numeric}

#' Run a Bayesian Model Using Ulam
#'
#' This function is a wrapper for the `ulam()` function from the `rethinking` library, 
#' which allows you to run Bayesian models using CmdStan for faster computation.
#' It provides the ability to cache models, force re-running them, and pass additional arguments to `ulam()`.
#'
#' @param model A list defining the model structure, typically created using `ulam()`'s syntax.
#' @param data A list of data to be passed to the model. This should match the format expected by `ulam()`.
#' @param model_name A character string specifying the name of the model. This will be used to name the directory and RDS file where the model is saved.
#' @param force A logical value indicating whether to overwrite existing models. If `TRUE`, the model directory will be deleted and the model will be re-run.
#' @param log_lik A logical value indicating whether to compute log-likelihood values. Defaults to `TRUE`, and it can be used for model comparison and checks.
#' @param ... Additional arguments passed to `ulam()`, such as `iter`, `warmup`, or `adapt_delta` for fine-tuning the MCMC process.
#'
#' @details
#' The function first constructs paths for saving the model and its results. If the `force` argument is `TRUE`, any existing model directory with the same name will be removed using `unlink()`.
#' It then calls the `ulam()` function with the specified model, data, and additional arguments. The results are saved in a reproducible format (RDS files), ensuring they can be loaded and reused later.
#' By default, four MCMC chains and four CPU cores are used for parallel computation. CmdStan is enabled for faster model computation.
#'
#' @return The model object produced by `ulam()`. This can be used for further analysis or saved for future use.
#'
#' @seealso
#' \code{\link[rethinking]{ulam}} for more details on specifying and running Bayesian models using `ulam()`.
#' 
#' @examples
#' \dontrun{
#'   # Define a simple model
#'   model <- alist(
#'     y ~ dnorm(mu, sigma),
#'     mu <- a + b * x,
#'     a ~ dnorm(0, 1),
#'     b ~ dnorm(0, 1),
#'     sigma ~ dunif(0, 10)
#'   )
#'   
#'   # Data for the model
#'   data <- list(y = rnorm(100), x = rnorm(100))
#'   
#'   # Run the model, forcing re-computation and saving it under "simple_model"
#'   m <- run_model(model = model, data = data, model_name = "simple_model", force = TRUE)
#' }
#'
#' @export
run_model <- function(model, data, model_name, force = FALSE, log_lik = TRUE, ...) {
  # Define the model directory and file paths
  model_dir <- here::here('data', 'models', model_name)
  model_file <- here::here(model_dir, paste0(model_name, '.rds'))
  
  # If force is TRUE, delete the existing model directory
  if (force) unlink(model_dir, recursive = TRUE)
  
  # Ensure the directory exists before saving files
  if (!dir.exists(model_dir))  dir.create(model_dir, recursive = TRUE)
  
  # Run the ulam model and save the output in the specified directory
  m <- ulam(
    flist = model,         # Model definition
    data = data,           # Data to be passed to the model
    file = model_file,     # RDS file to save the model output
    output_dir = model_dir, # Save all Stan data in the specified directory
    cmdstan = TRUE,        # Use CmdStan for faster computation
    chains = 4,            # Number of MCMC chains
    cores = 4,             # Number of CPU cores for parallel processing
    log_lik = log_lik,     # Compute log-likelihood for model comparison
    ...
  )

  play_beep_if_installed(2)
  
  # Return the fitted model object
  return(m)
}

# -------------------------------
# Helper Function Definitions
# -------------------------------

# Function to convert latent theta to raw OASIS score based on IRT parameters
# Inputs:
# - theta: Latent trait value
# - alpha: Item discrimination parameters
# - beta: Item difficulty parameters
# - cutpoints: Ordinal regression cutpoints
theta_to_estimate <- function(theta, alpha, beta, cutpoints) {
  # Calculate phi for each question based on IRT model
  phiArr <- alpha * (theta - beta)
  
  answerSeq = 1:(length(cutpoints)+1)
  
  # Compute expected raw score for each question
  raw_scores <- sapply(phiArr, function(phi) {
    # Calculate probability for each ordinal category using ordered logistic regression
    pk <- dordlogit(answerSeq, phi, cutpoints)
    # Compute expected score for this question
    expected_score <- sum(pk * (answerSeq - 1))
    return(expected_score)
  })
  
  # Sum raw scores across all questions to get the total raw OASIS score
  total_raw_score <- sum(raw_scores)
  
  return(total_raw_score)
}

plot_mean_sum_by_condition <- function(d, mname) {
  
  # Summarize value by id and wave
  means <- d %>%
    group_by(id, condition, wave) %>%
    summarize(sum_value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
    group_by(condition,wave) %>% summarize(mean = mean(sum_value), sd = sd(sum_value), .groups = 'drop')
  
  # Split data by condition
  conditions <- split(means, means$condition)
  
  # Define colors
  cols <- rainbow(length(conditions))
  
  # Plot setup
  plot(NULL, xlim = range(means$wave), ylim = range(means$mean),
       xlab = "Wave", ylab = "Mean Sum of Values",
       main = paste0("Values by Condition ",mname))
  
  # Add lines for each condition
  legend_labels <- names(conditions)
  for (i in 1:length(names(conditions))) {
    cond = names(conditions)[i]
    cond_data <- conditions[[cond]]
    lines(cond_data$wave, cond_data$mean, type = "o", col = cols[i], pch = 16)
  }
  
  # Add legend
  #legend("topright", legend = legend_labels, col = cols,  pch = 16, lty = 1, title = "Condition")
  legend( "top",  legend = legend_labels,
    col = cols,
    pch = 14 + (1:length(conditions)),
    lty = 1:length(conditions),
    horiz = TRUE,
    bty = "n",
    inset = c(0, -0.3), # Adjust to position the legend above the plot
    xpd=TRUE
  )
}

