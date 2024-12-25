source(here('scripts','model.helpers.R'))

#' Fit an Ordinal Regression Model with Longitudinal Effects
#'
#' This function fits an ordinal regression model with longitudinal effects using the \code{ulam} function from the \pkg{rethinking} package. The model incorporates item response theory (IRT) parameters, generalized additive model (GAM) components for time effects, and participant-specific random effects using a non-centered parameterization.
#'
#' @param d A data frame containing the following columns:
#'   \describe{
#'     \item{\code{value}}{Numeric vector of ordinal response values.}
#'     \item{\code{question}}{Numeric or factor vector indicating question identifiers.}
#'     \item{\code{wave}}{Numeric vector indicating the time point or wave.}
#'     \item{\code{id}}{Numeric or factor vector indicating participant IDs.}
#'     \item{\code{condition}}{Character or factor vector indicating the experimental condition, with levels \code{"treatment"} and \code{"control"}.}
#'   }
#' @param model_name A character string specifying the name of the model. This is used in the output and saving the model.
#'
#' @return An object containing the fitted model, as returned by the \code{run_model()} function.
#'
#' @details
#' The function prepares the data and specifies a complex ordinal regression model, which is then fitted using the \code{ulam} function from the \pkg{rethinking} package. The model includes:
#' \itemize{
#'   \item \strong{Ordinal regression (IRT model)}: Models the ordinal response variable \code{R} using a logistic ordinal regression, incorporating item discrimination (\code{alpha}) and item difficulty (\code{beta}) parameters.
#'   \item \strong{Longitudinal effects}: Includes baseline, treatment phase, and follow-up phases (\code{theta}), modeled using a generalized additive model (GAM) component.
#'   \item \strong{Participant-specific random effects}: Random intercepts and slopes for each participant (\code{b_baseline}, \code{b_treatment}, \code{b_fu1}, \code{b_fu2}), using a non-centered parameterization for efficient sampling.
#'   \item \strong{Priors}: Specifies priors for all model parameters, including the Cholesky factor of the correlation matrix (\code{L_Rho}) and standard deviations (\code{L_sigma}).
#'   \item \strong{Generated quantities}: Computes differences between conditions for treatment and follow-up effects.
#' }
#'
#' The data preparation involves converting variables to appropriate formats (e.g., integer, factor) and creating indicator variables for different phases. The function relies on the \pkg{rethinking} package and assumes that the \code{run_model()} function is defined elsewhere in your package or script.
#'
#' @examples
#' \dontrun{
#' # Example data frame
#' d <- data.frame(
#'   value = sample(1:5, 100, replace = TRUE),
#'   question = sample(1:10, 100, replace = TRUE),
#'   wave = sample(1:6, 100, replace = TRUE),
#'   id = sample(1:20, 100, replace = TRUE),
#'   condition = sample(c("treatment", "control"), 100, replace = TRUE)
#' )
#'
#' # Fit the model
#' result <- model.change(d, model_name = "ordinal_model")
#'
#' # Examine the result
#' print(result)
#' }
#'
#' @importFrom rethinking ulam
#' @export
model.change <- function(d, model_name, ...) {
  # Prepare data for the ulam model
  dat <- list(
    R = as.ordinal(d$value),             # Ordinal response variable
    q = as.ordinal(d$question),          # Question identifier as an integer
    t = as.integer(d$wave),              # Ordinal variable for wave (time point)
    id = as.factor(d$id),                # Participant ID as a factor
    N = nrow(d),                         # Total number of observations
    N_id = length(unique(d$id)),         # Number of unique participants
    N_t = length(unique(d$wave)),        # Number of unique waves (time points)
    condition = ifelse(d$condition == "treatment", 1, 2), # Binary variable for condition (1 = treatment, 2 = control)
    is_treatment = d$wave %in% 1:4,      # Indicator for baseline/treatment phase (waves 1 to 4)
    is_fu1 = d$wave == 5,                # Indicator for first follow-up phase (wave 5 or higher)
    is_fu2 = d$wave == 6                 # Indicator for second follow-up phase (wave 6)
  )
  
  model <- alist(
    # Ordinal regression model (IRT model for ordinal responses)
    R ~ dordlogit(phi, cutpoints),
    phi <- alpha[q] * (theta - beta[q]),
    
    # Priors for the Item Response Theory (IRT) model parameters
    cutpoints ~ dnorm(0, 1.5),           # Normal prior for cutpoints
    alpha[q] ~ dlnorm(0, 0.5),           # Log-normal prior for item discrimination
    beta[q] ~ dnorm(0, 1),               # Normal prior for item difficulty
    
    # Generalized Additive Model (GAM) component for longitudinal effects
    theta <- bbar_baseline + b_baseline[id] +
      (bbar_treatment[condition] + b_treatment[id]) * (t - 1),
    
    # Define participant-specific effects using a non-centered parameterization
    transpars> vector[N_id]:b_baseline <<- v[, 1],       # Baseline random intercept for each participant
    transpars> vector[N_id]:b_treatment <<- v[, 2],      # Random effect of treatment phase for each participant
    transpars> matrix[N_id, 2]:v <- compose_noncentered(L_sigma, L_Rho, Z), # Non-centered parameterization
    
    # Priors for random effects and other model parameters
    matrix[2, N_id]:Z ~ normal(0, 1),                    # Standard normal prior for latent variable Z
    bbar_baseline ~ normal(0, 1.5),                      # Normal prior for overall intercept
    bbar_treatment[condition] ~ normal(0, 0.5),          # Normal prior for average treatment effect
    bbar_fu1[condition] ~ normal(0, 0.5),                # Normal prior for average first follow-up effect
    bbar_fu2[condition] ~ normal(0, 0.5),                # Normal prior for average second follow-up effect
    
    cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky(2),# LKJ prior for Cholesky factor of correlation matrix
    vector[2]:L_sigma ~ exponential(1),                  # Exponential prior for standard deviations
    
    # Convert Cholesky factor to a full correlation matrix
    transpars> matrix[2, 2]:Rho <<- Chol_to_Corr(L_Rho)
  )
  
  return(run_model(model, data = dat, model_name = paste0('change.', model_name), ...))
}

# Function to link theta values to raw OASIS scores for a specific condition
# Inputs:
# - post: Posterior samples from the fitted model
# - condition: Integer representing the experimental condition (1: WET, 2: EW, 3: Waitlist)
change_link <- function(post, condition) {
  # Construct theta for each posterior sample and participant across waves
  theta <- with(post, cbind(
    bbar_baseline,
    bbar_treatment[, condition]*3
  )) %>%
    apply(1, cumsum) %>%  # Calculate cumulative sum across waves for longitudinal effects
    t  # Transpose to match the structure [samples x waves]
  
  # Initialize a matrix to store raw OASIS scores
  mat <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
  
  # Iterate over each posterior sample and wave to compute raw scores
  for (i in 1:nrow(theta)) {
    for (j in 1:ncol(theta)) {
      # defined in model.helpers
      mat[i, j] <- theta_to_estimate(
        theta = theta[i, j],
        alpha = post$alpha[i, ],
        beta = post$beta[i, ],
        cutpoints = post$cutpoints[i, ]
      )
    }
  }
  
  return(mat)
}