source(here('scripts','model.helpers.R'))

#' Fit an Ordinal Regression Model with Longitudinal Effects with FU (full growth curve)
#'
#' This function fits an ordinal regression model with longitudinal effects using the \code{ulam} function from the \pkg{rethinking} package. The model incorporates item response theory (IRT) parameters, longitudinal time effects, and participant-specific random effects using a non-centered parameterization.
#'
#' @param d A data frame containing the following columns:
#'   \describe{
#'     \item{\code{value}}{Numeric vector of ordinal response values.}
#'     \item{\code{question}}{Numeric or factor vector indicating question identifiers.}
#'     \item{\code{wave}}{Numeric vector indicating time points.}
#'     \item{\code{id}}{Numeric or factor vector indicating participant IDs.}
#'     \item{\code{condition}}{Character or factor vector indicating the experimental condition, with levels \code{"treatment"}, \code{"control"}, and \code{"waitlist"}.}
#'     \item{\code{Sex}}{Character or factor vector indicating participant gender, with levels \code{"Female"}, \code{"Male"}, etc.}
#'   }
#' @param model_name A character string specifying the name of the model. This is used in the output and for saving the model.
#' @param ... Additional arguments to be passed to the \code{run_model()} function.
#'
#' @return An object containing the fitted model, as returned by the \code{run_model()} function.
#'
#' @details
#' The function prepares the data and specifies a complex ordinal regression model, which is then fitted using the \code{ulam} function from the \pkg{rethinking} package. The model includes:
#' \itemize{
#'   \item \strong{Ordinal Regression (IRT Model)}: Models the ordinal response variable \code{R} using a logistic ordinal regression, incorporating item discrimination (\code{alpha}) and item difficulty (\code{beta}) parameters.
#'   \item \strong{Longitudinal Effects}: Includes baseline, treatment phase, and follow-up phases (\code{theta}), modeled as linear combinations of fixed and random effects.
#'   \item \strong{Participant-specific Random Effects}: Random intercepts and slopes for each participant (\code{b_baseline}, \code{b_treatment}, \code{b_fu1}, \code{b_fu2}), using a non-centered parameterization for efficient sampling.
#'   \item \strong{Priors}: Specifies priors for all model parameters, including the Cholesky factor of the correlation matrix (\code{L_Rho}) and standard deviations (\code{L_sigma}).
#'   \item \strong{Generated Quantities}: Computes the correlation matrix from the Cholesky factor.
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
#'   condition = sample(c("treatment", "control", "waitlist"), 100, replace = TRUE),
#'   Sex = sample(c("Female", "Male"), 100, replace = TRUE)
#' )
#'
#' # Fit the model
#' result <- model.growth(d, model_name = "ordinal_model")
#'
#' # Examine the result
#' print(result)
#' }
#'
#' @importFrom rethinking ulam
#' @export
model.growth <- function(d, model_name, ...) {
  # Prepare data for the ulam model
  dat <- list(
    R = as.ordinal(d$value),             # Ordinal response variable
    q = as.ordinal(d$question),          # Question identifier as an integer
    t = as.integer(d$wave),              # Ordinal variable for wave (time point)
    id = as.factor(d$id),                # Participant ID as a factor
    G = case_when(
      d$Sex == 'Female' ~ 1,
      d$Sex == 'Male' ~ -1,
      .default = 0
    ),
    N = nrow(d),                         # Total number of observations
    N_id = length(unique(d$id)),         # Number of unique participants
    N_t = length(unique(d$wave)),        # Number of unique waves (time points)
    condition = case_when(
      d$condition == "treatment"~ 1,
      d$condition == "control"~ 2,
      d$condition == "waitlist"~ 3
    ), 
    is_fu1 = d$wave == 5 | d$wave == 6,                # Indicator for first follow-up phase (wave 5 or higher)
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
    theta <- bbar_baseline[condition] + b_baseline[id] + G * gamma +
      (bbar_treatment[condition] + b_treatment[id]) * min(t - 1, 3) +
      (bbar_fu1[condition] + b_fu1[id]) * is_fu1 +
      (bbar_fu2[condition] + b_fu2[id]) * is_fu2,
    
    # Define participant-specific effects using a non-centered parameterization
    transpars> vector[N_id]:b_baseline <<- v[, 1],       # Baseline random intercept for each participant
    transpars> vector[N_id]:b_treatment <<- v[, 2],      # Random effect of treatment phase for each participant
    transpars> vector[N_id]:b_fu1 <<- v[, 3],            # Random effect of first follow-up phase for each participant
    transpars> vector[N_id]:b_fu2 <<- v[, 4],            # Random effect of second follow-up phase for each participant
    transpars> matrix[N_id, 4]:v <- compose_noncentered(L_sigma, L_Rho, Z), # Non-centered parameterization
    
    # Priors for random effects and other model parameters
    matrix[4, N_id]:Z ~ normal(0, 1),                    # Standard normal prior for latent variable Z
    bbar_baseline[condition] ~ normal(0, 1.5),                      # Normal prior for overall intercept
    bbar_treatment[condition] ~ normal(0, 0.5),          # Normal prior for average treatment effect
    bbar_fu1[condition] ~ normal(0, 0.5),                # Normal prior for average first follow-up effect
    bbar_fu2[condition] ~ normal(0, 0.5),                # Normal prior for average second follow-up effect
    gamma ~ normal(0,0.5),                               # Normal prior for gender
    
    cholesky_factor_corr[4]:L_Rho ~ lkj_corr_cholesky(2),# LKJ prior for Cholesky factor of correlation matrix
    vector[4]:L_sigma ~ exponential(1),                  # Exponential prior for standard deviations
    
    # Convert Cholesky factor to a full correlation matrix
    transpars> matrix[4, 4]:Rho <<- Chol_to_Corr(L_Rho)
  )
  
  return(run_model(model, data = dat, model_name = paste0('growth.', model_name), ...))
}

# Function to link theta values to raw OASIS scores for a specific condition
# Inputs:
# - post: Posterior samples from the fitted model
# - condition: Integer representing the experimental condition (1: WET, 2: EW, 3: Waitlist)
growth_link <- function(post, condition) {
  # Construct theta for each posterior sample and participant across waves
  theta <- with(post, cbind(
    bbar_baseline[, condition],
    bbar_treatment[, condition],
    bbar_treatment[, condition],
    bbar_treatment[, condition],
    bbar_fu1[, condition],
    bbar_fu2[, condition]
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