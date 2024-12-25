model.mechanism <- function(d_x, d_m, model_name, ...) {
  
  # Combine IDs from both data frames and create a unified ID mapping
  all_ids <- unique(c(d_x$id, d_m$id))
  id_map <- data.frame(id = all_ids, id_index = 1:length(all_ids))
  
  # Map IDs to integer indices in both data frames
  d_x <- d_x %>% left_join(id_map, by = "id")
  d_m <- d_m %>% left_join(id_map, by = "id")
  
  # Prepare data for the ulam model
  dat <- list(
    # Data from d_x
    R_x = as.ordinal(d_x$value),               # Ordinal response variable for x
    q_x = as.ordinal(d_x$question),            # Question identifier for x
    t_x = as.integer(d_x$wave),                # Wave (time point) for x
    id_x = d_x$id_index,                       # Participant IDs for x
    N_x = nrow(d_x),                           # Number of observations in x
    is_treatment_x = d_x$wave %in% 1:4,        # Treatment phase indicator for x
    is_fu1_x = d_x$wave == 5,                  # Follow-up 1 indicator for x
    is_fu2_x = d_x$wave == 6,                  # Follow-up 2 indicator for x
    N_id = length(all_ids),                    # Total number of unique participants
    G = case_when(
      d_x$Sex == 'Female' ~ 1,
      d_x$Sex == 'Male' ~ -1,
      .default = 0
    ),
    
    # Data from d_m
    R_m = as.ordinal(d_m$value),               # Ordinal response variable for m
    q_m = as.ordinal(d_m$question),            # Question identifier for m
    t_m = as.integer(d_m$wave),                # Wave (time point) for m
    id_m = d_m$id_index,                       # Participant IDs for m
    N_m = nrow(d_m),                           # Number of observations in m
    is_treatment_m = d_m$wave %in% 1:4,           # Treatment phase indicator for m
    is_fu1_m = d_m$wave == 5,                    # Follow-up 1 indicator for m
    is_fu2_m = d_m$wave == 6                     # Follow-up 2 indicator for m
  )
  
  # Define the model with duplicated variables
  model <- alist(
    # Ordinal regression model for x
    R_x ~ dordlogit(phi_x, cutpoints_x),
    phi_x <- alpha_x[q_x] * (theta_x - beta_x[q_x]),
    
    # Ordinal regression model for m
    R_m ~ dordlogit(phi_m, cutpoints_m),
    phi_m <- alpha_m[q_m] * (theta_m - beta_m[q_m]),
    
    # Priors for the IRT model parameters for x
    cutpoints_x ~ dnorm(0, 1.5),
    alpha_x[q_x] ~ dlnorm(0, 0.5),
    beta_x[q_x] ~ dnorm(0, 1),
    
    # Priors for the IRT model parameters for m
    cutpoints_m ~ dnorm(0, 1.5),
    alpha_m[q_m] ~ dlnorm(0, 0.5),
    beta_m[q_m] ~ dnorm(0, 1),
    
    # Longitudinal effects for x
    theta_x <-
      bbar_baseline_x + b_baseline_x[id_x] + G * gamma +
      (bbar_treatment_x + b_treatment_x[id_x]) * min(t_x - 1, 3) +
      (bbar_fu2_x + b_fu2_x[id_x]) * is_fu2_x,
    
    # Longitudinal effects for m
    theta_m <- bbar_baseline_m + b_baseline_m[id_m] +
      (bbar_treatment_m + b_treatment_m[id_m]) * min(t_m - 1, 3),
    
    # Participant-specific effects for x
    transpars> vector[N_id]:b_baseline_x <<- v[, 1],
    transpars> vector[N_id]:b_treatment_x <<- v[, 2],
    transpars> vector[N_id]:b_fu2_x <<- v[, 3],
    transpars> vector[N_id]:b_baseline_m <<- v[, 4],
    transpars> vector[N_id]:b_treatment_m <<- v[, 5],
    transpars> matrix[N_id, 5]:v <- compose_noncentered(L_sigma, L_Rho, Z),
    
    # Priors for random effects for x
    bbar_baseline_x ~ normal(0, 1.5),
    bbar_treatment_x ~ normal(0, 0.5),
    bbar_fu2_x ~ normal(0, 0.5),
    gamma ~ normal(0,0.5),                               # Normal prior for gender
    
    # Priors for random effects for m
    bbar_baseline_m ~ normal(0, 1.5),
    bbar_treatment_m ~ normal(0, 0.5),
    
    matrix[5, N_id]:Z ~ normal(0, 1),
    cholesky_factor_corr[5]:L_Rho ~ lkj_corr_cholesky(2),
    vector[5]:L_sigma ~ exponential(1),
    
    # Convert Cholesky factors to full correlation matrices
    transpars> matrix[5, 5]:Rho <<- Chol_to_Corr(L_Rho)
  )
  
  # Run the model using your existing run_model() function
  return(run_model(model, data = dat, model_name = paste0('mechanism.', model_name), ...))
}