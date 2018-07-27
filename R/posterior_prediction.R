#' Posterior predictive check
#'
#' @param fit: a stanfit object fit by one of the models in this packages
#' @param new_data: a data.frame containing the new data to estimate
#' @param draws: how many draws from the posterior
#' @param seed: seed the random generator
#'
#'@export

posterior_predict <- function(fit, draws, seed) {
  set.seed(seed = seed)
  model_name <- fit@model_name
  if(model_name == 'model1') {
    data <- attr(fit, 'data') %>%
      mutate(log_bsi = log(total_bsi))
    lambda_pos <- c(as.matrix(fit, pars='lambda'))
    sample_pos <- sample(x = lambda_pos, size = draws)
    tmp <- lapply(1:length(sample_pos), function(ix) {
      data %>%
        mutate(rep = ix,
               lambda = sample_pos[ix]) %>%
        mutate(count_hat = rpois(n = n(), lambda = exp(log_bsi + lambda)))
    })
  }
  return(dplyr::bind_rows(tmp))
}

