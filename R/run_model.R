#' Simple Bayesian models with no missing data imputation
#'
#' @param data A data.frame with some data in the same format as ouputted by `sim_counts`
#' @param model A string specifying which model to run
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
#'@export
run_models_ni <- function(data, model, ...) {
  notif_data <- data %>%
    dplyr::filter(disease_type == 'NOTIFIABLE' & total_bsi != -99)
  standata <- list(N = nrow(notif_data),
                  count = notif_data[['count_r']],
                  log_bsi = log(notif_data[['total_bsi']]))
  out <- rstan::sampling(bsibayes:::stanmodels[[model]], data = standata, ...)
  attr(out, "data") <- notif_data
  return(out)
}
