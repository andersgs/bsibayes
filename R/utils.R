#' sims2data
#'
#' Transform sim_counts output to a data table that might resemble what people might actually have.
#'
#' @param table The output from sim_counts
#'

sims2data <- function(sims) {
  dat <- sims %>%
    dplyr::select(disease_type, labcode, year, total_bsi, count_r) %>%
    tidyr::spread(key = disease_type, value = count_r)
  return(dat)
}

#' recode_id
#'
#' @description Recoding any factor to have be a continuous integer
#'
#' @param labid A vector of IDs to be transformed
#'
recode_id <- function(ids) {
  if (is.factor(ids)) {
    uniq_ids <- as.character(levels(ids))
  } else {
    uniq_ids <- unique(ids)
  }
  ids <- as.character(ids)
  sorted_ids <- sort(uniq_ids)
  n_ids <- length(sorted_ids)
  ix <- 1:n_ids
  names(ix) <- sorted_ids
  recoded_ids <- ix[ids]
  return(recoded_ids)
}

#' gather_counts
#'
#' @description A function to gather all the counts for the different species in a table
#'
#' @param data A data.frame as supplied to df2stan
#'

gather_counts <- function(data) {
  all_counts <- data %>%
    dplyr::select(-year, -labcode, -total_bsi) %>%
    dplyr::mutate(ix = 1:nrow(.)) %>%
    tidyr::gather(species, counts, -ix)
  return(all_counts)
}
