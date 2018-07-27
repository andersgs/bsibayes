#' df2stan
#' @description A function to transform data.frame in to a stan data list ready for the models described here
#'
#' @param data A data.frame. An example can be found by simulating data with `sim_counts` and looking at the wide-format
#' @param include_missing A boolean value on whether to include or exclude missing data (if included, NA will be replaced with the mean of the relevant data)
#' @param is_log_bsi A boolean value determing if total BSI is on the log scale or not
#'
#'
#'@export
#'

df2stan <- function(data, include_missing = FALSE, is_log_bsi = FALSE) {
  standata <- list()
  if(!include_missing) {
    data <- data %>%
      tidyr::drop_na()
  }
  counts <- gather_counts(data)
  standata[['N']] <- nrow(data)
  standata[['M']] <- nrow(counts)
  standata[['total_bsi']] <- data %>%
    dplyr::mutate(total_bsi = ifelse(is_log_bsi, total_bsi, log(total_bsi))) %>%
    dplyr::pull(total_bsi)
  standata[['count']] <- counts %>% dplyr::pull(counts)
  standata[['ix']] <- counts %>% dplyr::pull(ix)
}
