#' A function to simulate counts
#' @param disease_param A data frame with disease parameters
#' @param inst_param A data frame with institutional parameters
#' @param years A vector of years
#' @param  seed An integer to set the seed.
#'
#' @return A list with two elements:
#'           `long_sim`: a tibble with seven columns:
#'           (1) disease_type;
#'           (2) inst_id;
#'           (3) total_bsi: the reported bsi by the institution (-99 if missing);
#'           (4) count_r: the reported count by the institution (-99 if missing);
#'           (5) bsi: the actual bsi (equal to bsi_r if reporting is TRUE);
#'           (6) count: the actual count (equal to count_r if reporting is TRUE);
#'           (7) pi_dy: the year-specific disease rate
#'           `wide_sim`: a tibble the more closely resembles what someone's data might look like:
#'           (1) labcode;
#'           (2) year;
#'           (3) total_bsi; # data reported by a lab
#'           (4) one or more columns with counts of data for each species as reported by lab
#'
#'
#'@export
sim_counts <- function(disease_param,
                       inst_param,
                       years,
                       seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  # figure out how many years are being modelled
  n_years = length(years)
  years_c <- years - mean(years)
  I <- INST_PARAM %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bsi = list(rpois(n = n_years,
                                   lambda = lambda)))
  D <- DISEASE_PARAM %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pi_dy =  list(pi + beta*years_c))

  out = apply(I,1, function(i, yrs) {
    id <- i[['id']]
    size <- i[['size']]
    lambda <- i[['lambda']]
    reporting <- i[['reporting']]
    bsi <- i[['bsi']]
    years <- yrs
    out = apply(D, 1, function(j, id, reporting, bsi, years) {
      n_years = length(years)
      pi_dy = j[['pi_dy']]
      disease_type = j[['disease_type']]
      count = rbinom(n = n_years, size = bsi, prob = pi_dy)
      if(reporting) {
        bsi_reported = bsi
        count_reported = count
      } else {
        if(disease_type == 'NOTIFIABLE') {
          count_reported = count
        } else {
          count_reported = rep(NA, n_years)

        }
        bsi_reported = rep(NA, n_years)
      }
      out = tibble::tibble(disease_type = disease_type,
                           labcode = id,
                           year = years,
                           total_bsi = bsi_reported,
                           count_r = count_reported,
                           bsi = bsi,
                           count = count,
                           pi_dy = pi_dy)
    }, id, reporting, bsi, years)
    dplyr::bind_rows(out)
  }, yrs = years)
  out = dplyr::bind_rows(out)
  out = list(long_sim = out,
             wide_sim = sims2data(out))
  return(out)
}
