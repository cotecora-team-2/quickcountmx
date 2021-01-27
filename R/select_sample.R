#' Select simple and stratified random samples from a sampling frame.
#'
#' Select Simple Randmom Samples and Stratified Random Samples,
#' \code{select_sample_prop} can be used when sampling with equal probability
#'   across strata or when selecting a simple random sample.
#' @param sampling_frame \code{tibble} with the sampling frame it must contain a
#'   column with the stratum.
#' @param stratum unquoted column with strata in the allocation and
#'   sampling_frame \code{tibble}'s (the columns must have the same name in the
#'   two \code{tibble}'s). If one wants to select a SRS the stratum
#'   parameter is not used.
#' @param frac when sampling with equal probability across strata, frac is a
#'   numeric value indicating the fraction of the data to select.
#' @param replace logical value indicating whether the sample should be selected
#' with replacement.
#' @param seed integer value used to set the state of the random number generator.
#' @return A \code{tibble} with the selected sample, it will have the same
#'   columns as the original sampling frame plus a column indicating the sample
#'   size in the stratum of each selected observation.
#' @importFrom dplyr %>%
#' @name select_sample
NULL
#> NULL
#' @rdname select_sample
#' @export
select_sample_prop <- function(sampling_frame, stratum = stratum, frac,
                               seed = NA, replace = FALSE){
  if (!is.na(seed)) set.seed(seed)
  if (missing(stratum)) {
    sample <- dplyr::sample_frac(sampling_frame, size = frac, replace = replace)
  } else {
    sample <- sampling_frame %>%
      dplyr::group_by({{ stratum }}) %>%
      dplyr::sample_frac(size = frac, replace = replace) %>%
      dplyr::ungroup()
  }
  return(sample)
}
