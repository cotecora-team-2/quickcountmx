#' Select simple and stratified random samples from a sampling frame.
#'
#' Select Simple Randmom Samples and Stratified Random Samples,
#' \code{select_sample_prop} can be used when sampling with equal probability
#'   across strata or when selecting a simple random sample.
#' @param sampling_frame \code{tibble} with the sampling frame it must contain a
#'   column with the stratum.
#' @param allocation \code{data.frame} with a column defining the strata and a
#'   column with sample size allocations for each stratum (one line per stratum).
#' @param sample_size unquoted column with sample sizes in the allocation
#'   data.frame
#' @param stratum unquoted column with strata in the allocation and
#'   sampling_frame \code{tibble}'s (the columns must have the same name in the
#'   two \code{tibble}'s). If one wants to select a SRS the stratum
#'   parameter is not used.
#' @param frac when sampling with equal probability across strata, frac is a
#'   numeric value indicating the fraction of the data to select.
#' @param is_frac logical value indicating whether the allocation data.frame contains
#'   proportions to be sampled within each stratum (TRUE) or sample sizes.
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

#' @rdname select_sample
#' @importFrom rlang :=
#' @export
select_sample_str <- function(sampling_frame, allocation,
                              sample_size = sample_size, stratum = stratum,
                              is_frac = FALSE, seed = NA,
                              replace = FALSE){
  if (!is.na(seed)) set.seed(seed)

  stratum_var_str <- deparse(substitute(stratum))
  frame_grouped_tbl <- sampling_frame %>%
    dplyr::left_join(allocation, by = stratum_var_str) %>%
    dplyr::group_by({{ stratum }})

  if (is_frac) {
    sample_tbl <- frame_grouped_tbl %>%
      dplyr::sample_frac({{ sample_size }}, replace = replace)
  } else {
    # if sample size not integer we round it
    frame_grouped_tbl <- frame_grouped_tbl %>%
      dplyr::mutate("{{ sample_size}}" := round({{sample_size}}))
    sample_tbl <- frame_grouped_tbl %>%
      dplyr::sample_n({{ sample_size }}, replace = replace)
  }

  sample_tbl <- sample_tbl %>%
    dplyr::select(dplyr::any_of(colnames(sampling_frame)))
  return(sample_tbl)
}
