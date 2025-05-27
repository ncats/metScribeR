#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @title create_eic
#'
#' @description
#' Create an eic given data, a list of mzs to look for, and a mz tolerance
#'
#' @param data the data from parse_mzML function output
#' @param mz_list a numeric vector of the mz values to search for
#' @param eic_mz_tolerance the mz tolerance for eic creation in ppm
#'
#' @returns an eic as a tibble
#'
#' @export
create_eic <- function(data, mz_list, eic_mz_tolerance) {
  results_list <- list()

  for (i in 1:length(mz_list)) {
    mz_i <- mz_list[i]

    results_list[[i]] <- data %>%
      dplyr::filter(.data$mz > mz_i * (1 - eic_mz_tolerance / 1E6), .data$mz < mz_i * (1 + eic_mz_tolerance / 1E6)) %>%
      dplyr::group_by(.data$retention_time) %>% dplyr::summarize(intensity = sum(.data$intensity)) %>% dplyr::mutate(mz = mz_i)
  }
  return(dplyr::bind_rows(results_list))
}
