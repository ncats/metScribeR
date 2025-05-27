#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @title parse_mzML
#'
#' @description
#' This function returns data in an R dataframe from mzML format
#'
#' @details
#' This function is a wrapper around mzR::openMSfile(), mzR::header(), & mzR::peaks(). It is
#' similar to what Spectra does, but faster in the metScribeR context. Designed for use only for MS1 data.
#'
#' @param path path to the mzML file to be read
#' @param noise_level level below which all MS observations will be excluded.
#' @param mode ESI mode of data, "POS" or "NEG"
#'
#'  @export
parse_mzML <- function(path, noise_level, mode = NA) {

  #opens mzML file for reading on disk
  x_handle <- mzR::openMSfile(path)

  #Pull information about mode from path. File name must include POS or NEG to specify.
#  if (is.na(mode)) {
    mode <- stringr::str_extract(path, 'POS|NEG')
    if (is.na(mode)) warning('No POS or NEG detected in file path. Please add the correct mode to the file name')
#  }

  #Pull metadata about each scan into R, and select what we are interested in (retentionTime). Note retentionTime here starts in seconds and is converted to minutes.
  x_meta <- mzR::header(x_handle) %>%
    dplyr::select(.data$seqNum, .data$retentionTime) %>%
    dplyr::mutate(mode = mode, retentionTime = .data$retentionTime / 60)

  #Pull mz and intensity data into R as a list ordered by scan number. Convert each scan's data to be a dataframe.
  x_data <- mzR::peaks(x_handle) %>% lapply(data.frame)

  #label each scan with its index before binding the list they are in into a single dataframe in next step
  for (i in 1:length(x_data)) {
    x_data[[i]] <- dplyr::mutate(x_data[[i]], scan = i)
  }

  #Now pull everything together into a single dataframe.
  temp <- dplyr::bind_rows(x_data) %>%
    dplyr::left_join(x_meta, dplyr::join_by(x$scan == y$seqNum)) %>%
    dplyr::select(-.data$scan)

  comment(temp) <- stringr::str_c(min(temp$retentionTime), max(temp$retentionTime), sep = ',')

  if (nrow(temp > 0)) {
  temp %>%
    dplyr::filter(.data$intensity > noise_level) %>%
    dplyr::rename(retention_time = .data$retentionTime) %>%
    return()
  } else {

    return(data.frame())
  }
}
