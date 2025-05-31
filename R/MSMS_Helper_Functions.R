
#' @title obtain_spectra_from_inchiKey
#'
#' @description
#' One of several functions that collect MS/MS info from MoNA;
#' this function submits the httr request to their REST query service. See https://mona.fiehnlab.ucdavis.edu/documentation/query.
#' This is also where the filter on only collecting HMDB spectra is implemented. To change this, edit the query search term assigned to the variable url
#'
#' @param inchiKey The inchi key spectra are searched for. This function only takes a single inchiKey
#'
#' @returns an httr response that needs to be parsed by the next function, get_spectra_from_result.
#'
#' @keywords internal
#' @noRd
obtain_spectra_from_inchiKey <- function(inchiKey) {
  # Define the URL
  url <- stringr::str_c("https://mona.fiehnlab.ucdavis.edu/rest/spectra/search?query=exists(compound.inchiKey%3A'", inchiKey, "')%20and%20exists((metaData.name%3A'ms%20level'%20and%20metaData.value%3A'MS2'))%20and%20exists((tags.text%3A'HMDB'))")
  print(url)
  # Make the GET request
  if (!is.na(url)) {
    response <- httr::GET(url)
    
    
    # Check the response status
    if (httr::status_code(response) == 200) {
  
      return(response)
  
    } else {
      print(paste("Request failed with status:", httr::status_code(response)))
    }
  }
}

#' @title get_spectra_from_result
#'
#' @description
#' This function takes the result of obtain_spectra_from_inchiKey and parses it into the actual spectra.
#' Only takes a single inchiKey result as input. The output of this function needs further parsing from expand_by_spectra to be usable
#' because all spectrum mz values are in a single string here.
#'
#' @param result the output of obtain_spectra_from_inchiKey
#'
#' @returns a dataframe of the spectrum information and inchiKey. Each row is a spectra, including all mz values in a single string.
#'
#' @keywords internal
#' @noRd
get_spectra_from_result <- function(result) {
  res_list <- list()
  temp <- httr::content(result)
 
  for ( i in seq_len(length(temp))) {

    res_list[[i]] <- temp[[i]]$metaData %>%
      lapply(data.frame) %>%
      dplyr::bind_rows() %>%
      dplyr::select(.data$name, .data$value) %>%
      tidyr::pivot_wider(names_from = .data$name, values_from = .data$value)


    res_list[[i]]$spectrum <- temp[[i]]$spectrum
    res_list[[i]]$inchiKey <- temp[[i]]$compound[[1]]$inchiKey

  }

  res_list %>% dplyr::bind_rows() %>% return()
}

#' @title expand_by_spectra
#'
#' @description
#' Puts the spectra that were initially in a single string per condition into a numeric format that can be filtered
#'
#' @param parsed_result The output of the get_spectra_from_result function
#'
#' @returns a dataframe with each row as an observation from a spectra, so that each mz value is its own row
#'
#' @keywords internal
#' @noRd
expand_by_spectra <- function(parsed_result) {
  results_list <- list()

  for (i in seq_len(nrow(parsed_result))) {
    temp <- parsed_result[i,]$spectrum %>%
      stringr::str_split(' ') %>%
      data.frame()

    colnames(temp) <- c('spectra_data')

    results_list[[i]] <- temp %>%
      tidyr::separate_wider_delim(.data$spectra_data, delim = ':', names = c( 'mz', 'relative_intensity')) %>%
      dplyr::cross_join(parsed_result[i,] %>% dplyr::select(-.data$spectrum))

  }
  temp_res_df <- dplyr::bind_rows(results_list)
  
   if (nrow(temp_res_df > 0)) {
     temp_res_df <-  temp_res_df %>%
    dplyr::mutate(mode = `ionization mode`, mz = as.numeric(.data$mz), relative_intensity = as.numeric(.data$relative_intensity))

   }
  return(temp_res_df)
}
