#' @importFrom magrittr %>%

#' @title close_counter
#'
#' @description
#' Returns the number of points within the dist_threshold of each point. helper function for the density filtering thing after chromatographR::find_peaks()
#'
#' @param points_numeric_vector The vector of points given by a 1 dimensional coordinate. A numeric vector.
#' @param dist_threshold The distance used as the threshold to count points inside of.
#'
#' @returns vector of the number of points within the dist threshold for each point
#'
#' @keywords internal
#' @noRd
close_counter <- function(points_numeric_vector, dist_threshold) {
  result <- c()
  for (i in 1:length(points_numeric_vector)) {
    result[i] <- sum(abs(points_numeric_vector - points_numeric_vector[i]) < dist_threshold, na.rm = T)
  }
  return(result)
}



#' @title qscoreCalculator
#'
#' @description
#' helper function that calculates the beta_shape_parameter, novel_snr, and correlation_
#' with_beta_curve quality evaluation metrics. The function is borrowed from the GitHub of
#' https://github.com/wkumler/MS_metrics/blob/main/feature_extraction.R
#'
#' @param rt numeric vector of the retention times of points in a single peak being evaluated
#' @param int numeric vector of the intensities of points in the peak corresponding to each retention time
#'
#' @returns a list with the calculated SNR metric, peak correlation with fit beta curve, and beta shape parameter best fitting the data.
#'
#' @keywords internal
#' @noRd
qscoreCalculator <- function(rt, int){
  #Check for bogus EICs
  if(length(rt)<5){
    return(list(SNR=NA, peak_cor=NA))
  }
  #Calculate where each rt would fall on a beta dist (accounts for missed scans)
  scaled_rts <- (rt-min(rt))/(max(rt)-min(rt))

  # Create a couple different skews and test fit
  maybe_skews <- c(2.5,3,4,5) #Add 7 to catch more multipeaks and more noise
  #Add 2 to catch very slopey peaks and more noise

  best_skew <- maybe_skews[which.max(sapply(maybe_skews, function(x){

    stats::cor(stats::dbeta(scaled_rts, shape1 = x, shape2 = 5), int)
  }))]


  perf_peak <- stats::dbeta(scaled_rts, shape1 = best_skew, shape2 = 5)

  peak_cor <- stats::cor(perf_peak, int)



  #Calculate the normalized residuals
  residuals <- int/max(int)-perf_peak/max(perf_peak)
  #Calculate the minimum SD, after normalizing for any shape discrepancy

  old_res_sd <- stats::sd(residuals, na.rm = T)
  norm_residuals <- residuals
  new_res_sd <- stats::sd(norm_residuals)
  while(new_res_sd<old_res_sd){
    old_res_sd <- new_res_sd
    norm_residuals <- diff(residuals)
    new_res_sd <- stats::sd(residuals)
  }
  #Calculate SNR
  SNR <- (max(int)-min(int))/stats::sd(norm_residuals*max(int))
  #Return the quality score
  return(list(SNR=SNR, peak_cor=peak_cor, peak_shape_param = best_skew))
}

#' @title calculate_skew_and_sd
#'
#' @description
#' Returns the skew and standard deviation of the input distribution, assuming the distribution considered is the Riemann sum under the curve given.
#' The approach is to consider that we have points along the unnormalized probability distribution function of ions hitting the detector.
#'
#' @param x_value numeric vector of x values corresponding to f(x) values of the distribution i.e. the locations where the height of the curve is measured.
#' @param function_of_x numeric vector of the height of the distribution at the corresponding location from x_value
#'
#' @returns A list with the skew, given by the mean minus the median divided by the standard deviation, and the standard deviation
#'
#' @keywords internal
#' @noRd
calculate_skew_and_sd <- function(x_value, function_of_x) {
  temp <- data.frame(x_value = x_value, function_of_x = function_of_x) %>%
    dplyr::arrange(x_value) %>%
    dplyr::mutate(lagged_x = dplyr::lag(x_value),
           dist_to_next_x = x_value - .data$lagged_x,
           area_estimate = .data$dist_to_next_x * function_of_x,
           normalized_area_estimate = .data$area_estimate / sum(.data$area_estimate, na.rm=T),
           mean = sum(.data$normalized_area_estimate * x_value, na.rm = T),
           running_area = running_area(.data$normalized_area_estimate),
           standard_dev = sum(x_value^2 * .data$normalized_area_estimate, na.rm=T) - .data$mean^2,
           dist_to_median = abs(.data$running_area - 0.5),
           isMedian_row = .data$dist_to_median == min(.data$dist_to_median),
           median = max(x_value*.data$isMedian_row),
           skew = (.data$mean - .data$median) / .data$standard_dev)

  return(list(skew_result = mean(temp$skew), sd_result = mean(temp$standard_dev)))
}

#This is a helper function for calculate_skew_and_sd
#it returns the running sum of vector_of_areas.
#' @title running_area
#'
#' @description
#' Returns the running sum of the vector given, in this application in our code the values are areas under a curve
#'
#' @param vector_of_areas numeric vector of values to calculate a running sum for
#'
#' @returns a vector of the running sum of the input vector. Has the same length as the input.
#'
#' @keywords internal
#' @noRd
running_area <- function(vector_of_areas) {
  running_sum <- c()
  for (i in 1:length(vector_of_areas)) {
    running_sum[i] <- sum(vector_of_areas[1:i], na.rm=T)
  }
  return(running_sum)
}

#' @title directory_setup
#'
#' @description
#' Sets up a blank output directory if it does not already have the correct folders to receive metScribeR output.
#' Currently, only creates the Figures directory
#'
#' @param output_directory_path path to the location of the output directory
#'
#' @returns Returns nothing but has the external effect of creating the Figures directory in the specified output directory
#'
#' @keywords internal
#' @noRd
directory_setup <- function(output_directory_path) {
  output_directory_path <- stringr::str_c(output_directory_path, '/')
  if (!dir.exists(stringr::str_c(output_directory_path, 'Figures'))) {

    dir.create(stringr::str_c(output_directory_path, 'Figures'))
  }
}

#' @title sample_if_larger
#'
#' @description
#' if the input vector is longer in length than the given threshold, takes a sample of the input of the size of the threshold. Otherwise, does nothing.
#'
#' @param values_to_sample The vector of values to consider
#' @param threshold_for_sampling the threshold and maximum size of the returned vector
#'
#' @returns a subset or complete set of values_to_sample, depending on the length of values_to_sample
#'
#' @keywords internal
#' @noRd
sample_if_larger <- function(values_to_sample, threshold_for_sampling) {
  if (length(values_to_sample) > threshold_for_sampling) {
    return(sample(values_to_sample, threshold_for_sampling))
  } else if (length(values_to_sample) <= threshold_for_sampling) {
    return(values_to_sample)
  }
}

#' @title custom_naming_function
#'
#' @description
#' A custom naming function to simplify package code
#'
#' @param thing_to_name The object to be named
#' @param name The name assigned to thing_to_name
#'
#' @returns The named version of thing_to_name
#'
#' @keywords internal
#' @noRd
custom_naming_function <- function (thing_to_name, name) {
  names(thing_to_name) <- name
  return(thing_to_name)
}

#' @title check_standard_df
#'
#' @description
#' Produces an error if required columns are missing, standards are missing mzML files, or unknown columns are present. 
#'
#' @param standard_df The object being checked
#'
#' @returns T if df passes, otherwise produces error with message
#'
#' @keywords internal
#' @noRd
check_standard_df <- function(standard_df) {
  
  #check if required column names are present
  if(!all( c('common_name',	'monoisotopic_mass',	'pos_mode_mzML_file_path',	'neg_mode_mzML_file_path') %in% colnames(standard_df))) {
    stop('One or more required columns are missing from your standard_df input. Please confirm the following column names are included: common_name, monoisotopic_mass, pos_mode_msML_file_path, and neg_mode_mzML_file_path')
  }
  
  #Check for rows with no mzML file
  standard_df_temp <- standard_df %>% dplyr::mutate(one_file_good = !is.na(pos_mode_mzML_file_path) | !is.na(neg_mode_mzML_file_path))
  if(!all(standard_df_temp$one_file_good)) {
    stop('One or more rows in your data has no mzML files associated. Please remove these rows before proceeding.')
  }
  
  #checks for duplicate or missing common_name
  if(length(standard_df$common_name) != length(unique(standard_df$common_name)) | any(is.na(standard_df$common_name))) {
    stop('Duplicate common_name entries detected. Please ensure all common_name entries are unique and not missing.')
  }
  
  #checks for missing mass
  if(any(is.na(standard_df$monoisotopic_mass))) {
    stop('Missing entries in monoisotopic_mass detected. Please remove or fill these rows.')
  }
  
  
  #check for extra columns
  if(!all(colnames(standard_df) %in%
          c('common_name',	'monoisotopic_mass',	'pos_mode_mzML_file_path',	'neg_mode_mzML_file_path', 'inchiKey', 'additional_identifiers'))) {
    stop('Unknown column name identified. Please ensure only the following columns are present in your standard_df file: 
         common_name,	monoisotopic_mass, pos_mode_mzML_file_path,	neg_mode_mzML_file_path, inchikey, additional_identifiers')
  }
  
  return(T)

}

#' @title check_adduct_df
#'
#' @description
#' Produces an error if required columns are missing, or if information is missing from columns
#'
#' @param adduct_df The object being checked
#'
#' @returns T if df passes, otherwise produces error with message
#'
#' @keywords internal
#' @noRd
check_adduct_df <- function(adduct_df) {
  #check if required column names are present
  if(!all( c('adduct', 'change_from_neutral', 'mode') %in% colnames(adduct_df))) {
    stop('One or more required columns are missing from your standard_df input. Please confirm the following column names are included: adduct, change_from_neutral, mode')
  }
  
  #Check for rows with no mzML file
  adduct_df_temp <- adduct_df %>% dplyr::mutate(good = !is.na(adduct) & !is.na(change_from_neutral) & !is.na(mode))
  if(!all(adduct_df_temp$good)) {
    stop('One or more rows in your data has missing entries. Please remove or fill these rows before proceeding.')
  }
  
  #check for extra columns
  if(!all(colnames(adduct_df) %in%
          c('adduct', 'change_from_neutral', 'mode'))) {
    stop('Unknown column name identified. Please ensure only the following columns are present in your standard_df file: 
         adduct, change_from_neutral, mode')
  }
  
  return(T)
  
}













