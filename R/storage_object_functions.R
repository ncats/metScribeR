#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom foreach %dopar%


#' @title user_data_input
#'
#' @description Collects data and parameters from user and adds them to the storage_object.
#'
#' @details
#' The data and parameters collected here are for eic creation. Data are stored
#'  in the appropriate table/slot in the storage_object where they can be used in later computation.
#'
#'  These parameters are associated with the active library id.
#'
#' @param storage_object A metScribeR object
#'
#' @param standard_df An R dataframe with the following columns:
#'  common_name as character, monoisotopic_mass as numeric, pos_mode_mzML_file_path
#'  as character, neg_mode_mzML_file_path as character. File paths should be absolute,
#'  beginning with C:/ or H:/ or similar.
#'
#'  This dataframe is the input related to the standards being processed by metScribeR.
#'
#' @param adduct_search_df An R dataframe with the following columns:
#' adduct as character, change_from_neutral as numeric, and mode as character.
#' For example: data.frame(adduct = 'M+H', change_from_neutral = 1.008 amu, mode = 'POS')
#'
#' This dataframe is the input related to which adducts will be searched for each standard.
#'
#' @param eic_mz_tolerance a numeric value giving the m/z window for eic creation in PPM.
#'
#' @param eic_rt_tolerance a numeric value giving the rt window distinguishing adducts in min.
#'
#' @param mzML_parsing_noise_level Describes the level of intensity in the raw
#'  data below which all signals are discarded as noise.
#'
#' @return an updated storage_object
#'
#' @export
user_data_input <- function(
    storage_object,
    standard_df,
    adduct_search_df,
    eic_mz_tolerance,
    eic_rt_tolerance,
    mzML_parsing_noise_level,
    output_directory_path
) {

  #standard_df
  standard_df$unique_standard_id <- c(1:nrow(standard_df)) + max(0, max(storage_object$standard_df, na.rm = T))

  storage_object$standard_df <- dplyr::bind_rows(storage_object$standard_df, standard_df)


  #adduct_search_df
  storage_object$adduct_search_df <- dplyr::bind_rows(storage_object$adduct_search_df, adduct_search_df)

  #eic_mz_tolerance
  storage_object$library_data$eic_mz_tolerance <- eic_mz_tolerance

  #eic_rt_tolerance
  storage_object$library_data$eic_rt_tolerance <- eic_rt_tolerance

  #mzML_parsing_noise_level
  storage_object$library_data$mzML_parsing_noise_level <- mzML_parsing_noise_level
  
  storage_object$library_data$output_directory_path <- output_directory_path

  files_for_import <- c(storage_object$standard_df$pos_mode_mzML_file_path, storage_object$standard_df$neg_mode_mzML_file_path)

  if (!all(file.exists(files_for_import))) {
    files_missing <- files_for_import[!file.exists(files_for_import)]
    stop(stringr::str_c('Files ', paste(files_missing, collapse = ' '), ' from your standards csv do not exist. Please fix this before proceeding.'))
  }

  invisible(storage_object)
}


#' @title user_peak_params_input
#'
#' @description Collect information for feature finding in eic data.
#'
#' @details
#' This function collects parameters to pass to the feature finding function.
#' Feature finding uses chromatographR::find_peaks(), so see that package's documentation for details.
#' The density window and density threshold parameters are designed to eliminate hanging points on peaks
#' that are noise far in RT from any other point. The algorithm will count how many other observations belonging
#' to a given peak are within the RT window and eliminate any points with fewer found than the threshold parameter.
#'
#' These parameters are associated with the active library id.
#'
#'
#' @param storage_object A metScribeR object
#' @param smooth_type passed to chromatographR::find_peaks()
#' @param smooth_window passed to chromatographR::find_peaks()
#' @param slope_thresh passed to chromatographR::find_peaks()
#' @param amp_thresh passed to chromatographR::find_peaks()
#' @param density_window_rt_distance the distance, measured in RT, within which
#'  the number of other points is counted for subsequent filtering.
#' @param density_count_in_window_threshold any point with fewer points in the given RT window
#' than this threshold will be removed.
#'
#' @return an updated storage_object
#'
#' @export
user_peak_params_input <- function(
    storage_object,
    smooth_type,
    smooth_window,
    slope_thresh,
    amp_thresh,
    density_window_rt_distance,
    density_count_in_window_threshold
) {
  storage_object$library_data$smooth_type <- smooth_type

  storage_object$library_data$smooth_window <- smooth_window

  storage_object$library_data$slope_thresh <- slope_thresh

  storage_object$library_data$amp_thresh <- amp_thresh

  storage_object$library_data$density_window_rt_distance <- density_window_rt_distance

  storage_object$library_data$density_count_in_window_threshold <- density_count_in_window_threshold

  invisible(storage_object)
}

#' @title import_eic_data
#'
#' @details Imports mzML data and creates EICs for each adduct*standard combination provided with the adduct_search_df and standard_df
#' These are stored in the storage_object for use with further functions. Runs in parallel and could take several hours to run
#' depending on the number of files being processed. Only functions on the active library.
#'
#' @param storage_object a metScribeR object
#'
#' @return an updated metScribeR object
#'
#' @export
import_eic_data <- function (storage_object) {

  standards_for_import <- storage_object$standard_df

  #import those files to the dataframes here with foreach loop over standards:

  #set up the cluster for parallel processing
  ncores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  noise_level <- storage_object$library_data$mzML_parsing_noise_level

  print(stringr::str_c('Starting data processing with ', foreach::getDoParWorkers(), ' cores.'))

  adduct_search_df <- storage_object$adduct_search_df

  eic_mz_tolerance <- storage_object$library_data$eic_mz_tolerance

  results_list <- foreach::foreach(i = 1:nrow(standards_for_import), .packages = c('magrittr', 'dplyr'), .export = c('parse_mzML', 'create_eic')) %dopar% {

    data_pos <- parse_mzML(standards_for_import[[i, 'pos_mode_mzML_file_path']], noise_level) %>% dplyr::mutate(mode = 'POS')

    data_neg <- parse_mzML(standards_for_import[[i, 'neg_mode_mzML_file_path']], noise_level) %>% dplyr::mutate(mode = 'NEG')

    #create list of mz values for eic here
    adduct_search_df_i <- adduct_search_df %>% dplyr::mutate(mz = .data$change_from_neutral + standards_for_import[[i, 'monoisotopic_mass']])

    #create the eic's here
    eic_pos <- create_eic(data_pos, adduct_search_df_i %>% dplyr::filter(.data$mode == 'POS') %>% dplyr::pull(.data$mz), eic_mz_tolerance) %>%
      dplyr::mutate(mode = 'POS') %>%
      dplyr::left_join(adduct_search_df_i, dplyr::join_by(x$mz == y$mz, x$mode == y$mode))

    eic_neg <- create_eic(data_neg, adduct_search_df_i %>% dplyr::filter(.data$mode == 'NEG') %>% dplyr::pull(.data$mz), eic_mz_tolerance) %>%
      dplyr::mutate(mode = 'NEG') %>%
      dplyr::left_join(adduct_search_df_i, dplyr::join_by(x$mz == y$mz, x$mode == y$mode))

    dplyr::bind_rows(eic_pos, eic_neg) %>% dplyr::mutate(unique_standard_id = standards_for_import[[i, 'unique_standard_id']])
  }

  parallel::stopCluster(cl)

  results_list <- dplyr::bind_rows(results_list) %>% dplyr::rename(mz_value = .data$mz, adduct_identity = .data$adduct)

  #store appropriate adduct-level results in adduct_df

  temp_adduct_additions <- results_list %>% dplyr::distinct(.data$adduct_identity, .data$mz_value, .data$unique_standard_id) %>%
    dplyr::mutate(unique_adduct_id = c(1:nrow(.)) + max(0, max(storage_object$unique_adduct_id, na.rm = T))) %>%
    dplyr::select(.data$unique_adduct_id, .data$unique_standard_id, .data$mz_value, .data$adduct_identity) %>%
    dplyr::left_join(storage_object$adduct_search_df %>% dplyr::select(.data$adduct, .data$mode), dplyr::join_by(x$adduct_identity == y$adduct))

  storage_object$adduct_df <- dplyr::bind_rows(storage_object$adduct_df, temp_adduct_additions)

  #store eics in data_df
  temp_data_additions <- results_list %>% dplyr::left_join(temp_adduct_additions %>% dplyr::select(.data$unique_adduct_id, .data$adduct_identity, .data$mz_value, .data$unique_standard_id), dplyr::join_by(x$adduct_identity == y$adduct_identity, x$mz_value == y$mz_value, x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::select(.data$retention_time, .data$intensity, .data$unique_adduct_id)

  storage_object$data_df <- dplyr::bind_rows(storage_object$data_df, temp_data_additions)

  temp_library_data_additions <- dplyr::left_join(storage_object$data_df, storage_object$adduct_df, dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id))

  storage_object$library_data$max_rt_pos <- temp_library_data_additions %>% dplyr::filter(.data$mode == "POS") %>% dplyr::pull(.data$retention_time) %>% max(na.rm = T)
  storage_object$library_data$min_rt_pos <- temp_library_data_additions %>% dplyr::filter(.data$mode == "POS") %>% dplyr::pull(.data$retention_time) %>% min(na.rm = T)
  storage_object$library_data$max_rt_neg <- temp_library_data_additions %>% dplyr::filter(.data$mode == "NEG") %>% dplyr::pull(.data$retention_time) %>% max(na.rm = T)
  storage_object$library_data$min_rt_neg <- temp_library_data_additions %>% dplyr::filter(.data$mode == "NEG") %>% dplyr::pull(.data$retention_time) %>% min(na.rm = T)

  invisible(storage_object)
}


#' @title find_eic_peaks
#'
#' @description Feature finding for eics
#'
#' @details
#' This function is primarily a wrapper around chromatographR::find_peaks() with the
#'  addition of an additional density filtering step that removes outlier points.
#'  It loops through all the eics in the active library and puts the resulting peaks in
#' the peaks_df slot of the metScriber storage_object.
#'
#' @param storage_object a metScribeR storage_object
#'
#' @return an updated metScribeR storage_object
#'
#' @export
find_eic_peaks <- function (storage_object) {

  data_for_peak_finding <- storage_object$data_df

  #wrap around find_peaks
  unique_adduct_ids <- unique(data_for_peak_finding$unique_adduct_id)
  peaks_results <- list()

  for (i in seq_along(unique_adduct_ids)) {

    eic_data_i <- data_for_peak_finding %>% dplyr::filter(.data$unique_adduct_id == unique_adduct_ids[i]) %>% dplyr::arrange(.data$retention_time)


    if(nrow(eic_data_i) > 2) {

      peaks_results[[i]] <- chromatographR::find_peaks(y = eic_data_i$intensity,
                                                       smooth_type = storage_object$library_data$smooth_type,
                                                       smooth_window = storage_object$library_data$smooth_window,
                                                       slope_thresh = storage_object$library_data$slope_thresh,
                                                       amp_thresh= storage_object$library_data$amp_thresh,
                                                       bounds = T) %>%
        dplyr::mutate(unique_adduct_id = unique_adduct_ids[i])

      if (nrow(peaks_results[[i]]) > 0) {
        peaks_results[[i]] <- peaks_results[[i]] %>% dplyr::mutate(retention_time_min = eic_data_i$retention_time[.data$lower],
                                                            retention_time_max = eic_data_i$retention_time[.data$upper])
      }
    }}

  peak_df_new <- dplyr::bind_rows(peaks_results) %>% dplyr::mutate(unique_peak_id = c(1:nrow(.)) + max(0, max(storage_object$peak_df$unique_peak_id), na.rm = T))

  #add the peaks to the data_df
  for (i in seq_len(nrow(peak_df_new))) {
    storage_object$data_df$unique_peak_id[storage_object$data_df$unique_adduct_id == peak_df_new[[i, 'unique_adduct_id']] &
                                            storage_object$data_df$retention_time <= peak_df_new[[i, 'retention_time_max']] &
                                            storage_object$data_df$retention_time >= peak_df_new[[i, 'retention_time_min']]] <- peak_df_new[[i, 'unique_peak_id']]
  }

  #trim with density trimming
  storage_object$data_df <- storage_object$data_df %>%
    dplyr::group_by(.data$unique_peak_id) %>%
    dplyr::mutate(close_count = close_counter(points_numeric_vector = .data$retention_time, dist_threshold = storage_object$library_data$density_window_rt_distance)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unique_peak_id = ifelse((!is.na(.data$unique_peak_id)) & (.data$close_count <= storage_object$library_data$density_count_in_window_threshold), NA, .data$unique_peak_id))

  #update peak_df
  storage_object$peak_df <- dplyr::bind_rows(storage_object$peak_df, peak_df_new %>% dplyr::select(.data$unique_peak_id, .data$unique_adduct_id))

  invisible(storage_object)
}


#' @title calc_peak_metrics
#'
#' @description Calculates quality metrics and other feature-level information.
#'
#' @details
#' Calculates the following: feature retention time at apex, feature intensity at apex, feature standard deviation,
#' feature skew, feature upper and lower rt boundaries, feature width at half max, total number of eic points in a feature,
#' the number of features found in a feature's adduct, and quality metrics beta_shape_parameter, novel_snr, and correlation with beta curve
#' from Kumler, W., Hazelton, B.J. & Ingalls,  A.E. Picky with peakpicking: assessing chromatographic peak
#'  quality with simple metrics in metabolomics. BMC Bioinformatics 24, 404 (2023). https://doi.org/10.1186/s12859-023-05533-4.
#'
#' @param storage_object a metScribeR storage_object
#'
#' @return an updated metScribeR storage_object
#'
#' @export
calc_peak_metrics <- function(storage_object) {
  data_for_metrics <- storage_object$data_df
  peaks_for_metrics <- storage_object$peak_df

  temp_pearson_correlation_with_beta_curve <- rep(NA, nrow(peaks_for_metrics))
  temp_novel_snr <- rep(NA, nrow(peaks_for_metrics))
  temp_beta_shape_parameter <- rep(NA, nrow(peaks_for_metrics))
  temp_peak_lower_boundary_rt <- rep(NA, nrow(peaks_for_metrics))
  temp_peak_upper_boundary_rt <- rep(NA, nrow(peaks_for_metrics))
  temp_apex_rt <- rep(NA, nrow(peaks_for_metrics))
  temp_apex_intensity <- rep(NA, nrow(peaks_for_metrics))
  temp_peak_points_count <- rep(NA, nrow(peaks_for_metrics))
  temp_peak_width_half_max <- rep(NA, nrow(peaks_for_metrics))
  temp_peak_skew <- rep(NA, nrow(peaks_for_metrics))
  temp_peak_standard_deviation <- rep(NA, nrow(peaks_for_metrics))

  if (nrow(peaks_for_metrics) > 0) {
    for (i in seq_len(nrow(peaks_for_metrics))){

      data_i <- data_for_metrics %>% dplyr::filter(.data$unique_peak_id == peaks_for_metrics[[i, 'unique_peak_id']])

      if (nrow(data_i) > 0) {
        temp_result <- qscoreCalculator(data_i$retention_time, data_i$intensity)

        temp_pearson_correlation_with_beta_curve[i] <- temp_result$peak_cor
        temp_novel_snr[i] <- temp_result$SNR
        temp_beta_shape_parameter[i] <- ifelse(length(temp_result$peak_shape_param) == 1, temp_result$peak_shape_param, NA)

        temp_peak_lower_boundary_rt[i] <- data_i %>% dplyr::pull(.data$retention_time) %>% min()

        temp_peak_upper_boundary_rt[i] <- data_i %>% dplyr::pull(.data$retention_time) %>% max()

        temp_apex_intensity[i] <- data_i %>% dplyr::pull(.data$intensity) %>% max()

        temp_apex_rt[i] <- mean(data_i$retention_time[data_i$intensity == temp_apex_intensity[i]])

        temp_peak_points_count[i] <- nrow(data_i)

        temp_skew_and_sd_result <- calculate_skew_and_sd(data_i$retention_time, data_i$intensity)

        temp_peak_skew[i] <- temp_skew_and_sd_result$skew_result

        temp_peak_standard_deviation[i] <- temp_skew_and_sd_result$sd_result

        temp_peak_width_half_max[i] <- gsignal::fwhm(data_i$retention_time, data_i$intensity)

      } else {
        temp_peak_points_count[i] <- 0
      }

    }
    peaks_for_metrics <- peaks_for_metrics %>% dplyr::mutate(
      pearson_correlation_with_beta_curve = temp_pearson_correlation_with_beta_curve,
      novel_snr = temp_novel_snr,
      beta_shape_parameter = temp_beta_shape_parameter,
      lower_rt_boundary = temp_peak_lower_boundary_rt,
      upper_rt_boundary = temp_peak_upper_boundary_rt,
      apex_rt = temp_apex_rt,
      apex_intensity = temp_apex_intensity,
      points_count = temp_peak_points_count,
      peak_skew = temp_peak_skew,
      peak_standard_deviation = temp_peak_standard_deviation,
      peak_width_half_max = temp_peak_width_half_max
    ) %>% dplyr::group_by(.data$unique_adduct_id) %>%
      dplyr::mutate(number_boundaries_in_adduct = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(peak_point_density = (.data$upper_rt_boundary - .data$lower_rt_boundary) / .data$points_count )
  }

  storage_object$peak_df <- storage_object$peak_df %>%
    dplyr::select(.data$unique_peak_id, .data$unique_adduct_id) %>%
    dplyr::left_join(peaks_for_metrics, dplyr::join_by(x$unique_peak_id == y$unique_peak_id, x$unique_adduct_id == y$unique_adduct_id))

  invisible(storage_object)
}

#' @title eic_plot
#'
#' @description plots an eic figure for the given unique_standard_id
#'
#' @details For a given standard, plots a line graph of all adducts, colored by identity.
#'
#' @param storage_object a metScribeR storage_object
#' @param unique_standard_id_plotting the unique_standard_id for the standard being plotted. This will be an integer.
#'
#' @returns a ggplot object
#'
#' @export
eic_plot <- function(storage_object, unique_standard_id_plotting) {
  storage_object$standard_df %>%
    dplyr::filter(.data$unique_standard_id == unique_standard_id_plotting) %>%
    dplyr::left_join(storage_object$adduct_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::left_join(storage_object$data_df, dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%
    dplyr::left_join(storage_object$peak_df, dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id, x$unique_peak_id == y$unique_peak_id)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$retention_time, y = .data$intensity, color = .data$adduct_identity)) + ggplot2::geom_line()
}

#' @title peak_plot
#'
#' @description plots an eic figure for the given unique_peak_id. This is just one feature, in contrast to
#' one standard as with eic_plot
#'
#' @details For a given feature, plots a line graph of eic points.
#'
#' @param storage_object a metScribeR storage_object
#' @param unique_peak_id_plotting the unique_peak_id for the feature being plotted. This will be an integer.
#'
#' @returns a ggplot object
#'
#' @export
peak_plot <- function(storage_object, unique_peak_id_plotting) {
  peak_metrics <- storage_object$peak_df %>% dplyr::filter(.data$unique_peak_id == unique_peak_id_plotting) %>%
    dplyr::left_join(storage_object$adduct_df %>% dplyr::select(.data$unique_adduct_id, .data$adduct_identity, .data$unique_standard_id), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id))

  adduct_ids_plotting <- storage_object$adduct_df %>% dplyr::filter(.data$unique_standard_id == peak_metrics$unique_standard_id) %>% dplyr::pull(.data$unique_adduct_id)

  plotting_data <- storage_object$data_df %>% dplyr::filter(.data$unique_adduct_id %in% adduct_ids_plotting)

  plotting_data %>% dplyr::mutate(unique_adduct_id = as.factor(.data$unique_adduct_id)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$retention_time, y = .data$intensity, color = .data$unique_adduct_id)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = peak_metrics$apex_rt, color = 'black') +
    ggplot2::geom_vline(xintercept = peak_metrics$lower_rt_boundary, color = 'blue') +
    ggplot2::geom_vline(xintercept = peak_metrics$upper_rt_boundary, color = 'red')
}

#' @title peak_demo_plot
#'
#' @description plots an eic figure for the given unique_adduct_id. This plot is specific to the find peaks demo, and is not for the review peaks section.
#'
#' @param storage_object a metScribeR storage_object
#' @param unique_adduct_id_plotting the unique_adduct_id for the feature being plotted. This will be an integer.
#'
#' @returns a ggplot object
#'
#' @export
peak_demo_plot <- function(storage_object, unique_adduct_id_plotting) {
    custom_plot <- storage_object$data_df %>% dplyr::filter(.data$unique_adduct_id == unique_adduct_id_plotting) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$retention_time, y = .data$intensity)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::ggtitle(
        stringr::str_c(storage_object$adduct_df %>%
          dplyr::filter(.data$unique_adduct_id == unique_adduct_id_plotting) %>%
          dplyr::left_join(storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
          dplyr::select(.data$common_name, .data$adduct_identity) %>%
            paste(collapse = ' ')
        )
      ) + ggplot2::labs(x = 'RT (min)', y = 'EIC Intensity')

    mode_plotting <- storage_object$adduct_df %>%
      dplyr::filter(.data$unique_adduct_id == unique_adduct_id_plotting) %>%
      dplyr::pull(.data$mode)

    if (length(c(storage_object$library_data$max_rt_pos, storage_object$library_data$min_rt_pos)) > 0 |
        length(c(storage_object$library_data$max_rt_neg, storage_object$library_data$min_rt_neg)) > 0) {


    if (mode_plotting == 'POS') {
        custom_plot <- custom_plot + ggplot2::coord_cartesian(xlim = c(storage_object$library_data$min_rt_pos, storage_object$library_data$max_rt_pos))
    } else if (mode_plotting == 'NEG') {
        custom_plot <- custom_plot + ggplot2::coord_cartesian(xlim = c(storage_object$library_data$min_rt_neg, storage_object$library_data$max_rt_neg))
    }
    }


    peaks_list <- storage_object$peak_df %>% dplyr::filter(.data$unique_adduct_id == unique_adduct_id_plotting)

    if(nrow(peaks_list) > 0) {
    for (i in seq_len(nrow(peaks_list))){
      if (!is.na(peaks_list$lower_rt_boundary[i]) | !is.na(peaks_list$upper_rt_boundary[i])) {
      custom_plot <- custom_plot + ggplot2::geom_vline(xintercept = peaks_list$apex_rt[i], color = 'black') +
        ggplot2::geom_vline(xintercept = peaks_list$lower_rt_boundary[i], color = 'blue') +
        ggplot2::geom_vline(xintercept = peaks_list$upper_rt_boundary[i], color = 'red')
      }
    }
    }
    return(custom_plot)
}

#' @title peak_review_plot
#'
#' @description plots an eic figure for the given unique_peak_id. This plot is specific to the review peaks tab.
#'
#' @param storage_object a metScribeR storage_object
#' @param unique_peak_id_plotting the unique_peak_id for the feature being plotted. This will be an integer.
#'
#' @returns a ggplot object
#'
#' @export
peak_review_plot <- function(storage_object, unique_peak_id_plotting) {
  peak_metrics <- storage_object$peak_df %>% dplyr::filter(.data$unique_peak_id == unique_peak_id_plotting)

  custom_plot <- storage_object$data_df %>% dplyr::filter(.data$unique_adduct_id == peak_metrics$unique_adduct_id) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$retention_time, y = .data$intensity)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = peak_metrics$apex_rt, color = 'black', linetype = 'dashed') +
    ggplot2::geom_vline(xintercept = peak_metrics$lower_rt_boundary, color = 'blue') +
    ggplot2::geom_vline(xintercept = peak_metrics$upper_rt_boundary, color = 'red') +
    ggplot2::ggtitle(storage_object$adduct_df %>%
                       dplyr::filter(.data$unique_adduct_id == peak_metrics$unique_adduct_id) %>%
                       dplyr::left_join(storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
                       dplyr::mutate(rt_for_id = stringr::str_c('RT', round(peak_metrics$apex_rt, 2))) %>%
                       dplyr::select(.data$common_name, .data$adduct_identity, .data$rt_for_id) %>%
                       paste(collapse = ' ')
                     ) + ggplot2::labs(x = 'RT (min)', y = 'EIC Intensity')

  peaks_list <- storage_object$peak_df %>%
    dplyr::filter(.data$unique_adduct_id == peak_metrics$unique_adduct_id,
                  .data$unique_peak_id != unique_peak_id_plotting)

  if (!is.na(peak_metrics$manual_annotation)) {
    custom_plot <- custom_plot + ggplot2::labs(subtitle = stringr::str_c('Manual label is ', peak_metrics$manual_annotation))
  }
  if(nrow(peaks_list) > 0) {
    for (i in seq_len(nrow(peaks_list))){

      if (!is.na(peaks_list$lower_rt_boundary[i]) | !is.na(peaks_list$upper_rt_boundary[i])) {
        custom_plot <- custom_plot + ggplot2::geom_vline(xintercept = peaks_list$lower_rt_boundary[i], color = 'black') +
          ggplot2::geom_vline(xintercept = peaks_list$upper_rt_boundary[i], color = 'black')
      }
    }

  }

  mode_plotting <- storage_object$adduct_df %>%
    dplyr::filter(.data$unique_adduct_id == peak_metrics$unique_adduct_id) %>%
    dplyr::pull(.data$mode)

  if (length(c(storage_object$library_data$max_rt_pos, storage_object$library_data$min_rt_pos)) > 0 |
      length(c(storage_object$library_data$max_rt_neg, storage_object$library_data$min_rt_neg)) > 0) {


    if (mode_plotting == 'POS') {
      custom_plot <- custom_plot + ggplot2::coord_cartesian(xlim = c(storage_object$library_data$min_rt_pos, storage_object$library_data$max_rt_pos))
    } else if (mode_plotting == 'NEG') {
      custom_plot <- custom_plot + ggplot2::coord_cartesian(xlim = c(storage_object$library_data$min_rt_neg, storage_object$library_data$max_rt_neg))
    }
  }

  return(custom_plot)
}

#' @title manual_peak_annotation
#'
#' @description Add manual quality assessment result to a peak
#'
#' @param storage_object a metScribeR storage_object
#' @param unique_peak_id_annotate the unique_peak_id of the feature to recieve annotation
#' @param annotation logical description of peak quality. T if good, F if bad
#'
#' @returns a metScribeR storage_object
#'
#' @export
manual_peak_annotation <- function(storage_object, unique_peak_id_annotate, annotation) {
  if (annotation %in% c('Good', 'Bad', NA, 'Indeterminate')) {
    storage_object$peak_df[storage_object$peak_df$unique_peak_id == unique_peak_id_annotate, 'manual_annotation'] <- annotation
  }

  invisible(storage_object)
}

initial_filtering <- function(storage_object, number_of_boundaries_threshold, number_of_points_threshold, peak_width_half_max_threshold_min, peak_width_half_max_threshold_max, peak_intensity_threshold) {
  storage_object$peak_df <- storage_object$peak_df %>%
    dplyr::mutate(passed_initial_filtering = ifelse((.data$number_boundaries_in_adduct <= number_of_boundaries_threshold) &
                                               (.data$points_count >= number_of_points_threshold) &
                                                 (.data$peak_width_half_max > peak_width_half_max_threshold_min) &
                                                 (.data$peak_width_half_max < peak_width_half_max_threshold_max) &
                                                 (.data$apex_intensity > peak_intensity_threshold), T, F)) %>%
    dplyr::mutate(passed_initial_filtering = ifelse(.data$points_count == 0, F, passed_initial_filtering),
                  passed_initial_filtering = ifelse(is.na(.data$points_count), F, passed_initial_filtering))

  invisible(storage_object)
}

#' @title noise_plot
#'
#' @description plots an spectra figure for the given mzML file, indicated with a unique_standard_id and a mode.
#'
#' @details This plot is useful for choosing the background noise level while importing data.
#'
#' @param plotting_mzML_file_path path to file for plotting
#' @param noise_level_plotting optional argument to add a horizontal line at the indicated intensity value
#'
#' @returns a ggplot object
#'
#' @export
noise_plot <- function(plotting_mzML_file_path, noise_level_plotting = NA) {
  custom_plot <- parse_mzML(plotting_mzML_file_path, 0) %>%
    dplyr::slice_sample(n = min(nrow(.), 5E3)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$mz, y = 0, yend = .data$intensity)) +
    ggplot2::geom_segment() +
    ggplot2::labs(y = 'intensity', x = 'm/z', title = stringr::str_extract(plotting_mzML_file_path, '([^\\/]+$)'))

  if(!is.na(noise_level_plotting)) {custom_plot <- custom_plot + ggplot2::geom_hline(yintercept = noise_level_plotting, color = 'red')}

  custom_plot
}

#' @title update_best_peak_and_adduct
#'
#' @description Adds the unique peak id of the best peak to adduct_df. Adds the is_best_adduct label to adduct_df.
#'
#' @details
#' The best peak for an adduct is the one with the maximum intensity at its apex. This is because we are
#' working with authentic standard data, where we assume the standard will be more intense than any noise.
#'
#' The best adduct is one which has a peak that has passed manual quality classification
#' and has the greatest apex intensity. The representative peak for an adduct is determined by the rules in the
#' prior paragraph. This only updates after all adducts have been evaluated for a standard.
#'
#' @param storage_object a metScribeR storage_object
#' @param apex_intensity_ratio_tolerance the intensity tolerance for a peak to be considered multi-modal within an EIC
#'
#' @returns an updated metScribeR storage_object
#'
#' @export
update_best_peak_and_adduct <- function(storage_object, apex_intensity_ratio_tolerance = 0.5) {


  top_adduct_ids <- storage_object$peak_df %>%
    dplyr::group_by(.data$unique_adduct_id) %>%
    dplyr::slice_max(tibble::tibble(apex_intensity, points_count), n=1, with_ties = F) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$unique_adduct_id, best_peak_id = .data$unique_peak_id)

  storage_object$adduct_df <- storage_object$adduct_df %>%
    dplyr::select(-.data$best_peak_id) %>%
    dplyr::left_join(top_adduct_ids, dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id))
  
  multimodal_peaks <- storage_object$peak_df %>%
          dplyr::left_join(storage_object$adduct_df, dplyr::join_by(unique_adduct_id == unique_adduct_id)) %>%
          dplyr::filter(!is.na(best_peak_id), !is.na(apex_rt)) %>% dplyr::select(apex_intensity, unique_adduct_id, best_peak_id, unique_peak_id) %>%
          dplyr::filter(unique_peak_id != best_peak_id) %>%
          dplyr::left_join(storage_object$peak_df %>% dplyr::select(best_peak_id = unique_peak_id, best_peak_intensity = apex_intensity), dplyr::join_by(best_peak_id == best_peak_id)) %>%
          dplyr::filter(apex_intensity / best_peak_intensity > apex_intensity_ratio_tolerance)
        

  
  storage_object$peak_df <- storage_object$peak_df %>%
    dplyr::mutate(manual_annotation = ifelse(.data$unique_peak_id %in% (multimodal_peaks %>% dplyr::pull(best_peak_id) %>% unique()), 'Bad', manual_annotation)) %>%
    dplyr::mutate(manual_annotation = ifelse(.data$unique_peak_id %in% (multimodal_peaks %>% dplyr::pull(unique_peak_id) %>% unique()), 'Bad', manual_annotation))

  storage_object$adduct_df <- storage_object$adduct_df %>%
    
    dplyr::left_join(storage_object$peak_df, dplyr::join_by(x$best_peak_id == y$unique_peak_id, x$unique_adduct_id == y$unique_adduct_id)) %>%

    dplyr::group_by(.data$unique_standard_id) %>%

    dplyr::filter(all(!is.na(.data$manual_annotation) | !.data$passed_initial_filtering | is.na(.data$points_count)), .data$manual_annotation == 'Good') %>%

    dplyr::slice_max(.data$apex_intensity) %>%

    dplyr::mutate(is_best_adduct = T) %>%

    dplyr::ungroup() %>%

    dplyr::select(.data$unique_adduct_id, .data$is_best_adduct, rt_value = .data$apex_rt) %>%

    dplyr::right_join(storage_object$adduct_df %>% dplyr::select(-.data$is_best_adduct, -.data$rt_value), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%

    dplyr::mutate(is_best_adduct = ifelse(is.na(.data$is_best_adduct), F, .data$is_best_adduct))

  invisible(storage_object)
}

#' @title add_adduct_peak_presence
#'
#' @description Add is_good_peak label to peak_df. Add has_good_peak and rt_value to adduct_df.
#'
#' @details
#' The is_good_peak label requires a feature to both pass manual filtering and be within
#' the rt_tolerance of the primary feature for an adduct selected by update_best_peak_and_adduct. The reason this must
#' happen after already choosing a best feature is so that the rt of the standard is established.
#'
#' The has_good_peak label requires an adduct to have a feature that received the is_good_peak label. An RT_value is
#' then associated with that adduct. The same rt will be applied to all adducts of a standard. A multimodality filter is also included,
#' where if there are two peaks in a standard at different RTs with comparable intensity the standard is filtered out.
#'
#' @param storage_object a metScribeR storage_object
#'
#' @param rt_tolerance the rt_tolerance for a feature to be considered a good peak.
#'
#' @param apex_intensity_ratio_tolerance the intensity tolerance for a peak outside the RT tolerance to be considered multi-modal
#'
#' @returns an updated metScribeR storage_object
#'
#' @export
add_adduct_peak_presence <- function(storage_object, rt_tolerance, apex_intensity_ratio_tolerance = 0.5) {
  storage_object$peak_df <- storage_object$peak_df %>%
    dplyr::left_join(storage_object$adduct_df %>%
                       dplyr::select(.data$unique_adduct_id, .data$unique_standard_id), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%
    dplyr::left_join(storage_object$adduct_df %>%
                       dplyr::filter(!is.na(.data$rt_value)) %>%
                       dplyr::select(.data$rt_value, .data$unique_standard_id), dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::mutate(is_good_peak = ifelse(.data$manual_annotation == 'Good' &
                                   abs(.data$apex_rt - .data$rt_value) < rt_tolerance, T, F)) %>%
    dplyr::select(-.data$rt_value, -.data$unique_standard_id)

  #multimodality filter, remove standards that have a secondary adduct greater than 50% of primary adduct at a different RT
  temp <- storage_object$peak_df %>%
    dplyr::left_join(storage_object$adduct_df %>% dplyr::select(.data$unique_adduct_id, .data$best_peak_id, .data$rt_value), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%
    dplyr::filter(.data$unique_peak_id == .data$best_peak_id) %>%
    dplyr::select(top_adduct_intensity = .data$apex_intensity, .data$unique_adduct_id, top_adduct_rt_value = .data$rt_value)

  multimodal_adducts_for_removal <- storage_object$peak_df %>%
    dplyr::left_join(temp, dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%
    dplyr::filter(abs(.data$apex_rt - .data$top_adduct_rt_value) > rt_tolerance, .data$apex_intensity / .data$top_adduct_intensity > apex_intensity_ratio_tolerance) %>%
    dplyr::select(.data$unique_adduct_id)

 storage_object$adduct_df <- storage_object$adduct_df %>%
    dplyr::left_join(storage_object$peak_df %>% dplyr::select(.data$unique_adduct_id, .data$is_good_peak), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%

    dplyr::group_by(.data$unique_adduct_id) %>%
    dplyr::mutate(has_good_peak = any(.data$is_good_peak, na.rm = T)) %>%

    dplyr::ungroup() %>%
    dplyr::distinct(.data$unique_adduct_id, .data$has_good_peak) %>% dplyr::filter(!is.na(.data$has_good_peak)) %>%

    dplyr::right_join(storage_object$adduct_df %>% dplyr::select(-.data$has_good_peak), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%
    dplyr::mutate(rt_value = ifelse(!.data$has_good_peak, NA, .data$rt_value)) %>%
    dplyr::group_by(.data$unique_standard_id) %>%
    dplyr::mutate(rt_value = mean(.data$rt_value, na.rm=T)) %>%
    dplyr::ungroup()



  storage_object$adduct_df <- storage_object$adduct_df %>%
   dplyr::mutate(rt_value = ifelse(.data$unique_adduct_id %in% multimodal_adducts_for_removal$unique_adduct_id, NA, .data$rt_value)) %>%
   dplyr::mutate(has_good_peak = ifelse(.data$unique_adduct_id %in% multimodal_adducts_for_removal$unique_adduct_id, F, .data$has_good_peak)) %>%
   dplyr::mutate(is_best_adduct = ifelse(.data$unique_adduct_id %in% multimodal_adducts_for_removal$unique_adduct_id, F, .data$is_best_adduct))

  invisible(storage_object)
}

#' @title create_descriptive_boxplots
#'
#' @description
#' Creates descriptive box plots for display in Descriptive Box plots tab
#'
#' @param storage_object a metScribeR storage_object
#'
#' @returns a list of ggplot objects, named by the feature they display
#'
#' @keywords internal
#' @noRd
create_descriptive_boxplots <- function(storage_object) {

  data_for_boxplotting <- storage_object$peak_df %>%
    dplyr::filter(!is.na(.data$apex_rt)) %>%
    dplyr::mutate( log10_apex_intensity = log10(.data$apex_intensity))

  for (i in c('log10_apex_intensity', 'points_count', 'number_boundaries_in_adduct',
              'novel_snr', 'pearson_correlation_with_beta_curve', 'peak_skew',
              'peak_point_density', 'peak_width_half_max' )) {

    custom_plot <- data_for_boxplotting %>%
      ggplot2::ggplot(ggplot2::aes(y = .data[[i]]))  +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot(fill = 'darkgrey') +
      ggplot2::labs(x = 'All Peaks',
           y = i,
           title = i) +
      ggplot2::theme(legend.position = 'none', axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

    storage_object$library_data$descriptive_boxplots[[stringr::str_c(i, '_boxplot')]] <- custom_plot
  }

  invisible(storage_object)

}

#' @title find_adduct_conflicts
#'
#' @description Find conflicting adducts in the library
#'
#' @details
#' This function finds features that overlap with a adduct peak in the library within an RT and m/z tolerance. It reports
#' these conflicts in the adduct_df with the name, adduct, and rt of the conflicting feature. Note that this looks for
#' conflicting features, not necesarilly conflicting confirmed identified adduct species.
#'
#' @param storage_object a metScribeR storage_object
#' @param mz_tolerance the tolerance for a conflict, in ppm. For example mz_tolerance = 30 would be 30 ppm.
#' @param rt_tolerance the tolerance for a conflict, in min.
#'
#' @returns an updated metScribeR storage_object
#'
#' @export
find_adduct_conflicts <- function (storage_object, mz_tolerance = storage_object$library_data$eic_mz_tolerance, rt_tolerance) {

  for (i in seq_len(nrow(storage_object$adduct_df))) {
    mz_i <- storage_object$adduct_df[[i, 'mz_value']]
    rt_i <- storage_object$adduct_df[[i, 'rt_value']]

    conflict_results <- storage_object$adduct_df %>%

      dplyr::filter(.data$unique_adduct_id != storage_object$adduct_df[[i, 'unique_adduct_id']]) %>%

      dplyr::filter(
        .data$mz_value < mz_i * (1+ (mz_tolerance / 1E6)),
        .data$mz_value > mz_i * (1-(mz_tolerance / 1E6)),
        rt_tolerance > abs(rt_i - .data$rt_value),
        .data$mode == storage_object$adduct_df[[i, 'mode']]
      )  %>%
      dplyr::left_join(storage_object$standard_df %>% dplyr::select(.data$unique_standard_id, .data$common_name), dplyr::join_by(x$unique_standard_id == y$unique_standard_id))

    conflict_names <- conflict_results %>%
      dplyr::mutate(temp = stringr::str_c(.data$common_name, '_', .data$adduct_identity, '_RT', round(.data$rt_value, 2))) %>%
      dplyr::pull(.data$temp) %>% paste(collapse = '; ')

    conflict_count <- nrow(conflict_results)


    storage_object$adduct_df[[i, 'conflict_adduct_ids']] <- conflict_names

    storage_object$adduct_df[[i, 'internal_identification_probability']] <- 1 / (conflict_count + 1)

  }


  invisible(storage_object)
}

#' @title export_library_csv
#'
#' @description Exports a library csv file
#'
#' @param storage_object a metScribeR storage_object
#' @param save_file_path the location the csv file will be saved to
#'
#' @export
export_library_csv <- function(storage_object, save_file_path) {

  storage_object$standard_df %>%
    dplyr::left_join(storage_object$adduct_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::left_join(storage_object$peak_df, dplyr::join_by(x$best_peak_id == y$unique_peak_id)) %>%
    dplyr::left_join(storage_object$msms_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::select(
                  .data$common_name,
                  .data$mz_value,
                  .data$rt_value,
                  .data$adduct_identity,
                  .data$conflict_adduct_ids,
                  .data$has_good_peak
                  ) %>%
    readr::write_csv(save_file_path)
}

#' @title export_library_metrics_csv
#'
#' @description Exports a library csv file with added metrics
#'
#' @param storage_object a metScribeR storage_object
#' @param save_file_path the location the csv file will be saved to
#'
#' @export
export_library_metrics_csv <- function(storage_object, save_file_path) {

  storage_object$standard_df %>%
    dplyr::left_join(storage_object$adduct_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%

    dplyr::left_join(storage_object$peak_df, dplyr::join_by(x$best_peak_id == y$unique_peak_id)) %>%
    dplyr::select(
      .data$common_name,
      .data$mz_value,
      .data$rt_value,
      .data$adduct_identity,
      .data$conflict_adduct_ids,
      .data$has_good_peak,
      .data$internal_identification_probability,
      .data$lower_rt_boundary,
      .data$upper_rt_boundary,
      .data$apex_rt,
      .data$apex_intensity,
      .data$points_count,
      .data$pearson_correlation_with_beta_curve,
      .data$novel_snr,
      .data$beta_shape_parameter,
      .data$peak_skew,
      .data$peak_point_density,
      .data$number_boundaries_in_adduct,
      .data$peak_width_half_max,
      .data$manual_annotation,
      .data$passed_initial_filtering
    ) %>%
    readr::write_csv(save_file_path)
}

#' @title export_library_dataframe
#'
#' @description Exports a library dataframe
#'
#' @param storage_object a metScribeR storage_object
#'
#' @returns a dataframe object for display in metScribeR
#'
#' @export
export_library_dataframe <- function(storage_object) {

  storage_object$standard_df %>%
    dplyr::left_join(storage_object$adduct_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::left_join(storage_object$msms_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::select(.data$common_name, .data$mz_value, .data$rt_value, .data$adduct_identity, .data$conflict_adduct_ids, .data$has_good_peak, .data$internal_identification_probability, .data$has_msms_data) %>%
    dplyr::mutate(rt_value = round(.data$rt_value, 3), mz_value = round(.data$mz_value, 5)) %>%
    dplyr::filter(.data$has_good_peak) %>%
    dplyr::select(-.data$has_good_peak) %>%
    dplyr::rename(`m/z theoretical (ppm)` = .data$mz_value,
                  `RT (min)` = .data$rt_value,
                  adduct = .data$adduct_identity,
                  `conflicting adducts` = .data$conflict_adduct_ids,
                  `standard name` = .data$common_name,
                  `internal identification prob` = .data$internal_identification_probability,
                  `MS2 data found` = .data$has_msms_data)
}

#' @title save_storage_object
#'
#' @description Exports a rds file of the storage_object.
#'
#' @param storage_object a metScribeR storage_object
#' @param save_file_path the location the rds file will be saved to
#'
save_storage_object <- function(storage_object, save_file_path) {
  saveRDS(storage_object, file = save_file_path)
}


#' @title add_msms_information
#'
#' @description
#' Collects MS/MS information from MoNA for export with library
#'
#' @param relative_intensity_threshold Threshold for including ms2 peaks based on relative intensity
#' @param storage_object a metScribeR storage_object
#'
#' @keywords internal
#' @noRd
add_msms_information <- function(storage_object, relative_intensity_threshold = 40) {
  storage_object$msms_df <- storage_object$standard_df %>% dplyr::select(.data$unique_standard_id)

  results_list <- list()

  for (i in seq_len(nrow(storage_object$msms_df))) {

    temp <- obtain_spectra_from_inchiKey(storage_object$standard_df$inchiKey[i]) %>%
                          get_spectra_from_result() %>%
                          expand_by_spectra()

    if (nrow(temp) > 0 & all(c('ms level', 'relative_intensity', 'instrument type', 'collision energy voltage', 'mz') %in% colnames(temp))) {

      results_list[[i]] <- temp %>%
                          dplyr::filter(.data$`ms level` == 'MS2', .data$relative_intensity > relative_intensity_threshold) %>%
                          dplyr::mutate(column_name = stringr::str_c(.data$`instrument type`, '_CE',.data$`collision energy voltage`, 'V')) %>%
                          dplyr::group_by(.data$inchiKey, .data$column_name) %>%
                          dplyr::mutate(display_for_column = paste(.data$mz, collapse = '; ')) %>%
                          dplyr::ungroup() %>%
                          dplyr::select(.data$column_name, .data$display_for_column) %>%
                          dplyr::distinct() %>%
                          tidyr::pivot_wider(names_from = .data$column_name, values_from = .data$display_for_column) %>%
                          dplyr::mutate(unique_standard_id = storage_object$standard_df$unique_standard_id[i], has_msms_data = T)
    }

  }

  storage_object$msms_df <- dplyr::left_join(storage_object$msms_df, dplyr::bind_rows(results_list), dplyr::join_by(x$unique_standard_id == y$unique_standard_id))

  if (!'has_msms_data' %in% colnames(storage_object$msms_df)) {
    storage_object$msms_df$has_msms_data <- F
  } else {
    storage_object$msms_df <- dplyr::mutate(storage_object$msms_df, has_msms_data = ifelse(is.na(.data$has_msms_data), F, .data$has_msms_data))
  }

  invisible(storage_object)

}

#' @title export_msms_csv
#'
#' @description Exports a msms csv file
#'
#' @param storage_object a metScribeR storage_object
#' @param save_file_path the location the csv file will be saved to
#'
#' @export
export_msms_csv <- function(storage_object, save_file_path) {

  storage_object$msms_df %>%
    dplyr::left_join(storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
    dplyr::select(
      -.data$pos_mode_mzML_file_path,
      -.data$neg_mode_mzML_file_path,
      -.data$unique_standard_id
    ) %>%
    readr::write_csv(save_file_path)
}
