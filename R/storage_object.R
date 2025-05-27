#' @title storage_object
#'
#' @description
#' initializes a metScribeR storage_object
#'
#' @details
#' Calling this function creates a blank storage_object, including library_data, standard_df,
#' adduct_df, peak_df, and data_df.
#'
#' @export

storage_object <- function() {list(

  #This stores all of the parameters associated with each unique_library_id.
  #If the parameters change, a new library id requires duplication of all the data too
  #in the following dataframes
  library_data = list(
    eic_mz_tolerance = numeric(),
    rt_mz_tolerance = numeric(),
    mzML_parsing_noise_level = numeric(),
    smooth_type = character(),
    smooth_window = numeric(),
    slope_thresh = numeric(),
    amp_thresh = numeric(),
    density_window_rt_distance = numeric(),
    density_count_in_window_threshold = numeric(),
    min_points = numeric(),
    max_boundaries = numeric(),
    max_rt_pos = numeric(),
    min_rt_pos = numeric(),
    max_rt_neg = numeric(),
    min_rt_neg = numeric(),
    descriptive_boxplots = list(log10_apex_intensity_boxplot = ggplot2::ggplot(),
                                points_count_boxplot = ggplot2::ggplot(),
                                pearson_correlation_with_beta_curve_boxplot = ggplot2::ggplot(),
                                novel_snr_boxplot = ggplot2::ggplot(),
                                peak_skew_boxplot = ggplot2::ggplot(),
                                peak_point_density_boxplot = ggplot2::ggplot(),
                                peak_width_half_max_boxplot = ggplot2::ggplot(),
                                number_boundaries_in_adduct_boxplot = ggplot2::ggplot()),
    output_directory_path = character()
  ),

  #This is a dataframe of the adducts looked for for each standard
  adduct_search_df = data.frame(
    adduct = character(),
    change_from_neutral = numeric(),
    mode = character()
  ),

  #This is a dataframe of the standards associated with each library
  standard_df = data.frame(
    unique_standard_id = numeric(),
    inchiKey = character(),
    common_name = character(),
    monoisotopic_mass = numeric(),
    pos_mode_mzML_file_path = character(),
    neg_mode_mzML_file_path = character()
  ),

  msms_df = data.frame(
    unique_standard_id = numeric(),
    has_msms_data = logical()
  ),

  #this is a dataframe of the adducts associated with each standard
  adduct_df = data.frame(
    unique_standard_id = numeric(),
    unique_adduct_id = numeric(),
    is_best_adduct = logical(),
    best_peak_id = numeric(),
    mz_value = numeric(),
    rt_value = numeric(),
    mode = character(),
    adduct_identity = character(),
    conflict_adduct_ids = character(),
    has_good_peak = logical(),
    internal_identification_probability = numeric()
  ),

  #this is a dataframe of the features associated with each adduct
  peak_df = data.frame(
    unique_adduct_id = numeric(),
    unique_peak_id = numeric(),

    lower_rt_boundary = numeric(),
    upper_rt_boundary = numeric(),
    apex_rt = numeric(),
    apex_intensity = numeric(),
    points_count = numeric(),
    pearson_correlation_with_beta_curve = numeric(),
    novel_snr = numeric(),
    beta_shape_parameter = numeric(),
    peak_skew = numeric(),
    peak_point_density = numeric(),
    number_boundaries_in_adduct = numeric(),
    peak_width_half_max = numeric(),

    manual_annotation = character(),
    passed_initial_filtering = logical()
  ),

  #this is a dataframe of the data associated with each feature and adduct
  data_df = data.frame(
    unique_adduct_id = numeric(),
    unique_peak_id = numeric(),
    retention_time = numeric(),
    intensity = numeric()

  )

)
}
