#' @title noise_plot_zoom_observer_function
#'
#' @description
#' Assists in the ability to zoom into the noise_plot function in the Import EIC Data tab.
#'
#' @param input r shiny input object
#' @param noise_plot_ranges the coordinates of the brush click
#'
#' @keywords internal
#' @noRd
noise_plot_zoom_observer_function <- function(input, noise_plot_ranges){
  shiny::observeEvent(input$noise_plot_dblclick, {
    brush <- input$noise_plot_brush
    if (!is.null(brush)) {
      noise_plot_ranges$x <- c(brush$xmin, brush$xmax)
      noise_plot_ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      noise_plot_ranges$x <- NULL
      noise_plot_ranges$y <- NULL
    }

  })
}

#' @title standard_df_path_observer_function
#'
#' @description
#' Reads in standard_df file when it is input in the Import EIC Data tab. This directly feeds into creating the noise plot.
#'
#' @param input r shiny input object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
standard_df_path_observer_function <- function(input, values) {
shiny::observeEvent(input$standard_df_path, {

  if (!is.integer(input$standard_df_path)) {

    if (stringr::str_detect(values$standard_df_path(), '.csv')) {
      values$standard_df <- readr::read_csv(values$standard_df_path(), show_col_types = F)
      check_standard_df(values$standard_df)
    } else if (stringr::str_detect(values$standard_df_path(), '.tsv')) {
      values$standard_df <- readr::read_tsv(values$standard_df_path(), show_col_types = F)
      check_standard_df(values$standard_df)
    }

    choices_for_selection <- c(values$standard_df$pos_mode_mzML_file_path, values$standard_df$neg_mode_mzML_file_path)
    names(choices_for_selection) <- stringr::str_extract(choices_for_selection, '(?<=\\/)[^\\/]+\\.mzML$')
    
    choices_for_selection <- choices_for_selection[!is.na(choices_for_selection)]

    shiny::updateSelectInput(

      inputId = 'mzML_parsing_noise_level_switch_file',
      choices = choices_for_selection
    )
  }
})
}

#' @title noise_plot_render_observer_function
#'
#' @description
#' Renders noise plot when the noise level changes.
#'
#' @param input r shiny input object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
noise_plot_render_observer_function <- function(input, values) {
  shiny::observeEvent(input$mzML_parsing_noise_level,
                      if (!is.integer(input$standard_df_path)) {
                        values$noise_plot <- noise_plot(c(values$standard_df$pos_mode_mzML_file_path, values$standard_df$neg_mode_mzML_file_path)[values$noise_plot_index], input$mzML_parsing_noise_level)
                      }
  )
}

#' @title switch_mzML_parsing_noise_level_file_observer_function
#'
#' @description
#' Facilitates switching files in the example noise level plot
#'
#' @param input r shiny input object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
switch_mzML_parsing_noise_level_file_observer_function <- function(input, values) {
  shiny::observeEvent(input$mzML_parsing_noise_level_switch_file, {
    if (!is.integer(input$standard_df_path)) {
      values$noise_plot_index <- which(c(values$standard_df$pos_mode_mzML_file_path, values$standard_df$neg_mode_mzML_file_path) == input$mzML_parsing_noise_level_switch_file)[1]
      if (length(values$noise_plot_index) == 0) {values$noise_plot_index <- 1}

      values$noise_plot <- noise_plot(c(values$standard_df$pos_mode_mzML_file_path, values$standard_df$neg_mode_mzML_file_path)[values$noise_plot_index], input$mzML_parsing_noise_level)
    }})
}

#' @title input_data_submitted_observer_function
#'
#' @description
#' This is a big function that performs all of the actions that take place when the import eic data tab submit button is clicked.
#' See the function comments for individual components of this action.
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
input_data_submitted_observer_function <- function(input, output, values){
  shiny::observeEvent(input$input_data_submitted, {

    #wrap all of this in an if statement to make sure all the important stuff is in place to not error out!
    if (all(length(values$output_directory_path()) > 0,
            length(values$standard_df_path()) > 0,
            length(values$adduct_df_path()) > 0)) {

      #Set up files
      directory_setup(values$output_directory_path())

      #Read compound spreadsheet
      if (stringr::str_detect(values$standard_df_path(), '.csv')) {
        values$standard_df <- readr::read_csv(values$standard_df_path(), show_col_types = F)
        check_standard_df(values$standard_df)
      } else if (stringr::str_detect(values$standard_df_path(), '.tsv')) {
        values$standard_df <- readr::read_tsv(values$standard_df_path(), show_col_types = F)
        check_standard_df(values$standard_df)
      }


      #Read adduct spreadsheet
      if (stringr::str_detect(values$adduct_df_path(), '.csv')) {
        values$adduct_df <- readr::read_csv(values$adduct_df_path(), show_col_types = F)
        check_adduct_df(values$adduct_df)
      } else if (stringr::str_detect(values$adduct_df_path(), '.tsv')) {
        values$adduct_df <- readr::read_tsv(values$adduct_df_path(), show_col_types = F)
        check_adduct_df(values$adduct_df)
      }

      #initialize metScribeR storage_object
      values$storage_object <- storage_object()

      #collect user input with package
      values$storage_object <- user_data_input(
        storage_object = values$storage_object,
        standard_df = values$standard_df,
        adduct_search_df = values$adduct_df,
        eic_mz_tolerance = input$eic_mz_tolerance,
        eic_rt_tolerance = input$eic_rt_tolerance,
        mzML_parsing_noise_level = input$mzML_parsing_noise_level,
        output_directory_path = values$output_directory_path()
      )

      #Create library spreadsheet
      values$library_df <- export_library_dataframe(values$storage_object)
      output$library_table <- DT::renderDT(values$library_df)

      #Import the eic data

      values$storage_object <- import_eic_data(values$storage_object)
      values$storage_object <- user_peak_params_input(
        storage_object = values$storage_object,
        smooth_type = 'mva',
        smooth_window = 3,
        slope_thresh = 0,
        amp_thresh = values$storage_object$library_data$mzML_parsing_noise_level,
        density_window_rt_distance = values$storage_object$library_data$eic_rt_tolerance,
        density_count_in_window_threshold = 7
      )

      #find peaks and calculate metrics for display in find peaks tab
      values$storage_object <- find_eic_peaks(values$storage_object)
      values$storage_object <- calc_peak_metrics(values$storage_object)

      #initialize display in find peaks tab
      values$current_peak_demo_plot <- peak_demo_plot(values$storage_object, min(values$storage_object$peak_df$unique_peak_id))
      output$demo_feature_plot <- shiny::renderPlot({
        values$current_peak_demo_plot
      }, res = 96)

      #initialize select input menu in find peaks tab
      shiny::updateSelectInput(
        inputId = 'demo_feature_plot_select',
        choices = custom_naming_function(values$storage_object$adduct_df %>%
                                           dplyr::pull(.data$unique_adduct_id), values$storage_object$adduct_df %>%
                                           dplyr::left_join(values$storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
                                           dplyr::mutate(temp = stringr::str_c(.data$common_name, ' ', .data$adduct_identity)) %>% dplyr::pull(.data$temp))  %>%
          sample_if_larger(50))


      save_storage_object(values$storage_object, stringr::str_c(values$storage_object$library_data$output_directory_path, '/storage_object_initial.RDS'))

    

    #set output path for figures in the review results tab
    values$figures_output_path <- stringr::str_c(values$storage_object$library_data$output_directory_path, '/Figures/')

    #create and display boxplots for descriptive peak metrics
    values$storage_object <- create_descriptive_boxplots(values$storage_object)

    output$number_boundaries_in_adduct_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$number_boundaries_in_adduct_boxplot}, res = 96)
    output$peak_width_half_max_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_width_half_max_boxplot}, res = 96)
    output$peak_point_density_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_point_density_boxplot}, res = 96)
    output$peak_skew_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_skew_boxplot}, res = 96)
    output$novel_snr_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$novel_snr_boxplot}, res = 96)
    output$pearson_correlation_with_beta_curve_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$pearson_correlation_with_beta_curve_boxplot}, res = 96)
    output$points_count_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$points_count_boxplot}, res = 96)
    output$log10_apex_intensity_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$log10_apex_intensity_boxplot}, res = 96)
    }
  })
}

#' @title plot_location_event_printing_observer_function
#'
#' @description
#' Prints the x coordinate of a click on a peak review plot in the r session. This is useful mostly for package testing purposes.
#'
#' @param input r shiny input object
#'
#' @keywords internal
#' @noRd
#'
plot_location_event_printing_observer_function <- function(input) {
  shiny::observeEvent(input$plot_click, {
    print(stringr::str_c('x=',input$plot_click$x, ' y=', input$plot_click$y))
  })
}

#' @title load_saved_data_observer_function
#'
#' @description
#' This facilitates loading an existing metScribeR storage object to resume progress. See comments in function for detail on individual components of this processs.
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
load_saved_data_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$input_data_loaded, {

    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

    #read RDS of storage object, and load the library table
    if (length(shinyFiles::parseFilePaths(volumes, input$input_data_loaded)$datapath) > 0) {
      values$storage_object <- readRDS(shinyFiles::parseFilePaths(volumes, input$input_data_loaded)$datapath)
      values$library_df <- export_library_dataframe(values$storage_object)
      output$library_table <- DT::renderDT(values$library_df)
      values$current_peak_demo_plot <- peak_demo_plot(values$storage_object, min(values$storage_object$peak_df$unique_peak_id))

      #initialize the peak finding demo plot
      output$demo_feature_plot <- shiny::renderPlot({
        values$current_peak_demo_plot
      }, res = 96)

      #initialize the peak finding demo select menu
      shiny::updateSelectInput(
        inputId = 'demo_feature_plot_select',
        choices = custom_naming_function(values$storage_object$adduct_df %>%
                                           dplyr::pull(.data$unique_adduct_id), values$storage_object$adduct_df %>%
                                           dplyr::left_join(values$storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
                                           dplyr::mutate(temp = stringr::str_c(.data$common_name, ' ', .data$adduct_identity)) %>% dplyr::pull(.data$temp))  %>%
          sample_if_larger(50))

      values$features_to_plot_list <- values$storage_object$peak_df %>%
        dplyr::filter(is.na(.data$passed_initial_filtering) | .data$passed_initial_filtering, .data$points_count > 0) %>%
        dplyr::pull(.data$unique_peak_id)
      
      values$figures_output_path <- stringr::str_c(values$storage_object$library_data$output_directory_path, '/Figures/')

      names(values$features_to_plot_list) <- values$storage_object$peak_df %>%
        dplyr::filter(is.na(.data$passed_initial_filtering) | .data$passed_initial_filtering, .data$points_count > 0) %>%
        dplyr::left_join(values$storage_object$adduct_df %>% dplyr::select(.data$unique_adduct_id, .data$adduct_identity, .data$unique_standard_id), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%
        dplyr::left_join(values$storage_object$standard_df %>% dplyr::select(.data$unique_standard_id, .data$common_name), dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
        dplyr::mutate(temp = stringr::str_c('peak_', .data$unique_peak_id, ' ', .data$adduct_identity, ' ', .data$common_name)) %>%
        dplyr::pull(.data$temp)

      #initialize the peak review plot
      values$index_for_plot <- min(values$storage_object$peak_df$unique_peak_id, na.rm = T)

      values$plot <- peak_review_plot(values$storage_object, values$features_to_plot_list[values$index_for_plot])

      output$plot <- shiny::renderPlot({values$plot}, res = 96)

      #initialize the peak review plot select menu
      shiny::updateSelectInput(
        inputId = 'review_plot_switch_file',
        choices = values$features_to_plot_list,
        selected = values$features_to_plot_list[values$index_for_plot])

      peak_data <- values$storage_object$peak_df %>%
        dplyr::filter(.data$unique_peak_id == values$features_to_plot_list[values$index_for_plot])

      output$number_boundaries_in_adduct_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$number_boundaries_in_adduct_boxplot}, res = 96)
      output$peak_width_half_max_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_width_half_max_boxplot}, res = 96)
      output$peak_point_density_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_point_density_boxplot}, res = 96)
      output$peak_skew_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_skew_boxplot}, res = 96)
      output$novel_snr_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$novel_snr_boxplot}, res = 96)
      output$pearson_correlation_with_beta_curve_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$pearson_correlation_with_beta_curve_boxplot}, res = 96)
      output$points_count_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$points_count_boxplot}, res = 96)
      output$log10_apex_intensity_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$log10_apex_intensity_boxplot}, res = 96)


    }
  })
}

#' @title select_demo_plot_peak_observer_function
#'
#' @description
#' This function responds to a selection in the peak finding demo plot selection menu.
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
select_demo_plot_peak_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$demo_feature_plot_select, {
    values$current_peak_demo_plot <- peak_demo_plot(values$storage_object, input$demo_feature_plot_select)

    output$demo_feature_plot <-shiny::renderPlot({
      values$current_peak_demo_plot
    }, res = 96)
  }, ignoreInit = T)
}

#' @title change_smoothing_for_features_finding_observer_function
#'
#' @description
#' This function allows the user to change the toggle of whether to use smoothing in the peak finding step.
#' See comments in function for individual actions taken by this function
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
change_smoothing_for_feature_finding_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$smooth_features, {
    
    if(!is.null(values$storage_object$data_df)) {

    #update the params passed to find_eic_peaks
    values$storage_object <- user_peak_params_input(
      storage_object = values$storage_object,
      smooth_type = ifelse(input$smooth_features == 'On', 'mva', 'none'),
      smooth_window = 3,
      slope_thresh = 0,
      amp_thresh = values$storage_object$library_data$mzML_parsing_noise_level,
      density_window_rt_distance = values$storage_object$library_data$eic_rt_tolerance,
      density_count_in_window_threshold = ifelse(input$density_filter_features == 'On', 7, 0)
    ) %>%
      #recalculate find eic peaks and the peak metrics
      find_eic_peaks() %>% calc_peak_metrics()

    #initialize the peak finding demo plot with the updated results
    values$current_peak_demo_plot <- peak_demo_plot(values$storage_object, input$demo_feature_plot_select)
    output$demo_feature_plot <-shiny::renderPlot({
      values$current_peak_demo_plot
    }, res = 96)

    #re-initialize the selection menu
    shiny::updateSelectInput(
      inputId = 'demo_feature_plot_select',
      choices = custom_naming_function(values$storage_object$adduct_df %>%
                                         dplyr::pull(.data$unique_adduct_id), values$storage_object$adduct_df %>%
                                         dplyr::left_join(values$storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
                                         dplyr::mutate(temp = stringr::str_c(.data$common_name, ' ',.data$adduct_identity)) %>% dplyr::pull(.data$temp))  %>%
        sample_if_larger(50))
    }
  }, ignoreInit = T)
}

#' @title change_density_filtering_for_feature_finding_observer_function
#'
#' @description
#' This function allows the user to change the toggle of whether to use point density filtering in the peak finding step.
#' See comments in function for individual actions taken by this function
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
change_density_filtering_for_feature_finding_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$density_filter_features, {

    if(!is.null(values$storage_object$data_df)) {
    
    #update the params passed to find_eic_peaks
    values$storage_object <- user_peak_params_input(
      storage_object = values$storage_object,
      smooth_type = ifelse(input$smooth_features == 'On', 'mva', 'none'),
      smooth_window = 3,
      slope_thresh = 0,
      amp_thresh = values$storage_object$library_data$mzML_parsing_noise_level,
      density_window_rt_distance = values$storage_object$library_data$eic_rt_tolerance,
      density_count_in_window_threshold = ifelse(input$density_filter_features == 'On', 7, 0)
    ) %>%
      #recalculate the peak finding and peak metrics
      find_eic_peaks() %>% calc_peak_metrics()

    #initialize the peak finding demo plot with the updated results
    values$current_peak_demo_plot <- peak_demo_plot(values$storage_object, input$demo_feature_plot_select)

    output$demo_feature_plot <-shiny::renderPlot({
      values$current_peak_demo_plot
    }, res = 96)

    #re-initialize the selection menu
    shiny::updateSelectInput(
      inputId = 'demo_feature_plot_select',
      choices = custom_naming_function(values$storage_object$adduct_df %>%
                                         dplyr::pull(.data$unique_adduct_id), values$storage_object$adduct_df %>%
                                         dplyr::left_join(values$storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
                                         dplyr::mutate(temp = stringr::str_c(.data$common_name, ' ', .data$adduct_identity)) %>% dplyr::pull(.data$temp))  %>%
        sample_if_larger(50))
    }
  }, ignoreInit = T)
}

#' @title submit_peak_finding_tab_observer_function
#'
#' @description
#' This is the function that observes the submit button for the peak finding tab. See comments in the code for specific descriptions of function actions.
#' This code also implements the initial peak filtering based on user specified metric threshold parameters. Also creates boxplots for descriptive peak metrics.
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
submit_peak_finding_tab_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$peak_finding_data_submitted, {
    #collect params for finding peaks
    
    if(all(!is.null(values$storage_object$data_df),
           !is.na(input$max_boundaries_count_for_init_filtering),
           !is.na(input$min_points_count_for_init_filtering),
          !is.na(input$min_width_for_init_filtering),
          !is.na(input$max_width_for_init_filtering),
          !is.na(input$min_apex_intensity_for_init_filtering))) {
    
    values$storage_object <- user_peak_params_input(
      storage_object = values$storage_object,
      smooth_type = ifelse(input$smooth_features == 'On', 'mva', 'none'),
      smooth_window = 3,
      slope_thresh = 0,
      amp_thresh = values$storage_object$library_data$mzML_parsing_noise_level,
      density_window_rt_distance = values$storage_object$library_data$eic_rt_tolerance,
      density_count_in_window_threshold = ifelse(input$density_filter_features == 'On', 7, 0)
    ) %>%
      #find the peaks and calculate metrics
      find_eic_peaks() %>% calc_peak_metrics()

    #initialize peak demo plot
    values$current_peak_demo_plot <- peak_demo_plot(values$storage_object, input$demo_feature_plot_select)

    output$demo_feature_plot <-shiny::renderPlot({
      values$current_peak_demo_plot
    }, res = 96)

    #initialize peak demo plot drop down menu
    shiny::updateSelectInput(
      inputId = 'demo_feature_plot_select',
      choices = custom_naming_function(values$storage_object$adduct_df %>%
                                         dplyr::pull(.data$unique_adduct_id), values$storage_object$adduct_df %>%
                                         dplyr::left_join(values$storage_object$standard_df, dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
                                         dplyr::mutate(temp = stringr::str_c(.data$common_name, ' ',.data$adduct_identity)) %>% dplyr::pull(.data$temp))  %>%
        sample_if_larger(50))

    #filter peaks based on user-supplied criteria
    values$storage_object <- initial_filtering(values$storage_object,
                                               number_of_boundaries_threshold = input$max_boundaries_count_for_init_filtering,
                                               number_of_points_threshold = input$min_points_count_for_init_filtering,
                                               peak_width_half_max_threshold_min = input$min_width_for_init_filtering,
                                               peak_width_half_max_threshold_max = input$max_width_for_init_filtering,
                                               peak_intensity_threshold = input$min_apex_intensity_for_init_filtering)

    #Select and name peaks that will be included in the peak review stage (i.e. those that passed initial filtering)
    values$features_to_plot_list <- values$storage_object$peak_df %>% dplyr::filter(.data$passed_initial_filtering) %>% dplyr::pull(.data$unique_peak_id)
    names(values$features_to_plot_list) <- values$storage_object$peak_df %>%
      dplyr::filter(.data$passed_initial_filtering) %>%
      dplyr::left_join(values$storage_object$adduct_df %>% dplyr::select(.data$unique_adduct_id, .data$adduct_identity, .data$unique_standard_id), dplyr::join_by(x$unique_adduct_id == y$unique_adduct_id)) %>%
      dplyr::left_join(values$storage_object$standard_df %>% dplyr::select(.data$unique_standard_id, .data$common_name), dplyr::join_by(x$unique_standard_id == y$unique_standard_id)) %>%
      dplyr::mutate(temp = stringr::str_c('peak_', .data$unique_peak_id, ' ', .data$adduct_identity, ' ', .data$common_name)) %>%
      dplyr::pull(.data$temp)

    #Set index for peak review
    values$index_for_plot <- 1

    #initialize plot for peak review
    values$plot <- peak_review_plot(values$storage_object, values$features_to_plot_list[values$index_for_plot])

    output$plot <- shiny::renderPlot({values$plot}, res = 96)


    #initialize dropdown menu for peak review
    shiny::updateSelectInput(
      inputId = 'review_plot_switch_file',
      choices = values$features_to_plot_list,
      selected = values$features_to_plot_list[values$index_for_plot])

    #create and display boxplots for descriptive peak metrics
    values$storage_object <- create_descriptive_boxplots(values$storage_object)

    output$number_boundaries_in_adduct_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$number_boundaries_in_adduct_boxplot}, res = 96)
    output$peak_width_half_max_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_width_half_max_boxplot}, res = 96)
    output$peak_point_density_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_point_density_boxplot}, res = 96)
    output$peak_skew_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$peak_skew_boxplot}, res = 96)
    output$novel_snr_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$novel_snr_boxplot}, res = 96)
    output$pearson_correlation_with_beta_curve_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$pearson_correlation_with_beta_curve_boxplot}, res = 96)
    output$points_count_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$points_count_boxplot}, res = 96)
    output$log10_apex_intensity_boxplot <- shiny::renderPlot({values$storage_object$library_data$descriptive_boxplots$log10_apex_intensity_boxplot}, res = 96)
    }
  })
}

#' @title move_to_next_plot_observer_function
#'
#' @description
#' This function updates the index_for_plot to advance one position to the next plot
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
move_to_next_plot_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$move_next_plot, {

    if(!is.null(values$storage_object$data_df) & !is.null(values$features_to_plot_list)) {

    values$index_for_plot <- min(c(length(values$features_to_plot_list), values$index_for_plot + 1))
    
    }

  }, ignoreInit = T)
}

#' @title move_to_previous_plot_observer_function
#'
#' @description
#' This function updates the index_for_plot to regress one position to the previous plot
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
move_to_previous_plot_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$move_previous_plot, {
    
    if(!is.null(values$storage_object$data_df) & !is.null(values$features_to_plot_list)) {

    values$index_for_plot <- max(c(1, values$index_for_plot - 1))
    
    }

  }, ignoreInit = T)
}

#' @title switch_review_plot_observer_function
#'
#' @description
#' This function updates the index_for_plot to the one selected from the dropdown standards menu.
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
switch_review_plot_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$review_plot_switch_file, {
    
    if(!is.null(values$storage_object$data_df) & !is.null(values$features_to_plot_list)) {
      
    values$index_for_plot <- which(values$features_to_plot_list == input$review_plot_switch_file)
    values$plot <- peak_review_plot(values$storage_object, values$features_to_plot_list[values$index_for_plot])
    output$plot <- shiny::renderPlot({values$plot}, res = 96)
    
    }
  }, ignoreInit = T)

}

#' @title update_review_plot_index_observer_function
#'
#' @description
#' This function observes a change in the review plot index, such as is caused by advancing, regressing, or switching with the drop down menu.
#' Then, it makes several updates, including to the drop down menu and plot.
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
update_review_plot_index_observer_function <- function(input, output, values) {
  shiny::observeEvent(values$index_for_plot, {
    
    if(!is.null(values$storage_object$data_df)) {
      
      
    #updates drop down to choose plot for review
    shiny::updateSelectInput(
      inputId = 'review_plot_switch_file',
      choices = values$features_to_plot_list,
      selected = values$features_to_plot_list[values$index_for_plot])

    peak_data <- values$storage_object$peak_df %>%
      dplyr::filter(.data$unique_peak_id == values$features_to_plot_list[values$index_for_plot])

    #if a manual annotation exists, this displays it below the plot
    if (!is.na(peak_data$manual_annotation)) {
      output$current_label_output <- shiny::renderText(stringr::str_c('Assigned label ', peak_data$manual_annotation))
    } else { output$current_label_output <- shiny::renderText(NULL) }

    }
  }, ignoreInit = T)
}

#' @title manual_annotation_good_observer_function
#'
#' @description
#' This function is triggered when the user indicated a peak should receive a good classification.
#' It updates the storage object with the label and recreates the library summary table
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
manual_annotation_good_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$dark_green, {
    
    if(!is.null(values$storage_object$data_df)) {
      
    values$storage_object <- manual_peak_annotation(values$storage_object, values$features_to_plot_list[values$index_for_plot], 'Good')
    output$current_label_output <- shiny::renderText('Assigned label Good')

    ggplot2::ggsave(filename = stringr::str_c(values$figures_output_path, values$plot$labels$title, '.png'), plot = values$plot)
    }

  })
}

#' @title manual_annotation_bad_observer_function
#'
#' @description
#' This function is triggered when the user indicated a peak should receive a bad classification.
#' It updates the storage object with the label and recreates the library summary table
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
manual_annotation_bad_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$red, {
    if(!is.null(values$storage_object$data_df)) {
    values$storage_object <- manual_peak_annotation(values$storage_object, values$features_to_plot_list[values$index_for_plot], 'Bad')
    output$current_label_output <- shiny::renderText('Assigned label Bad')


    ggplot2::ggsave(filename = stringr::str_c(values$figures_output_path, values$plot$labels$title, '.png'), plot = values$plot)
}
  })
}

#' @title manual_annotation_indeterminate_observer_function
#'
#' @description
#' This function is triggered when the user indicated a peak should receive an indeterminate classification.
#' It updates the storage object with the label and recreates the library summary table
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
manual_annotation_indeterminate_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$purple, {
    if(!is.null(values$storage_object$data_df)) {
      
    values$storage_object <- manual_peak_annotation(values$storage_object, values$features_to_plot_list[values$index_for_plot], 'Indeterminate')
    output$current_label_output <- shiny::renderText('Assigned label Indeterminate')

    print(values$plot)
    
    print(stringr::str_c(values$figures_output_path, values$plot$labels$title, '.png'))

    ggplot2::ggsave(filename = stringr::str_c(values$figures_output_path, values$plot$labels$title, '.png'), plot = values$plot)

    }
  })
}

#' @title crossed_adduct_submit_observer_function
#'
#' @description
#' This function identifies conflicting adducts when the check adducts submit button is triggered.
#' It also updates the library table with results.
#'
#' #' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
crossed_adduct_submit_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$check_adducts_submitted, {

    if(!is.null(values$storage_object$data_df)) {
      
    values$storage_object <- update_best_peak_and_adduct(values$storage_object)
    values$storage_object <- add_adduct_peak_presence(values$storage_object, rt_tolerance = values$storage_object$library_data$eic_rt_tolerance)
    values$library_df <- export_library_dataframe(values$storage_object)
    output$library_table <- DT::renderDT(values$library_df)


    values$storage_object <- find_adduct_conflicts(values$storage_object, rt_tolerance = values$storage_object$library_data$eic_rt_tolerance)

    values$library_df <- export_library_dataframe(values$storage_object)
    output$library_table <- DT::renderDT(values$library_df)

    }
  })
}

#' @title export_library_data_observer_function
#'
#' @description
#' This function exports library csv files and saves the storage_object when the export button is triggered
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
export_library_data_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$export_data_submitted, {
    
    if(!is.null(values$storage_object$data_df)) {
    
    values$storage_object <- update_best_peak_and_adduct(values$storage_object)
   
    values$storage_object <- add_adduct_peak_presence(values$storage_object, rt_tolerance = values$storage_object$library_data$eic_rt_tolerance)
   
    values$library_df <- export_library_dataframe(values$storage_object)

    output$library_table <- DT::renderDT(values$library_df)

    export_library_csv(values$storage_object, stringr::str_c(values$storage_object$library_data$output_directory_path, '/exported_metScribeR_library.csv'))

    export_library_metrics_csv(values$storage_object, stringr::str_c(values$storage_object$library_data$output_directory_path, '/exported_metScribeR_library_with_metrics.csv'))

    export_msms_csv(values$storage_object, stringr::str_c(values$storage_object$library_data$output_directory_path, '/exported_metScribeR_MoNA_MSMS.csv'))

    save_storage_object(values$storage_object, stringr::str_c(values$storage_object$library_data$output_directory_path, '/storage_object.RDS'))
    
    }
  })
}

#' @title collect_MSMS_data_observer_function
#'
#' @description
#' This function collects MS/MS information from MANA when the MS/MS collection button is triggered.
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
collect_MSMS_data_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$msms_data_collection_submitted, {
    
    if(!is.null(values$storage_object$standard_df)) {

    values$storage_object <- add_msms_information(values$storage_object)
    values$library_df <- export_library_dataframe(values$storage_object)
    output$library_table <- DT::renderDT(values$library_df)
    
    }
  })
}

#' @title update_library_table_observer_function
#'
#' @description
#' This function updates the library
#'
#' @param input r shiny input object
#' @param output r shiny output object
#' @param values values object of reactive values
#'
#' @keywords internal
#' @noRd
#'
update_library_observer_function <- function(input, output, values) {
  shiny::observeEvent(input$update_library_table, {
    
    if(!is.null(values$storage_object$data_df)) {

    values$storage_object <- update_best_peak_and_adduct(values$storage_object)
    values$storage_object <- add_adduct_peak_presence(values$storage_object, rt_tolerance = values$storage_object$library_data$eic_rt_tolerance)
    values$library_df <- export_library_dataframe(values$storage_object)
    output$library_table <- DT::renderDT(values$library_df)
    
    }

  })
}
