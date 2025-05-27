#' @title metScribeR_Server_function
#'
#' @description
#' The complete server for the metScribeR shiny app. Includes a collection of observer functions and other logic to run the metScribeR app.
#' The code comments describe what each part is used for. In most places, the server is organized where an observer for an input is a self-contained function of all changes
#' that occur when that input action takes place.
#'
#' @param input The R shiny input object
#' @param output The R shiny output object
#' @param session The R shiny session object
#'
#' @keywords internal
#' @noRd
metScribeR_Server_function <- function(input, output, session) {
  ######################### tab Import EIC Data #########################
  #Collects and parses the inputs to file input and directory input from the data input part of the app
  parsed_input_paths <- parse_file_input(input, output, values, session)

  #initializes the values of the values object, which contains most intermediate and stored values and is passed around the observer functions
  values <- define_reactive_values_object(parsed_input_paths)

  #Identify what file will be displayed in noise window
  standard_df_path_observer_function(input, values)

  #This allows zooming on the noise plot
  noise_plot_ranges <- shiny::reactiveValues(x = NULL, y = NULL)
  noise_plot_zoom_observer_function(input, noise_plot_ranges)
  output$noise_plot <- shiny::renderPlot({
    values$noise_plot + ggplot2::coord_cartesian(xlim = noise_plot_ranges$x, ylim = noise_plot_ranges$y, expand = FALSE)
  })


  #This renders the noise plot when the noise level changes
  noise_plot_render_observer_function(input, values)


  #This allows you to switch the .mzML file in the noise plot
  switch_mzML_parsing_noise_level_file_observer_function(input, values)

  #This allows you to load a saved metScribeR object.
  load_saved_data_observer_function(input, output, values)

  #Submit button after data is input to parse data and create EICs
  input_data_submitted_observer_function(input, output, values)

  ################################ tab Find Features #################################
  #Allows selection of the peak demo plot for the peak finding functionality
  select_demo_plot_peak_observer_function(input, output, values)

  #Allows toggle of the peak smoothing option in peak finding
  change_smoothing_for_feature_finding_observer_function(input, output, values)

  #Allows toggle of the density filtering option in peak finding
  change_density_filtering_for_feature_finding_observer_function(input, output, values)

  #Submit button for the peak_finding tab
  submit_peak_finding_tab_observer_function(input, output, values)

  ####################### tab Review Results ############################
  #This makes the R session display the x coordinate of a click on a review plot.
  plot_location_event_printing_observer_function(input)

  #Allows plot switch to the next review plot with the "Next" button
  move_to_next_plot_observer_function(input, output, values)

  #Allows plot switch to the next review plot with the "Previous" button
  move_to_previous_plot_observer_function(input, output, values)

  #Allows plot switch to a review plot of choice with dropdown menu
  switch_review_plot_observer_function(input, output, values)

  #Updates the select plot input and the review plot after plot switching
  update_review_plot_index_observer_function(input, output, values)

  #adds manual annotation of good to review plot peak
  manual_annotation_good_observer_function(input, output, values)

  #adds manual annotation of bad to review plot peak
  manual_annotation_bad_observer_function(input, output, values)

  #adds manual annotation of indeterminate to review plot peak
  manual_annotation_indeterminate_observer_function(input, output, values)

  ################### tab Check Adduct Conflicts #######################

  #checks for crossed adducts
  crossed_adduct_submit_observer_function(input, output, values)

  ####################### tab View/Export Library #####################

  #Export library table and png figures
  export_library_data_observer_function(input, output, values)

  #Collects MSMS data from MoNA
  collect_MSMS_data_observer_function(input, output, values)

  #updates the library
  update_library_observer_function(input, output, values)

}
