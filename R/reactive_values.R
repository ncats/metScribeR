#' @title define_reactive_values_object
#'
#' @description
#' For organization of the server functions, this reactive values object carries most reactive values in the server code.
#'
#' @param parsed_input_paths These are the paths to the specified files as reactive objects collected elsewhere.
#'
#' @returns The values object
#'
#' @keywords internal
#' @noRd
define_reactive_values_object <- function(parsed_input_paths) {
  shiny::reactiveValues(
    index_for_plot = 1,
    plot = ggplot2::ggplot(),
    standard_df = tibble::tibble(),
    adduct_df = tibble::tibble(),
    library_df = tibble::tibble(),
    current_peak_demo_plot = ggplot2::ggplot(),
    noise_plot = ggplot2::ggplot(),
    noise_plot_index = 1,
    output_directory_path = parsed_input_paths$output_path_reactive,
    standard_df_path = parsed_input_paths$standard_df_path_reactive,
    adduct_df_path = parsed_input_paths$adduct_df_path_reactive,
    storage_object = NULL,
    features_to_plot_list = c(),
    figures_output_path = c()
  )
}
