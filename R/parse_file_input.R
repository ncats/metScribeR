#' @title parse_file_input
#'
#' @description
#' Takes the input of the shinyDirChoose and shinyFileChoose and parses it into a usable path.
#' Also provides information to assist in the choosing process (runs the user selection boxes)
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param values Object that stores intermediate reactive values for use in server
#' @param session shiny session object
#'
#' @returns A list including the paths of the standard_df csv, adduct_df csv, and output dir reactive objects which are later stored in the values object
#'
#' @keywords internal
#' @noRd

parse_file_input <- function(input, output, values, session) {
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

  shinyFiles::shinyDirChoose(input, "output_directory_path", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = T)

  output$output_directory_path_text <- shiny::renderPrint({
    if (is.integer(input$output_directory_path)) {
      cat("No directory has been selected (Select Output Directory)")
    } else {
      shinyFiles::parseDirPath(volumes, input$output_directory_path)
    }
  })

  shinyFiles::shinyFileChoose(input, "standard_df_path", roots = volumes, session = session)

  output$standard_df_path_text <- shiny::renderPrint({
    if (is.integer(input$standard_df_path)) {
      cat("No file has been selected (Select Compound .csv)")
    } else {
      shinyFiles::parseDirPath(volumes, input$standard_df_path)
    }
  })

  shinyFiles::shinyFileChoose(input, "adduct_df_path", roots = volumes, session = session)

  output$adduct_df_path_text <- shiny::renderPrint({
    if (is.integer(input$adduct_df_path)) {
      cat("No file has been selected (Select Adduct .csv)")
    } else {
      shinyFiles::parseDirPath(volumes, input$adduct_df_path)
    }
  })

  shinyFiles::shinyFileChoose(input, "input_data_loaded", roots = volumes, session = session)

  #These statements parse the shinyFiles inputs into strings for use later. They are then incorporated in the reactiveValues object below so Adam can understand his code.
  #However, even in the reactiveValues object they still need to be called like a function when used in the package.
  output_path_reactive <- shiny::reactive(shinyFiles::parseDirPath(volumes, input$output_directory_path))
  standard_df_path_reactive <- shiny::reactive(shinyFiles::parseFilePaths(volumes, input$standard_df_path)$datapath)
  adduct_df_path_reactive <- shiny::reactive(shinyFiles::parseFilePaths(volumes, input$adduct_df_path)$datapath)

  list('output_path_reactive' = output_path_reactive, 'standard_df_path_reactive' = standard_df_path_reactive, 'adduct_df_path_reactive' = adduct_df_path_reactive)
}
