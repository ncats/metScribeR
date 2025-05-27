#' @title runMetScribeRShinyApp
#'
#' @description
#' Initialize and launch the metScribeR shiny application
#'
#' @export
runMetScribeRShinyApp <- function () {
  shiny::shinyApp(ui = metScribeR_UI_function(),
                  server = metScribeR_Server_function)
}
