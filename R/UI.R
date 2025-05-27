#' @title metScribeR_UI_function
#'
#' @description
#' Creates metScribeR shiny app UI
#'
#' @returns
#' shiny UI object
#'
#' @keywords internal
#' @noRd
metScribeR_UI_function <- function() {
  shiny::fluidPage(
    theme = bslib::bs_theme() %>%
      bslib::bs_add_rules(".approve-curve { background-color: green; color: white }") %>%
      bslib::bs_add_rules(".disapprove-curve { background-color: red; color: white }") %>%
      bslib::bs_add_rules(".multimodal-curve { background-color: purple; color: white }") %>%
      bslib::bs_add_rules(".dir-input-button { border-color: black; }") %>%
      bslib::bs_add_rules(".dir-input-button:hover { background-color: grey; color: white }"),

    shinybusy::add_busy_spinner(spin = 'double-bounce'),

    shiny::titlePanel(title = 'metScribeR'),

    shiny::tabsetPanel(


      #this calls a function from the instructions text .R file to add the instructions tab.
      #    instructions_text_return(),

      shiny::tabPanel('Import EIC Data',

                      shiny::sidebarLayout(

                        shiny::sidebarPanel(

                          shinyFiles::shinyFilesButton("standard_df_path", "Select Standard Reference .csv File", 'Select Standard Reference .csv File', F, class = 'dir-input-button'),

                          shiny::verbatimTextOutput('standard_df_path_text'),

                          shinyFiles::shinyFilesButton("adduct_df_path", "Select Adduct Reference .csv File", 'Select Adduct Reference .csv File', F, class = 'dir-input-button'),

                          shiny::verbatimTextOutput('adduct_df_path_text'),

                          shinyFiles::shinyDirButton("output_directory_path", "Select Output Directory", 'Select Output Directory', F, class = 'dir-input-button'),

                          shiny::verbatimTextOutput('output_directory_path_text'),

                          shiny::numericInput(
                            inputId = 'eic_mz_tolerance',
                            label = 'm/z Tolerance (ppm)',
                            value = 30
                          ),

                          shiny::numericInput(
                            inputId = 'eic_rt_tolerance',
                            label = 'RT Tolerance (min)',
                            value = 0.1
                          ),

                          shiny::actionButton(
                            inputId = 'input_data_submitted',
                            label = 'Submit'
                          ),

                          shinyFiles::shinyFilesButton("input_data_loaded", "Load metScribeR storage_object", "Load metScribeR storage_object", F, class = 'dir-input-button'),
                        ),

                        shiny::mainPanel(
                          shiny::h1('Noise Level Selection'),
                          shiny::p('Use the following to identify and input the level of background noise in your data.'),
                          shiny::p('Click and drag to select a rectangle, then double click on the plot to zoom in. Double click with no rectangle selected to zoom out.'),
                          shiny::plotOutput("noise_plot",
                                            dblclick = "noise_plot_dblclick",
                                            brush = shiny::brushOpts(
                                              id = "noise_plot_brush",
                                              resetOnNew = TRUE
                                            )),

                          shiny::numericInput(
                            inputId = 'mzML_parsing_noise_level',
                            label = 'Noise Level',
                            value = 1E3
                          ),

                          shiny::selectInput(
                            inputId = 'mzML_parsing_noise_level_switch_file',
                            label = 'Switch File',
                            choices = c()
                          )
                        )

                      )
      ),

      shiny::tabPanel('Find Peaks',
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::radioButtons('smooth_features', 'Add Data Smoothing', choices = c('On', 'Off'), selected = c('On')),
                          shiny::radioButtons('density_filter_features', 'Add Density Filter', choices = c('On', 'Off'), selected = c('On')),
                          shiny::numericInput('min_points_count_for_init_filtering', 'Minimum Points per Peak', 7, min = 0),
                          shiny::numericInput('max_boundaries_count_for_init_filtering', 'Maximum Peaks per Adduct', 250, min = 0),
                          shiny::numericInput('min_width_for_init_filtering', 'Minimum Peak Width at Half Max', 0, min = 0),
                          shiny::numericInput('max_width_for_init_filtering', 'Maximum Peak Width at Half Max', 0.25, min = 0),
                          shiny::numericInput('min_apex_intensity_for_init_filtering', 'Minimum Peak Intensity', 10000, min = 0),

                          shiny::actionButton(
                            inputId = 'peak_finding_data_submitted',
                            label = 'Submit'
                          )
                        ),
                        shiny::mainPanel(
                          shiny::h1('Demo Peak Plot'),
                          shiny::p('Use the plot below to choose appropriate smoothing and density toggles. \n 50 random peaks are available in the Switch Peak dropdown menu.'),

                          shiny::plotOutput("demo_feature_plot",
                                            dblclick = "demo_feature_plot_dblclick",
                                            brush = shiny::brushOpts(
                                              id = "demo_feature_plot_brush",
                                              resetOnNew = TRUE
                                            )),


                          shiny::selectInput("demo_feature_plot_select",
                                             label = 'Switch Peak',
                                             choices = c())

                        )
                      )),

      shiny::tabPanel("Descriptive Boxplots",
                      shiny::splitLayout(cellWidths = c('33%', '33%', '33%'),
                                         shiny::plotOutput('log10_apex_intensity_boxplot'),
                                         shiny::plotOutput('points_count_boxplot'),
                                         shiny::plotOutput('peak_width_half_max_boxplot')),

                      shiny::splitLayout(cellWidths = c('33%', '33%', '33%'),
                                         shiny::plotOutput('number_boundaries_in_adduct_boxplot'),
                                         shiny::plotOutput('peak_skew_boxplot'),
                                         shiny::plotOutput('peak_point_density_boxplot')
                                         ),

                      shiny::splitLayout(cellWidths = c('33%', '33%'),
                                         shiny::plotOutput('pearson_correlation_with_beta_curve_boxplot'),
                                         shiny::plotOutput('novel_snr_boxplot'))
      ),

      shiny::tabPanel("Review Results",
                      shiny::plotOutput("plot", click = "plot_click"),

                      shiny::verbatimTextOutput("info"),

                      shiny::actionButton(
                        inputId = 'move_previous_plot',
                        label = NULL,
                        shiny::icon("backward")

                      ),

                      shiny::actionButton(
                        inputId = 'move_next_plot',
                        label = NULL,
                        shiny::icon('forward')
                      ),

                      shiny::actionButton(
                        inputId = 'dark_green',
                        label = 'Good Peak',
                        class = 'approve-curve'
                      ),

                      shiny::actionButton(
                        inputId = 'red',
                        label = 'Bad Peak',
                        class = 'disapprove-curve'
                      ),

                      shiny::actionButton(
                        inputId = 'purple',
                        label = 'Multimodal/Indeterminate',
                        class = 'multimodal-curve'
                      ),

                      shiny::selectInput(
                        inputId = 'review_plot_switch_file',
                        label = 'Switch File',
                        choices = c()
                      ),
                      shiny::verbatimTextOutput('current_label_output')

      ),

      shiny::tabPanel("View/Export Library",
                      shiny::actionButton('update_library_table', 'Update Library Table'),
                      shiny::actionButton(
                        inputId = 'check_adducts_submitted',
                        label = 'Check for Indistinguishable Adducts'
                      ),
                      shiny::actionButton('msms_data_collection_submitted', 'Search MoNA for MS/MS Data'),
                      shiny::actionButton('export_data_submitted', 'Save and Export Library'),
                      DT::DTOutput('library_table')
      ),
      id = 'tabs'
    )

  )
}
