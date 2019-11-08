setup_panel_ui <- function(id) {
    
    ns <- NS(id)
    tabPanel(
        id,
        fluidPage(
            useShinyjs(),
            id = "outer_area",
            tags$style(
                type = "text/css",
                ".button_row { padding: 5px; }",
                "#column_select_noselectize { height: 500px; }"
            ),
            fluidRow(
                column(4,
                       wellPanel(
                           fileInput(
                               ns("data_file_1"), 
                               "Choose data file (TSV)",
                               multiple = FALSE,
                               accept = c("test/tsv", ".tsv")
                           ),
                           selectInput(
                               ns("data_selected_columns_1"),
                               "Select columns",
                               choices = c("Upload a file", "to see colnames here"),
                               multiple = TRUE,
                               selectize = FALSE,
                               size = 20
                           )
                       )
                ),
                column(3,
                       align="center",
                       wellPanel(
                           fluidRow(
                               class = "button_row",
                               h4("Select samples"),
                               actionButton(
                                   ns("sample_deselect_button_1"),
                                   width = "30%",
                                   "<"
                               ),
                               actionButton(
                                   ns("sample_select_button_1"),
                                   width = "30%",
                                   ">"
                               )
                           )
                       ),
                       wellPanel(
                           fluidRow(
                               h4("Settings"),
                               checkboxInput(ns("setting_onehitwonders"), "Include one-hit-wonders", value=FALSE),
                               numericInput(ns("setting_minoverlap"), "Minimum overlap", min=0, max=Inf, step=1, value=3),
                               numericInput(ns("setting_minpresence"), "Minimum presence", min=0, max=Inf, step=0.01, value=0.5)
                           )
                       ),
                       wellPanel(
                           fluidRow(
                               actionButton(ns("perform_rollup"), "Perform rollup")
                           ),
                           fluidRow(
                               textOutput(ns("rollup_status"))
                           ),
                           fluidRow(
                               downloadButton(ns("download_results"))
                           )
                       )
                ),
                column(5,
                       
                       wellPanel(
                           selectInput(
                               ns("sample_selected_1"),
                               "Sample columns",
                               choices = c(""),
                               multiple = TRUE,
                               selectize = FALSE,
                               size = 20
                           ),
                           selectInput(
                               ns("feature_col_1"),
                               "Feature column",
                               choices = c(""),
                               multiple = FALSE,
                               selectize = FALSE
                           )
                       )
                )
            )
        )
    )
}


