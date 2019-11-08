library(shiny)
library(R6)
library(plotly)
library(shinyjs)

source("modules/module_setup_ui.R")

ui <- function() {
    shinyUI({
        navbarPage(
            theme = shinythemes::shinytheme("flatly"),
            "ProteinRolluper",
            id="navbar",
            setup_panel_ui("Rollup")
        )
    })
}
