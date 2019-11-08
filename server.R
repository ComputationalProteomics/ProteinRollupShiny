library(shiny)
library(tidyverse)

options(shiny.maxRequestSize=100*1024^2)

source("modules/module_setup_server.R")

server <- shinyServer(function(session, input, output) {
    
    callModule(module_setup_server, id="Rollup")
})


