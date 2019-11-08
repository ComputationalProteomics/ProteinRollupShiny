source("R/setup_server_utils.R")

module_setup_server <- function(input, output, session) {
    
    output$rollup_status <- renderText({
        "Add sample columns, select feature column and press 'Perform rollup' to start rollup"
    }) 
    
    shinyjs::disable(id="download_results")
    
    rv <- list()
    rv$filedata_1 <- reactive({
        infile <- input$data_file_1
        if (is.null(infile)) {
            return(NULL)
        }
        read_tsv(infile$datapath, col_types = cols())
    })
    
    rv$protein_data <- reactiveVal({
        NULL
    })


    rv$selected_cols_obj <- reactiveVal({
        list()
    })
    
    rv$filename_1 <- reactive({
        infile <- input$data_file_1
        if (is.null(infile)) {
            return(NULL)
        }
        stringi::stri_extract_first(str = infile$name, regex = ".*")
    })
    
    update_selcol_obj <- function(rv, dataset, colname, new_value) {
        selcol_obj <- rv$selected_cols_obj()
        selcol_obj[[dataset]][[colname]] <- new_value
        
        rv$selected_cols_obj(selcol_obj)
        rv
    }
    
    observeEvent(input$perform_rollup, {
        print("Perform rollup")
        output$rollup_status <- renderText({"Starting rollup, this can take a few moments..."})
        
        rdf <- rv$filedata_1()
        selected_samples <- rv$selected_cols_obj()[[1]]$samples
        sdf <- rdf[, selected_samples] %>% as.matrix()
        annot_col <- rdf[[input$feature_col_1]]
        
        rollup_df <- ProteinRollup::protein_rollup(
            protein_ids=annot_col, 
            pep_mat=sdf, 
            protein_col_name="Protein", 
            get_debug_info=FALSE, 
            one_hit_wonders=input$setting_onehitwonders, 
            min_presence=input$setting_minpresence, 
            min_overlap=input$setting_minoverlap
        )
        
        shinyjs::enable(id="download_results")
        
        rv$protein_data(rollup_df)
        print("Protein rollup done!")
        output$rollup_status <- renderText({"Rollup is done! Press download to retrieve the results"})
    })
    
    output$download_results <- downloadHandler(
        filename = function() {
            "protein_rollup_output.tsv"
        },
        content = function(file) {
            if (!is.null(rv$protein_data())) {
                write_tsv(rv$protein_data(), path = file)
            }
            else {
                alert("Need to process protein data first!")
            }
        }
    )
    
    # output$download <- renderUI({
    #     if (!is.null(rv$protein_data())) {
    #         downloadButton("download_results", "Download results")
    #     }
    # })
    
    # downloadButton(ns("download_results"), "Download results")
    
    
    
    # ------------------- Sample 1 Management --------------------

    observeEvent(input$sample_select_button_1, {
        
        selected_samples <- column_selection_action(
            input$data_selected_columns_1,
            rv$selected_cols_obj()[[rv$filename_1()]]$samples
        )

        rv <- update_selcol_obj(rv, rv$filename_1(), "samples", selected_samples)

        sync_select_inputs(
            session, 
            "data_selected_columns_1", 
            "sample_selected_1", 
            rv$filedata_1, 
            selected_samples
        )
    })

    observeEvent(input$sample_deselect_button_1, {
        selected_samples <- column_selection_action(
            input$sample_selected_1,
            rv$selected_cols_obj()[[rv$filename_1()]]$samples, 
            is_deselect = TRUE
        )
        rv <- update_selcol_obj(rv, rv$filename_1(), "samples", selected_samples)
        sync_select_inputs(
            session, 
            "data_selected_columns_1", 
            "sample_selected_1", 
            rv$filedata_1, 
            selected_samples
        )
    })

    observeEvent(input$feature_col_1, {
        if (!is.null(rv$filename_1())) {
            rv <- update_selcol_obj(rv, rv$filename_1(), "feature_col", input$feature_col_1)
        }
    })
    # --------------------------- End ----------------------------
    
    observeEvent(rv$filedata_1(), {
        clear_file_fields(session, rv$filedata_1, c("data_selected_columns_1", "feature_col_1"))
        rv$selected_cols_obj(
            c(rv$selected_cols_obj(), setNames(list(list()), rv$filename_1()))
        )
    })
    
}

