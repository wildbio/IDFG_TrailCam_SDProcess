evaluateR <- function(input, output, session){
        output$submission <- shiny::renderUI({
                if (input$submitData == 0){
                        return()
                }
                else {
                        shiny::isolate({ 
                                shiny::withProgress(
                                        value = 0, {
                                                incProgress(6/10, message = "Updating metadata...")
                                                # Generate labels
                                                pcode <- reactive({
                                                        req(input$project)
                                                        input$project
                                                })
                                                region <- reactive({
                                                        req(input$region)
                                                        paste0("R",input$region)
                                                })
                                                meta_prefix <- reactive({
                                                        req(pcode())
                                                        paste0(pcode(),"_",region())})
                                                
                                                badreason <- reactive({
                                                        paste(input$badreason, collapse = "; ")
                                                })
                                                
                                                if(stringr::str_sub(input$selectedBackup,start=-1)=="\\"){
                                                        backup<-gsub("\\\\","",input$selectedBackup)}
                                                else{backup <- gsub("\\\\","/",input$selectedBackup)}
                                                
                                                pfolder <- file.path(backup,pcode())
                                                
                                                metafile <- file.path(pfolder,paste0(meta_prefix(),"_MasterMetadata.csv"))
                                                metadata <- read.csv(metafile)
                                                n <- nrow(metadata)
                                                
                                                meta_new <- metadata %>%
                                                        dplyr::mutate(GoodCamera = ifelse(dplyr::row_number()==dplyr::n(),input$goodcam, GoodCamera), 
                                                                      BadReason = ifelse(dplyr::row_number()==dplyr::n(), badreason(), BadReason))
                                                
                                                write.csv(meta_new, metafile, row.names = F)
                                                
                                                list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>THANKS FOR EVALUATING THIS CAMERA</b></font></center>")),
                                                     h4(HTML("If it's time to move on to the next camera, click the <b>SELECT New Camera</b> button below")),
                                                     actionButton(inputId = "evaluate_restart",label = HTML("<font size = 4><b>SELECT New Camera</b></font>"), 
                                                                  style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                                     br(),br(),
                                                     h4(HTML("If it's time to rename a batch of photos, click the <b>RENAME All Photos</b> button below")),
                                                     h5(HTML("To save yourself some time, save this step until you have multiple cameras copied and ready for renaming,
                                                              and run at the end of the day")),
                                                     actionButton(inputId = "evaluate_rename",label = HTML("<font size = 4><b>RENAME All Photos</b></font>"), 
                                                                  style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%")
                                                )
                                        }) # shiny progress
                        })
                }
        })
        

        
        shiny::observeEvent(input$evaluate_restart, {
                shiny::updateTabsetPanel(session=session, "tabs",
                                  selected = 'select')
                
                shiny::updateTextInput(session, "cell", value = "")
                shiny::updateTextInput(session, "camid", value = "")
                shiny::updateTextInput(session, "ID", value = "")
                shiny::updateSelectInput(session, "eoe_style", selected=character(0))
                shiny::updateSelectInput(session, "wolf_type", selected=character(0))
                shiny::updateRadioButtons(session, "goodcam", selected = character(0))
                shiny::updateCheckboxGroupInput(session, "badreason", selected = character(0))
                shiny::removeModal()
        })
        
        shiny::observeEvent(input$evaluate_rename, {
                shiny::updateTabsetPanel(session=session, "tabs",
                                  selected = 'rename')
                
                shiny::updateTextInput(session, "project_rename", value = input$project)
                shiny::updateTextInput(session, "region_rename", value = input$region)
                shiny::updateTextInput(session, "cell", value = "")
                shiny::updateTextInput(session, "camid", value = "")
                shiny::updateTextInput(session, "ID", value = "")
                shiny::updateSelectInput(session, "eoe_style", selected=character(0))
                shiny::updateSelectInput(session, "wolf_type", selected=character(0))
                shiny::updateRadioButtons(session, "goodcam", selected=character(0))
                shiny::updateCheckboxGroupInput(session, "badreason", selected=character(0))
                shiny::removeModal()
                
        })
}