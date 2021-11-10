renameR <- function(input, output, session){
        
        # Choose folder with original photos
        shiny::observeEvent(input$chooseBatch, {
                orig_folder <- choose.dir("")
                if(!is.na(orig_folder)){
                        output$selectedBatch <- shiny::renderUI({
                                checkboxGroupInput(inputId = "selectedBatch", label = "",
                                                   choices = orig_folder, selected = orig_folder)
                        })
                } else {"Folder selection cancelled."}
        })
        
        
        # Choose folder for renamed photos
        shiny::observeEvent(input$chooseRename, {
                rename_folder <- choose.dir("")
                if(!is.na(rename_folder)){
                        output$selectedRename <- shiny::renderUI({
                                checkboxGroupInput(inputId = "selectedRename", label = "",
                                                   choices = rename_folder, selected = rename_folder)
                        })
                } else {"Folder selection cancelled."}
        })
        
        output$rename_message <- shiny::renderUI({
                if (input$rename == 0){
                        return()
                }
                else {
                        shiny::isolate({ 
                                shiny::withProgress(
                                        value = 0, {
                                                incProgress(2/10, message = "Finding photos to rename...")
                                                # Generate labels
                                                pcode <- reactive({
                                                        req(input$project_rename)
                                                        input$project_rename
                                                })
                                                prename <- reactive({
                                                        req(pcode())
                                                        paste0(pcode(),"_rename")
                                                })
                                                region <- reactive({
                                                        req(input$region_rename)
                                                        paste0("R",input$region_rename)})
                                                err_prefix<-reactive({
                                                        req(region())
                                                        paste0(pcode(),"_",region())})
                                                
                                                # Create error file/parent folder as necessary
                                                if(stringr::str_sub(input$selectedRename,start=-1)=="\\"){
                                                        backup<-gsub("\\\\","",input$selectedRename)}
                                                else{backup <- gsub("\\\\","/",input$selectedRename)}
                                                
                                                pfolder <- file.path(backup,pcode())
                                                if(!dir.exists(pfolder)){
                                                        dir.create(pfolder)}
                                                rename_log <- file.path(pfolder,paste0(err_prefix(),"_RenameLog.txt"))
                                                lapply(rename_log, function(x) if(!file.exists(x)) file.create(x))
                                                
                                                new_folder <- file.path(pfolder,prename())
                                                lapply(new_folder, function(x) if(!dir.exists(x)) dir.create(x))
                                                new_folder <- file.path(new_folder,region())
                                                lapply(new_folder, function(x) if(!dir.exists(x)) dir.create(x))
                                                
                                                files <- list.files(input$selectedBatch, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
                                                nf <- length(files)
                                                if(nf == 0){
                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>OOPS!</b></font></center>")),
                                                             br(),
                                                             h4(HTML("There are no photos to rename. Please check the folder path for your photos"))
                                                        )
                                                        
                                                } else {
                                                        
                                                        incProgress(3/10, message = "Renaming photos...")
                                                        # Rename photos
                                                        rename_cmnd <- paste("exiftool -r -d %Y%m%d_%H%M%S \"-filename<",new_folder,"/${userlabel;s/ //g}/",pcode(),"_${userlabel;s/ //g}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1/}.%e\" ",
                                                                             input$selectedBatch, sep = "")
                                                        sysout <- file(rename_log, "a")
                                                        sink(sysout,append = T)
                                                        rcd<-system(rename_cmnd, intern = T)
                                                        print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round One~~~~~~~~~~~~~~~~~~~~~~"))
                                                        print(rcd)
                                                        sink()
                                                        close(sysout)
                                                        
                                                        incProgress(4/10, message = "Checking for errors...")
                                                        # Check for photos left behind (not renamed)
                                                        left.files <- list.files(input$selectedBatch, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE)
                                                        lf <- length(left.files)
                                                        
                                                        left.dirs <- list.dirs(input$selectedBatch)
                                                        if(lf == 0){
                                                                unlink(left.dirs, recursive = T)
                                                                new.dirs <- list.dirs(new_folder)
                                                                new.dirs <- new.dirs[new.dirs != new_folder]
                                                                todays.dirs <- file.info(new.dirs) %>% 
                                                                        tidyr::separate(col = mtime, into = c("Date","Time"), sep = " ", remove = F) %>% 
                                                                        tibble::rownames_to_column(var = "folder") %>% 
                                                                        dplyr::filter(Date == Sys.Date())
                                                                new.files <- list.files(todays.dirs$folder[1], pattern = "*.JPG$", full.names = TRUE, recursive = F, include.dirs = FALSE)
                                                                nf <- length(new.files)
                                                                if(nf <= 5){
                                                                        rdf <- exifr::read_exif(new.files, args = "-SourceFile")
                                                                        rdf <- rdf %>%
                                                                                dplyr::mutate(`Old File` = basename(files[1:nf]),
                                                                                              `New File` = basename(SourceFile)) %>%
                                                                                dplyr::select(-SourceFile)
                                                                        output$renametable1 <- DT::renderDataTable({
                                                                                (rdf)},
                                                                                options = list(pageLength = 5,
                                                                                               lengthMenu = c(1,3,5),
                                                                                               scrollX = TRUE
                                                                                ),
                                                                                rownames = FALSE
                                                                        )
                                                                } else {
                                                                        rdf<- exifr::read_exif(new.files[1:5], args = "-SourceFile")
                                                                        rdf <- rdf %>%
                                                                                dplyr::mutate(`Old File` = basename(files[1:5]),
                                                                                              `New File` = basename(SourceFile)) %>%
                                                                                dplyr::select(-SourceFile)
                                                                        output$renametable1 <- DT::renderDataTable({
                                                                                (rdf)},
                                                                                options = list(pageLength = 5,
                                                                                               lengthMenu = c(1,3,5),
                                                                                               scrollX = TRUE
                                                                                ),
                                                                                rownames = FALSE
                                                                        ) 
                                                                }
                                                                list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>ALL DONE!</b></font></center>")),
                                                                     br(),
                                                                     h4(HTML("You renamed all your photos!")),
                                                                     br(),
                                                                     h4(HTML("Please check the example file names below to make sure the rename structure seems correct")),
                                                                     h4(HTML("Then when you're ready, hit the <b>SELECT New Camera</b> button below")),
                                                                     br(),br(),
                                                                     DT::dataTableOutput(outputId = "renametable1", width = "100%"),
                                                                     br(),br(),
                                                                     actionButton(inputId = "rename_restart",label = HTML("<font size = 4><b>SELECT New Camera</b></font>"), 
                                                                                  style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%")
                                                                )
                                                                
                                                                
                                                        } else {
                                                                rename_rnd2 <- paste("exiftool -r -d %Y%m%d_%H%M%S \"-filename<",new_folder,"/${userlabel;s/ //g}/",pcode(),"_${userlabel;s/ //g}_${DateTimeOriginal}_${triggermode;s/(.)(.*\\s)(.)(.*)/$1$3/}${sequence;s/(\\d)(.*)/_$1b/}.%e\" ",
                                                                                     input$selectedBatch, sep = "")
                                                                sysout <- file(rename_log, "a")
                                                                sink(sysout,append = T)
                                                                rcd<-system(rename_rnd2, intern = T)
                                                                print(paste("~~~~~~~~~~~~~~~~~~~~~~Renaming Log ",Sys.time()," Round Two~~~~~~~~~~~~~~~~~~~~~~"))
                                                                print(rcd)
                                                                sink()
                                                                close(sysout)
                                                                
                                                                left.files_2 <- list.files(input$selectedBatch, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE)
                                                                lf_2 <- length(left.files_2)
                                                                
                                                                left.dirs_2 <- list.dirs(input$selectedBatch)
                                                                if(lf_2 == 0){
                                                                        unlink(left.dirs_2, recursive = T)
                                                                        new.dirs <- list.dirs(new_folder)
                                                                        new.dirs <- new.dirs[new.dirs != new_folder]
                                                                        todays.dirs <- file.info(new.dirs) %>% 
                                                                                tidyr::separate(col = mtime, into = c("Date","Time"), sep = " ", remove = F) %>% 
                                                                                tibble::rownames_to_column(var = "folder") %>% 
                                                                                dplyr::filter(Date == Sys.Date())
                                                                        new.files <- list.files(todays.dirs$folder[1], pattern = "*.JPG$", full.names = TRUE, recursive = F, include.dirs = FALSE)
                                                                        nf <- length(new.files)
                                                                        if(nf <= 5){
                                                                                rdf <- exifr::read_exif(new.files, args = "-SourceFile")
                                                                                rdf <- rdf %>%
                                                                                        dplyr::mutate(`Old File` = basename(left.files_2[1:nf]),
                                                                                                      `New File` = basename(SourceFile)) %>%
                                                                                        dplyr::select(-SourceFile)
                                                                                output$renametable1 <- DT::renderDataTable({
                                                                                        (rdf)},
                                                                                        options = list(pageLength = 5,
                                                                                                       lengthMenu = c(1,3,5),
                                                                                                       scrollX = TRUE
                                                                                        ),
                                                                                        rownames = FALSE
                                                                                )
                                                                        } else {
                                                                                rdf<- exifr::read_exif(new.files[1:5], args = "-SourceFile")
                                                                                rdf <- rdf %>%
                                                                                        dplyr::mutate(`Old File` = basename(left.files[1:5]),
                                                                                                      `New File` = basename(SourceFile)) %>%
                                                                                        dplyr::select(-SourceFile)
                                                                                output$renametable1 <- DT::renderDataTable({
                                                                                        (rdf)},
                                                                                        options = list(pageLength = 5,
                                                                                                       lengthMenu = c(1,3,5),
                                                                                                       scrollX = TRUE
                                                                                        ),
                                                                                        rownames = FALSE
                                                                                ) 
                                                                        }
                                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>ALL DONE!</b></font></center>")),
                                                                             br(),
                                                                             h4(HTML("You renamed all your photos!")),
                                                                             br(),
                                                                             h4(HTML("Please check the example file names below to make sure the rename structure seems correct")),
                                                                             h4(HTML("Then when you're ready, hit the <b>SELECT New Camera</b> button below")),
                                                                             br(),br(),
                                                                             DT::dataTableOutput(outputId = "renametable1", width = "100%"),
                                                                             br(),br(),
                                                                             actionButton(inputId = "rename_restart",label = HTML("<font size = 4><b>SELECT New Camera</b></font>"), 
                                                                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%")
                                                                        )
                                                                        
                                                                        
                                                                } else {
                                                                        invisible(lapply(left.dirs_2, function(x) {
                                                                                fi <- file.info(x)
                                                                                if(is.na(fi$isdir) | fi$isdir == T) {
                                                                                        f <- list.files(x, all.files=TRUE, recursive=TRUE, full.names=TRUE)
                                                                                        sz <- sum(file.info(f)$size)
                                                                                        if (sz==0L) unlink(x, recursive = T)
                                                                                }
                                                                        }))
                                                                        new.dirs <- list.dirs(new_folder)
                                                                        new.dirs <- new.dirs[new.dirs != new_folder]
                                                                        todays.dirs <- file.info(new.dirs) %>% 
                                                                                tidyr::separate(col = mtime, into = c("Date","Time"), sep = " ", remove = F) %>% 
                                                                                tibble::rownames_to_column(var = "folder") %>% 
                                                                                dplyr::filter(Date == Sys.Date())
                                                                        new.files <- list.files(todays.dirs$folder[1], pattern = "*.JPG$", full.names = TRUE, recursive = F, include.dirs = FALSE)
                                                                        nf <- length(new.files)
                                                                        if(nf <= 5){
                                                                                rdf <- exifr::read_exif(new.files, args = "-SourceFile")
                                                                                rdf <- rdf %>%
                                                                                        dplyr::mutate(`Old File` = basename(left.files[1:nf]),
                                                                                                      `New File` = basename(SourceFile)) %>%
                                                                                        dplyr::select(-SourceFile)
                                                                                output$renametable1 <- DT::renderDataTable({
                                                                                        (rdf)},
                                                                                        options = list(pageLength = 5,
                                                                                                       lengthMenu = c(1,3,5),
                                                                                                       scrollX = TRUE
                                                                                        ),
                                                                                        rownames = FALSE
                                                                                )
                                                                        } else {
                                                                                rdf<- exifr::read_exif(new.files[1:5], args = "-SourceFile")
                                                                                rdf <- rdf %>%
                                                                                        dplyr::mutate(`Old File` = basename(left.files[1:5]),
                                                                                                      `New File` = basename(SourceFile)) %>%
                                                                                        dplyr::select(-SourceFile)
                                                                                output$renametable1 <- DT::renderDataTable({
                                                                                        (rdf)},
                                                                                        options = list(pageLength = 5,
                                                                                                       lengthMenu = c(1,3,5),
                                                                                                       scrollX = TRUE
                                                                                        ),
                                                                                        rownames = FALSE
                                                                                ) 
                                                                        }
                                                                        
                                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>HMM...</b></font></center>")),
                                                                             br(),
                                                                             h4(HTML(paste0("You finished renaming and moving photos from <font face=\"Courier New\">", 
                                                                                            input$selectedBatch, "</font> to <font face=\"Courier New\">", new_folder, 
                                                                                            "</font>, but there are still <b>", lf, 
                                                                                            " photos left</b> in the original folder."))),
                                                                             br(),
                                                                             h4(HTML(paste0("</b>Please check <font face=\"Courier New\">",rename_log, "</font> 
                                                                             for information regarding this error, and/or 
                                                                                            <a href=\"mailto:amanda.carr@idfg.idaho.gov\">send us the file</a> for troubleshooting"))),
                                                                             h4(HTML("When you're ready to move on, hit the <b>SELECT New Camera</b> button below")),
                                                                             br(),
                                                                             actionButton(inputId = "rename_restart",label = HTML("<font size = 4><b>SELECT New Camera</b></font>"), 
                                                                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%")
                                                                        ) # output(rename_message) last one
                                                                        
                                                                } # else (still photos left in original folder after round 2)
                                                        } # else (start rename round 2)
                                                } # there are photos to rename (at beginning)
                                        })# shiny progress
                        })
                }
        })
        
        # Define additional action buttons
        
        shiny::observeEvent(input$rename_restart, {
                shiny::updateTabsetPanel(session=session, "tabs",
                                  selected = 'select')
                
                shiny::updateTextInput(session, "cell", value = "")
                shiny::updateTextInput(session, "camid", value = "")
                shiny::updateTextInput(session, "ID", value = "")
                shiny::updateSelectInput(session, "eoe_style", selected=character(0))
                shiny::updateSelectInput(session, "wolf_type", selected=character(0))
                
        })
}
        
        