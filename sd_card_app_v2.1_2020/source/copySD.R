copySD <- function(input, output, session){
        shiny::observeEvent(input$chooseSourceButton, {
                current_folder <- choose.dir("")
                if(!is.na(current_folder)){
                        output$selectedSource <- shiny::renderUI({
                                checkboxGroupInput(inputId = "selectedSource", label = NULL,
                                                   choices = current_folder, selected = current_folder)
                        })
                } else {"Folder selection cancelled."}
        })
        
        shiny::observeEvent(input$chooseBackupButton,{
                backup_dir <- choose.dir("")
                output$selectedBackup <- shiny::renderUI({
                        checkboxGroupInput(inputId = "selectedBackup", label = NULL,
                                           choices = backup_dir, selected = backup_dir)
                })
                
        })
        
        
        output$copymessage <- shiny::renderUI({
                if (input$fileCopyExecute == 0){
                        return()
                }
                else {
                        shiny::isolate({ 
                                shiny::withProgress(
                                        value = 0, {
                                                incProgress(2/10, message = "Reading photo metadata...")
                                                # Generate labels 
                                                pcode <- reactive({
                                                        req(input$project)
                                                        input$project
                                                })
                                                pcopy <- reactive({
                                                        req(pcode())
                                                        paste0(pcode(),"_copy")})
                                                region <- reactive({
                                                        req(input$region)
                                                        paste0("R",input$region)})
                                                cellid <- reactive({
                                                        req(input$cell)
                                                        if(input$project == "CAMLN2020"){
                                                                input$cell
                                                        } else if (input$project == "EOE2020" & input$eoe_style == "neo_pred"){
                                                                paste0("P_", input$cell)
                                                        } else if (input$project == "EOE2020" & input$eoe_style == "ung"){
                                                                paste0("U_", input$cell)
                                                        } else if(input$project == "SWUNG2020") {
                                                                paste0(region(),"_",input$rsf,"_",input$cell)
                                                        } else if(input$wolf_type == "wolf_a") {
                                                                paste0("A_",input$cell)
                                                        } else if(input$wolf_type == "wolf_o") {
                                                                paste0("O_",input$cell)
                                                        } else if(input$wolf_type == "wolf_b") {
                                                                paste0("B_",input$cell)
                                                        } 
                                                })
                                                meta_prefix <- reactive({
                                                        req(pcode())
                                                        paste0(pcode(),"_",region())})
                                                camid<-reactive({
                                                        req(input$camid)
                                                        paste0("IDFG",input$camid)})
                                                
                                                ### READ PHOTO METADATA
                                                photos <- list.files(input$selectedSource, pattern = "*.JPG$", recursive = TRUE, full.names = TRUE)
                                                num_files_orig_folder <- length(photos)
                                                
                                                if(num_files_orig_folder > 0){
                                                        
                                                        if(num_files_orig_folder <= 2000){
                                                                dat <- exifr::read_exif(photos, args = "-DateTimeOriginal -TriggerMode -UserLabel -SourceFile")
                                                        } 
                                                        else {
                                                                psub <- photos[c(1:1000,((num_files_orig_folder-1000):num_files_orig_folder))]
                                                                dat <- exifr::read_exif(psub, args = "-DateTimeOriginal -TriggerMode -UserLabel -SourceFile")
                                                        }
                                                        
                                                        sub <- dat %>%
                                                                dplyr::filter(!is.na(UserLabel)) 
                                                        nd <- length(dat$SourceFile)
                                                        ns <- length(sub$SourceFile)
                                                        
                                                        if (nd != ns){
                                                                list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>UH OH!</b></font></center>")),
                                                                     br(),
                                                                     h4(HTML(paste0("There are <b>", nd-ns, " photos</b> on this SD card that were taken on a personal camera, not a game camera"))),
                                                                     h4(HTML("Check the DCIM folder for a sub-folder that does not follow the 100RECNX, 101RECNX, etc. folder structure.
                                                                     Please delete those photos from the SD card, and then click <b>Copy Photos</b> again"))
                                                                ) 
                                                                
                                                        }
                                                        
                                                        else {
                                                                sub <- dat %>%
                                                                        tidyr::separate(col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) %>%
                                                                        dplyr::mutate(Date = stringr::str_replace_all(Date,pattern = ":", replacement = "/")) %>%
                                                                        dplyr::mutate(StartDate = min(Date), EndDate = max(Date)) %>%
                                                                        dplyr::select(-Time) %>%
                                                                        dplyr::mutate(Project = pcode(), Region = region(), CellID = cellid(), CamID = UserLabel)%>%
                                                                        dplyr::mutate(CamID = stringr::str_replace_all(CamID, " ", "")) %>%
                                                                        dplyr::mutate(FixID = F, GoodCamera = NA, BadReason = NA) %>%
                                                                        dplyr::select(Project, Region, CellID, CamID, StartDate, EndDate, FixID, GoodCamera, BadReason) %>%
                                                                        dplyr::distinct() 
                                                                sdf<-as.data.frame(sub, row.names = NULL)
                                                                n<-length(sdf$CellID)
                                                                
                                                                share_table <- sdf %>%
                                                                        dplyr::select(Project, Region, CellID, CamID, StartDate, EndDate)
                                                                output$metatable <- DT::renderDataTable({
                                                                        (share_table)},
                                                                        options = list(scrollX = TRUE
                                                                        ),
                                                                        rownames = FALSE
                                                                )
                                                        }
                                                        
                                                        
                                                        incProgress(3/10, message = "Copying photos...")
                                                        ### COPY PHOTOS
                                                        # Read all files at first level of folder organization
                                                        list_of_dirs <- list.files(input$selectedSource)
                                                        
                                                        # Specify name of new folder in backup_dir, and create if doesn't exist
                                                        if(stringr::str_sub(input$selectedBackup,start=-1)=="\\"){
                                                                backup<-gsub("\\\\","",input$selectedBackup)}
                                                        else{backup <- gsub("\\\\","/",input$selectedBackup)}
                                                        
                                                        pfolder <- file.path(backup,pcode())
                                                        lapply(pfolder, function(x) if(!dir.exists(x)) dir.create(x))
                                                        new_folder <- file.path(pfolder,pcopy())
                                                        lapply(new_folder, function(x) if(!dir.exists(x)) dir.create(x))
                                                        new_folder <- file.path(new_folder,region())
                                                        lapply(new_folder, function(x) if(!dir.exists(x)) dir.create(x))
                                                        new_folder <- file.path(new_folder,cellid())
                                                        lapply(new_folder, function(x) if(!dir.exists(x)) dir.create(x))
                                                        
                                                        # Auto-populate folder inputs for later functions
                                                        output$selectedDirectory <- shiny::renderUI({
                                                                checkboxGroupInput(inputId = "selectedDirectory", label = NULL,
                                                                                   choices = new_folder, selected = new_folder)
                                                        })
                                                        output$selectedFixees <- shiny::renderUI({
                                                                checkboxGroupInput(inputId = "selectedFixees", label = NULL,
                                                                                   choices = new_folder, selected = new_folder)
                                                        })
                                                        output$selectedOrigPhotos <- shiny::renderUI({
                                                                checkboxGroupInput(inputId = "selectedOrigPhotos", label = NULL,
                                                                                   choices = new_folder, selected = new_folder)
                                                        })
                                                        # Initiate copy, make sure all images copy, calculate processing time
                                                        file.copy(from = file.path(input$selectedSource, list_of_dirs),   
                                                                  to = new_folder,
                                                                  overwrite = FALSE,
                                                                  recursive = TRUE,
                                                                  copy.mode = TRUE,
                                                                  copy.date = TRUE)
                                                        
                                                        incProgress(3/10, message = "Checking for errors...")
                                                        newphotos <- list.files(new_folder, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE)
                                                        np <- length(newphotos)
                                                        
                                                        if (np == num_files_orig_folder){
                                                                if (sdf$CamID[1]!=camid() | sdf$CamID[n]!=camid()){
                                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>UH OH!</b></font></center>")),
                                                                             br(),
                                                                             h4(HTML(paste0("<b>GOOD NEWS:</b> You copied <b>", np,
                                                                                            " </b>out of <b>", num_files_orig_folder, " </b>photos into back-up directory <font face=\"Courier New\">",
                                                                                            new_folder,"</font>"))),
                                                                             h4(HTML(paste0("<br><b>BAD NEWS:</b> The photos on this camera have an ID that does not match the user-specified camera ID <b>",
                                                                                            camid(),"</b>"))),
                                                                             h4(HTML("Please check the SD card envelope, deployment data sheet, or inside of camera for the correct camera ID<br><br>")),
                                                                             br(),
                                                                             DT::dataTableOutput(outputId = "metatable", width = "100%"),
                                                                             br(),br(),
                                                                             shiny::splitLayout(cellWidths = c("60%", "40%"),h4(HTML("If the <b>camera ID in the table</b> is correct:")),
                                                                                         actionButton(inputId = "copy_select",label = HTML("<font size = 4><b>Go Back to SELECT</b></font>"), 
                                                                                                      style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%")),
                                                                             br(),br(),
                                                                             shiny::splitLayout(cellWidths = c("60%", "40%"),h4(HTML("If the <b>camera ID you entered</b> is correct:")),
                                                                                         actionButton(inputId = "copy_fix",label = HTML("<font size = 4><b>Go to FIX</b></font>"),
                                                                                                      style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"))
                                                                             )
                                                                         
                                                                        
                                                                } else {
                                                                        incProgress(2/10, message = "Looking for animals...")
                                                                        ## Grab some motion pics for folks to look at
                                                                        motiondata <- dat %>% dplyr::filter(TriggerMode == "M")
                                                                        motionphotos <- motiondata$SourceFile
                                                                        if(length(motionphotos)==0){
                                                                                output$animalphotos <- shiny::renderUI({
                                                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>LET'S WRAP IT UP</b></font></center>")),
                                                                                             br(),
                                                                                             h4("Unfortunately, there are no motion-triggered photos to display. 
                                                                                Please look at some of the photos on the SD card using your computer's photo viewing app,
                                                                                and give your overall assessment of the camera in the quick survey to the left")
                                                                                        )
                                                                                })
                                                                        } else{
                                                                                subdata <- motiondata %>%
                                                                                        tidyr::separate(col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) %>%
                                                                                        dplyr::mutate(Date = stringr::str_replace_all(Date,pattern = ":", replacement = "/")) %>%
                                                                                        dplyr::mutate(Date.graph = as.Date(Date))
                                                                                
                                                                                output$slickr <- slickR::renderSlickR({
                                                                                        if(length(motionphotos)<30){
                                                                                                slickR::slickR(motionphotos, height = "400px") +
                                                                                                        slickR::settings(dots = T)
                                                                                        } else {
                                                                                                motion <- sample(motionphotos, 30)
                                                                                                slickR::slickR(motion, height = "400px") +
                                                                                                        slickR::settings(dots = T)
                                                                                        }
                                                                                })
                                                                                output$animalphotos <- shiny::renderUI({
                                                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>LET'S WRAP IT UP!</b></font></center>")),
                                                                                             br(),
                                                                                             h4(HTML("Scroll through randomly selected motion photos in the slideshow below, 
                                                                                     and give your overall assessment of the camera in the quick survey to the left")),
                                                                                             br(),
                                                                                             slickR::slickROutput("slickr")
                                                                                        )
                                                                                })
                                                                        }
                                                                        
                                                                        
                                                                        csvhead <- stats::setNames(data.frame(matrix(ncol = 11, nrow = 0)), c("Project", "Region", "CellID", "CamID", "StartDate", "EndDate", 
                                                                                                                                       "FixID", "GoodCamera", "BadReason"))
                                                                        csvfile <- file.path(pfolder,paste0(meta_prefix(),"_MasterMetadata.csv"))
                                                                        makemeta <- function(x,y){
                                                                                if(!file.exists(x)) write.csv(y, x)
                                                                        }
                                                                        makemeta(x = csvfile, y = csvhead)
                                                                        md <- read.csv(csvfile, colClasses = "character")
                                                                        md.sdf <- rbind(md,sdf)
                                                                        write.csv(md.sdf, csvfile, row.names = F)
                                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>GREAT JOB!</b></font></center>")),
                                                                             br(),
                                                                             h4(HTML(paste0("You copied <b>", np,
                                                                                            " </b>out of <b>", num_files_orig_folder, " </b>photos into back-up directory <font face=\"Courier New\">",
                                                                                            new_folder,"</font>,"))),
                                                                             h4(HTML("<b>AND</b> we found no errors in the photo metadata")),
                                                                             br(),
                                                                             h4(HTML("Please check the table below to make sure the information seems correct. 
                                                                             <br> Then scroll through some probable animal photos and evaluate this camera!")),
                                                                             br(),
                                                                             actionButton(inputId = "copy_evaluate",label = HTML("<font size = 4><b>EVALUATE Camera</b></font>"), 
                                                                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                                                             br(),br(),
                                                                             DT::dataTableOutput(outputId = "metatable", width = "100%")
                                                                        )
                                                                        
                                                                } 
                                                                
                                                        } else {
                                                                list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>UH OH!</b></font></center>")),
                                                                     br(),
                                                                     h4(HTML("Something went wrong with copying (number of files in back-up folder does not match number of files on SD card). Please check folder paths and try again"))
                                                                )
                                                                
                                                        }
                                                } else {
                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>UH OH!</b></font></center>")),
                                                             br(),
                                                             h4(HTML("There are no photos at the specified folder path. Please check SD card and try again"))
                                                        )
                                                        
                                                        
                                                }  
                                                
                                        })# shiny progress
                        }) # isolate
                }
        })
        
        shiny::observeEvent(input$fileCopyExecute, {
                  
        }) # shiny::observeEvent(input$filecopyexecute)
        
        # Define additional action buttons
        shiny::observeEvent(input$copy_select, {
                shiny::updateTabsetPanel(session=session, "tabs",
                                  selected = 'select'
                )
        })
        
        shiny::observeEvent(input$copy_fix, {
                shiny::updateTabsetPanel(session=session, "tabs",
                                  selected = 'fix'
                )
                shiny::updateTextInput(session = session, inputId = "ID",
                                value = input$camid)
        })
        
        shiny::observeEvent(input$copy_evaluate, {
                shiny::updateTabsetPanel(session=session, "tabs",
                                  selected = 'evaluate')
                
        })
        }
