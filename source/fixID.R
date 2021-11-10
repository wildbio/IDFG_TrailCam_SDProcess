fixID <- function(input, output, session){
        shiny::observeEvent(input$ID,{
                if(stringr::str_length(input$ID) > 0 & (stringr::str_length(input$ID) != 4 | grepl("[a-zA-Z]", input$ID))){
                        output$fix_alert <- shiny::renderUI({
                                h4(HTML("<font color=\"red\">ID should be 4 numbers</font>"))
                        })
                } else {
                        output$fix_alert <- shiny::renderUI({
                                h4("")
                        })
                }
        })
        
        output$fixmessage <- shiny::renderUI({
                if (input$fix_metadata == 0){
                        return()
                }
                else {
                        shiny::isolate({ 
                                shiny::withProgress(
                                        value = 0, {
                                                incProgress(3/10, message = "Finding photos to fix...")
                                                # Generate labels
                                                pcode <- reactive({
                                                        req(input$project)
                                                        input$project
                                                })
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
                                                err_prefix<-reactive({
                                                        req(region())
                                                        paste0(pcode(),"_",region())})
                                                
                                                # Create error file/parent folder as necessary
                                                if(stringr::str_sub(input$selectedBackup,start=-1)=="\\"){
                                                        backup<-gsub("\\\\","",input$selectedBackup)}
                                                else{backup <- gsub("\\\\","/",input$selectedBackup)}
                                                
                                                pfolder <- file.path(backup,pcode())
                                                if(!dir.exists(pfolder)){
                                                        dir.create(pfolder)}
                                                errfile <- file.path(pfolder,paste0(err_prefix(),"_FixIDLog.txt"))
                                                lapply(errfile, function(x) if(!file.exists(x)) file.create(x))
                                                
                                                files <- list.files(input$selectedFixees, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE)
                                                np <- length(files)
                                                
                                                if(np == 0){
                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>OOPS!</b></font></center>")),
                                                             br(),
                                                             h4(HTML("There are no photos to overwrite. Please check the folder path for your photos"))
                                                        )
                                                        
                                                        
                                                } else if(np > 0 & np <= 2000){
                                                        incProgress(3/10, message = "Fixing photo metadata...")
                                                        cmnd<-paste("exiftool -r -overwrite_original -UserLabel=IDFG",
                                                                    input$ID," ",input$selectedFixees, sep = "")
                                                        sysout <- file(errfile, "a")
                                                        sink(sysout,append = T)
                                                        rcd<-system(cmnd, intern = T)
                                                        print(paste("~~~~~~~~~~~~~~~~~~~~~~Fix ID Log ",Sys.time(),"~~~~~~~~~~~~~~~~~~~~~~"))
                                                        print(rcd)
                                                        sink()
                                                        close(sysout)
                                                        dat <- exifr::read_exif(files, args = "-DateTimeOriginal -TriggerMode -UserLabel -SourceFile")
                                                } else {
                                                        incProgress(3/10, message = "Fixing photo metadata...")
                                                        cmnd<-paste("exiftool -r -overwrite_original -UserLabel=IDFG",
                                                                    input$ID," ",input$selectedFixees, sep = "")
                                                        sysout <- file(errfile, "a")
                                                        sink(sysout,append = T)
                                                        rcd<-system(cmnd, intern = T)
                                                        print(paste("~~~~~~~~~~~~~~~~~~~~~~Fix ID Log ",Sys.time(),"~~~~~~~~~~~~~~~~~~~~~~"))
                                                        print(rcd)
                                                        sink()
                                                        close(sysout)
                                                        psub <- files[c(1:1000,((np-1000):np))]
                                                        dat <- exifr::read_exif(psub, args = "-DateTimeOriginal -TriggerMode -UserLabel -SourceFile")
                                                }
                                                
                                                sub <- dat %>%
                                                        tidyr::separate(col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) %>%
                                                        dplyr::mutate(Date = stringr::str_replace_all(Date,pattern = ":", replacement = "/")) %>%
                                                        dplyr::mutate(StartDate = min(Date), EndDate = max(Date)) %>%
                                                        dplyr::select(-Time) %>%
                                                        dplyr::mutate(Project = pcode(), Region = region(), CellID = cellid(), CamID = UserLabel)%>%
                                                        dplyr::mutate(CamID = stringr::str_replace_all(CamID, " ", "")) %>%
                                                        dplyr::mutate(FixID = T, GoodCamera = NA, BadReason = NA) %>%
                                                        dplyr::select(Project, Region, CellID, CamID, StartDate, EndDate, FixID, GoodCamera, BadReason) %>%
                                                        dplyr::distinct() 
                                                sdf<-as.data.frame(sub, row.names = NULL)
                                                n<-length(sdf$CellID)
                                                
                                                share_table <- sdf %>%
                                                        dplyr::select(Project, Region, CellID, CamID, StartDate, EndDate)
                                                output$fixtable <- DT::renderDataTable({
                                                        (share_table)},
                                                        options = list(scrollX = TRUE
                                                        ),
                                                        rownames = FALSE
                                                )
                                                
                                                if (sdf$CamID[1]!=camid() | sdf$CamID[n]!=camid()){
                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>HMM...</b></font></center>")),
                                                             br(),
                                                             h4(HTML(paste0("The photos on this camera still have an ID that does not match the user-specified camera ID <b>",
                                                                            camid(),"</b>"))),
                                                             br(),
                                                             h4(HTML(paste0("<br><br>Please check ",errfile, " for information regarding this error,
                                                      and/or <a href=\"mailto:amanda.carr@idfg.idaho.gov\">send us the file</a> for troubleshooting"))),
                                                             br(),br(),
                                                             DT::dataTableOutput(outputId = "fixtable", width = "100%")
                                                        )
                                                        
                                                } else {
                                                        incProgress(3/10, message = "Looking for animals...")
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
                                                        list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>GOOD WORK!</b></font></center>")),
                                                             h4(HTML(paste0("<br>You changed all photo IDs to <b>IDFG", input$ID, "</b>"))),
                                                             br(),
                                                             h4(HTML("Please check the table below to make sure the information seems correct.
                                                             <br>Then scroll through some probable animal photos and evaluate this camera!")),
                                                             br(),
                                                             actionButton(inputId = "fix_evaluate",label = HTML("<font size = 4><b>EVALUATE Camera</b></font>"), 
                                                                          style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                                             br(),br(),
                                                             DT::dataTableOutput(outputId = "fixtable", width = "100%")
                                                        )
                                                        
                                                        
                                                }
                                        }) # shiny progress
                        })
                }
        })
        
        # Define additional action button 
        
        shiny::observeEvent(input$fix_evaluate, {
                shiny::updateTabsetPanel(session=session, "tabs",
                                  selected = 'evaluate')
                
        })
}




