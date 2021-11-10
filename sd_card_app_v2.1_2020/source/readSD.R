readSD <- function(input, output, session){
  
  output$metamessage <- shiny::renderUI({
    if (input$read_meta == 0){
      return()
    }
    else {
      shiny::isolate({ 
        shiny::withProgress(
          value = 0, {
            incProgress(4/10, message = "Reading photo metadata...")
            photos <- list.files(input$selectedDirectory, pattern = "*.JPG$", full.names = TRUE, recursive = TRUE)
            np <- length(photos)
            if(np > 0 & np <= 2000){
              dat <- exifr::read_exif(photos, args = "-DateTimeOriginal -TriggerMode -UserLabel -SourceFile")
            } 
            else {
              psub <- photos[c(1:1000,((np-1000):np))]
              dat <- exifr::read_exif(psub, args = "-DateTimeOriginal -TriggerMode -UserLabel -SourceFile")
            }
            
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
            output$readtable <- DT::renderDataTable({
              (share_table)},
              options = list(scrollX = TRUE
              ),
              rownames = FALSE
            )
            
            if (sdf$CamID[1]!=camid() | sdf$CamID[n]!=camid()){
              list(h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>UH OH!</b></font></center>")),
                   h4(HTML(paste0("<br>The photos on this camera still have an ID that does not match the user-specified camera ID <b>",
                                  camid(),"</b>"))),
                   h4(HTML("Please check the SD card envelope, deployment data sheet, or inside of camera for the correct camera ID<br><br>")),
                   br(),
                   DT::dataTableOutput(outputId = "readtable", width = "100%"),
                   br(),br(),
                   shiny::splitLayout(cellWidths = c("60%", "40%"),h4(HTML("If the <b>camera ID in the table</b> is correct:")),
                               actionButton(inputId = "read_select",label = HTML("<font size = 4><b>Go Back to SELECT</b></font>"), 
                                            style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%")),
                   br(),br(),
                   shiny::splitLayout(cellWidths = c("60%", "40%"),h4(HTML("If the <b>camera ID you entered</b> is correct:")),
                               actionButton(inputId = "read_fix",label = HTML("<font size = 4><b>Go to FIX</b></font>"),
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%;"))
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
              
              if(stringr::str_sub(input$selectedBackup,start=-1)=="\\"){
                backup<-gsub("\\\\","",input$selectedBackup)}
              else{backup <- gsub("\\\\","/",input$selectedBackup)}
              
              pfolder <- file.path(backup,pcode())
              if(!dir.exists(pfolder)){
                dir.create(pfolder)}
              
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
                   h4(HTML("Everything looks good on our end, but please check the table below to make sure the information seems correct.")),
                   br(),
                   h4(HTML("Then scroll through some probable animal photos and evaluate this camera!")),
                   br(),
                   actionButton(inputId = "read_evaluate",label = HTML("<font size = 4><b>EVALUATE Camera</b></font>"), 
                                style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                   br(),br(),
                   DT::dataTableOutput(outputId = "readtable", width = "100%")
              )
            }
          }) # shinyprogress
      })
    }
  })
  
  shiny::observeEvent(input$read_select, {
    shiny::updateTabsetPanel(session=session, "tabs",
                      selected = 'select'
    )
  })
  
  shiny::observeEvent(input$read_fix, {
    shiny::updateTabsetPanel(session=session, "tabs",
                      selected = 'fix'
    )
    shiny::updateTextInput(session = session, inputId = "ID",
                    value = input$camid)
  })

  shiny::observeEvent(input$read_evaluate, {
    shiny::updateTabsetPanel(session=session, "tabs",
                      selected = 'evaluate')
  })
}