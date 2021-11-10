# Remove existing objects from global environment
objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")

#lib1 <- file.path(getwd(),"packages")
#myPaths <- .libPaths()   # get the paths
#myPaths <- c(lib1,myPaths)  # switch them
#.libPaths(myPaths)

# Install/load required packages
dependencies<-c("htmltools","shiny","shinyBS","shinydashboard","shinycssloaders","shinyjs",
                "tidyverse","readr","exifr","DT","stringr",
                "utils","tools","lifecycle","stats","xml2","base64enc","htmlwidgets",
                "V8","tippy", "slickR", "plotly", "ggpubr", "rlang")

for(i in 1:length(dependencies)){
  if(dependencies[i] %in% installed.packages()==FALSE){
    install.packages(dependencies[i])
    require(dependencies[i],character.only=TRUE)
  } else{
    update.packages(dependencies[i])
    require(dependencies[i],character.only=TRUE)
  }
}


#devtools::install_github('metrumresearchgroup/slickR')

# Specify path to 'camprocessR' source file
source("source/readSD.R")
source("source/copySD.R")
source("source/fixID.R")
source("source/evaluateR.R")
source("source/renameR.R")


mycss <- "
.slick-prev {
left: 2%;
z-index: 1;
}
.slick-next {
right: 2%;
}
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
.radio label {
font-size: 18px;
}

.checkbox label {
font-size: 18px;
}

.shiny-split-layout > div {
overflow: visible;
}

"

# UI
ui <- dashboardPage(title = "SD Card Processing App",
  dashboardHeader(
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 70px}"),
            tags$style(".main-header .logo {
                       height: 70px; 
                       line-height: 75px !important;
                       padding: 0 0px}")
    ),
    # Use image in title
    title = "SD Card Processing App",
    titleWidth = "300px",
    tags$li(a(href = 'http://idfg.idaho.gov',
              img(src = 'idfglogo.png',
                  title = "Idaho Fish and Game", height = "50px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown")
  ),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    fluidPage(
      tags$head(tags$style(HTML(mycss))),
      tabsetPanel(id = "tabs",
        tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>START</b></font>"), value = "home",
                 fluidPage(
                   fluidRow(
                     column(width = 12, align = "left",
                            
                            h1(HTML("<center><b>Welcome to the <br>
                                    <font face=\"Gadugi\" size = 8 color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>SD Card Processing App</b></font>
                                    <br>for remote camera projects</center></b>")),
                            br()),
                     column(width = 6, align = "left",
                            wellPanel(h3(HTML("<b>System Requirements</b>")),
                            h4(HTML(" All new users must download and install <a href=\"http://strawberryperl.com\">
                            STRAWBERRY PERL</a> to its default location in <font face=\"Courier New\">C:\\Strawberry</font></font></center>")),
                            h5(HTML("Check "),
                                         tippy("<font color=\"#347499\">System Information</font>", 
                                               tooltip = "<font size = 4><div align = \"left\">
                                                 Two ways to find this information:
                                                 <ol>
                                                 <li>Start Menu > Settings > System > About > System type</li>
                                                 <li>Start Menu search bar > \"system information\" > Open App</li>
                                                 </ol>
                                                 </font>",
                                               placement = "bottom", theme = "light"),
                                         HTML(" to see whether you have a 64-bit or 32-bit processor")
                            ))
                            ),
                     column(width = 6, align = "left",
                            wellPanel(h3(HTML("<b>What's new in v2.1?</b>")),
                                      h4(HTML("<ol>
                            <li><b>Renaming</b> photos is now a batch step, meaning it can be applied to
                            multiple cameras at the end of the workday or week. Also, when photos are moved to the 
                            \"rename\" folder, the empty camera folders in the \"copy\" folder are now deleted</li>
                            <li>\"<b>Evaluate</b>\" has replaced \"Enjoy\" to allow users to evaluate the 
                            camera set-up/function while scanning motion-triggered photos</li>
                            <li>Detailed <b>progress bars</b> in the bottom right corner of the screen 
                                    have replaced the old progress \"spinners\"</li>
                                              <li>\"<b>Troubleshoot</b>\" has replaced \"FAQ\" to more effectively
                                              address the most common app issues</li></ol>")))
                            
                     ),
                     column(width = 12, align = "center",
                            actionButton(inputId = "start", label = HTML("<font size = 5><b>Select Camera</b></font>"), 
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%"),
                            br(),br(),
                            actionButton(inputId = "start_rename", label = HTML("<font size = 5><b>Rename Batch of Photos</b></font>"), 
                                                style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                            br(),br()
                   ),
                   column(width = 12, align = "center",
                          h4(HTML("<b>Questions?</b> <a href=\"mailto:amanda.carr@idfg.idaho.gov\">Contact Us</a>")))
                 )
                 )
                 ),
        tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>SELECT</b></font>"), value = "select",
                 fluidPage(
                   fluidRow(
                     column(width = 6,
                            wellPanel(
                              #h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP ONE</b></font>")),
                              tags$img(src="schematic_select.png", 
                                       style="display: block; max-height: 150px; max-width: 100%"),
                              br(),
                              h4(tippy("<font color=\"#347499\" size = 4>Where can I find this information?</font>",
                                       tooltip = paste("<font size = 4><div align = \"justify\">
                                  Camera location and ID can be found on the SD card <b>retrieval envelope</b> 
                                  and <b>deployment data sheet</b>
                                  <br><br>
                                  Camera ID can also be found <b>inside the camera</b>.
                                  Only enter the <b>4 digits after IDFG</b>
                                  </div></font><br>",img(src = "camid.png",
                                                         title = "Camera ID location", height = "180px")),
                                       placement = "bottom", theme = "light")),
                              
                              selectInput(inputId = "project", label = HTML("<font size=5>Project</font>"),
                                          choices = c("", "EOE2020 (Ecology of everything)" = "EOE2020",
                                                      "CAMLN2020 (Caribou mountain lions)" = "CAMLN2020",
                                                      "SWWLF2020 (Statewide wolf)" = "SWWLF2020",
                                                      "SWUNG2020 (Statewide ungulate)" = "SWUNG2020")),
                              selectizeInput(inputId = "region", label = HTML("<font size=5>Region</font>"),
                                             choices = c("",1:7),
                                             options = list(create = TRUE)),
                              uiOutput("rsf"),
                              conditionalPanel(condition = "input.project == 'SWWLF2020'",
                                               selectInput(inputId = "wolf_type", label = HTML("<font size=5>Camera Type</font>"),
                                                           choices = c("", "Wolf abundance" = "wolf_a",
                                                                       "Wolf occupancy" = "wolf_o",
                                                                       "Wolf abundance and occupancy (combination camera)" = "wolf_b"))),
                              conditionalPanel(condition = "input.project == 'EOE2020'",
                                               selectInput(inputId = "eoe_style", label = HTML("<font size=5>Camera Style</font>"),
                                                           choices = c("", "Neonate/predator" = "neo_pred",
                                                                       "Ungulate" = "ung"))),
                              textInput(inputId = "cell", label = HTML("<font size=5>Cell Number</font>")),
                              textInput(inputId = "camid", label = HTML("<font size=5>4-digit Camera ID</font>")),
                              htmlOutput("id_alert")
                            )
                     ),
                     column(width = 6,
                            wellPanel(
                              h3(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>INPUT SUMMARY</b></font>")),
                              br(),
                              htmlOutput(outputId = "pcode"),
                              htmlOutput(outputId = "region"),
                              htmlOutput(outputId = "cell"),
                              htmlOutput(outputId = "camid")
                              ),
                            conditionalPanel(
                              condition = "input.cell >=1 & input.camid >=1",
                              h3(HTML("<b>EVERY NEW CAMERA:</b>")),
                              h4(HTML("Verify information above before processing")),
                              actionButton(inputId = "acceptinfo", label = HTML("<font size = 5><b>Accept Camera Information</b></font>"), 
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%"),
                              br(),br(),
                              h3(HTML("<b>IF PROMPTED BY <font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>COPY</b></font>
                                      TO SELECT CORRECT CAMERA</b>:</b>")),
                              h4(HTML("Enter correct camera ID, then read photo metadata again before renaming")),
                              actionButton(inputId = "readagain", label = HTML("<font size = 5><b>Go to READ</b></font>"), 
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                              )
                            )
                     )
                   )
                 ),
        tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>COPY</b></font>"), value = "copy",
                 sidebarLayout(
                   sidebarPanel(width = 6,
                                #h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP TWO</b></font>")),
                                tags$img(src="schematic_copy.png", 
                                         style="display: block; max-height: 100px; max-width: 100%"),
                                br(),
                                br(),
                                wellPanel(style = "background:#d0e2f2",
                                          h3(HTML("<b>Choose folder to copy</b>")),
                                          h5(HTML("<ul style=\"list-style-type:circle;\">
                                            <li>Select <b>DCIM</b> folder on SD card</li>
                                                    <li>"),
                                             tippy("<font color=\"#347499\">What if I'm working with a hard drive that has multiple cameras on it?</font>",
                                                   tooltip = "<font size = 3><div align = \"justify\">
                                                     Select highest folder level that contains photos <u>only</u> from 
                                                     camera you're currently processing</div></font>",
                                                   placement = "bottom", theme = "light"),
                                             HTML("</li></ul>")),
                                          actionButton(inputId = "chooseSourceButton", label = HTML("<font size = 4>Click to Choose Folder</font>"), 
                                                       style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                          uiOutput("selectedSource"),
                                          br()
                                          ,
                                          
                                          h3(tags$b('Choose external hard drive for photo back-up')),
                                          uiOutput(outputId = "copy_note"),
                                          actionButton(inputId = "chooseBackupButton", label = HTML("<font size = 4>Click to Choose Drive</font>"), 
                                                       style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                          uiOutput("selectedBackup"),
                                          br()
                                          ,
                                          
                                          h3(tags$b('Ready to copy?')),
                                          actionButton('fileCopyExecute',HTML('<font size = 5><b>Copy Photos</b></font>'), 
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                                )
                                
                                
                   ),
                   mainPanel(width = 6,
                             conditionalPanel(
                               condition = "input.evaluate_restart >= 1 | input.rename_upload_restart",
                               br(),
                               wellPanel(
                                 style = "background:rgba(219, 230, 240, 0.25)",
                                 h4(HTML("<b>CAUTION:</b> Verify that the output below is for
                                         the current camera before moving on to the next step!"))
                               ),
                               br()
                             ),
                             uiOutput(outputId = "copymessage") %>% withSpinner(color="#3c8dbc")
                             
                   )
                   
                 )
        ),
        tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>READ</b></font>"), value = "read",
                 sidebarLayout(
                   sidebarPanel(width = 6,
                                #h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP THREE</b></font>")),
                                tags$img(src="schematic_read.png", 
                                         style="display: block; max-height: 150px; max-width: 100%"),
                                br(),
                                br(),
                                h4(HTML("Only run if prompted to <b>Read Photo Metadata
                                        Again</b> after you <font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>COPY</b></font> and <font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>SELECT</b></font> correct camera")),
                                br(),
                                wellPanel(style = "background:rgba(219, 230, 240, 0.25)",
                                          h3(tags$b("Verify input (auto-populated)")),
                                          br(),
                                          h4(tags$b('Folder with photos to read:')),
                                          uiOutput("selectedDirectory")
                                ),
                                wellPanel(style = "background:#d0e2f2",
                                          h3(tags$b('Ready to read metadata?')),
                                          actionButton('read_meta',HTML('<font size = 5><b>Read Photo Metadata</b></font>'), 
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                                )
                                
                                
                   ),
                   mainPanel(width = 6,
                             conditionalPanel(
                               condition = "input.evaluate_restart >= 1 | input.rename_restart >= 1",
                               br(),
                               wellPanel(
                                 style = "background:rgba(219, 230, 240, 0.25)",
                                 h4(HTML("<b>CAUTION:</b> Verify that the output below is for
                                         the current camera before moving on to the next step!"))
                               ),
                               br()
                             ),
                             uiOutput(outputId = "metamessage") %>% withSpinner(color="#3c8dbc")
                             
                   )
                   
                 )
        ),
        tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>FIX</b></font>"), value = "fix",
                 sidebarLayout(
                   sidebarPanel(width = 6,
                                #h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP FOUR</b></font>")),
                                tags$img(src="schematic_fixid.png", 
                                         style="display: block; max-height: 150px; max-width: 100%"),
                                br(),
                                br(),
                                h4(HTML("Only run if photo ID error identified during <font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>COPY</b></font> or <font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>READ</b></font>")),
                                br(),
                                wellPanel(style = "background:rgba(219, 230, 240, 0.25)",
                                          h3(HTML("<b>Verify input (auto-populated)</b>")),
                                          br(),
                                          h4(tags$b('Folder with photos to fix')),
                                          uiOutput("selectedFixees")
                                ),
                                wellPanel(style = "background:#d0e2f2",
                                          h3(HTML('<b>Enter four-digit Camera ID</b>')),
                                          h4(tippy("<font color=\"#347499\" size = 4>Where can I find the correct camera ID?</font>",
                                                   tooltip = paste("<font size = 4><div align = \"justify\">
                                         Camera ID can be found on the SD card <b>retrieval envelope</b>,
                                         <b>deployment data sheet</b>, and <b>inside the camera</b>.
                                         Only enter the <b>4 digits after IDFG</b>
                                                         </div></font><br>",img(src = "camid.png",
                                                                                title = "Camera ID location", height = "180px")),
                                                   placement = "bottom", theme = "light")),
                                          textInput(inputId = "ID", label = ""),
                                          htmlOutput(outputId = "fix_alert"),
                                          uiOutput("fixid_preview"),
                                          br(),
                                          h3(tags$b('Ready to fix IDs?')),
                                          actionButton('fix_metadata',HTML('<font size = 5><b>Fix Photo IDs</b></font>'), 
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                                )
                                
                   ),
                   mainPanel(width = 6,
                             conditionalPanel(
                               condition = "input.evaluate_restart >= 1 | input.rename_restart >= 1",
                               br(),
                               wellPanel(
                                 style = "background:rgba(219, 230, 240, 0.25)",
                                 h4(HTML("<b>CAUTION:</b> Verify that the output below is for
                                         the current camera before moving on to the next step!"))
                               ),
                               br()
                             ),
                             uiOutput(outputId = "fixmessage") %>% withSpinner(color="#3c8dbc")
                             )
                 )
        ),
        tabPanel(title = HTML("<font color=\"#3c8dbc\" size = 5 style = \"text-shadow: 1px 1px #4d3a7d\"><b>EVALUATE</b></font>"), value = "evaluate",
                 sidebarLayout(
                   sidebarPanel(width = 6,
                                #h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP FIVE</b></font>")),
                                tags$img(src="schematic_evaluate.png", 
                                         style="display: block; max-height: 150px; max-width: 100%"),
                                br(),
                                br(),
                                wellPanel(style = "background:rgba(219, 230, 240, 0.25)",
                                          radioButtons("goodcam", h4(tags$b("Does this seem like a good camera?")), 
                                               choices = c("Yes" = "yes", "No" = "no"), selected = "yes"),
                                          checkboxGroupInput("badreason", h4(tags$b("If no, why not?")),
                                                             choices = c("Field of view too large (e.g., pointed up trail)" = "largeview",
                                                                         "Cluttered background" = "cluttered",
                                                                         "Waving vegetation/water/other" = "motionsensor",
                                                                         "Night visibility low (e.g., vegetation or other object too close)" = "night",
                                                                         "Pointed too far down" = "pointedown",
                                                                         "Too high in tree" = "toohigh",
                                                                         "Knocked by animal or otherwise skewed" = "knocked",
                                                                         "Malfunction" = "malfunction")),
                                          actionButton(inputId = "submitData",HTML('<font size = 5><b>Save Evaluation</b></font>'), 
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                                )
                                
                   ),
                   mainPanel(width = 6,
                             conditionalPanel(
                               condition = "input.evaluate_restart >= 1 | input.rename_restart >= 1",
                               br(),
                               wellPanel(
                                 style = "background:rgba(219, 230, 240, 0.25)",
                                 h4(HTML("<b>CAUTION:</b> Verify that the output below is for
                                         the current camera before moving on to the next step!"))
                               ),
                               br()
                             ),
                             br(),
                             uiOutput(outputId = "animalphotos"),
                             br(),br()
                             
                   )
                   
                 )
        ),
        tabPanel(title = HTML("<font color=\"#355d8f\" size = 5 style = \"text-shadow: 1px 1px #234064\"><b>RENAME (batch)</b></font>"), value = "rename",
                 sidebarLayout(
                   sidebarPanel(width = 6,
                                #h2(HTML("<font color=\"#3c8dbc\" style = \"text-shadow: 1px 1px #4d3a7d\"><b>STEP SIX</b></font>")),
                                tags$img(src="schematic_rename.png", 
                                         style="display: block; max-height: 150px; max-width: 100%"),
                                br(),
                                br(),
                                h4(HTML("This step applies to multiple cameras and should be completed <b>at the end of the day or week</b>")),
                                br(),
                                wellPanel(style = "background:#d0e2f2",
                                          h3(HTML("<b>1. Verify project and region</b>")),
                                          splitLayout(
                                            selectInput(inputId = "project_rename", label = HTML("<font size=4>Project</font>"),
                                                        choices = c("", "EOE2020 (Ecology of everything)" = "EOE2020",
                                                                    "CAMLN2020 (Caribou mountain lions)" = "CAMLN2020",
                                                                    "SWWLF2020 (Statewide wolf)" = "SWWLF2020",
                                                                    "SWUNG2020 (Statewide ungulate)" = "SWUNG2020")),
                                            selectizeInput(inputId = "region_rename", label = HTML("<font size=4>Region</font>"),
                                                           choices = c("",1:7),
                                                           options = list(create = TRUE))
                                          ),
                                          h3(HTML("<b>2. Choose folder containing multiple cameras for batch renaming</b>")),
                                          uiOutput("batch_hint"),
                                          actionButton(inputId = "chooseBatch", label = HTML("<font size = 4>Click to Choose Folder</font>"), 
                                                       style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                          uiOutput("selectedBatch"),
                                          br(),
                                          h3(tags$b('3. Choose external hard drive for renamed photos')),
                                          uiOutput("rename_hint"),
                                          actionButton(inputId = "chooseRename", label = HTML("<font size = 4>Click to Choose Drive</font>"), 
                                                       style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                          uiOutput("selectedRename"),
                                          br(),
                                          h3(tags$b('4. Ready to rename?')),
                                          actionButton('rename',HTML('<font size = 4><b>Rename Photos</b></font>'), 
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
                                )
                                
                   ),
                   mainPanel(width = 6,
                             conditionalPanel(
                               condition = "input.evaluate_restart >= 1 | input.rename_restart >= 1",
                               br(),
                               wellPanel(
                                 style = "background:rgba(219, 230, 240, 0.25)",
                                 h4(HTML("<b>CAUTION:</b> Verify that the output below is for
                                         the current batch of photos before moving on!"))
                               ),
                               br()
                             ),
                             uiOutput(outputId = "rename_message") %>% withSpinner(color="#3c8dbc")
                   )
                 )
        ),
        tabPanel(title = HTML("<font color=\"#4d3a7d\" size = 5 style = \"text-shadow: 1px 1px #3c8dbc\"><b>TROUBLESHOOT</b></font>"), value = "troubleshoot",
                   fluidPage(
                     fluidRow(
                       column(width = 12, align = "left",
                              h2(HTML("<b><font color=\"#4d3a7d\" size = 5 style = \"text-shadow: 1px 1px #3c8dbc\"><b>TROUBLESHOOT COMMON ISSUES</b></font></b>")),
                              br(),
                              bsCollapse(multiple = TRUE,
                                         bsCollapsePanel(title = HTML("The app created a second project folder within the original project folder"),
                                                         h4(HTML("<div align = \"justify\">
                                                              This happens when you select the project folder instead of the <b>external hard 
                                                              drive itself</b> (e.g., you should select E:\\, not E:\\SWWLF2019). 
                                                              Move all folders create within
                                                              the second folder into the larger project folder, and select the external hard
                                                              drive itself for all future cameras.
                                                              </div>"))
                                         ),
                                         bsCollapsePanel(title = "When I copied photos, the number of images in the newly created folder 
                                                              on the backup drive didn't match the number in the original folder",
                                                         h4(HTML("<div align = \"justify\">
                                                              This usually results because of improper folder selection, either for the
                                                              original photos or the destination drive. 
                                                              <ol><b><li>Original photos:</b>
                                                              If you're copying from an SD card, select the <b>DCIM</b>
                                                              folder on the card. If you're copying from a hard drive
                                                              containing images from multiple cameras, select the highest folder
                                                              level that contains photos <u>only</u> from the camera you're processing
                                                              </li><b><li>Destination for copied photos:</b>
                                                              Ideally, you should select the external hard drive itself (e.g., <b>E:\\</b> or <b>F:\\</b>),
                                                              not a folder on the hard drive. The <b>app will create a project
                                                              folder on the hard drive</b> based on the project code, <b>and
                                                              sub-folders</b> based on the location information provided.
                                                              If you do select a folder, make sure there are <b>no spaces</b>
                                                              in the folder name. Spaces break the app!
                                                              </li></ol>
                                                              Another possible cause of this error is if the user accidentally types
                                                              the same camera ID for two different SD cards. This leads to the photos
                                                              from two SD cards being copied into the same camera folder on the 
                                                              external hard drive. If this occurs, delete all the photos in <u>that
                                                              camera folder only</u> and delete the two rows of the data in the 
                                                              metadata.csv file created within the project folder on the hard drive. 
                                                              Then, process those two SD cards again.
                                                                      </div>"))
                                         ),
                                         bsCollapsePanel(title = HTML("There are still camera ID errors after I attempted to fix the IDs and/or selected a new camera"),
                                                         h4(HTML("<div align = \"justify\">
                                                              The most common cause of persistent camera ID errors is a <b>space</b> somewhere in the folder structure
                                                              (e.g., the space in <font face=\"Courier New\">\"O:\\My Pictures\"</font>). 
                                                              Look for spaces, and if you can't find any, open the <font face=\"Courier New\">FixIDProcessLog.txt</font>
                                                              file to try to <b>diagnose the issue</b>, or 
                                                              <a href=\"mailto:amanda.carr@idfg.idaho.gov\">send us</a> the file for interpretation
                                                              </div>"))
                                         ),
                                         bsCollapsePanel(title = HTML("My computer restarted during the renaming process"),
                                                         h4(HTML("<div align = \"justify\">
                                                              First, check to see if there are any photos left in the <b>\"PROJECT_copy\"</b> folder on your 
                                                              hard drive. If there are none, hooray! The rename was successful. If there are photos left,
                                                              simply go to the RENAME tab, select the same folder (e.g., <b>\"PROJECT_copy\\R2\"</b>) and the same 
                                                              hard drive (e.g., <b>\"E:\"</b>), and click <b>Rename Photos</b> again.
                                                              </div>"))
                                         ),
                                         bsCollapsePanel(title = HTML("After the app finished renaming, there are still photos in the original folder"),
                                                         h4(HTML("<div align = \"justify\">
                                                              As with persistent camera ID errors (addressed above),
                                                              the most common cause of renaming errors is a <b>space</b> somewhere in the folder or file name structure
                                                              (e.g., the space in <font face=\"Courier New\">\"O:\\My Pictures\"</font>). However,
                                                              sometimes image files are <b>empty or corrupted</b>, and no amount of coercing can move or rename them.
                                                              Look for spaces first, and if you can't find any, open the <font face=\"Courier New\">FixIDProcessLog.txt</font>
                                                              file to try to <b>diagnose the issue</b>, or 
                                                              <a href=\"mailto:amanda.carr@idfg.idaho.gov\">send us</a> the file for interpretation
                                                              </div>"))
                                         ),
                                         bsCollapsePanel(title = HTML("I discovered an error in the data I entered after the app copied (or read, fixed, etc.) all the photos"),
                                                         h4(HTML("<div align = \"justify\">
                                                              Unfortunately, because the data you enter provide the link between the deployment information and the
                                                              photos themselves, you'll need to re-do that camera to make sure all IDs are correct and photos are 
                                                              organized appropriately. If you have to re-do a camera, please open the 
                                                              <font face=\"Courier New\">MasterMetadata.csv</font> file created in the 
                                                              project folder on the external hard drive and <b>delete the incorrect row</b> for that camera.
                                                              </div>"))
                                         ),
                                         bsCollapsePanel(title = HTML("I keep clicking buttons, but nothing's happening"),
                                                         h4(HTML("<div align = \"justify\">
                                                              First check to see if any folder selection windows are open. Then check to see if the progress bars
                                                              are running on any of the pages. Finally, close the app, and open it again. Restart whatever camera 
                                                              you had started before the app stopped responding.
                                                              </div>"))
                                         ),
                                         bsCollapsePanel(title = HTML("Something's wrong, but I don't know what"),
                                                         h4(HTML("<div align = \"justify\">
                                                              It happens. Gather as much information as possible (what were you trying to do when the error occurred? 
                                                              has it happened before? if it happened during the Fix or Rename stage, what does the error file say?),
                                                              and contact Mandie Carr by <a href=\"mailto:amanda.carr@idfg.idaho.gov\">email</a> or 
                                                                                 by phone (727-409-4942)</div>"))
                                         )
                                         
                              ),
                              column(width = 12, align = "left",
                                     h4(HTML("<b>Other questions?</b> <a href=\"mailto:amanda.carr@idfg.idaho.gov\">Contact Us</a>"))) # bsCollapse
                              
                              
                       ) # column
                     ) #fluidrow
                   ) # fluidpage
        ) # LAST TABPANEL
      ) # tabset panel
    ) # fluidpage
  ) # dashboard body
) # dashboard page

  
    
server <- function(input, output, session) {
  
  shiny::observeEvent(input$submitData, {
    shiny::showModal(modalDialog(
      uiOutput(outputId = "submission") %>% withSpinner(color="#3c8dbc")
    ))
  })
  
  
  
  shiny::observeEvent(input$start, {
    shiny::updateTabsetPanel(session=session, "tabs",
                      selected = 'select'
    )
  })
  
  shiny::observeEvent(input$start_rename, {
    shiny::updateTabsetPanel(session=session, "tabs",
                      selected = 'rename'
    )
  })
  
  pcode <- reactive({
    req(input$project)
    input$project
  })
  
  output$pcode <- shiny::renderUI({
    req(input$project)
    h4(HTML(paste0("<b>Project code:</b> ", pcode())))
  })
  
  region <- reactive({
    req(input$region)
    paste0("R",input$region)
  })
  
  output$region <- shiny::renderUI({
    req(input$region)
    h4(HTML(paste0("<b>Region:</b> ", region())))
  })
  
  output$rsf <- shiny::renderUI({
    req(input$project == "SWUNG2020")
    selectInput("rsf", label = HTML("<font size=5>RSF Level</font>"),
                choices = c("",
                            "Low" = "low",
                            "High" = "high"))
  })
  
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
  
  output$cell <- shiny::renderUI({
    req(input$cell)
    h4(HTML(paste0("<b>Cell ID:</b> ",cellid())))
  })
  
  camid <- reactive({
    req(input$camid)
    paste0("IDFG",input$camid)
  })
  
  output$camid <- shiny::renderUI({
    if(stringr::str_length(req(input$camid))==4){
      h4(HTML(paste0("<b>Camera ID:</b> ", camid())))
    }
  })
  
  output$id_alert <- shiny::renderUI({
    req(input$camid)
    if(stringr::str_length(input$camid) >0 & (stringr::str_length(input$camid) != 4 | grepl("[a-zA-Z]", input$camid))){
      h4(HTML("<font color=\"red\">ID should be 4 numbers</font>"))
    }
  })
  
  newid <- reactive({
    req(input$ID)
    paste0("IDFG",input$ID)
  })
  
  output$fixid_preview <- shiny::renderUI({
    req(input$ID)
    h4(HTML(paste0("<b>New Photo ID:</b> ", newid())))
  })
  
  shiny::observeEvent(input$acceptinfo, {
    shiny::updateTabsetPanel(session=session, "tabs",
                      selected = 'copy'
    )
  })
  
  shiny::observeEvent(input$readagain, {
    shiny::updateTabsetPanel(session=session, "tabs",
                      selected = 'read'
    )
  })
  
  output$copy_note <- shiny::renderUI({
    req(input$cell)
    h5(HTML(paste0("<ul style=\"list-style-type:circle;\">
    <li>Select the external hard drive itself (e.g., <b>E:\\</b> or <b>F:\\</b>), 
    not a folder on the hard drive</li> 
    <li><b>Do not select the project folder created by the app</b>
    (app will automatically create folder structure 
    ",pcode(),"\\",pcode(),"_copy\\",region(),"\\",cellid()," on hard drive)</li>
    <li>Only select once per session (drive should be the same for all cameras)</li>
    <li><b>No spaces</b> are allowed in the folder path</li>
                   </ul>")))
  })
  
  
  region_rename <- reactive({
    req(input$region_rename)
    paste0("R",input$region_rename)
  })
  
  output$batch_hint <- shiny::renderUI({
    req(input$region_rename)
    h5(HTML(paste0("<ul style=\"list-style-type:circle;\">
                                            <li>Select the folder <b>", 
                   input$project_rename,"\\",input$project_rename,"_copy\\",region_rename(),
                   "</b> created by the app on your external hard drive</li></ul>")))
  })
  
  output$rename_hint <- shiny::renderUI({
    req(input$region_rename)
    h5(HTML(paste0("<ul style=\"list-style-type:circle;\">
                   <li>Select the <b>same hard drive (e.g., E:\\ or F:\\)</b> you chose for copying photos (app will create folder structure ",
                   input$project_rename,"\\",input$project_rename,"_rename\\",
                   region_rename()," on hard drive)</li></ul>")))
  })
  
  readSD(input, output, session)
  copySD(input, output, session)
  fixID(input, output, session)
  evaluateR(input, output, session)
  renameR(input, output, session)
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

