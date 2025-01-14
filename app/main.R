#* @Path app/main.R
options(warn=-1)

#************************ START: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND VIEWS **********************#
box::use(config)

box::use(
  dplyr[`%>%`, inner_join],
  shinypanels[panelsPage, panel],
  stats[aggregate],
  shinyjs[useShinyjs, addClass, removeClass],
  rmarkdown[render, pdf_document],
  shinyWidgets[materialSwitch, updateMaterialSwitch],
  shinydashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, box, tabItems, tabItem, sidebarMenu, menuItem, menuSubItem],
  shinydashboardPlus[accordion, accordionItem],
  shiny[moduleServer, NS, observe, tags, reactive, observeEvent, reactiveVal, actionButton, tagList, div, checkboxGroupInput, sliderInput, selectInput, updateSelectInput, checkboxInput, fluidRow, column, icon, textOutput, HTML, downloadHandler, downloadButton, uiOutput, renderUI, wellPanel, selectizeInput, updateSelectizeInput, reactiveValues, radioButtons, renderText, req, updateCheckboxGroupInput],
  zip[zip],
  waiter[useWaiter, waiterShowOnLoad, spin_fading_circles, waiter_hide]
)

# Loading views
box::use(
  view/overview,
  view/demo_analysis_barplot,
  view/demo_analysis_diagnosis,
  view/demo_analysis_metrics,
  view/demo_analysis_patient_characteristics
)

box::use(
  ./logic/utils/db_common_utils[overviewdf, uniquePcpNames, lastSyncDate, unique_anatomic_codes, unique_morphology_codes, unique_race, unique_site_name,
                                unique_pcp_full_name],
  ./logic/utils/common_utils[generate_report_filter_fields, downloadReportHandler, generateRMD]
)
#************************ START: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND VIEWS **********************#

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  dashboardPage(title="Project demo",
                dashboardHeader(title = tags$img(class="logo-img", height = "20px", alt="Project demo", src="./static/img/logotype-black.png"),
                                tags$li(class = "dropdown",
                                        useWaiter(), 
                                        waiterShowOnLoad(html = spin_fading_circles()),
                                        # Javasript Code
                                        tags$head(
                                          HTML(
                                            "
                                              <script type='text/javascript'>
                                                var socket_timeout_interval
                                                var n = 0
                                                $(document).on('shiny:connected', function(event) {
                                                  socket_timeout_interval = setInterval(function(){
                                                    console.log('Setting new interval...')
                                                  }, 50000)
                                                });
                                                $(document).on('shiny:disconnected', function(event) {
                                                  clearInterval(socket_timeout_interval)
                                                });
                                                
                                                $(document).ready(function() {
                                                  // Simulate a click on the second radio button
                                                  $('input[name=\"app-overview-overviewIndicationsSunburstRadioFilter\"]').eq(1).prop('checked', true).change();
                                                });
                                              </script>
                                            "
                                          )
                                        ),
                                        tags$span( class = "maintenance-message",
                                                   ""
                                        ),
                                        tags$span( class = "last-sync-time",
                                                   icon("sync"),
                                                   "data updated on ", HTML('&nbsp;'), textOutput(ns("showLastSyncDate")), HTML('&nbsp;'), "UTC"
                                        )
                                )),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Overview", tabName = "overview"),
                    menuItem("Analysis", tabName = "demo_analysis_landing",
                             menuSubItem('Barplot', tabName = 'demo_analysis_barplot'),
                             menuSubItem('Diagnosis', tabName = 'demo_analysis_diagnosis'),
                             menuSubItem('Metrics', tabName = 'demo_analysis_metrics'),
                             menuSubItem('Patient Characteristics', tabName = 'demo_analysis_patient_characteristics'))
                  )
                ),
                dashboardBody(
                  tags$div(
                    useShinyjs(),
                    panelsPage(
                      panel(title = list(textOutput(ns("appliedFiltersCount")), HTML("&nbsp;"), HTML("&nbsp;"), HTML("&nbsp;"), "Filters"),
                            title_complent = "HOLAAAA", color = "#04bb7a", collapsed = TRUE, width =  400,
                            head = h2("Head"),
                            body = div(
                              checkboxGroupInput(ns("filterSex"), "Sex",
                                                 c("Male" = "Male",
                                                   "Female" = "Female",
                                                   "NA" = "No Data Available"),
                                                 inline = TRUE),
                              div( class = "display-none",
                                   sliderInput(ns("filterAge"), "Age",
                                               min = 0, max = 120, value = c(18, 100)
                                   ),
                                   checkboxInput(ns("filterAgeNAValues"), "Include NA", TRUE)
                              ),
                              div(
                                checkboxGroupInput(ns("filterRace"), "Race",
                                                   c(unique_race),
                                                   inline = TRUE)
                              ),
                              div(
                                checkboxGroupInput(ns("filterSiteName"), "Site Name",
                                                   c(unique_site_name),
                                                   inline = TRUE)
                              ),
                              div(
                                fluidRow(
                                  column(10, class = "margin-bottom-minus-18px",
                                         selectizeInput(ns("filterPcp"),"Assigned MD", c(unique_pcp_full_name), multiple = TRUE,
                                                        options = list(plugins= list('remove_button'),
                                                                       placeholder = "Select Assigned MD")),
                                  )
                                )
                              ),
                              div(
                                fluidRow(
                                  column(10, class = "filterAnatomic margin-bottom-minus-18px",
                                         selectizeInput(ns("filterAnatomic"),"Anatomic Code", c(unique_anatomic_codes), multiple = TRUE,
                                                        options = list(plugins= list('remove_button'),
                                                                       placeholder = "Select Anatomic Code"))
                                  )
                                )
                              ),
                              div(
                                fluidRow(
                                  column(10, class = "filterMorphology margin-bottom-minus-18px",
                                         selectizeInput(ns("filterMorphology"),"Morphology Code", 
                                                        c(unique_morphology_codes), 
                                                        multiple = TRUE,
                                                        options = list(plugins= list('remove_button'),
                                                                       placeholder = "Select Morphology Code"))
                                  )
                                )
                              ),
                              div(
                                fluidRow(
                                  column(10, class = "filterConsentStatus",
                                         checkboxGroupInput(ns("filterConsentStatus"), "Consent Status",
                                                            c("Consented" = "Agreed",
                                                              "Quiet" = "Quiet",
                                                              "Disagreed" = "Disagreed"),
                                                            selected = c("Agreed", "Quiet", "Disagreed"),
                                                            inline = TRUE)
                                  )
                                )
                              ),
                              div(class = "filterLastElement",
                                  fluidRow(
                                    column(4,
                                           materialSwitch(ns("filterIncludeExcludeNAIndications"), label = "Remove NA", status = "info", value = TRUE)         
                                    )
                                  )
                              )
                            ),
                            footer = list(
                              actionButton(ns("resetFilters"), "Reset", class = "btn btn-warning color-white margin-right-5px margin-left-13px"),
                              actionButton(ns("applyFilters"), "Apply Filter", class = "btn btn-danger color-white")
                            )
                      )
                    )
                  ),
                  tabItems(
                    # First tab content
                    tabItem(tabName = "overview",
                            overview$ui(ns("overview"))
                    ),
                    
                    # Second tab content
                    tabItem(tabName = "demo_analysis_barplot",
                            demo_analysis_barplot$ui(ns("demo_analysis_barplot"))
                    ),
                    
                    # Third tab content
                    tabItem(tabName = "demo_analysis_diagnosis",
                            demo_analysis_diagnosis$ui(ns("demo_analysis_diagnosis"))
                    ),
                    
                    # Fourth tab content
                    tabItem(tabName = "demo_analysis_metrics",
                            demo_analysis_metrics$ui(ns("demo_analysis_metrics"))
                    ),
                    
                    # Fifth tab content
                    tabItem(tabName = "demo_analysis_patient_characteristics",
                            demo_analysis_patient_characteristics$ui(ns("demo_analysis_patient_characteristics"))
                    )
                  ),
                  tags$div(class = "container-fluid selected-generate-report-containt-list",
                           fluidRow(
                             column(6, tags$h4("Content to be added in report")),
                             column(6, class = "downloadReportButtonSection",
                                    downloadButton(ns("downloadHTMLReport"), "HTML", class = "btn btn-primary color-white"),
                                    downloadButton(ns("downloadPDFReport"), "PDF", class = "btn btn-primary color-white"),
                                    downloadButton(ns("downloadRMDReport"), "RMD", class = "btn btn-primary color-white"),
                                    actionButton(ns("clearDownloadReportList"), "Clear", class = "btn btn-warning color-white"),
                             )
                           ),
                           fluidRow(class = "selected-generate-report-containt-list-input-box",
                                    column(12,
                                           selectizeInput(ns("mainSelectedGenerateReportContaintList"), "",
                                                          c(""), multiple = TRUE,
                                                          options = list(plugins= list('remove_button')) )
                                    )
                           )
                  ) 
                )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    shinyjs::addClass(selector = ".selected-generate-report-containt-list", class = "display-none")
    
    if(config$get("environment") == 'production'){
      shinyjs::addClass(selector = ".maintenance-message", class = "display-none")
    }
    
    
    #************************ START: RESET FILTER **********************#
    
    observeEvent(input$resetFilters, {
      
      #************************ START: RESET FILTER AGE **********************#
      updateCheckboxGroupInput(session = session, inputId = "filterSex", choices = c("Male" = "Male",
                                                                                     "Female" = "Female",
                                                                                     "NA" = "No Data Available"),
                               inline = TRUE, 
                               selected = NULL)
      #************************ END: RESET FILTER AGE **********************#
      
      
      #************************ START: RESET FILTER RACE **********************#
      updateCheckboxGroupInput(session = session, inputId = "filterRace", choices = c(unique_race),
                               inline = TRUE, 
                               selected = NULL)
      #************************ END: RESET FILTER RACE **********************#
      
      
      #************************ START: RESET FILTER Tess site NAME **********************#
      updateCheckboxGroupInput(session = session, inputId = "filterSiteName", choices = c(unique_site_name),
                               inline = TRUE, 
                               selected = NULL)
      #************************ END: RESET FILTER Tess site NAME **********************#
      
      
      #************************ START: RESET FILTER Assigned MD **********************#
      updateSelectizeInput(session = session, inputId = "filterPcp", choices = c(unique_pcp_full_name), selected = c(), server = TRUE)  
      #************************ END: RESET FILTER Assigned MD **********************#
      
      
      #************************ START: RESET FILTER ANATOMIC CODE **********************#
      updateSelectizeInput(session = session, inputId = "filterAnatomic", choices = c(unique_anatomic_codes), selected = c(), server = TRUE)  
      #************************ END: RESET FILTER ANATOMIC CODE PROVIDER **********************#
      
      
      #************************ START: RESET FILTER MORPHOLOGY CODE **********************#
      updateSelectizeInput(session = session, inputId = "filterMorphology", choices = c(unique_morphology_codes), selected = c(), server = TRUE)  
      #************************ END: RESET FILTER MORPHOLOGY CODE **********************#
      
      
      #************************ START: RESET FILTER CONSENT STATUS **********************#
      updateCheckboxGroupInput(session = session, inputId = "filterConsentStatus", choices = c("Consented" = "Agreed",
                                                                                               "Quiet" = "Quiet",
                                                                                               "Disagreed" = "Disagreed"),
                               inline = TRUE, 
                               selected = c("Agreed", "Quiet", "Disagreed"))
      #************************ END: RESET FILTER CONSENT STATUS **********************#
      
      
      #************************ START: RESET FILTER ABSTRACTED **********************#
      updateMaterialSwitch(session = session, inputId = "filterIncludeExcludeNAIndications", value = TRUE)  
      #************************ END: RESET FILTER ABSTRACTED **********************#
    })
    
    #************************ END: RESET FILTER **********************#
    
    session$userData$filter <- reactiveVal(overviewdf)
    
    appliedFiltersCount <- reactiveVal(paste0("Applied Filters 1"))
    
    session$userData$contentToBeAddedInReport <- reactiveValues(filterMinAge = 'NULL',
                                                                filterMaxAge = 'NULL',
                                                                filterSex = 'NULL',
                                                                filterRace = 'NULL',
                                                                filterSiteName = 'NULL',
                                                                filterPcp = 'NULL',
                                                                plotSpecificFilter = "NULL",
                                                                plotSpecificFilterRows = "NULL",
                                                                plotSpecificFilterColumns = "NULL",
                                                                plotSpecificFilterSingleColumn = "NULL",
                                                                filterDateFrom = "NULL",
                                                                filterDateTo = "NULL",
                                                                filterIncludeExcludeNAIndications = "NULL",
                                                                filterConsentStatus = "NULL",
                                                                filterAnatomic = "NULL",
                                                                filterMorphology = "NULL",
                                                                plotName = 'NULL')
    
    session$userData$contentToBeAddedInReportDf <- data.frame(filterMinAge = c('NULL'),
                                                              filterMaxAge = c('NULL'),
                                                              filterSex = c('NULL'),
                                                              filterRace = c('NULL'),
                                                              filterSiteName = c('NULL'),
                                                              filterPcp = c('NULL'),
                                                              plotSpecificFilter = c("NULL"),
                                                              plotSpecificFilterRows = c("NULL"),
                                                              plotSpecificFilterColumns = c("NULL"),
                                                              plotSpecificFilterSingleColumn = c("NULL"),
                                                              filterDateFrom = c("NULL"),
                                                              filterDateTo = c("NULL"),
                                                              filterIncludeExcludeNAIndications = c("NULL"),
                                                              filterConsentStatus =  c("NULL"),
                                                              filterAnatomic = c("NULL"),
                                                              filterMorphology = c("NULL"),
                                                              plotName = c('NULL'))
    
    
    output$showLastSyncDate <- reactive({
      lastSyncDate()
    })
    
    output$appliedFiltersCount <- reactive({
      appliedFiltersCount()
    })
    waiter_hide()
    observeEvent(input$filterAnatomic, {
      if(is.null(input$filterAnatomic)){
        updateSelectizeInput(session, "filterMorphology", choices = unique_morphology_codes, server = TRUE)  
      } else {
        get_morphology_codes <- overviewdf[overviewdf$anatomic_site_code %in% c(input$filterAnatomic), ]
        updateSelectizeInput(session, "filterMorphology", choices = unique(get_morphology_codes$icd03_morphology_code), server = TRUE)  
      }
    })
    
    session$userData$contentToBeAddedInReport$filterMinAge = as.numeric(18)
    session$userData$contentToBeAddedInReport$filterMaxAge = as.numeric(100)

    # filteredDf1 <- overviewdf[overviewdf$consent_status %in% c("Agreed"),]
    session$userData$contentToBeAddedInReport$filterConsentStatus = "Agreed, Disagreed, Quiet"

    # session$userData$filter(filteredDf1)
    
    observeEvent(input$applyFilters, {
      
      shinyjs::addClass(selector = ".is-collapsible", class = "is-collapsed")
      
      tryCatch({
        count <- 0
        #************************ START: FILTER AGE **********************#
        FilterAgeInput <- strsplit(as.character(input$filterAge), " ")
        
        if(input$filterAgeNAValues){
          filteredAgeWithNA <- overviewdf[overviewdf$age %in% c(NA),]  
          filteredAgeWithRange <- overviewdf[overviewdf$age >= as.numeric(FilterAgeInput[1]) & overviewdf$age <= as.numeric(FilterAgeInput[2]),]  
          
          session$userData$contentToBeAddedInReport$filterMinAge = as.numeric(FilterAgeInput[1])
          session$userData$contentToBeAddedInReport$filterMaxAge = as.numeric(FilterAgeInput[2])
          
          filteredDf <- rbind(filteredAgeWithNA, filteredAgeWithRange)
        } else {
          
          session$userData$contentToBeAddedInReport$filterMinAge = as.numeric(FilterAgeInput[1])
          session$userData$contentToBeAddedInReport$filterMaxAge = as.numeric(FilterAgeInput[2])
          
          filteredDf <- overviewdf[overviewdf$age >= as.numeric(FilterAgeInput[1]) & overviewdf$age <= as.numeric(FilterAgeInput[2]),]
        }
        #************************ END: FILTER AGE **********************#
        
        
        #************************ START: FILTER SEX **********************#
        if(!is.null(input$filterSex)){
          
          count <- count + 1
          
          # combining elements using ,
          session$userData$contentToBeAddedInReport$filterSex <- toString(input$filterSex)
          
          filteredDf <- filteredDf[filteredDf$sex %in% c(input$filterSex),] 
        }
        else {
          session$userData$contentToBeAddedInReport$filterSex = 'NULL'
        }
        #************************ END: FILTER SEX **********************#
        
        
        #************************ START: FILTER RACE **********************#
        if(!is.null(input$filterRace)){
          
          count <- count + 1
          
          inputData <- input$filterRace
          
          if("NA" %in% c(inputData)){
            inputData <- c(inputData, "No Data Available")
          }
          
          # combining elements
          session$userData$contentToBeAddedInReport$filterRace <- toString(inputData) #paste(fvec, collapse = ", ")
          
          filteredDf <- filteredDf[filteredDf$race %in% c(inputData),] 
          
        } 
        else {
          session$userData$contentToBeAddedInReport$filterRace = 'NULL'
        }
        #************************ END: FILTER RACE **********************#
        
        
        #************************ START: FILTER SITE NAME **********************#
        if(!is.null(input$filterSiteName)){
          
          count <- count + 1
          
          inputData <- input$filterSiteName
          
          if("NA" %in% c(inputData)){
            inputData <- c(inputData, "No Data Available")
          }
          
          # combining elements using ,
          session$userData$contentToBeAddedInReport$filterSiteName <- toString(inputData)
          
          filteredDf <- filteredDf[filteredDf$site_name %in% c(inputData),] 
        } 
        else {
          session$userData$contentToBeAddedInReport$filterSiteName = 'NULL'
        }
        #************************ END: FILTER SITE NAME **********************#
        
        
        #************************ START: FILTER PCP **********************#
        if(!is.null(input$filterPcp)){
          
          count <- count + 1
          
          inputData <- input$filterPcp
          
          if("NA" %in% c(inputData)){
            inputData <- c(inputData, "No Data Available")
          }
          
          # combining elements using
          session$userData$contentToBeAddedInReport$filterPcp <- toString(inputData)
          
          filteredDf <- filteredDf[filteredDf$assigned_md %in% c(inputData),] 
        } 
        else {
          session$userData$contentToBeAddedInReport$filterPcp = 'NULL'
        }
        #************************ END: FILTER RACE **********************#
        
        
        #************************ START: FILTER INCLUDE EXCLUDE NA INDICATIONS **********************#
        if(!is.null(input$filterIncludeExcludeNAIndications)){
          
          inputData <- input$filterIncludeExcludeNAIndications
          
          if(inputData == TRUE){
            count <- count + 1
            session$userData$contentToBeAddedInReport$filterIncludeExcludeNAIndications <- inputData
            
            filteredDf <- filteredDf[!(filteredDf$icd03_code %in% "No Data Available"),] 
          } else {
            session$userData$contentToBeAddedInReport$filterIncludeExcludeNAIndications <- 'NULL'
          }
        } 
        else {
          session$userData$contentToBeAddedInReport$filterIncludeExcludeNAIndications <- 'NULL'
        }
        #************************ END: FILTER INCLUDE EXCLUDE NA INDICATIONS **********************#
        
        
        #************************ START: FILTER CONSENT STATUS **********************#
        if(!is.null(input$filterConsentStatus)){
          
          count <- count + 1
          
          inputData <- input$filterConsentStatus
          
          if("NA" %in% c(inputData)){
            inputData <- c(inputData, "No Data Available")
          }
          
          # combining elements using
          session$userData$contentToBeAddedInReport$filterConsentStatus <- toString(inputData)
          
          filteredDf <- filteredDf[filteredDf$consent_status %in% c(inputData),] 
        } 
        else {
          filteredDf <- filteredDf[filteredDf$consent_status %in% c("Agreed", "Disagreed", "Quiet"),] 
          session$userData$contentToBeAddedInReport$filterConsentStatus = "Agreed, Disagreed, Quiet"
        }
        #************************ END: FILTER CONSENT STATUS **********************#
        
        
        #************************ START: FILTER ANATOMIC CODES **********************#
        if(!is.null(input$filterAnatomic)){
          
          count <- count + 1
          
          inputData <- c(input$filterAnatomic)
          
          if("NA" %in% c(inputData)){
            inputData <- c(inputData, "No Data Available")
          }
          
          # combining elements using
          session$userData$contentToBeAddedInReport$filterAnatomic <- toString(inputData)
          
          filteredDf <- filteredDf[filteredDf$anatomic_site_code %in% c(inputData),] 
        } 
        else {
          session$userData$contentToBeAddedInReport$filterAnatomic = NULL
        }
        #************************ END: FILTER ANATOMIC CODES **********************#
        
        
        #************************ START: FILTER MORPHOLOGY **********************#
        if(!is.null(input$filterMorphology)){
          
          count <- count + 1
          
          inputData <- c(input$filterMorphology)
          
          if("NA" %in% c(inputData)){
            inputData <- c(inputData, "No Data Available")
          }
          
          # combining elements using
          session$userData$contentToBeAddedInReport$filterMorphology <- toString(inputData)
          
          filteredDf <- filteredDf[filteredDf$icd03_morphology_code %in% c(inputData),] 
        } 
        else {
          session$userData$contentToBeAddedInReport$filterMorphology = NULL
        }
        #************************ END: FILTER MORPHOLOGY **********************#
        appliedFiltersCount(paste("Applied Filters ", count))
        session$userData$filter(filteredDf)
      }
      , error = function(e) {
        return(NULL)
      })
    })
    
    globalToBeAddInReportList <- reactive({ 
      session$userData$globalToBeAddInReportList()
    })
    
    output$downloadHTMLReport <- downloadHandler(
      filename = function() {
        paste('Demo-Dashboard-Report-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".html", sep = '')
      },
      content = function(file) {
        
        #creating temporary directory
        temporary_directory <- tempdir()
        
        tempLogo <- file.path(temporary_directory, "logotype-red.png")
        
        file.copy("app/static/img/logotype-red.png", tempLogo, overwrite = TRUE)
        
        if(!is.null(input$mainSelectedGenerateReportContaintList)){
          mainSelectedGenerateReportContaintList <- data.frame(input$mainSelectedGenerateReportContaintList)
          names(mainSelectedGenerateReportContaintList) <- c("plotName")
          
          filtersAndcContentToBeInReport <- dplyr::inner_join(data.frame(session$userData$contentToBeAddedInReportDf), mainSelectedGenerateReportContaintList)
          
          # outputPDFList <- downloadReportHandler(overviewdf, filtersAndcContentToBeInReport, lastSyncDate())
          
          filters <- filtersAndcContentToBeInReport
          filters$plotName <- sapply(strsplit(split = ':', filters$plotName),function(i){i[1]})
          
          if(!is.null(filters)){
            
            shiny::withProgress(
              message = paste0("Downloading File"),
              value = 0,
              {
                shiny::incProgress(1/10)
                Sys.sleep(1)
                shiny::incProgress(5/10)
                
                file_path <- generateRMD(overviewdf, filters, "html", lastSyncDate())
                
                input_params <- list(
                  data = overviewdf, # data
                  filter = filters,
                  logo_path = tempLogo,
                  current_time = Sys.Date(),
                  data_last_sync_date = lastSyncDate(),
                  common_utils = file_path[[2]])
                
                out <- rmarkdown::render(input = file_path[[1]], output_format= "html_document", params = input_params, output_file = file)
                
                file.rename(out, file)
                
                # unlink(outputPDFList, recursive=TRUE)
              }
            )
          }
        }
      }
    )
    
    output$downloadPDFReport <- downloadHandler(
      filename = function() {
        paste('Demo-Dashboard-Report-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".pdf", sep = '')
      },
      content = function(file) {
        
        #creating temporary directory
        temporary_directory <- tempdir()
        
        markdown_file_name <- "generate_report.rmd"
        
        tempReport <- file.path(temporary_directory, markdown_file_name)
        
        #copying the markdown and reference file to the temporary directory so that markdown will be able to access the reference fi$
        file.copy(markdown_file_name, tempReport, overwrite = TRUE)
        
        tempLogo <- file.path(temporary_directory, "logotype-red.png")
        
        file.copy("app/static/img/logotype-red.png", tempLogo, overwrite = TRUE)
        
        if(!is.null(input$mainSelectedGenerateReportContaintList)){
          mainSelectedGenerateReportContaintList <- data.frame(input$mainSelectedGenerateReportContaintList)
          names(mainSelectedGenerateReportContaintList) <- c("plotName")
          
          filtersAndcContentToBeInReport <- dplyr::inner_join(data.frame(session$userData$contentToBeAddedInReportDf), mainSelectedGenerateReportContaintList)
          
          outputPDFList <- downloadReportHandler(overviewdf, filtersAndcContentToBeInReport, lastSyncDate())
          
          filters <- filtersAndcContentToBeInReport
          filters$plotName <- sapply(strsplit(split = ':', filters$plotName),function(i){i[1]})
          
          if(!is.null(outputPDFList[[1]])){
            
            input_params <- list(
              data = outputPDFList[[1]],
              logo_path = tempLogo,
              current_time = Sys.Date(),
              data_last_sync_date = lastSyncDate())
            
            shiny::withProgress(
              message = paste0("Downloading File"),
              value = 0,
              {
                shiny::incProgress(1/10)
                Sys.sleep(1)
                shiny::incProgress(5/10)
                
                out <- rmarkdown::render(input = tempReport, output_format= "pdf_document", params = input_params, output_file = file)
                
                file.rename(out, file)
                
                # unlink(outputPDFList, recursive=TRUE)
              }
            )
          }
        }
      }
    )
    
    output$downloadRMDReport <- downloadHandler(
      filename = function() {
        paste('Demo-Dashboard-Report-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".zip", sep = '')
      },
      content = function(file) {
        
        if(!is.null(input$mainSelectedGenerateReportContaintList)){
          mainSelectedGenerateReportContaintList <- data.frame(input$mainSelectedGenerateReportContaintList)
          names(mainSelectedGenerateReportContaintList) <- c("plotName")
          
          filters <- dplyr::inner_join(data.frame(session$userData$contentToBeAddedInReportDf), mainSelectedGenerateReportContaintList)
          
          filters$plotName <- sapply(strsplit(split = ':', filters$plotName),function(i){i[1]})
          
          out <- generateRMD(overviewdf, filters, "zip", lastSyncDate())
          file.rename(out, file)
        }
      }
    )
    
    observeEvent(input$clearDownloadReportList, {
      shinyjs::addClass(selector = ".selected-generate-report-containt-list", class = "display-none")
      session$userData$globalToBeAddInReportList(NULL)
      updateSelectizeInput(
        session, "mainSelectedGenerateReportContaintList", 
        choices = character(0),
        selected = character(0)
      )
    })
    
    observe({
      if(length(c(globalToBeAddInReportList())) > 0){
        shinyjs::removeClass(selector = ".selected-generate-report-containt-list", class = "display-none")
        
        updateSelectizeInput(session, "mainSelectedGenerateReportContaintList",
                             choices = c(globalToBeAddInReportList()),
                             selected = c(globalToBeAddInReportList())
        )
      }
    })
    
    observeEvent(input$clearDownloadReportList, {
      shinyjs::addClass(selector = ".selected-generate-report-containt-list", class = "display-none")
    })
    
    overview$server("overview")   
    demo_analysis_barplot$server("demo_analysis_barplot")
    demo_analysis_diagnosis$server("demo_analysis_diagnosis")
    demo_analysis_metrics$server("demo_analysis_metrics")
    demo_analysis_patient_characteristics$server("demo_analysis_patient_characteristics")
  })
}