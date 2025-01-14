#* @Path app/view/demo_analysis_diagnosis.R

#************************ START: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#
box::use(
  dplyr[`%>%`, group_by, do, vars],
  shinycssloaders[withSpinner],
  shinydashboardPlus[accordion, accordionItem],
  plotly[plotlyOutput, renderPlotly, plot_ly, subplot, layout, add_trace],
  shiny[moduleServer, NS, fluidPage, fluidRow, column, uiOutput, renderUI, tags, HTML, reactive, renderPrint, textOutput, observe, actionButton, observeEvent, reactiveVal, radioButtons, reactiveValues, icon, selectizeInput, downloadButton, downloadHandler],
  DT[datatable]
)

box::use(
  ../logic/utils/common_utils[spinnerColor, getConsentDiagnosisDfForChart, ICD03CodesForIndicationsSunburst, generateReportFiltersManagement, downloadCSV],
  ../logic/utils/db_common_utils[unique_anatomic_code_name, data_sync_date_for_plots]
)
#************************ END: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#


#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$div( class="main-div",
              # fluidRow(class="section-row",
              #          column(1, class="empty-column"),
              #          column(11, class="section",
              #                 fluidRow(
              #                   column(11, class = "section-title",
              #                          tags$text('Recent Diagnosis')
              #                   ),
              #                   column(1, class = "section-title",
              #                          tags$div(class = "add-to-report-button-container float-right",
              #                                   tags$div(style="display:inline-block", title="Add To Report",
              #                                            actionButton(ns("consentDiagnosisIndicationsSunburstPlotGenerateReport"), "", class = "add-to-report-button",  icon = icon(lib="glyphicon", 'copy'))
              #                                   )
              #                          )
              #                   )
              #                 ),
              #                 radioButtons(ns("consentDiagnosisIndicationsSunburstRadioFilter"), "",
              #                              inline = TRUE,
              #                              selected = "icd03_codes",
              #                              c("ICD03 Code" = "icd03_codes", 
              #                                "Human Readable" = "human_readables")),
              #                 textOutput(ns("consentDiagnosisWarningMessageIndicationSunburst")),
              #                 plotlyOutput(ns("consentDiagnosisIndicationsSunburstPlot")) %>% withSpinner(color = spinnerColor),
              #                 accordion(
              #                   id = "accordiondemo_analysisDiagnosisIndicationsSunburstDownloadData",
              #                   accordionItem(
              #                     title = list("Underlying data", HTML("&nbsp;") ,icon(lib="glyphicon", 'menu-down')),
              #                     status = "danger",
              #                     collapsed = TRUE,
              #                     fluidRow(
              #                       column(11, class = "section-title"),
              #                       column(1, class = "section-title",
              #                              tags$div(class = "add-to-report-button-container float-right",
              #                                       tags$div(style="display:inline-block", title="Download CSV",
              #                                                downloadButton(ns("demo_analysisDiagnosisIndicationsSunburstDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
              #                                       )
              #                              )
              #                       )
              #                     ),
              #                     uiOutput(ns("demo_analysisDiagnosisIndicationsSunburstDatatable")) %>% withSpinner(color = spinnerColor),
              #                   )
              #                 )
              #          )
              # ),
              fluidRow(class="section-row",
                       column(1, class="empty-column"),
                       column(11, class="section",
                              fluidRow(
                                column(11, class = "section-title",
                                       tags$text('Recent Diagnosis')
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("consentDiagnosisIndicationsDiagnosisBarchartGenerateReport"), "", class = "add-to-report-button",  icon = icon(lib="glyphicon", 'copy'))
                                                )
                                       )
                                )
                              ),
                              plotlyOutput(ns("consentDiagnosisIndicationsDiagnosisBarchart")) %>% withSpinner(color = spinnerColor),
                              textOutput(ns("consentDiagnosisWarningMessageIndicationsDiagnosisBarchart")),
                              accordion(
                                id = "accordiondemo_analysisDiagnosisIndicationsDiagnosisBarcharttDownloadData",
                                accordionItem(
                                  title = list("Underlying data", HTML("&nbsp;") ,icon(lib="glyphicon", 'menu-down')),
                                  status = "danger",
                                  collapsed = TRUE,
                                  fluidRow(
                                    column(11, class = "section-title"),
                                    column(1, class = "section-title",
                                           tags$div(class = "add-to-report-button-container float-right",
                                                    tags$div(style="display:inline-block", title="Download CSV",
                                                             downloadButton(ns("demo_analysisDiagnosisIndicationsDiagnosisBarcharttDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                    )
                                           )
                                    )
                                  ),
                                  uiOutput(ns("demo_analysisDiagnosisIndicationsDiagnosisBarchartDatatable")) %>% withSpinner(color = spinnerColor),
                                )
                              )
                              
                       )
              ),
              fluidRow(class="section-row",
                       column(1, class="empty-column"),
                       column(11, class="section",
                              fluidRow(
                                column(12, class = "section-title",
                                       tags$text('RPAP')
                                )
                              ),
                              tags$div(class = "diagnosis-restricting-to-pathology-abstracted-patients-section",
                                       tags$div(
                                         fluidRow(
                                           column(11, class = "section-title",
                                                  tags$h4("CANCER DIAGNOSIS")
                                           ),
                                           column(1, class = "section-title",
                                                  tags$div(class = "add-to-report-button-container float-right",
                                                           tags$div(style="display:inline-block", title="Add To Report",
                                                                    actionButton(ns("consentDiagnosisDiagnosisPlotGenerateReport"), "", class = "add-to-report-button",  icon = icon(lib="glyphicon", 'copy'))
                                                           )
                                                  )
                                           )
                                         ),
                                         fluidRow(
                                           column(12,
                                                  plotlyOutput(ns("consentDiagnosisDiagnosis")) %>% withSpinner(color = spinnerColor),
                                                  textOutput(ns("consentDiagnosisWarningMessageDiagnosisPlot")),
                                                  accordion(
                                                    id = "accordiondemo_analysisPrimaryCancerDiagnosisDownloadData",
                                                    accordionItem(
                                                      title = list("Underlying data", HTML("&nbsp;") ,icon(lib="glyphicon", 'menu-down')),
                                                      status = "danger",
                                                      collapsed = TRUE,
                                                      fluidRow(
                                                        column(11, class = "section-title"),
                                                        column(1, class = "section-title",
                                                               tags$div(class = "add-to-report-button-container float-right",
                                                                        tags$div(style="display:inline-block", title="Download CSV",
                                                                                 downloadButton(ns("demo_analysisPrimaryCancerDiagnosisDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                                        )
                                                               )
                                                        )
                                                      ),
                                                      uiOutput(ns("demo_analysisPrimaryCancerDiagnosisDatatable")) %>% withSpinner(color = spinnerColor),
                                                    )
                                                  )
                                           )
                                         )
                                       ),
                                       tags$div( class = "border-bottom-none margin-bottom-0px padding-bottom-0px",
                                                 fluidRow(
                                                   column(4,
                                                          selectizeInput(ns("demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection"), 
                                                                         "Anatomic Code:",
                                                                         c(unique_anatomic_code_name),
                                                                         multiple = TRUE,
                                                                         selected = c("Colon", "Breast", "Skin"),
                                                                         options = list(plugins= list('remove_button')))
                                                   )
                                                 )
                                       ),
                                       tags$div(
                                         fluidRow(
                                           column(11, class = "section-title",
                                                  tags$h4("M Status")
                                           ),
                                           column(1, class = "section-title",
                                                  tags$div(class = "add-to-report-button-container float-right",
                                                           tags$div(style="display:inline-block", title="Add To Report",
                                                                    actionButton(ns("consentDiagnosisStagePlotGenerateReport"), "", class = "add-to-report-button",  icon = icon(lib="glyphicon", 'copy'))
                                                           )
                                                  )
                                           )
                                         ),
                                         fluidRow(
                                           column(12,
                                                  plotlyOutput(ns("consentDiagnosisStage")) %>% withSpinner(color = spinnerColor),
                                                  textOutput(ns("consentDiagnosisWarningMessageDiagnosisStagePlot")),
                                                  accordion(
                                                    id = "accordiondemo_analysisMetastaticDistributionDatatable",
                                                    accordionItem(
                                                      title = list("Underlying data", HTML("&nbsp;") ,icon(lib="glyphicon", 'menu-down')),
                                                      status = "danger",
                                                      collapsed = TRUE,
                                                      fluidRow(
                                                        column(11, class = "section-title"),
                                                        column(1, class = "section-title",
                                                               tags$div(class = "add-to-report-button-container float-right",
                                                                        tags$div(style="display:inline-block", title="Download CSV",
                                                                                 downloadButton(ns("demo_analysisMetastaticDistributionDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                                        )
                                                               )
                                                        )
                                                      ),
                                                      uiOutput(ns("demo_analysisMetastaticDistributionDatatable")) %>% withSpinner(color = spinnerColor),
                                                    )
                                                  )
                                           )
                                         )
                                       )
                              )
                       )
              )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    initial_data <- reactive({
      session$userData$filter()
    })
    
    indicationsSunburstDf <- reactive({
      tryCatch({
        ICD03CodesForIndicationsSunburst(initial_data())
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })
    
    observe({      
      if(length(initial_data()$pccipr) > 0){
        output$consentDiagnosisWarningMessageIndicationSunburst <- renderPrint(cat(""))
        
        output$consentDiagnosisIndicationsSunburstPlot <- renderPlotly({
          if(input$consentDiagnosisIndicationsSunburstRadioFilter == "icd03_codes"){
            if(is.null(indicationsSunburstDf()$labels) || is.null(indicationsSunburstDf()$parents) || is.null(indicationsSunburstDf()$values)){
              output$consentDiagnosisWarningMessageIndicationSunburst <- renderPrint(cat("No data available."))
            } else {
              plot_ly(
                ids = indicationsSunburstDf()$ids,
                labels = indicationsSunburstDf()$labels,
                parents = indicationsSunburstDf()$parents,
                values = indicationsSunburstDf()$values,
                type = 'sunburst',
                branchvalues = 'total',
                hovertemplate = paste('Total count of <b>',indicationsSunburstDf()$labels, '</b> is <b>', indicationsSunburstDf()$values, '</b> <extra></extra>'),
                colors = c("#872b29", "#294787", "#877629", "#6c8729", "#87293a")
              ) %>% layout(
                font = list(family = "Assistant"),
                title = list(text = data_sync_date_for_plots, y = 0, x = 0.5, xanchor = 'center', yanchor =  'bottom', font = list(size = 13))
              )
            }
          } else if(input$consentDiagnosisIndicationsSunburstRadioFilter == "human_readables"){
            if(is.null(indicationsSunburstDf()$human_readable_labels) || is.null(indicationsSunburstDf()$human_readable_parents) || is.null(indicationsSunburstDf()$values)){
              output$consentDiagnosisWarningMessageIndicationSunburst <- renderPrint(cat("No data available."))
            } else {
              plot_ly(
                ids = indicationsSunburstDf()$human_readable_ids,
                labels = indicationsSunburstDf()$human_readable_labels,
                parents = indicationsSunburstDf()$human_readable_parents,
                values = indicationsSunburstDf()$values,
                type = 'sunburst',
                branchvalues = 'total',
                hovertemplate = paste('Total count of <b>',indicationsSunburstDf()$human_readable_labels, '</b> is <b>', indicationsSunburstDf()$values, '</b> <extra></extra>'),
                colors = c("#872b29", "#294787", "#877629", "#6c8729", "#87293a")
              ) %>% layout(
                font = list(family = "Assistant"),
                title = list(text = data_sync_date_for_plots, y = 0, x = 0.5, xanchor = 'center', yanchor =  'bottom', font = list(size = 13))
              )
            }
          }
        })
        
        output$demo_analysisDiagnosisIndicationsSunburstDatatable <- renderUI({
          datatable(indicationsSunburstDf(), class = "table table-striped table-responsive", 
                    colnames = c("Parents", "Labels", "Ids", "Human Readable Parents", "Human Readable Labels", "Human Readable Ids", "Values"),
                    options = list(pageLength = 10))
        })
      } else {
        output$consentDiagnosisWarningMessageIndicationSunburst <- renderPrint(cat("No data available."))
      }
    })
    
    output$demo_analysisDiagnosisIndicationsSunburstDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Indications Sunburst-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(indicationsSunburstDf())
        file.rename(out, file)
      }
    )
    
    consentDiagnosisCharts <- reactive({
      tryCatch({
        initial_dataa <<- initial_data()
        output <- getConsentDiagnosisDfForChart(initial_data())
        return(output)
      }, error=function(e) {
        return(NULL)
      })
    })
    
    consentDiagnosisIndicationsBarPlot <- reactive({
      tryCatch({
        output <- data.frame("anatomic_code_names" = initial_data()$anatomic_site)
        return(output)
      }, error=function(e) {
        return(NULL)
      })
    })
    
    observe({
      consentDiagnosisChartss <<- consentDiagnosisCharts()
      if(!is.null(consentDiagnosisCharts())){
        
        output$consentDiagnosisWarningMessageDiagnosisPlot <- renderPrint(cat(""))
        output$consentDiagnosisWarningMessageDiagnosisStagePlot <- renderPrint(cat(""))
        output$consentDiagnosisWarningMessageDiagnosisTreatmentPlot <- renderPrint(cat(""))
        output$consentDiagnosisWarningMessageTreatmentStatusPlot <- renderPrint(cat(""))
        output$consentDiagnosisWarningMessageIndicationsDiagnosisBarchart <- renderPrint(cat(""))
        
        output$consentDiagnosisIndicationsDiagnosisBarchart <- renderPlotly({
          plot_ly(consentDiagnosisIndicationsBarPlot(), x = ~anatomic_code_names, type = "histogram") %>% 
            layout(yaxis = list(title="Frequency"),
                   xaxis = list(title = paste("Anatomic Site", data_sync_date_for_plots, sep = "\n\n")),
                   font = list(family = "Assistant"))
        })
        
        output$demo_analysisDiagnosisIndicationsDiagnosisBarchartDatatable <- renderUI({
          datatable(initial_data()[c("patient_uuid", "anatomic_site")], class = "table table-striped table-responsive", 
                    colnames = c("Patient UUID", "Anatomic Site"),
                    options = list(pageLength = 10))
        })
        
        output$consentDiagnosisDiagnosis <- renderPlotly({
          plot_ly(consentDiagnosisCharts(),x=~anatomic_site,type="histogram") %>% 
            layout(yaxis = list(title="Frequency"),
                   xaxis = list(title = paste("CANCER DIAGNOSIS", data_sync_date_for_plots, sep = "\n\n")),
                   font = list(family = "Assistant"))
        })
        
        output$demo_analysisPrimaryCancerDiagnosisDatatable <- renderUI({
          datatable(consentDiagnosisCharts()[c("patient_uuid", "anatomic_site")], class = "table table-striped table-responsive", 
                    colnames = c("Patient UUID", "Anatomic Site", "Abstraction Pathology"),
                    options = list(pageLength = 10))
        })
        
      } else {
        output$consentDiagnosisWarningMessageDiagnosisPlot <- renderPrint(cat("No data available."))
        output$consentDiagnosisWarningMessageDiagnosisStagePlot <- renderPrint(cat("No data available."))
        output$consentDiagnosisWarningMessageDiagnosisTreatmentPlot <- renderPrint(cat("No data available."))
        output$consentDiagnosisWarningMessageTreatmentStatusPlot <- renderPrint(cat("No data available."))
        output$consentDiagnosisWarningMessageIndicationsDiagnosisBarchart <- renderPrint(cat("No data available."))
      }
    })
    
    output$demo_analysisDiagnosisIndicationsDiagnosisBarcharttDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Recent Diagnosis - Anatomic Site Frequency-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(initial_data()[c("patient_uuid", "anatomic_site")])
        file.rename(out, file)
      }
    )
    
    output$demo_analysisPrimaryCancerDiagnosisDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Pathology abstracted patients - CANCER DIAGNOSIS-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(consentDiagnosisCharts()[c("patient_uuid", "anatomic_site")])
        file.rename(out, file)
      }
    )
    
    observeEvent(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection, {
      if(!is.null(consentDiagnosisCharts())){
        if(length(consentDiagnosisCharts()$anatomic_site[consentDiagnosisCharts()$anatomic_site %in% c("Colon", "Breast", "NSCLC")]) > 0){
          if(!is.null(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection)){
            
            output$consentDiagnosisWarningMessageDiagnosisStagePlot <- renderPrint(cat(""))
            output$consentDiagnosisWarningMessageDiagnosisTreatmentPlot <- renderPrint(cat(""))
            output$consentDiagnosisWarningMessageTreatmentStatusPlot <- renderPrint(cat(""))  
            
            output$consentDiagnosisStage <- renderPlotly({
              sub <- consentDiagnosisCharts()[consentDiagnosisCharts()$anatomic_site %in% 
                                                c(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection),]
              
              sub %>% group_by(anatomic_site) %>%
                do(p=plot_ly(., x = ~mstatus,type = "histogram",
                             color = ~vars(anatomic_site),name = ~anatomic_site)) %>%
                subplot(nrows = 1, shareX = FALSE, shareY = TRUE) %>% 
                layout(xaxis = list(categoryorder = "trace",title="Metastatic Distribution"),
                       yaxis = list(title="Frequency"), 
                       font = list(family = "Assistant"),
                       title = list(text = data_sync_date_for_plots, y = 0, x = 0.5, xanchor = 'center', yanchor =  'bottom', font = list(size = 13)))
            })
            
            output$demo_analysisMetastaticDistributionDatatable <- renderUI({
              datatable(consentDiagnosisCharts()[c("patient_uuid", "m_status", "anatomic_site")][consentDiagnosisCharts()$anatomic_site %in% 
                                                   c(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection),], 
                        class = "table table-striped table-responsive", 
                        colnames = c("Patient UUID", "M Status", "Anatomic Site"),
                        options = list(pageLength = 10))
            })
          } else {
            output$consentDiagnosisWarningMessageDiagnosisStagePlot <- renderPrint(cat("Select a anatomic code to get a result."))
            output$consentDiagnosisWarningMessageDiagnosisTreatmentPlot <- renderPrint(cat("Select a anatomic code to get a result."))
            output$consentDiagnosisWarningMessageTreatmentStatusPlot <- renderPrint(cat("Select a anatomic code to get a result."))  
          }
        } else {
          output$consentDiagnosisWarningMessageDiagnosisStagePlot <- renderPrint(cat("No data available."))
          output$consentDiagnosisWarningMessageDiagnosisTreatmentPlot <- renderPrint(cat("No data available."))
          output$consentDiagnosisWarningMessageTreatmentStatusPlot <- renderPrint(cat("No data available."))
        }
      } else {
        output$consentDiagnosisWarningMessageDiagnosisStagePlot <- renderPrint(cat("No data available."))
        output$consentDiagnosisWarningMessageDiagnosisTreatmentPlot <- renderPrint(cat("No data available."))
        output$consentDiagnosisWarningMessageTreatmentStatusPlot <- renderPrint(cat("No data available."))
      }
    })
    
    output$demo_analysisMetastaticDistributionDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Pathology abstracted patients - Metastatic Distribution-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(
          consentDiagnosisCharts()[c("patient_uuid", "m_status", "anatomic_site")][consentDiagnosisCharts()$anatomic_site %in% 
                                                                                              c(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection),]
          )
        file.rename(out, file)
      }
    )
    
    output$demo_analysisTreatmentStatusIntentDistributionDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Pathology abstracted patients - SACT Treatment Status Intent-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(
          consentDiagnosisCharts()[c("patient_uuid", "SACT_treatment", "anatomic_site")][
            consentDiagnosisCharts()$anatomic_site %in% c(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection),]
          )
          file.rename(out, file)
      }
    )
    
    output$demo_analysisTreatmentStatusDistributionDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Pathology abstracted patients - SACT Treatment Status-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(
          consentDiagnosisCharts()[c("patient_uuid", "SACT_treatment_status", "anatomic_site")][
            consentDiagnosisCharts()$anatomic_site %in% c(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection),]
          )
        file.rename(out, file)
      }
    )
    
    session$userData$globalToBeAddInReportList <- reactiveVal()
    
    count_1 <- reactiveValues(x = 1)
    count_2 <- reactiveValues(x = 1)
    count_3 <- reactiveValues(x = 1)
    count_4 <- reactiveValues(x = 1)
    count_5 <- reactiveValues(x = 1)
    count_6 <- reactiveValues(x = 1)
    
    observeEvent(input$consentDiagnosisIndicationsSunburstPlotGenerateReport, { 
      
      plot_name <- paste("consent_sunburst:", count_1$x, sep = "")
      count_1$x <- count_1$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilter <- input$consentDiagnosisIndicationsSunburstRadioFilter
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
    
    observeEvent(input$consentDiagnosisDiagnosisPlotGenerateReport, { 
      
      plot_name <- paste("consented_diagnosis_primary_cancer_diagnosis:", count_2$x, sep = "")
      count_2$x <- count_2$x + 1
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
    
    observeEvent(input$consentDiagnosisStagePlotGenerateReport, { 
      
      plot_name <- paste("consented_diagnosis_metastatic_distribution:", count_3$x, sep = "")
      count_3$x <- count_3$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection)
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
    
    observeEvent(input$consentDiagnosisTreatmentPlotGenerateReport, { 
      
      plot_name <- paste("consented_diagnosis_treatment_intent_status:", count_4$x, sep = "")
      count_4$x <- count_4$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection)
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
    
    observeEvent(input$consentDiagnosisTreatmentStatusPlotGenerateReport, { 
      
      plot_name <- paste("consented_diagnosis_treatment_status:", count_5$x, sep = "")
      count_5$x <- count_5$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString(input$demo_analysisDiagnosisRestrictingToPathologyAbstractedPatientsSection)
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
    
    observeEvent(input$consentDiagnosisIndicationsDiagnosisBarchartGenerateReport, { 
      
      plot_name <- paste("indications_diagnosis_most_recent_diagnosis:", count_6$x, sep = "")
      count_6$x <- count_6$x + 1
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
  })
}