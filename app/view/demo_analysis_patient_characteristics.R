#* @Path app/view/demo_analysis_patient_characteristics.R

#************************ START: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#
box::use(
  dplyr[`%>%`],
  tern[summarize_vars],
  shinycssloaders[withSpinner],
  rtables[basic_table, trim_levels_in_group, split_cols_by, split_rows_by, summarize_row_groups, build_table, as_html],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  shiny[moduleServer, NS, fluidPage, fluidRow, column, tags, HTML, reactive, renderPrint, textOutput, observe, div, uiOutput, renderUI, selectizeInput, actionButton, observeEvent, reactiveValues, reactiveVal, icon, downloadButton, downloadHandler]
)

box::use(
  ../logic/utils/common_utils[spinnerColor, consentedRtable, generateReportFiltersManagement, downloadCSV]
)
#************************ END: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#


#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$div( class="main-div",
              fluidRow(class="section-row",
                       column(1, class="empty-column"),
                       column(11, class="section",
                              fluidRow(
                                column(11, class = "section-title",
                                       tags$text('Patient Characteristics')
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(title="Download CSV",
                                                         downloadButton(ns("demo_analysisPatientCharacteristicsRtableDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                ),
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("consentPatientCharacteristicsRtablesGenerateReport"), "", class = "add-to-report-button",  icon = icon(lib="glyphicon", 'copy'))
                                                )
                                       )
                                  )
                                ),
                                fluidRow(
                                  column(6, class = "consent-patient-characteristics-rtables",
                                         selectizeInput(ns("consentPatientCharacteristicsRtablesRow"), "Rows:",
                                                        c("Consent status" = "consent_status",
                                                          "Tess site name" = "site_name",
                                                          "Sex" = "sex",
                                                          "Race" = "race",
                                                          "ICD03 Anatomic Site" = "icd03_anatomic_code_name",
                                                          "ICD03 Morphology Site" = "icd03_morphology_code_name",
                                                          "ICD03 Anatomic Code" = "icd03_anatomic_code",
                                                          "ICD03 Morphology Code" = "icd03_morphology_code",
                                                          "M Status" = "m_status",
                                                          "Assigned MD" = "assigned_md"),
                                                        multiple = TRUE,
                                                        selected = "consent_status",
                                                        options = list(plugins= list('remove_button')))
                                  ),
                                  column(6, class = "consent-patient-characteristics-rtables",
                                         selectizeInput(ns("consentPatientCharacteristicsRtablesColumn"), "Columns:",
                                                        c("Consent status" = "consent_status",
                                                          "Tess site name" = "site_name",
                                                          "Sex" = "sex",
                                                          "Race" = "race",
                                                          "ICD03 Anatomic Site" = "icd03_anatomic_code_name",
                                                          "ICD03 Morphology Site" = "icd03_morphology_code_name",
                                                          "ICD03 Anatomic Code" = "icd03_anatomic_code",
                                                          "ICD03 Morphology Code" = "icd03_morphology_code",
                                                          "M Status" = "m_status",
                                                          "Assigned MD" = "assigned_md"))
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                         uiOutput(ns("consentPatientCharacteristicsRtables")) %>% withSpinner(color = spinnerColor),
                                         textOutput(ns("consentPatientCharacteristicsWarningMessageRtables"))
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
    
    consentedRtableDf <- reactive({
      tryCatch({
        consentedRtable(initial_data())
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })
    
    observe({
      if(!is.null(consentedRtableDf())){
        
        session$userData$consentedRtableDf <- consentedRtableDf()
        
        output$consentPatientCharacteristicsWarningMessageRtables <- renderPrint(cat(""))
        
        output$consentPatientCharacteristicsRtables <- renderUI({
          
          vars <- c(input$consentPatientCharacteristicsRtablesRow)  
          if(!is.null(vars)) {
            output$consentPatientCharacteristicsWarningMessageRtables <- renderPrint(cat(""))
            vars_col <- c(input$consentPatientCharacteristicsRtablesColumn)  
            
            lyt <- basic_table(show_colcounts = TRUE) |>
              split_cols_by(var = vars_col) |>
              summarize_vars(vars, var_labels = vars)
            
            rtable_view <- build_table(lyt, consentedRtableDf())
            
            htmltools::tags$div(
              class = "table-responsive",
              as_html(rtable_view, class_table = "table table-bordered")
            )
          } else {
            output$consentPatientCharacteristicsWarningMessageRtables <- renderPrint(cat("Kindly select atleast one Row to get the result."))
          }
        })
      } else {
        output$consentPatientCharacteristicsWarningMessageRtables <- renderPrint(cat("No data available."))
      }
    })
    
    output$demo_analysisPatientCharacteristicsRtableDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Patient Characteristics Rtable-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        
        vars <- c(input$consentPatientCharacteristicsRtablesRow)  
        vars_col <- c(input$consentPatientCharacteristicsRtablesColumn)  
        
        lyt <- basic_table(show_colcounts = TRUE) |>
          split_cols_by(var = vars_col) |>
          summarize_vars(vars, var_labels = vars)
        
        rtable_view <- build_table(lyt, consentedRtableDf())
        
        output_df <- rtables::path_enriched_df(rtable_view)
        
        out <- downloadCSV(output_df)
        file.rename(out, file)
      }
    )
    
    session$userData$globalToBeAddInReportList <- reactiveVal()
    
    count <- reactiveValues(x = 1)
    
    observeEvent(input$consentPatientCharacteristicsRtablesGenerateReport, { 
      
      plot_name <- paste("consent_patient_characteristics_rtables:", count$x, sep = "")
      count$x <- count$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString(input$consentPatientCharacteristicsRtablesRow)
      session$userData$contentToBeAddedInReport$plotSpecificFilterColumns <- toString(input$consentPatientCharacteristicsRtablesColumn)
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
  })
}