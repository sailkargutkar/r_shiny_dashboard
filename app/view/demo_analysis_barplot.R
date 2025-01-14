#************************ START: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#
box::use(
  dplyr[`%>%`, group_by, summarise, n, sym],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  shinydashboard[tabBox],
  shinycssloaders[withSpinner],
  shinydashboardPlus[accordion, accordionItem],
  shiny[moduleServer, NS, fluidPage, fluidRow, column, tags, HTML, reactive, observe, tabsetPanel, tabPanel, tableOutput, renderTable, actionButton, observeEvent, reactiveVal, reactiveValues, icon, selectizeInput, textOutput, renderPrint, renderUI, uiOutput, downloadButton, downloadHandler, req],
  DT[datatable],
  echarts4r[echarts4rOutput, renderEcharts4r, e_charts, e_bar, e_title, e_tooltip, e_x_axis, e_y_axis],
  shinyjs[useShinyjs, runjs],
)

box::use(
  ../logic/utils/common_utils[spinnerColor, generateReportFiltersManagement, downloadCSV, summarise_count],
  ../logic/utils/db_common_utils[data_sync_date_for_plots]
)
#************************ END: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#


#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tags$div( class="main-div",
              fluidRow(class="section-row",
                       
                       column(1, class="empty-column"),
                       column(11, class="section",
                              fluidRow(
                                column(10, class="section-title",
                                       tags$text('Distribution') 
                                )
                              ),
                              fluidRow(
                                column(4,
                                       selectizeInput(ns("columnName"), "Field:",
                                                      c("Consent status" = "consent_status",
                                                        "Tess Site name" = "site_name",
                                                        "Sex" = "sex",
                                                        "Race" = "race",
                                                        "ICD03 Anatomic Site" = "anatomic_site",
                                                        "ICD03 Morphology Site" = "icdo3_description",
                                                        "M Status" = "m_status",
                                                        "Assigned MD" = "assigned_md"),
                                                      selected = "consent_status")  
                                ),
                                column(4,
                                       selectizeInput(ns("demoAnalysisBarplotPackageType"), "Package:",
                                                      c("echarts4r" = "echarts4r",
                                                        "Plotly" = "plotly"),
                                                      selected = "echarts4r")  
                                )
                              ),
                              textOutput(ns("overviewWarningMessageDistributionPlotUnderdemo_analysis")),
                              tabsetPanel(
                                tabBox(title = "",side = "right", width = "100%",
                                       tabPanel("Distribution",
                                                tags$div(class = "add-to-report-button-container", style="display:inline-block", title="Add To Report",
                                                         tags$div(class = "blankSection"),
                                                         actionButton(ns("distributionPlotGenerateReport"), "", class = "add-to-report-button",  icon = icon(lib="glyphicon", 'copy'), disabled = TRUE)
                                                ),
                                                uiOutput(ns("distributionPlotUnderdemo_analysis")) %>% withSpinner(color = spinnerColor)
                                                # echarts4rOutput(ns("distributionPlotUnderdemo_analysisEchart4r")) %>% withSpinner(color = spinnerColor)
                                       ),
                                       tabPanel("Numbers",
                                                tags$div(class = "add-to-report-button-container", style="display:inline-block",
                                                         tags$div(class = "blankSection"),
                                                         tags$div(title="Download CSV",
                                                                  downloadButton(ns("distributionCountDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                         ),
                                                         tags$div(title="Add To Report",
                                                                  actionButton(ns("distributionCountGenerateReport"), "", class = "add-to-report-button",  icon = icon(lib="glyphicon", 'copy')))
                                                ),
                                                tableOutput(ns("distributionCount")) %>% withSpinner(color = spinnerColor)
                                       )
                                )
                              ),
                              accordion(
                                id = "accordionDistributionPlotDownloadData",
                                accordionItem(
                                  title = list("Underlying data", HTML("&nbsp;") ,icon(lib="glyphicon", 'menu-down')),
                                  status = "danger",
                                  collapsed = TRUE,
                                  fluidRow(
                                    column(11, class = "section-title"),
                                    column(1, class = "section-title",
                                           tags$div(class = "add-to-report-button-container float-right",
                                                    tags$div(style="display:inline-block", title="Download CSV",
                                                             downloadButton(ns("distributionPlotDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                    )
                                           )
                                    )
                                  ),
                                  uiOutput(ns("distributionPlotDatatable")) %>% withSpinner(color = spinnerColor),
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
    
    observe({
      
      data <- initial_data()
      
      if(input$demoAnalysisBarplotPackageType == 'echarts4r'){
        if(length(data[[input$columnName]]) > 0){

          runjs('document.getElementById("app-demo_analysis_barplot-distributionPlotGenerateReport").setAttribute("disabled", "TRUE");')
          data <- initial_data()[!(initial_data()[[input$columnName]] %in% "No Data Available"), ]

          output$overviewWarningMessageDistributionPlotUnderdemo_analysis <- renderPrint(cat(""))
          
          output$distributionPlotUnderdemo_analysis <- renderUI({ 
          # output$distributionPlotUnderdemo_analysisEchart4r <- renderEcharts4r({

            req(input$columnName)  # Ensure input$columnName is available

            colName <- as.character(input$columnName)
            
            # Group by the specified column and summarize
            # summary_data <- data %>%
            #   group_by(!!sym(colName)) %>%
            #   summarise(count = n(), .groups = 'drop')

            summary_data <- summarise_count(data, !!sym(colName))

            colnames(summary_data) <- c("col_name", "count")
            
            summary_data %>%
              e_charts(col_name) %>%
              e_bar(count) %>%
              e_tooltip(trigger = "axis") # Add a tooltip
          })

        } else {
          output$overviewWarningMessageDistributionPlotUnderdemo_analysis <- renderPrint(cat("No data available."))
        }
      } else {
        output$distributionPlotUnderdemo_analysis <- renderUI({ 
        # output$distributionPlot <- renderPlotly({
          if(!is.null(session$userData$contentToBeAddedInReport$filterIncludeExcludeNAIndications)){
            if(session$userData$contentToBeAddedInReport$filterIncludeExcludeNAIndications == TRUE){
              data <- initial_data()[!(initial_data()[[input$columnName]] %in% "No Data Available"), ]
            } 
          }
          
          if(length(data[[input$columnName]]) > 0){
            runjs('document.getElementById("app-demo_analysis_barplot-distributionPlotGenerateReport").removeAttribute("disabled");')
            output$demo_analysisOverviewPlotWarningMessage <- renderPrint(cat(""))
            fig <- plot_ly(y = ~table(data[[input$columnName]]), 
                          x=~c(names(table(data[[input$columnName]]))),
                          type = "bar") %>%
              layout(title = list(text = data_sync_date_for_plots, y = 0, x = 0.5, 
                                  xanchor = 'center', yanchor =  'bottom', font = list(size = 13)),
                    yaxis = list(title = 'Frequency'), 
                    xaxis = list(title = ''),
                    font = list(family = "Assistant"))
            fig
          } else {
            output$demo_analysisOverviewPlotWarningMessage <- renderPrint(cat("No data available."))
          }
        })
      }
      
      output$distributionCount <- renderTable({
        if(!is.null(session$userData$contentToBeAddedInReport$filterIncludeExcludeNAIndications)){
          if(session$userData$contentToBeAddedInReport$filterIncludeExcludeNAIndications == TRUE){
            data <- initial_data()[!(initial_data()[[input$columnName]] %in% "No Data Available"), ]
          } 
        }
        
        if(length(data[[input$columnName]]) > 0){
          x = t(table(data[[input$columnName]]))
          cbind(names = rownames(x), 'Years' = round(x, digits=2))
        }
      })
      
      output$distributionPlotDatatable <- renderUI({ 
        datatable(initial_data()[c("patient_uuid", input$columnName)], class = "table table-striped table-responsive", 
                  colnames = c("Patient UUID", "Consent Status"),
                  options = list(pageLength = 10))
      })
    })
    
    output$distributionPlotDownloadData <- downloadHandler(
      filename = function() {
        paste(input$columnName, ' Distribution Data-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(initial_data()[c("consent_status", "site_name", "sex", "race", "anatomic_site", "icdo3_description", "m_status", "assigned_md")])
        file.rename(out, file)
      }
    )
    
    output$distributionCountDownloadData <- downloadHandler(
      filename = function() {
        paste(input$columnName, ' Distribution-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        data <- initial_data()[!(initial_data()[[input$columnName]] %in% "No Data Available"), ]
        x = t(table(data[[input$columnName]]))
        
        output_df = subset(data.frame(x), select = -c(Var1))
        
        out <- downloadCSV(output_df)
        file.rename(out, file)
      }
    )
    
    session$userData$globalToBeAddInReportList <- reactiveVal()
    
    count_1 <- reactiveValues(x = 1)
    count_2 <- reactiveValues(x = 1)
    
    observeEvent(input$distributionPlotGenerateReport, { 
      
      plot_name <- paste("distribution_plot:", count_1$x, sep = "")
      count_1$x <- count_1$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilterSingleColumn <- toString(input$columnName)
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
    
    observeEvent(input$distributionCountGenerateReport, { 
      
      plot_name <- paste("distribution_count:", count_2$x, sep = "")
      count_2$x <- count_2$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilterSingleColumn <- toString(input$columnName)
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
  })
}