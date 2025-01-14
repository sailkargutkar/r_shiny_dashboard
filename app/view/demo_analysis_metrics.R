#* @Path app/view/demo_analysis_metrics.R

#************************ START: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#
box::use(
  dplyr[`%>%`, group_by, do, vars, arrange, desc],
  shinycssloaders[withSpinner],
  shinydashboardPlus[accordion, accordionItem],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_text, layout],
  shiny[moduleServer, NS, fluidPage, fluidRow, column, tags, HTML, reactive, renderPrint, renderTable, textOutput, tableOutput, observe, dateRangeInput, checkboxGroupInput, renderText, reactiveVal, div, updateDateRangeInput, observeEvent, actionButton, downloadHandler, downloadButton, selectInput, selectizeInput, updateSelectizeInput, reactiveValues, icon, uiOutput, renderUI, isolate],
  DT[datatable]
)

box::use(
  ../logic/utils/common_utils[spinnerColor, getMetricsDfForCharts, generateReportFiltersManagement, getDefaultMetricsDfForChart, downloadCSV],
  ../logic/utils/db_common_utils[uniquePcpNames, data_sync_date_for_plots, unique_consent_status]
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
                                       tags$text("Consent Summary")
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("consentMetricsPlotGenerateReport"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'copy'))
                                                )
                                       )
                                )
                              ),
                              fluidRow(
                                column(4,
                                       checkboxGroupInput(ns("consentMetricsPlotRadioFilter"), "",
                                                          c("Cumulative" = "Cumulative", 
                                                            "Weekly" = "Weekly"),
                                                          selected = c("Cumulative", "Weekly"),
                                                          inline = TRUE)
                                ),
                                column(8, class = "padding-none float-right",
                                       fluidRow(
                                         column(3),
                                         column(4, class = "padding-top-10px font-size-14px text-align-last-right",
                                                tags$span("Showing data between date range")
                                         ),
                                         column(4, class = "padding-none",
                                                dateRangeInput(ns("consentMetricsDateRangeFilter"), "",
                                                               start = "2022-08-01",
                                                               end   = as.Date(Sys.Date() + 7))
                                         )
                                       )
                                )
                              ),
                              fluidRow(
                                column(12, class = "consent-metrics-main-filter-section",
                                       column(3,
                                              selectInput(ns("consentMetricsCumulativeWeeklyFilterColumns"), "Fields:",
                                                          c("Sex"= "sex",
                                                            "Race"= "race",
                                                            "Tess site Name" = "site_name",
                                                            "Assigned MD" = "assigned_md",
                                                            "Consent Status" = "consent_status",
                                                            "All" = "All"), selected = "consent_status")
                                       ),
                                       column(3,
                                              selectizeInput(ns("consentMetricsCumulativeWeeklyFilterCategoricalFields"), "Values:",
                                                             c("Consented" = "Agreed",
                                                               "Quiet" = "Quiet",
                                                               "Disagreed" = "Disagreed"), 
                                                             multiple = TRUE,
                                                             options = list(plugins= list('remove_button')),
                                                             selected = c("Agreed", "Quiet", "Disagreed"))
                                       )
                                )
                              ),
                              fluidRow(
                                column(12, class="section",
                                       plotlyOutput(ns("consentMetricsPlot")) %>% withSpinner(color = spinnerColor),
                                       textOutput(ns("consentMetricsWarningMessageConsentMetricsPlot")),
                                       accordion(
                                         id = "accordionMetricsSummaryDownloadData",
                                         accordionItem(
                                           title = list("Underlying data", HTML("&nbsp;") ,icon(lib="glyphicon", 'menu-down')),
                                           status = "danger",
                                           collapsed = TRUE,
                                           fluidRow(
                                             column(11, class = "section-title"),
                                             column(1, class = "section-title",
                                                    tags$div(class = "add-to-report-button-container float-right",
                                                             tags$div(style="display:inline-block", title="Download CSV",
                                                                      downloadButton(ns("metricsSummaryDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                             )
                                                    )
                                             )
                                           ),
                                           uiOutput(ns("metricsSummaryDatatable")) %>% withSpinner(color = spinnerColor),
                                         )
                                       )
                                )
                              )
                       )         
              ),
              fluidRow(class="section-row",
                       column(1, class="empty-column"),
                       column(11, class="section",
                              fluidRow(
                                column(11, class = "section-title",
                                       tags$text('Indications')
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(style="display:inline-block", title="Download CSV",
                                                         downloadButton(ns("demo_analysisMetricsTableDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                ),
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("consentMetricsConsentWeeksTableGenerateReport"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'copy'))
                                                )
                                       )
                                )
                              ),
                              tableOutput(ns("consentMetricsConsentWeeksTable")) %>% withSpinner(color = spinnerColor),
                              textOutput(ns("consentMetricsWarningMessageConsentWeeksTable"))
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
    
    observeEvent(input$consentMetricsCumulativeWeeklyFilterColumns, {
      selected <- "All"
      if(input$consentMetricsCumulativeWeeklyFilterColumns == "sex"){
        choices <- c("Male", "Female", "No Data Available", "All")
      } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "race"){
        choices <- c("Other",
                     "White",
                     "American Indian or Alaska Native",
                     "Not Reported",
                     "Native Hawaiian or Other Pacific Islander",
                     "Asian",
                     "Black or African American",
                     "No Data Available",
                     "All")
      } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "site_name"){
        choices <- c("Site_1",
                     "Site_2",
                     "Site_3",
                     "No Data Available",
                     "All")
      } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "assigned_md"){
        choices <- c(uniquePcpNames, "No Data Available", "All")
      } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "All"){
        choices <- NULL
        selected <- NULL
      } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "consent_status"){
        choices <- c("Consented" = "Agreed",
                     "Quiet" = "Quiet",
                     "Disagreed" = "Disagreed")
        selected <- c("Agreed", "Quiet", "Disagreed")
      }
      updateSelectizeInput(session, "consentMetricsCumulativeWeeklyFilterCategoricalFields", choices = choices, selected = selected, server = TRUE)  
    })
    
    getMetricsChartDf <- reactive({
      tryCatch({
        
        if(!("All" %in% c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields))) {
          
          getMetricsDfForCharts(initial_data(), input$consentMetricsCumulativeWeeklyFilterColumns, 
                                c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields))
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "sex" & "All" %in% c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          
          getMetricsDfForCharts(initial_data(), input$consentMetricsCumulativeWeeklyFilterColumns, 
                                c('Male', 'Female', "No Data Available"))
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "race" & "All" %in% c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          
          getMetricsDfForCharts(initial_data(), input$consentMetricsCumulativeWeeklyFilterColumns, 
                                c("Other",
                                  "White",
                                  "American Indian or Alaska Native",
                                  "Not Reported",
                                  "Native Hawaiian or Other Pacific Islander",
                                  "Asian",
                                  "Black or African American",
                                  "No Data Available"))
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "site_name" & "All" %in% c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          
          getMetricsDfForCharts(initial_data(), input$consentMetricsCumulativeWeeklyFilterColumns, 
                                c("Site_1",
                                  "Site_2",
                                  "Site_3",
                                  "No Data Available"))
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "assigned_md" & "All" %in% c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          
          getMetricsDfForCharts(initial_data(), input$consentMetricsCumulativeWeeklyFilterColumns, 
                                c(uniquePcpNames, "No Data Available"))
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "consent_status"){
          
          getMetricsDfForCharts(initial_data(), input$consentMetricsCumulativeWeeklyFilterColumns, 
                                c(unique_consent_status))
          
        }
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })
    
    defaultMetricsDfForChart <- reactive({
      tryCatch({
        getDefaultMetricsDfForChart(initial_data())
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })
    
    plot_datatable <- reactiveValues(df_data = NULL)
    
    observe({
      
      tryCatch({
        if(!is.null(getMetricsChartDf())){
          output$consentMetricsWarningMessageConsentMetricsPlot <- renderPrint(cat(""))
          
          updateDateRangeInput(session, "consentMetricsDateRangeFilter",
                               label = "",
                               start = as.Date(paste0(input$consentMetricsDateRangeFilter[1])),
                               end = as.Date(paste0(input$consentMetricsDateRangeFilter[2]))
          )
          
          output$consentMetricsPlot <- renderPlotly({
            if(length(c(input$consentMetricsPlotRadioFilter)) == 2){
              
              if(input$consentMetricsCumulativeWeeklyFilterColumns == "All"){
                plot_data <- defaultMetricsDfForChart()[
                  defaultMetricsDfForChart()$Start >= input$consentMetricsDateRangeFilter[1] &
                    defaultMetricsDfForChart()$Start <= input$consentMetricsDateRangeFilter[2],]
                
                plot_data$type <- paste("Weekly", sep = "")
                
                cumsum_data <- data.frame("Year-Week" = plot_data$`Year-Week`,
                                          "Start" = plot_data$Start,
                                          "End" = plot_data$End,
                                          "Consent" = plot_data$Consent,
                                          "type" = paste("Cumsum", sep = ""))
                
                cumsum_data$Consent <- cumsum(plot_data$Consent)
                
                names(cumsum_data) <- c("Year-Week", "Start", "End", "Consent", "type")
                
                plot_data <- rbind(plot_data, cumsum_data)
                
                plot_datatable$df_data <- plot_data
                plot_ly(plot_data,
                        x = ~Start,
                        y = ~Consent,
                        color = ~type,
                        type = "scatter",
                        mode = "markers+lines",
                        text = ~Consent) %>%
                  add_text(textposition = "top") %>%
                  layout(yaxis = list(title = "Frequency",
                                      tick0 = 50, dtick = 50),
                         xaxis = list(title = paste("Time Weeks", data_sync_date_for_plots, sep = "\n\n")),
                         font = list(family = "Assistant"))
                
              } else {
                plot_data <- getMetricsChartDf()[
                  getMetricsChartDf()$Start >= input$consentMetricsDateRangeFilter[1] &
                    getMetricsChartDf()$Start <= input$consentMetricsDateRangeFilter[2],]
                
                temp_type <- plot_data
                
                plot_data$type <- paste("Weekly-", plot_data$type, sep = "")
                
                for(fields in unique(temp_type$type)){
                  
                  plot_data_tmp <- temp_type[temp_type$type %in% fields, ]
                  
                  cumsum_data <- data.frame("Year-Week" = plot_data_tmp$`Year-Week`,
                                            "Start" = plot_data_tmp$Start,
                                            "End" = plot_data_tmp$End,
                                            "Consent" = cumsum(plot_data_tmp$Consent),
                                            "type" = paste("Cumsum-", fields, sep = ""))
                  
                  names(cumsum_data) <- c("Year-Week", "Start", "End", "Consent", "type")
                  
                  plot_data <- rbind(plot_data, cumsum_data)
                }
                
                plot_datatable$df_data <- plot_data
                plotly::plot_ly(data = plot_data, x = ~Start, y = ~Consent,
                                color = ~type,
                                type = 'scatter', mode = 'markers+lines',
                                text = ~Consent) %>%
                  add_text(textposition = "top") %>%
                  layout(yaxis = list(title = "Frequency",
                                      tick0 = 50, dtick = 50),
                         xaxis = list(title = paste("Time Weeks", data_sync_date_for_plots, sep = "\n\n")),
                         font = list(family = "Assistant"))
              }
            }
            else if(c(input$consentMetricsPlotRadioFilter) %in% "Weekly" & length(c(input$consentMetricsPlotRadioFilter)) == 1){
              if(input$consentMetricsCumulativeWeeklyFilterColumns == "All"){
                plot_data <- defaultMetricsDfForChart()[
                  defaultMetricsDfForChart()$Start >= input$consentMetricsDateRangeFilter[1] &
                    defaultMetricsDfForChart()$Start <= input$consentMetricsDateRangeFilter[2],]
                
                plot_datatable$df_data <- plot_data
                plot_ly(plot_data,
                        x = ~Start,
                        y = ~Consent,
                        type = "scatter",
                        mode = "markers+lines",
                        text = ~Consent) %>%
                  add_text(textposition = "top") %>%
                  layout(yaxis = list(title = "Frequency"),
                         xaxis = list(title = paste("Time Weeks", data_sync_date_for_plots, sep = "\n\n")),
                         font = list(family = "Assistant"))
              } else {
                plot_data <- getMetricsChartDf()[
                  getMetricsChartDf()$Start >= input$consentMetricsDateRangeFilter[1] &
                    getMetricsChartDf()$Start <= input$consentMetricsDateRangeFilter[2],]
                
                plot_datatable$df_data <- plot_data
                plotly::plot_ly(data = plot_data, x = ~Start, y = ~Consent,
                                color = ~type,
                                type = 'scatter', mode = 'markers+lines',
                                text = ~Consent) %>%
                  add_text(textposition = "top") %>%
                  layout(yaxis = list(title = "Frequency",
                                      tick0 = 50, dtick = 50),
                         xaxis = list(title = paste("Time Weeks", data_sync_date_for_plots, sep = "\n\n")),
                         font = list(family = "Assistant"))
              }
              
            } 
            else if(c(input$consentMetricsPlotRadioFilter) %in% "Cumulative" & length(c(input$consentMetricsPlotRadioFilter)) == 1){
              
              if(input$consentMetricsCumulativeWeeklyFilterColumns == "All"){
                plot_data <- defaultMetricsDfForChart()[
                  defaultMetricsDfForChart()$Start >= input$consentMetricsDateRangeFilter[1] &
                    defaultMetricsDfForChart()$Start <= input$consentMetricsDateRangeFilter[2],]
                
                plot_datatable$df_data <- plot_data
                plot_ly(plot_data,
                        x = ~Start,
                        y = ~cumsum(Consent),
                        type = "scatter",
                        mode = "markers+lines",
                        text = ~cumsum(Consent)) %>%
                  add_text(textposition = "top") %>%
                  layout(yaxis = list(title = "Frequency",
                                      tick0 = 50, dtick = 50),
                         xaxis = list(title = paste("Time Weeks", data_sync_date_for_plots, sep = "\n\n")),
                         font = list(family = "Assistant"))
                
              } else {
                plot_data <- getMetricsChartDf()[
                  getMetricsChartDf()$Start >= input$consentMetricsDateRangeFilter[1] &
                    getMetricsChartDf()$Start <= input$consentMetricsDateRangeFilter[2],]
                
                plot_datatable$df_data <- plot_data
                
                plotly::plot_ly(data = plot_data, x = ~Start, y = ~cumsum,
                                color = ~type,
                                type = 'scatter', mode = 'markers+lines',
                                text = ~cumsum) %>%
                  add_text(textposition = "top") %>%
                  layout(yaxis = list(title = "Frequency",
                                      tick0 = 50, dtick = 50),
                         xaxis = list(title = paste("Time Weeks", data_sync_date_for_plots, sep = "\n\n")),
                         font = list(family = "Assistant"))
              }
            } 
          })
          
          output$metricsSummaryDatatable <- renderUI({
            datatable(plot_datatable$df_data, class = "table table-striped table-responsive", 
                      colnames = c("Year-Week", "Start", "End", "Consent", "Type", "Cumsum"),
                      options = list(pageLength = 10))
          })
        } else {
          output$consentMetricsWarningMessageConsentMetricsPlot <- renderPrint(cat("No data available."))
        }
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })
    
    output$metricsSummaryDownloadData <- downloadHandler(
      filename = function() {
        paste('demo_analysis Metrics Summary-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(plot_datatable$df_data)
        file.rename(out, file)
      }
    )
    
    maindfForMetricsTable <- reactive({
      tryCatch({
        getDefaultMetricsDfForChart(initial_data())
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })
    
    observe({
      if(!is.null(maindfForMetricsTable())){
        output$consentMetricsWarningMessageConsentWeeksTable <- renderPrint(cat(""))
        output$consentMetricsConsentWeeksTable <- renderTable({
          maindfForMetricsTable()
        },striped = TRUE, bordered = TRUE,  
        hover = TRUE, spacing = 'xs',  
        rownames = FALSE)
      } else {
        output$consentMetricsWarningMessageConsentWeeksTable <- renderPrint(cat("No data available."))
      }
    })
    
    session$userData$globalToBeAddInReportList <- reactiveVal()
    
    count_1 <- reactiveValues(x = 1)
    count_2 <- reactiveValues(x = 1)
    
    observeEvent(input$consentMetricsPlotGenerateReport, { 
      
      tryCatch({
        plot_name <- paste("consent_summary:", count_1$x, sep = "")
        count_1$x <- count_1$x + 1
        
        if(input$consentMetricsCumulativeWeeklyFilterColumns == "sex" & "All" %in% 
           c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString("Male, Female, No Data Available")
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "race" & "All" %in% 
                  c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString("Other,
                                                                                White,
                                                                                American Indian or Alaska Native,
                                                                                Not Reported,
                                                                                Native Hawaiian or Other Pacific Islander,
                                                                                Asian, Black or African American, No Data Available")
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "site_name" & "All" %in% 
                  c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString("Site_1,
                                                                                Site_2,
                                                                                Site_3,
                                                                                No Data Available")
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "assigned_md" & "All" %in% 
                  c(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)){
          session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString(paste(uniquePcpNames, "No Data Available", sep = ","))
          
        }  else if(input$consentMetricsCumulativeWeeklyFilterColumns == "consent_status"){
          session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString(unique_consent_status)
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns != "All") {
          session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- toString(input$consentMetricsCumulativeWeeklyFilterCategoricalFields)
          
        } else if(input$consentMetricsCumulativeWeeklyFilterColumns == "All") {
          session$userData$contentToBeAddedInReport$plotSpecificFilterRows <- NULL
        }
        
        session$userData$contentToBeAddedInReport$plotSpecificFilterSingleColumn <- toString(input$consentMetricsCumulativeWeeklyFilterColumns)
        session$userData$contentToBeAddedInReport$plotSpecificFilter <- toString(input$consentMetricsPlotRadioFilter)
        session$userData$contentToBeAddedInReport$filterDateFrom <- toString(input$consentMetricsDateRangeFilter[1])
        session$userData$contentToBeAddedInReport$filterDateTo <- toString(input$consentMetricsDateRangeFilter[2])
        
        content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
        
        session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
        
        list <- append(session$userData$globalToBeAddInReportList(), plot_name)
        session$userData$globalToBeAddInReportList(list)
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })
    
    observeEvent(input$consentMetricsConsentWeeksTableGenerateReport, { 
      
      plot_name <- paste("consent_metrics_consent_weeks_table:", count_2$x, sep = "")
      count_2$x <- count_2$x + 1
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
    
    output$demo_analysisMetricsTableDownloadData <- downloadHandler(
      filename = function() {
        paste('Metrics Table-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(maindfForMetricsTable())
        file.rename(out, file)
      }
    )
  })
}