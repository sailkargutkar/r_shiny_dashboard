#* @Path app/view/overview.R

#************************ START: LOADING REQUIRED PACKAGES, RESPECTIVE CLASSES AND MODULES **********************#
box::use(
  dplyr[`%>%`, group_by, summarise, n],
  plotly[plot_ly, add_trace, layout, plotlyOutput, renderPlotly],
  shinycssloaders[withSpinner],
  shinydashboardPlus[accordion, accordionItem],
  shiny[moduleServer, NS, fluidPage, fluidRow, column, tags, HTML, reactive, renderPrint, textOutput, observe, actionButton, observeEvent, reactiveVal, reactiveValues, radioButtons, icon, uiOutput, renderUI, downloadButton, downloadHandler],
  DT[datatable],
  echarts4r[echarts4rOutput, renderEcharts4r, e_charts, e_bar, e_title, e_tooltip, e_x_axis, e_y_axis]
)

box::use(
  ../logic/utils/common_utils[spinnerColor, ICD03CodesForIndicationsSunburst, generateReportFiltersManagement, downloadCSV],
  ../logic/utils/db_common_utils[data_sync_date_for_plots],
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
                                column(10, class="section-title",
                                       tags$text('Patient Enrollment') 
                                )
                              ),
                              fluidRow(
                                column(3, 
                                       tags$div(class = "small-card overview-block-1",
                                                fluidRow(
                                                  column(3, class="card-icon-body",
                                                         tags$div(class="card-icon",
                                                                  HTML('<i class="fa-solid fa-thumbs-up"></i>')
                                                         )
                                                  ),
                                                  column(8, class="card-description",
                                                         tags$div(class="card-body",
                                                                  tags$text(
                                                                    textOutput(ns("consentFullySigned")) %>% withSpinner(color = spinnerColor)
                                                                  )
                                                         ),
                                                         tags$text(class="card-title", textOutput(ns("consentFullySignedPercentage")), HTML('&nbsp;'), "AGREED")
                                                  )
                                                )
                                       )
                                ),
                                # column(1, class="empty-column"),
                                column(3,
                                       tags$div(class = "small-card overview-block-2",
                                                fluidRow(
                                                  column(3, class="card-icon-body",
                                                         tags$div(class="card-icon",
                                                                  HTML('<i class="fa-solid fa-thumbs-down"></i>')
                                                         )
                                                  ),
                                                  column(8, class="card-description",
                                                         tags$div(class="card-body",
                                                                  tags$text(
                                                                    textOutput(ns("consentDeclined")) %>% withSpinner(color = spinnerColor)
                                                                  )
                                                         ),
                                                         tags$text(class="card-title", textOutput(ns("consentDeclinedPercentage")), HTML('&nbsp;'), "DISAGREED")
                                                  )
                                                )
                                       )
                                ),
                                # column(1, class="empty-column"),
                                column(3,
                                       tags$div(class = "small-card overview-block-3",
                                                fluidRow(
                                                  column(3, class="card-icon-body",
                                                         tags$div(class="card-icon",
                                                                  HTML('<i class="fa-solid fa-rotate-left"></i>')
                                                         )
                                                  ),
                                                  column(8, class="card-description",
                                                         tags$div(class="card-body",
                                                                  tags$text(
                                                                    textOutput(ns("consentWithdrawn")) %>% withSpinner(color = spinnerColor)
                                                                  )
                                                         ),
                                                         tags$text(class="card-title", textOutput(ns("consentWithdrawnPercentage")), HTML('&nbsp;'), "QUIET")
                                                  )
                                                )
                                       )
                                ),
                                # column(1, class="empty-column"),
                                column(3,
                                       tags$div(class = "small-card overview-block-4",
                                                fluidRow(
                                                  column(3, class="card-icon-body",
                                                         tags$div(class="card-icon",
                                                                  HTML('<i class="fa-solid fa-user-circle"></i>')
                                                         )
                                                  ),
                                                  column(8, class="card-description",
                                                         tags$div(class="card-body",
                                                                  tags$text(
                                                                    textOutput(ns("totalPatientSmallCard")) %>% withSpinner(color = spinnerColor)
                                                                  )
                                                         ),
                                                         tags$text(class="card-title", "100 % TOTAL PTS")
                                                  )
                                                )
                                       )
                                )
                              )
                       )
              ),
              fluidRow(class="section-row",
                       
                      column(4, class="section overview-indications",
                              
                              fluidRow(
                                column(11, class = "section-title",
                                       tags$text('Gender Distribution')
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("overview1"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'copy'), disabled = TRUE)
                                                )
                                       )
                                )
                              ),
                              textOutput(ns("overviewWarningMessageoverviewAnalysisGenderEchart4r")),
                              echarts4rOutput(ns("overviewAnalysisGenderEchart4r")) %>% withSpinner(color = spinnerColor)
                      ),
                      column(4, class="section overview-indications",
                              
                              fluidRow(
                                column(11, class = "section-title",
                                       tags$text('Site Distribution')
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("overview2"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'copy'), disabled = TRUE)
                                                )
                                       )
                                )
                              ),
                              textOutput(ns("overviewWarningMessageoverviewAnalysisSiteEchart4r")),
                              echarts4rOutput(ns("overviewAnalysisSiteEchart4r")) %>% withSpinner(color = spinnerColor)
                      ),
                      column(4, class="section overview-indications",
                              
                              fluidRow(
                                column(11, class = "section-title",
                                       tags$text('Race Distribution')
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("overview3"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'copy'), disabled = TRUE)
                                                )
                                       )
                                )
                              ),
                              textOutput(ns("overviewWarningMessageoverviewAnalysisRaceEchart4r")),
                              echarts4rOutput(ns("overviewAnalysisRaceEchart4r")) %>% withSpinner(color = spinnerColor)
                      )
              ),
              fluidRow(class="section-row",
                       
                       column(1, class="empty-column"),
                       column(11, class="section overview-indications",
                              
                              fluidRow(
                                column(11, class = "section-title",
                                       tags$text('Indications')
                                ),
                                column(1, class = "section-title",
                                       tags$div(class = "add-to-report-button-container float-right",
                                                tags$div(style="display:inline-block", title="Add To Report",
                                                         actionButton(ns("overviewIndicationSunburstGenerateReport"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'copy'))
                                                )
                                       )
                                )
                              ),
                              radioButtons(ns("overviewIndicationsSunburstRadioFilter"), "",
                                           inline = TRUE,
                                           selected = "icd03_codes",
                                           c("ICD03 Code" = "icd03_codes", 
                                             "Human Readable" = "human_readables")),
                              textOutput(ns("overviewWarningMessageIndicationSunburst")),
                              plotlyOutput(ns("overviewIndicationsSunburstPlot")) %>% withSpinner(color = spinnerColor),
                              accordion(
                                id = "accordionOverviewIndicationsSunburstDownloadData",
                                accordionItem(
                                  title = list("Underlying data", HTML("&nbsp;") ,icon(lib="glyphicon", 'menu-down')),
                                  status = "danger",
                                  collapsed = TRUE,
                                  fluidRow(
                                    column(11, class = "section-title"),
                                    column(1, class = "section-title",
                                           tags$div(class = "add-to-report-button-container float-right",
                                                    tags$div(style="display:inline-block", title="Download CSV",
                                                             downloadButton(ns("overviewIndicationsSunburstDownloadData"), "", class = "add-to-report-button", icon = icon(lib="glyphicon", 'download-alt'))
                                                    )
                                           )
                                    )
                                  ),
                                  uiOutput(ns("overviewIndicationsSunburstDatatable")) %>% withSpinner(color = spinnerColor),
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
    
    #* @Description: Fetching all data which is required for overview page.
    maindf <- reactive({ 
      session$userData$filter()
    })
    
    observe({
      
      consented_data <- maindf()[maindf()$consent_status %in% "Agreed" & maindf()$site_name != "No Data Available", ]
      
      output$consentFullySigned <- renderPrint({
        cat(length(unique(consented_data$patient_uuid)))
      })
      
      declined_data <- maindf()[maindf()$consent_status %in% "Disagreed" & maindf()$site_name != "No Data Available", ]
      
      output$consentDeclined <- renderPrint({
        cat(length(unique(declined_data$patient_uuid)))
      })
      
      withdrawn_data <- maindf()[maindf()$consent_status %in% "Quiet" & maindf()$site_name != "No Data Available", ]
      
      output$consentWithdrawn <- renderPrint({
        cat(length(unique(withdrawn_data$patient_uuid)))
      })
      
      output$consentFullySigned <- renderPrint({
        cat(length(unique(consented_data$patient_uuid)))
      })
      
      declined_data <- maindf()[maindf()$consent_status %in% "Disagreed" & maindf()$site_name != "No Data Available", ]
      
      output$consentDeclined <- renderPrint({
        cat(length(unique(declined_data$patient_uuid)))
      })
      
      withdrawn_data <- maindf()[maindf()$consent_status %in% "Quiet" & maindf()$site_name != "No Data Available", ]
      
      output$consentWithdrawn <- renderPrint({
        cat(length(unique(withdrawn_data$patient_uuid)))
      })
      
      consent_fully_signed_percentage <- length(unique(consented_data$patient_uuid)) / (length(unique(declined_data$patient_uuid)) +
                                                       length(unique(consented_data$patient_uuid)) +
                                                       length(unique(withdrawn_data$patient_uuid)))
      
      output$consentFullySignedPercentage <- renderPrint({
        cat(paste(round(consent_fully_signed_percentage*100, digits = 1), "%", sep = " "))
      })
      
      consent_declined_percentage <- length(unique(declined_data$patient_uuid)) / (length(unique(declined_data$patient_uuid)) +
                                                      length(unique(consented_data$patient_uuid)) +
                                                      length(unique(withdrawn_data$patient_uuid)))
      
      output$consentDeclinedPercentage <- renderPrint({
        cat(paste(round(consent_declined_percentage*100, digits = 1), "%", sep = " "))
      })
      
      consent_withdrawn_percentage <- length(unique(withdrawn_data$patient_uuid)) / (length(unique(declined_data$patient_uuid)) +
                                                                                      length(unique(consented_data$patient_uuid)) +
                                                                                      length(unique(withdrawn_data$patient_uuid)))
      
      output$consentWithdrawnPercentage <- renderPrint({
        cat(paste(round(consent_withdrawn_percentage*100, digits = 1), "%", sep = " "))
      })

      output$totalPatientSmallCard <- renderPrint({
        cat(paste0(length(unique(maindf()$patient_uuid))))
      })
    })
    
    
    indicationsSunburstDf <- reactive({
      tryCatch({
        ICD03CodesForIndicationsSunburst(maindf())
      }, error=function(e) {
        return(NULL)
      }, warning=function(w) {
        return(NULL)
      })
    })

    observe({
      if(length(maindf()$sex) > 0){
        output$overviewWarningMessageoverviewAnalysisGenderEchart4r <- renderPrint(cat(""))
        output$overviewAnalysisGenderEchart4r <- renderEcharts4r({
          maindf() %>%
            group_by(sex) %>%
            summarise(count = n()) %>%
            e_charts(sex) %>%       # Specify the x-axis variable
            e_bar(count) %>%        # Specify the y-axis variable and chart type
            e_tooltip(trigger = "axis") %>%   # Add a tooltip
            e_x_axis(name = "Gender") %>%     # Add x-axis label
            e_y_axis(name = "Patients")  # Add y-axis label
        })

      } else {
        output$overviewWarningMessageoverviewAnalysisGenderEchart4r <- renderPrint(cat("No data available."))
      }


      if(length(maindf()$site_name) > 0){
        output$overviewWarningMessageoverviewAnalysisSiteEchart4r <- renderPrint(cat(""))
        output$overviewAnalysisSiteEchart4r <- renderEcharts4r({
          maindf() %>%
            group_by(site_name) %>%
            summarise(count = n()) %>%
            e_charts(site_name) %>%       # Specify the x-axis variable
            e_bar(count) %>%        # Specify the y-axis variable and chart type
            e_tooltip(trigger = "axis") %>%   # Add a tooltip
            e_x_axis(name = "Site") %>%     # Add x-axis label
            e_y_axis(name = "Patients")  # Add y-axis label
        })

      } else {
        output$overviewWarningMessageoverviewAnalysisSiteEchart4r <- renderPrint(cat("No data available."))
      }


      if(length(maindf()$race) > 0){
        output$overviewWarningMessageoverviewAnalysisRaceEchart4r <- renderPrint(cat(""))
        output$overviewAnalysisRaceEchart4r <- renderEcharts4r({
          maindf() %>%
            group_by(race) %>%
            summarise(count = n()) %>%
            e_charts(race) %>%       # Specify the x-axis variable
            e_bar(count) %>%        # Specify the y-axis variable and chart type
            e_tooltip(trigger = "axis") %>%   # Add a tooltip
            e_x_axis(name = "Race") %>%     # Add x-axis label
            e_y_axis(name = "Patients")  # Add y-axis label
        })

      } else {
        output$overviewWarningMessageoverviewAnalysisRaceEchart4r <- renderPrint(cat("No data available."))
      }
    })
    
    observe({      
      if(length(maindf()$pccipr) > 0){
        session$userData$indicationsSunburstDf <- indicationsSunburstDf()
        
        output$overviewWarningMessageIndicationSunburst <- renderPrint(cat(""))
        
        output$overviewIndicationsSunburstPlot <- renderPlotly({
          if(input$overviewIndicationsSunburstRadioFilter == "icd03_codes"){
            if(is.null(indicationsSunburstDf()$labels) || is.null(indicationsSunburstDf()$parents) || is.null(indicationsSunburstDf()$values)){
              output$overviewWarningMessageIndicationSunburst <- renderPrint(cat("No data available."))
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
                width = 750, height = 800,
                font = list(family = "Assistant"),
                title = list(text = data_sync_date_for_plots, y = 0, x = 0.5, xanchor = 'center', yanchor =  'bottom', font = list(size = 13))
              )
            }
          } else if(input$overviewIndicationsSunburstRadioFilter == "human_readables"){
            if(is.null(indicationsSunburstDf()$human_readable_labels) || is.null(indicationsSunburstDf()$human_readable_parents) || is.null(indicationsSunburstDf()$values)){
              output$overviewWarningMessageIndicationSunburst <- renderPrint(cat("No data available."))
            } else {
              plot_ly(
                ids = indicationsSunburstDf()$human_readable_ids,
                labels = indicationsSunburstDf()$human_readable_labels,
                parents = indicationsSunburstDf()$human_readable_parents,
                values = indicationsSunburstDf()$values,
                type = 'sunburst',
                branchvalues = 'total',
                hovertemplate = paste('Total count of <b>',indicationsSunburstDf()$human_readable_labels, '</b> is <b>', indicationsSunburstDf()$values, '</b> <extra></extra>'),
                insidetextorientation = 'radial',
                colors = c("#872b29", "#294787", "#877629", "#6c8729", "#87293a")
              ) %>% layout(
                width = 750, height = 800,
                font = list(family = "Assistant"),
                title = list(text = data_sync_date_for_plots, y = 0, x = 0.5, xanchor = 'center', yanchor =  'bottom', font = list(size = 13))
              )
            }
          }
        })
        
        output$overviewIndicationsSunburstDatatable <- renderUI({
          datatable(indicationsSunburstDf(), class = "table table-striped table-responsive", 
                    colnames = c("Parents", "Labels", "Ids", "Human Readable Parents", "Human Readable Labels", "Human Readable Ids", "Values"),
                    options = list(pageLength = 10))
        })
      } else {
        output$overviewWarningMessageIndicationSunburst <- renderPrint(cat("No data available."))
      }
    })
    
    output$overviewIndicationsSunburstDownloadData <- downloadHandler(
      filename = function() {
        paste('Overview Indications Sunburst-', format(Sys.time(), '%d-%B-%Y-%H-%M-%S'), ".csv", sep = '')
      },
      content = function(file) {
        out <- downloadCSV(indicationsSunburstDf())
        file.rename(out, file)
      }
    )
    
    session$userData$globalToBeAddInReportList <- reactiveVal()
    
    count <- reactiveValues(x = 1)
    
    observeEvent(input$overviewIndicationSunburstGenerateReport, { 
      
      plot_name <- paste("indications_sunburst:", count$x, sep = "")
      count$x <- count$x + 1
      
      session$userData$contentToBeAddedInReport$plotSpecificFilter <- input$overviewIndicationsSunburstRadioFilter
      
      content_to_be_added_in_report <- generateReportFiltersManagement(session$userData$contentToBeAddedInReport, plot_name)
      
      session$userData$contentToBeAddedInReportDf <- rbind(session$userData$contentToBeAddedInReportDf, content_to_be_added_in_report)
      
      # List to showing added plots on dashboard.
      list <- append(session$userData$globalToBeAddInReportList(), plot_name)
      session$userData$globalToBeAddInReportList(list)
    })
  })
}