#* @Path app/logic/utils/common_utils.R

#************************ START: LOADING REQUIRED PACKAGES AND RESPECTIVE CLASSES **********************#
#*# Import config file to get database credentials.
box::use(config)

box::use(
  dplyr[`%>%`, select, group_by, ungroup, count, as_tibble, rename, left_join, mutate, summarise, n],
  haven[read_xpt],
  ISOweek[date2ISOweek, ISOweek2date],
  clock[iso_year_week_day, set_day],
  stringr[str_sub],
  rtables[in_rows, rcell],
  rmarkdown[render],
  stats[aggregate],
  shiny[withProgress, incProgress],
  readr[write_rds],
  utils[write.csv],
  rlang[inform, enquo]
)
#************************ END: LOADING REQUIRED PACKAGES AND RESPECTIVE CLASSES **********************#

spinnerColor <- "#dd4b39"

ICD03CodesForIndicationsSunburst <- function(input_data){
  
  tryCatch({
    
    morphology_code <- sapply(strsplit(split = '-', input_data$icd03_code),function(i){i[1]})
    
    icd03_code <- input_data$icd03_code
    
    sunburst_parents_anatomic_site_code <- data.frame("parents" = c(input_data$anatomic_site_code), 
                                                      "labels" = c(input_data$topography_code), 
                                                      "ids" = c(input_data$topography_code), 
                                                      "human_readable_parents" = c(paste("All", input_data$anatomic_site, sep = "-")), #c(input_data$anatomic_site), 
                                                      "human_readable_labels" = c(input_data$topography_description), 
                                                      "human_readable_ids" = c(input_data$topography_description), 
                                                      "values" = c(NA), stringsAsFactors = FALSE)
    
    labels_count_parents_anatomic_site_code  <- sunburst_parents_anatomic_site_code %>% 
      group_by(sunburst_parents_anatomic_site_code$parents, sunburst_parents_anatomic_site_code$labels,
               sunburst_parents_anatomic_site_code$ids, sunburst_parents_anatomic_site_code$human_readable_parents,
               sunburst_parents_anatomic_site_code$human_readable_labels, sunburst_parents_anatomic_site_code$human_readable_ids) %>% 
      summarise(value = n(), .groups = 'drop')
    names(labels_count_parents_anatomic_site_code) <- c("parents", "labels", "ids", "human_readable_parents", 
                                                        "human_readable_labels", "human_readable_ids", "values")
    
    sunburst_parents_topography_code <- data.frame("parents" = c(input_data$topography_code), 
                                                   "labels" = c(morphology_code), 
                                                   "ids" = c(paste(input_data$topography_code, morphology_code, sep = "-")),
                                                   "human_readable_parents" = c(input_data$topography_description), 
                                                   "human_readable_labels" = c(input_data$icdo3_description), 
                                                   "human_readable_ids" = c(paste(input_data$topography_description, input_data$icdo3_description, sep = "-")),
                                                   "values" = c(NA),
                                                   stringsAsFactors = FALSE)
    
    labels_count_parents_topography_code  <- sunburst_parents_topography_code %>% 
      group_by(sunburst_parents_topography_code$parents, sunburst_parents_topography_code$labels, 
               sunburst_parents_topography_code$ids, sunburst_parents_topography_code$human_readable_parents,
               sunburst_parents_topography_code$human_readable_labels, sunburst_parents_topography_code$human_readable_ids) %>% 
      summarise(value = n(), .groups = 'drop')
    names(labels_count_parents_topography_code) <- c("parents", "labels", "ids", "human_readable_parents", 
                                                     "human_readable_labels", "human_readable_ids", "values")
    
    labels_count_parents_topography_code <- labels_count_parents_topography_code[!(
      labels_count_parents_topography_code$parents %in% "No Data Available"), ]
    
    d3 <- data.frame(
      c("All"),
      c(input_data$anatomic_site_code),
      c(input_data$anatomic_site_code),
      c("All"),
      c(input_data$anatomic_site),
      c(paste("All", input_data$anatomic_site, sep = "-")),
      NA
    )
    names(d3) <- c("parents", "labels", "ids", "human_readable_parents", "human_readable_labels", "human_readable_ids", "values")
    
    d3  <- d3 %>% 
      group_by(d3$parents, d3$labels, d3$ids, d3$human_readable_parents, d3$human_readable_labels, d3$human_readable_ids) %>% 
      summarise(value = n(), .groups = 'drop')
    names(d3) <- c("parents", "labels", "ids", "human_readable_parents", "human_readable_labels", "human_readable_ids", "values")
    
    labels_count_parents_anatomic_site_code <- labels_count_parents_anatomic_site_code[!(
      labels_count_parents_anatomic_site_code$parents %in% "No Data Available"), ]
    
    finaldf <- rbind(d3, labels_count_parents_anatomic_site_code, labels_count_parents_topography_code)
    
    d4 <- data.frame(
      c(""),
      c("All"),
      c("All"),
      c(""),
      c("All"),
      c("All"),
      c(sum(d3$values))
    )
    names(d4) <- c("parents", "labels", "ids", "human_readable_parents", "human_readable_labels", "human_readable_ids", "values")
    
    finaldf <- rbind(finaldf, d4)
    
    return(finaldf)
  }, error=function(e) {
    inform(paste("An error occurred:", e))
    return(NULL)
  })
}

cleanUpRaceCodes <- function(racedf){
  racedf <- gsub("[[:punct:]]", "", racedf)  # no libraries needed for gsub
  racedf <- droplevels(factor(racedf))
  racedf <- gsub('cWhite ', 'White', racedf)
  racedf <- gsub('cAmerican Indian or Alaska Native ', 'American Indian or Alaska Native', racedf)
  racedf <- gsub('cNot Reported ', 'Not Reported', racedf)
  racedf <- gsub('cBlack or African American ', 'Black or African American', racedf)
  racedf <- gsub('cAsian ', 'Asian', racedf)
  racedf <- gsub('cOther ', 'Other', racedf)
  racedf <- gsub('cNative Hawaiian or Other Pacific Islander ', 'Native Hawaiian or Other Pacific Islander', racedf)
  return(racedf)
}

cleanUpPunctuation <- function(data){
  updatedData <- gsub("[[:punct:]]", "", data)  # no libraries needed for gsub
  outputDf <- gsub(" ", "", updatedData)  # no libraries needed for gsub
  
  return(outputDf)
}

getConsentDiagnosisDfForChart <- function(input_data){
  
  input_data$m_status <- gsub('\"','',input_data$m_status)
  ids <- which(input_data$m_status == 'Unknown')
  input_data$m_status[ids] = "No"
  input_data$mstatus <- input_data$m_status
  input_data$mstatus[is.na(input_data$mstatus)] <- "In process"
  
  return(input_data)
}

getMetastaticStatus <- function(input_data){
  
  input_data$m_status <- gsub('\"','',input_data$m_status)
  ids <- which(input_data$m_status == 'No')
  input_data$m_status[ids] = "No"
  input_data$mstatus <- input_data$m_status
  input_data$mstatus[is.na(input_data$mstatus)] <- "In process"
  
  return(input_data)
}

getDefaultMetricsDfForChart <- function(input_data){
  
  input_data <- input_data[input_data$consent_time >= "2022-08-01", ]
  
  if(length(input_data$consent_time) > 0){
    
    week_iso <- ISOweek::date2ISOweek(input_data$consent_time) %>% 
      strsplit("-") %>% 
      sapply(function(i)paste(i[1], i[2], sep = "-"))
    
    output <- data.frame(week_iso = week_iso)
    
    input_data$consent_time <- as.Date(input_data$consent_time, format = "%Y-%m-%d")
    
    output$start <- lubridate::floor_date(input_data$consent_time, 'weeks') + 1
    output$end <- lubridate::ceiling_date(input_data$consent_time, 'weeks')
    
    output <- output %>% group_by(week_iso, start, end) %>% 
      count() %>% ungroup
    
    output$week_iso <- gsub("0W", "-W", output$week_iso)
    
    output$start <- as.character(output$start)
    output$end <- as.character(output$end)
    
    names(output) <- c("Year-Week", "Start", "End", "Consent")
    
    return(output)
  } else {
    return(NULL)
  }
}

getMetricsDfForCharts <- function(input_data, column, categorical_fields){
  
  tryCatch({
    
    output_data <- data.frame("Year.Week" = NULL,
                              "start" = NULL,
                              "end" = NULL,
                              "consent" = NULL,
                              "type" = NULL)
    
    constant_data <- input_data
    
    for(fields in categorical_fields){
      
      input_data <- constant_data[constant_data$consent_time >= "2022-08-01", ]
      
      input_data <- input_data[input_data[[column]] %in% c(fields),]
      if(sum(lengths(input_data)) > 0){
        
        if(length(input_data$consent_time) > 0){
          week_iso <- ISOweek::date2ISOweek(input_data$consent_time) %>% 
            strsplit("-") %>% 
            sapply(function(i)paste(i[1], i[2], sep = "-"))
          
          output <- data.frame(week_iso = week_iso)
          
          input_data$consent_time <- as.Date(input_data$consent_time, format = "%Y-%m-%d")
          
          output$start <- lubridate::floor_date(input_data$consent_time, 'weeks') + 1
          output$end <- lubridate::ceiling_date(input_data$consent_time, 'weeks')
          
          output <- output %>% group_by(week_iso, start, end) %>% 
            count() %>% ungroup
          
          output$week_iso <- gsub("0W", "-W", output$week_iso)
          
          output$start <- as.character(output$start)
          output$end <- as.character(output$end)
          
          names(output) <- c("Year-Week", "Start", "End", "Consent")
          
          output$type <- fields
          
          output <- output %>% group_by(type) %>% mutate(cumsum = cumsum(Consent))
          
          output_data <- rbind(output_data, output)
        } else {
          return(NULL)
        }
      }
    }
    
    return(output_data)
  }, error=function(e) {
    inform(paste("An error occurred:", e))
    return(NULL)
  }, warning=function(w) {
    return(NULL)
  })
}

getDailyBasedConsentCount <- function(input_data){
  
  input_data$date = sapply(strsplit(split = " ", as.character(input_data$consent_time)),function(i){i[1]})
  output_data <- input_data %>% group_by(date) %>% summarise(n = n())
  
  return(output_data)
}

replaceNANullToNoData <- function(input_data){
  input_data[input_data == ''] <- 'No Data Available'
  input_data[is.na(input_data)] <- 'No Data Available'
  input_data[is.null(input_data)] <- 'No Data Available'
  input_data[input_data == 'NA NA'] <- 'No Data Available'
  
  return(input_data)
}

replaceNoDataToNA <- function(input_data){
  input_data[input_data == 'No Data Available'] <- NA
  return(input_data)
}

consentedRtable <- function(data){
  
  tryCatch({
    
    data$race <- cleanUpRaceCodes(data$race)
    
    consented <- data
    
    consented$race <- replaceNANullToNoData(consented$race)
    consented$sex <- replaceNANullToNoData(consented$sex)
    consented$consent_status <- replaceNANullToNoData(consented$consent_status)
    consented$assigned_md <- replaceNANullToNoData(consented$assigned_md)
    consented$site_name <- replaceNANullToNoData(consented$site_name)
    
    ## for simplicity grab non-sparse subset
    consented <- consented[!(consented$consent_status %in% "") & !(consented$consent_status %in% NA) 
                           & !(consented$race %in% "") & !(consented$race %in% NA),]
    
    
    icd03_anatomic_code <- consented$anatomic_site_code
    icd03_morphology_code <- consented$icd03_morphology_code
    
    icd03_anatomic_code <- replaceNANullToNoData(icd03_anatomic_code)
    icd03_morphology_code <- replaceNANullToNoData(icd03_morphology_code)
    
    consented$race <- as.factor(consented$race)
    consented$sex <- as.factor(consented$sex)
    consented$consent_status <- as.factor(consented$consent_status)
    consented$assigned_md <- as.factor(consented$assigned_md)
    consented$site_name <- as.factor(consented$site_name)
    
    consented$icd03_anatomic_code_name <- as.factor(consented$anatomic_site)
    consented$icd03_morphology_code_name <- as.factor(consented$icdo3_description)
    
    consented$icd03_anatomic_code <- as.factor(icd03_anatomic_code)
    consented$icd03_morphology_code <- as.factor(icd03_morphology_code)
    
    consented$m_status <- as.factor(consented$m_status)
    
    return(consented)
  }, error=function(e) {
    inform(paste("An error occurred:", e))
    return(NULL)
  }, warning=function(w) {
    return(NULL)
  })
}


age_range_18_39 <- function(x, ...){
  y <- x[x >= 18 & x <= 39]
  val <- if(length(y)) round(mean(y), 2) else "no data"
  in_rows(
    "Age 18-39" = rcell(val)
  )
}

age_range_40_49 <- function(x, ...){
  y <- x[x >= 40 & x <= 49]
  val <- if(length(y)) round(mean(y), 2) else "no data"
  in_rows(
    "Age 40-49" = rcell(val)
  )
}

age_range_50_64 <- function(x, ...){
  y <- x[x >= 50 & x <= 64]
  val <- if(length(y)) round(mean(y), 2) else "no data"
  in_rows(
    "Age 50-64" = rcell(val)
  )
}

age_range_65_plus <- function(x, ...){
  y <- x[x >= 65]
  val <- if(length(y)) round(mean(y), 2) else "no data"
  in_rows(
    "Age 65+" = rcell(val)
  )
}

assignICD03Code <- function(data){
  
  tryCatch({
    
    data$icd03_code[is.na(data$icd03_code)] <- 'No Data Available'
    
    CVALUE <- sapply(strsplit(split = '-', data$icd03_code),function(i){i[1]})
    
    MMMM <- sapply(strsplit(split = '-', data$icd03_code),function(i){i[2]})
    MMMM[is.na(MMMM)] <- 'No Data Available'
    
    ids <- paste(CVALUE,MMMM, sep = "-")
    ids <- gsub(' ', '', ids)
    ids <- gsub(',', '', ids)
    ids <- gsub('-C', 'C', ids)
    ids[grep("NANA", ids)] = NA
    ids[grep("NA-NA", ids)] = NA
    ids[grep("system", ids)] = "No Data"
    
    return(ids)
  }, error=function(e) {
    inform(paste("An error occurred:", e))
    return(NULL)
  }, warning=function(w) {
    return(NULL)
  })
} 

generateReportFiltersManagement <- function(data, plot_name){
  
  if(is.null(data$filterMinAge)){
    data$filterMinAge = 'NULL'
  }
  if(is.null(data$filterMaxAge)){
    data$filterMaxAge = 'NULL'
  }
  if(is.null(data$filterSex)){
    data$filterSex = 'NULL'
  }
  if(is.null(data$filterRace)){
    data$filterRace = 'NULL'
  }
  if(is.null(data$filterSiteName)){
    data$filterSiteName = 'NULL'
  }
  if(is.null(data$filterPcp)){
    data$filterPcp = 'NULL'
  }
  if(is.null(data$plotSpecificFilter)){
    data$plotSpecificFilter = 'NULL'
  }
  if(is.null(data$plotSpecificFilterRows)){
    data$plotSpecificFilterRows = 'NULL'
  }
  if(is.null(data$plotSpecificFilterColumns)){
    data$plotSpecificFilterColumns = 'NULL'
  }
  if(is.null(data$plotSpecificFilterSingleColumn)){
    data$plotSpecificFilterSingleColumn = 'NULL'
  }
  if(is.null(data$filterDateFrom)){
    data$filterDateFrom = 'NULL'
  }
  if(is.null(data$filterDateTo)){
    data$filterDateTo = 'NULL'
  }
  if(is.null(data$filterIncludeExcludeNAIndications)){
    data$filterIncludeExcludeNAIndications = 'NULL'
  }
  if(is.null(data$filterConsentStatus)){
    data$filterConsentStatus = 'NULL'
  }
  if(is.null(data$filterAnatomic)){
    data$filterAnatomic = 'NULL'
  }
  if(is.null(data$filterMorphology)){
    data$filterMorphology = 'NULL'
  }
  
  contentToBeAddedInReport <- data.frame(filterMinAge = data$filterMinAge,
                                         filterMaxAge = data$filterMaxAge,
                                         filterSex = data$filterSex,
                                         filterRace = data$filterRace,
                                         filterSiteName = data$filterSiteName,
                                         filterPcp = data$filterPcp,
                                         plotSpecificFilter = data$plotSpecificFilter,
                                         plotSpecificFilterRows = data$plotSpecificFilterRows,
                                         plotSpecificFilterColumns = data$plotSpecificFilterColumns,
                                         plotSpecificFilterSingleColumn = data$plotSpecificFilterSingleColumn,
                                         filterDateFrom = data$filterDateFrom,
                                         filterDateTo = data$filterDateTo,
                                         filterIncludeExcludeNAIndications = data$filterIncludeExcludeNAIndications,
                                         filterConsentStatus = data$filterConsentStatus,
                                         filterAnatomic = data$filterAnatomic,
                                         filterMorphology = data$filterMorphology,
                                         plotName = plot_name)
  
  return(contentToBeAddedInReport)
}

filterDataHandler <- function(data, filters){
  
  tryCatch({
    filteredData <- data
    
    #************************ START: FILTER SEX **********************#
    if(!is.null(filters$filterSex)){
      unlistVector <- unlist(strsplit(filters$filterSex, ","))
      RemoveFrontWhiteSpace <- sub("^\\s+", "", unlistVector)
      
      if(grepl(RemoveFrontWhiteSpace, c('NULL', NULL), fixed = TRUE) == FALSE){
        if("NA" %in% c(RemoveFrontWhiteSpace)){
          filteredData <- filteredData[filteredData$sex %in% c(RemoveFrontWhiteSpace, NA),] 
        } else {
          filteredData <- filteredData[filteredData$sex %in% c(RemoveFrontWhiteSpace),] 
        }
      }
    }
    #************************ END: FILTER SEX **********************#
    
    
    #************************ START: FILTER RACE **********************#
    if(!is.null(filters$filterRace)){
      
      unlistVector <- unlist(strsplit(filters$filterRace,","))
      RemoveFrontWhiteSpace <- sub("^\\s+", "", unlistVector)
      
      if(grepl(RemoveFrontWhiteSpace, c('NULL', NULL), fixed = TRUE) == FALSE){
        if("NA" %in% c(RemoveFrontWhiteSpace)){
          filteredData <- filteredData[filteredData$race %in% c(RemoveFrontWhiteSpace, NA),] 
        } else {
          filteredData <- filteredData[filteredData$race %in% c(RemoveFrontWhiteSpace),] 
        }
      }
    }
    #************************ END: FILTER RACE **********************#
    
    
    #************************ START: FILTER SITE NAME **********************#
    if(!is.null(filters$filterSiteName)){
      
      unlistVector <- unlist(strsplit(filters$filterSiteName,","))
      RemoveFrontWhiteSpace <- sub("^\\s+", "", unlistVector)
      
      if(grepl(RemoveFrontWhiteSpace, c('NULL', NULL), fixed = TRUE) == FALSE){
        if("NA" %in% c(RemoveFrontWhiteSpace)){
          filteredData <- filteredData[filteredData$site_name %in% c(RemoveFrontWhiteSpace, NA),] 
        } else {
          filteredData <- filteredData[filteredData$site_name %in% c(RemoveFrontWhiteSpace),] 
        }
      }
    }
    #************************ END: FILTER SITE NAME **********************#
    
    
    #************************ START: FILTER PCP **********************#
    if(!is.null(filters$filterPcp)){
      
      unlistVector <- unlist(strsplit(filters$filterPcp,","))
      RemoveFrontWhiteSpace <- sub("^\\s+", "", unlistVector)
      
      if(grepl(RemoveFrontWhiteSpace, c('NULL', NULL), fixed = TRUE) == FALSE){
        if("NA" %in% c(RemoveFrontWhiteSpace)){
          filteredData <- filteredData[filteredData$assigned_md %in% c(RemoveFrontWhiteSpace, NA),] 
        } else {
          filteredData <- filteredData[filteredData$assigned_md %in% c(RemoveFrontWhiteSpace),] 
        }
      }
    }
    #************************ END: FILTER PCP **********************#
    
    
    #************************ START: FILTER INCLUDE EXCLUDE NA INDICATIONS **********************#
    if(!is.null(filters$filterIncludeExcludeNAIndications)){
      
      if(filters$filterIncludeExcludeNAIndications == TRUE){
        filteredData <- filteredData[!(filteredData$icd03_code %in% "No Data Available"),] 
      }
    }
    #************************ END: FILTER INCLUDE EXCLUDE NA INDICATIONS **********************#
    
    
    #************************ START: FILTER CONSENT STATUS **********************#
    if(!is.null(filters$filterConsentStatus)){
      
      unlistVector <- unlist(strsplit(filters$filterConsentStatus,","))
      RemoveFrontWhiteSpace <- sub("^\\s+", "", unlistVector)
      
      if(grepl(RemoveFrontWhiteSpace, c('NULL', NULL), fixed = TRUE) == FALSE){
        filteredData <- filteredData[filteredData$consent_status %in% c(RemoveFrontWhiteSpace),] 
      }
    }
    #************************ END: FILTER CONSENT STATUS **********************#
    
    
    #************************ START: FILTER ANATOMIC CODES **********************#
    if(!is.null(filters$filterAnatomic)){
      
      unlistVector <- unlist(strsplit(filters$filterAnatomic, ","))
      RemoveFrontWhiteSpace <- sub("^\\s+", "", unlistVector)
      
      if(grepl(RemoveFrontWhiteSpace, c('NULL', NULL), fixed = TRUE) == FALSE){
        filteredData <- filteredData[filteredData$anatomic_site_code %in% c(RemoveFrontWhiteSpace),] 
      }
    }
    #************************ END: FILTER ANATOMIC CODES **********************#
    
    
    #************************ START: FILTER MORPHOLOGY **********************#
    if(!is.null(filters$filterMorphology)){
      
      unlistVector <- unlist(strsplit(filters$filterMorphology, ","))
      RemoveFrontWhiteSpace <- sub("^\\s+", "", unlistVector)
      
      if(grepl(RemoveFrontWhiteSpace, c('NULL', NULL), fixed = TRUE) == FALSE){
        filteredData <- filteredData[filteredData$icd03_morphology_code %in% c(RemoveFrontWhiteSpace),] 
      }
    }
    #************************ END: FILTER MORPHOLOGY **********************#
    
    return(filteredData)
  }, error=function(e) {
    inform(paste("An error occurred:", e))
    return(NULL)
  })
}

downloadReportHandler <- function(data, filtersAndcContentToBeInReport, lastSyncDate){
  
  tryCatch({
    
    #creating temporary directory
    temporary_directory <- tempdir()
    markdown_file_name <- "generate_plot.rmd"
    common_utils <- "common_utils.R"
    
    tempReport <- file.path(temporary_directory, markdown_file_name)
    tempCommonUtils <- file.path(temporary_directory, common_utils)
    
    #copying the markdown and reference file to the temporary directory so that markdown will be able to access the reference fi$
    file.copy(markdown_file_name, tempReport, overwrite = TRUE)
    file.copy("./app/logic/utils/common_utils.R", tempCommonUtils, overwrite = TRUE)
    
    if(!is.null(filtersAndcContentToBeInReport)){
      aggregatedFilters <- filtersAndcContentToBeInReport
      outputPDFList <- NULL
      output_image_list <- NULL
      
      shiny::withProgress(
        message = paste0("Preparing Reports"),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          
          for(i in 1:nrow(aggregatedFilters)) {
            filters <- aggregatedFilters[i,]
            
            filterDataHandlerDf <- filterDataHandler(data, filters)
            
            plotName = sapply(strsplit(split = ':', filters$plotName),function(i){i[1]})
            
            input_params <- list(
              data = filterDataHandlerDf,
              filter = filters,
              count = i,
              common_utils = tempCommonUtils,
              img_save_path = paste(temporary_directory, "/", sep = ""),
              to_be_printed = plotName,
              plotly_username = config$get("plotly_username"),
              plotly_api_key = config$get("plotly_api_key"),
              data_last_sync_date = lastSyncDate
            )
            
            outputPDFName <- paste("output_", i, ".pdf", sep = '')
            outputPDFName <- file.path(temporary_directory, outputPDFName)
            
            rmarkdown::render(input = tempReport, output_format= "pdf_document", params = input_params, output_file = outputPDFName)
            outputPDFList <- append(outputPDFList, outputPDFName)
            
            output_image <- file.path(temporary_directory, plotName)
            
            output_image_list <- append(output_image_list, paste(output_image, "_", i, ".png", sep = ""))
          }
        }
      )
      
      if(is.null(outputPDFList) | is.null(output_image_list))
        return(NULL)
      
      return(list(outputPDFList, output_image_list, tempCommonUtils))
    }
  }, error=function(e) {
    inform(paste("An error occurred:", e))
    return(NULL)
  })
}

createHeader <- function(title = "Demo Dashboard Report", 
                         author = "",
                         date = "", 
                         data.source = "",
                         output = c("html_document"), 
                         output_type = "",
                         toc = TRUE,
                         data_last_sync_date = ""
) {
  
  if(!(output_type %in% c("html"))){
    if (is.null(output)) {
      output <- "html_document"
    }
    # output <- match.arg(output)
    output <- c(output, "md_document")
    if (toc) {
      output2 <- as.vector(rbind(paste0(output, ":"), 
                                 rep("  toc: true", length(output))))
    } else {
      output2 <- paste0(output, ": default")
    }
    header <- c("---",
                paste0("title: ", title),
                paste0("author: ", author),
                paste0("date: ", date),
                "always_allow_html: true",
                paste0("params:"),
                paste0("  data_last_sync_date: ", "'", data_last_sync_date, "'"),
                paste0("output:"),
                paste0("  ", output2),
                "---")
    
    header <- c(header, "", "```{r setup, include=FALSE, knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', fig.fullwidth = TRUE, fig.width = 11.6, dpi=72, cache=FALSE)}")
    
    header <- c(header,
                "library(dplyr)",
                "library(stringr)",
                "library(stats)",
                "library(readr)",
                "library(ISOweek)",
                "library(clock)",
                "library(rtables)",
                "library(tern)",
                "library(stats)",
                "library(readr)",
                "library(plotly)",
                "library(knitr)",
                "```",
                "#'", 
                "", 
                "## Introduction", 
                "", 
                "### This report has been generated from the Demo RShiny app for data exploration",
                "#'",
                "",
                "```{r echo=FALSE}",
                "source(here::here('Downloads/r files/demo/demo-main (1)/common_utils.R'))",
                "data_last_sync_date <- paste('Data as of', params$data_last_sync_date, 'UTC', sep = ' ')",
                "```",
                ""
    )
  } else {
    header <- c("---",
                paste0("title: ", title),
                "display_as: basic
language: r
layout: base
params:
  data: 'data'
  filter: 'filter'
  logo_path: 'logo_path'
  current_time: 'current_time'
  data_last_sync_date: 'data_last_sync_date'
  common_utils: 'common_utils'
always_allow_html: true
classoption: dvipsnames
output: html_document
---"
    )
  }
  
  header <- c(header,
              "```{r echo=FALSE}",
              "data_last_sync_date <- paste('Data as of', params$data_last_sync_date, 'UTC', sep = ' ')",
              "```")
  header
}

generateRMD <- function(data, aggregated_filter, output_type, data_last_sync_date){
  
  tryCatch({
    shiny::withProgress(
      message = paste0("Downloading File"),
      value = 0,
      {
        shiny::incProgress(1/10)
        Sys.sleep(1)
        shiny::incProgress(5/10)
        #creating temporary directory
        temporary_directory <- tempdir()
        
        common_utils <- "common_utils.R"
        tempCommonUtils <- file.path(temporary_directory, common_utils)
        file.copy("./app/logic/utils/common_utils.R", tempCommonUtils, overwrite = TRUE)
        
        if(!(output_type %in% c("html"))){
          
          data_file_name <- "demo_data.rds" # Do not change the names impact on plot_common_utils.R
          filter_file_name <- "demo_filter_values.rds" # Do not change the names impact on plot_common_utils.R
          
          tempRDS <- file.path(temporary_directory, data_file_name)
          temp_filter_values <- file.path(temporary_directory, filter_file_name)
          
          readr::write_rds(data, tempRDS)
          readr::write_rds(aggregated_filter, temp_filter_values)
          
          rmd_file_name <- "demo_dashboard_reports.rmd"
          rmd_file_path <- file.path(temporary_directory, rmd_file_name)
          
          rmd_script <- getContentList(data, aggregated_filter, output_type)
          
          report <- c(createHeader(output_type = output_type, data_last_sync_date = data_last_sync_date), unlist(rmd_script, use.names = FALSE))
          
          cat(report, sep = "\n", file = rmd_file_path)
          
          zip_file_name <- "demo_dashboard_report_rmd"
          
          tempZIP <- file.path(temporary_directory, zip_file_name)
          
          zip::zip(zipfile = tempZIP,
                   files = c(tempRDS, temp_filter_values, tempCommonUtils, rmd_file_path),
                   mode = "cherry-pick")
          return(tempZIP)
        } else {
          common_utils <- "common_utils.R"
          tempCommonUtils <- file.path(temporary_directory, common_utils)
          file.copy("./app/logic/utils/common_utils.R", tempCommonUtils, overwrite = TRUE)
          
          rmd_file_name <- "demo_dashboard_reports.rmd"
          rmd_file_path <- file.path(temporary_directory, rmd_file_name)
          
          rmd_script <- getContentList(data, aggregated_filter, output_type)
          
          report <- c(createHeader(output_type = output_type, data_last_sync_date = data_last_sync_date), unlist(rmd_script, use.names = FALSE))
          
          cat(report, sep = "\n", file = rmd_file_path)
          return(list(rmd_file_path, tempCommonUtils))
        }
      }
    )
  }, error=function(e) {
    inform(paste("An error occurred:", e))
    return(e)
  })
}

# Custom summarise function using tidy evaluation
summarise_count <- function(data, group_var, summarise_expr) {
  group_var <- enquo(group_var)
  
  data %>%
    group_by(!!group_var) %>%
    summarise(count = n())
}

getContentList <- function(data = NULL, filter = NULL, output_type){
  
  content_list <- NULL
  
  if(!(output_type %in% c("html"))){
    content <- '```{r}
  demo_data <- readr::read_rds("demo_data.rds", refhook = NULL)
  filter <- readr::read_rds("demo_filter_values.rds", refhook = NULL)
```
'
  } else {
    content <- '```{r setup, include=FALSE, echo = FALSE, message=FALSE}
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = FALSE)
```

```{r}
demo_data <- params$data
filter <- params$filter
```

```{r, include=FALSE}
source(params$common_utils, local = knitr::knit_global())
```

```{r fig.align="center", out.width="20%", fig.width = 10}
cat("\n")
cat("\n")
knitr::include_graphics(params$logo_path)
cat("\n")
cat("\n")
```
'
  }
  content_list <- append(content_list, content) 
  
  content <- '```{r}
  printSummary <- function(filters){
    cat("\n\n")
    if(grepl("indications_sunburst", filters$plotName, fixed = TRUE)){
      cat("## Overview Indications")
    }
    if(grepl("consent_sunburst", filters$plotName, fixed = TRUE)){
      cat("## Recent Diagnosis")
    }
    if(grepl("distribution_plot", filters$plotName, fixed = TRUE)){
      cat("## Distribution Plot")
    }
    if(grepl("distribution_count", filters$plotName, fixed = TRUE)){
      cat("## Distribution Count")
    }
    if(grepl("consented_diagnosis_primary_cancer_diagnosis", filters$plotName, fixed = TRUE)){
      cat("## CANCER DIAGNOSIS Plot")
    }
    if(grepl("consented_diagnosis_metastatic_distribution", filters$plotName, fixed = TRUE)){
      cat("## Metastatic Distribution Plot")
    }
    if(grepl("indications_diagnosis_most_recent_diagnosis", filters$plotName, fixed = TRUE)){
      cat("## Indications Diagnosis Recent Diagnosis")
    }
    if(grepl("consented_diagnosis_treatment_intent_status", filters$plotName, fixed = TRUE)){
      cat("## Treatment Status Intent Plot")
    }
    if(grepl("consented_diagnosis_treatment_status", filters$plotName, fixed = TRUE)){
      cat("## Treatment Status Plot")
    }
    if(grepl("consent_summary", filters$plotName, fixed = TRUE)){
      cat("## Consent Summary")
    }
    if(grepl("consent_metrics_consent_weeks_table", filters$plotName, fixed = TRUE)){
      cat("## Metrics Table")
    }
    if(grepl("consent_patient_characteristics_rtables", filters$plotName, fixed = TRUE)){
      cat("## Patient Characteristics RTables")
    }
    
    cat("\n")
    cat("#### Applied filters:\n")
    
    if(!grepl("NULL", filters$plotSpecificFilter, fixed=TRUE) & ("indications_sunburst" %in% filters$plotName | "consent_summary" %in% filters$plotName)){
      
      unlistVector <- unlist(strsplit(filters$plotSpecificFilter,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))
      
      cat(paste("#### Type:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterMinAge, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterMinAge,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))
      
      cat(paste("#### Min Age:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterMaxAge, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterMaxAge,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Max Age:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterSex, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterSex,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Sex:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterRace, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterRace,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Race:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterSiteName, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterSiteName,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Tess site Name:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterPcp, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterPcp,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### PCP:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$plotSpecificFilterColumns, fixed=TRUE) & ("consent_patient_characteristics_rtables" %in% filters$plotName)){
      unlistVector <- unlist(strsplit(filters$plotSpecificFilterColumns,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Columns:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$plotSpecificFilterSingleColumn, fixed=TRUE) & 
      (("consent_summary" %in% filters$plotName) | 
        ("distribution_plot" %in% filters$plotName) |
        ("distribution_count" %in% filters$plotName))){
      
      if(filters$plotSpecificFilterSingleColumn == "All" & "consent_summary" %in% filters$plotName){
        unlistVector <- unlist(strsplit(filters$plotSpecificFilterSingleColumn,","))
        values <- unique(sub("^\\\\s+", "", unlistVector))

        cat(paste("#### Column: sex, race, site_name, assigned_md", "\n"))
        
      } else {
        unlistVector <- unlist(strsplit(filters$plotSpecificFilterSingleColumn,","))
        values <- unique(sub("^\\\\s+", "", unlistVector))

        cat(paste("#### Column:", toString(values), "\n"))
        
      }
    }
    if(!grepl("NULL", filters$plotSpecificFilterRows, fixed=TRUE) 
      & ("consent_summary" %in% filters$plotName | 
          "consented_diagnosis_metastatic_distribution" %in% filters$plotName |
          "consented_diagnosis_treatment_status_intent" %in% filters$plotName |
          "consented_diagnosis_treatment_status" %in% filters$plotName
          )){
      unlistVector <- unlist(strsplit(filters$plotSpecificFilterRows,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Field:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterIncludeExcludeNAIndications, fixed=TRUE)){
      cat(paste("#### Abstracted:", toString(filters$filterIncludeExcludeNAIndications), "\n"))
    }
    if(!grepl("NULL", filters$filterConsentStatus, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterConsentStatus,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Consent Status:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterAnatomic, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterAnatomic,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("#### Anatomic Codes:", toString(values), "\n"))
    }
    if(!grepl("NULL", filters$filterMorphology, fixed=TRUE)){
      unlistVector <- unlist(strsplit(filters$filterMorphology,","))
      values <- unique(sub("^\\\\s+", "", unlistVector))

      cat(paste("## Morphology:", toString(values), "\n"))
    }
  }

```
'
  
  content_list <- append(content_list, content)
  count <- 1
  for(plot_name in filter$plotName){
    if("indications_sunburst" %in% plot_name){
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        library(plotly)
        
        plotData <- ICD03CodesForIndicationsSunburst(demo_data_filtered)
        if(filters$plotSpecificFilter == "icd03_codes"){
          fig <- plotly::plot_ly(
            ids = plotData$ids,
            labels = plotData$labels,
            parents = plotData$parents,
            values = plotData$values,
            type = "sunburst",
            branchvalues = "total",
            colors = c("#872b29", "#294787", "#877629", "#6c8729", "#87293a")
          ) %>% plotly::layout(
                font = list(family = "Assistant"),
                title = list(text = data_last_sync_date, y = 0, x = 0.5, xanchor = "center", yanchor =  "bottom", font = list(size = 13))
              )
        } else if(filters$plotSpecificFilter == "human_readables"){
          fig <- plotly::plot_ly(
            ids = plotData$ids,
            labels = plotData$human_readable_labels,
            parents = plotData$parents,
            values = plotData$values,
            type = "sunburst",
            branchvalues = "total",
            colors = c("#872b29", "#294787", "#877629", "#6c8729", "#87293a")
          ) %>% plotly::layout(
                font = list(family = "Assistant"),
                title = list(text = data_last_sync_date, y = 0, x = 0.5, xanchor = "center", yanchor =  "bottom", font = list(size = 13))
              )
        }

        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("consent_sunburst" %in% plot_name){
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        library(plotly)
        
        plotData <- ICD03CodesForIndicationsSunburst(demo_data_filtered)
        if(filters$plotSpecificFilter == "icd03_codes"){
          fig <- plotly::plot_ly(
            ids = plotData$ids,
            labels = plotData$labels,
            parents = plotData$parents,
            values = plotData$values,
            type = "sunburst",
            branchvalues = "total",
            colors = c("#872b29", "#294787", "#877629", "#6c8729", "#87293a")
          ) %>% plotly::layout(
                font = list(family = "Assistant"),
                title = list(text = data_last_sync_date, y = 0, x = 0.5, xanchor = "center", yanchor =  "bottom", font = list(size = 13))
              )
        } else if(filters$plotSpecificFilter == "human_readables"){
          fig <- plotly::plot_ly(
            ids = plotData$ids,
            labels = plotData$human_readable_labels,
            parents = plotData$parents,
            values = plotData$values,
            type = "sunburst",
            branchvalues = "total",
            colors = c("#872b29", "#294787", "#877629", "#6c8729", "#87293a")
          ) %>% plotly::layout(
                font = list(family = "Assistant"),
                title = list(text = data_last_sync_date, y = 0, x = 0.5, xanchor = "center", yanchor =  "bottom", font = list(size = 13))
              )
        }

        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("distribution_plot" %in% plot_name){
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        
        result <- demo_data_filtered[[filters$plotSpecificFilterSingleColumn]]
        
        fig <- plotly::plot_ly(y = ~table(result), 
                              x=~c(names(table(result))), 
                              type = "bar") %>% 
          plotly::layout(yaxis = list(title = "Frequency"), 
          xaxis = list(title = ""), 
          font = list(family = "Assistant"),
          title = list(text = data_last_sync_date, y = 0, x = 0.5, xanchor = "center", yanchor =  "bottom", font = list(size = 13))
          )
        
        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("distribution_count" %in% plot_name){
      
      if(!(output_type %in% c("html"))){
        print_rtable <- "knitr::kable(t(table(result)))"
      } else {
        print_rtable <- "knitr::kable(t(table(result)))"
      }
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        library(knitr)
        
        result <- demo_data_filtered[[filters$plotSpecificFilterSingleColumn]]',
                       print_rtable,
                       
                       
                       '}, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("indications_diagnosis_most_recent_diagnosis" %in% plot_name){
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        
        output <- data.frame("anatomic_code_names" = demo_data_filtered$anatomic_site)
        
        fig <- plotly::plot_ly(output, x=~anatomic_code_names,type="histogram") %>% 
                  plotly::layout(yaxis = list(title = "Frequency"),
                  xaxis = list(title = paste("Anatomic Site", data_last_sync_date, sep = "\\n\\n")),
                  font = list(family = "Assistant")
                  )
        
        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("consented_diagnosis_primary_cancer_diagnosis" %in% plot_name){
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        primaryCancerDiagnosisData <- getConsentDiagnosisDfForChart(demo_data_filtered)
        
        fig <- plotly::plot_ly(primaryCancerDiagnosisData, x = ~anatomic_site, 
                              type = "histogram") %>% 
          plotly::layout(yaxis = list(title = "Frequency"),
          xaxis = list(title = paste("CANCER DIAGNOSIS", data_last_sync_date, sep = "\\n\\n")),
          font = list(family = "Assistant")
          )
        
        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("consented_diagnosis_metastatic_distribution" %in% plot_name){
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        primaryCancerDiagnosisData <- getConsentDiagnosisDfForChart(demo_data_filtered)
        
        unlistVector <- unlist(strsplit(filters$plotSpecificFilterRows,","))
        rows <- sub("^\\\\s+", "", unlistVector)
        rows <- unique(rows)
            
        sub <- primaryCancerDiagnosisData[primaryCancerDiagnosisData$anatomic_site %in% 
                                                      c(rows),]
        
        fig <- sub %>% dplyr::group_by(anatomic_site) %>% 
          dplyr::do(p = plotly::plot_ly(., x = ~mstatus,type = "histogram",
                                        color = ~dplyr::vars(anatomic_site),
                                        name = ~anatomic_site)) %>% 
          plotly::subplot(nrows = 1, shareX = FALSE, shareY = TRUE) %>% 
          plotly::layout(
            xaxis = list(categoryorder = "trace", title = "Metastatic Distribution"),             
            yaxis = list(title="Frequency"), 
            font = list(family = "Assistant"),
            title = list(text = data_last_sync_date, y = 0, x = 0.5, xanchor = "center", yanchor =  "bottom", font = list(size = 13))
            )
        
        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("consented_diagnosis_treatment_intent_status" %in% plot_name){
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        primaryCancerDiagnosisData <- getConsentDiagnosisDfForChart(demo_data_filtered)
        
        unlistVector <- unlist(strsplit(filters$plotSpecificFilterRows,","))
        rows <- sub("^\\\\s+", "", unlistVector)
        rows <- unique(rows)
            
        sub <- primaryCancerDiagnosisData[primaryCancerDiagnosisData$anatomic_site %in% 
                                                      c(rows),]
        
        fig <- sub %>% dplyr::group_by(anatomic_site) %>%
          dplyr::do(p = plotly::plot_ly(., x = ~SACT_treatment,
                                        type = "histogram",
                                        color = ~dplyr::vars(anatomic_site),
                                        name = ~anatomic_site)) %>%
          plotly::subplot(nrows = 1, shareX = FALSE, shareY = TRUE) %>% 
          plotly::layout(
            xaxis = list(title = "Treatment Status Intent"),
            yaxis = list(title = "Frequency"), 
            font = list(family = "Assistant"),
            title = list(text = data_last_sync_date, y = 0, x = 0.5, xanchor = "center", yanchor =  "bottom", font = list(size = 13))
            )
        
        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("consent_summary" %in% plot_name){
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        
        unlistVector <- unlist(strsplit(filters$plotSpecificFilter,","))
        values <- unique(sub("^\\\\s+", "", unlistVector))
        
        if(filters$plotSpecificFilterSingleColumn == "All"){
          
          defaultMetricsDfForChart <- getDefaultMetricsDfForChart(demo_data_filtered)
          
          if(length(c(values)) == 2){
            plot_data <- defaultMetricsDfForChart[
                      defaultMetricsDfForChart$Start >= filters$filterDateFrom &
                        defaultMetricsDfForChart$Start <= filters$filterDateTo,]
                    
            plot_data$type <- paste("Weekly", sep = "")
            
            cumsum_data <- data.frame("Year-Week" = plot_data$`Year-Week`,
                                          "Start" = plot_data$Start,
                                          "End" = plot_data$End,
                                          "Consent" = plot_data$Consent,
                                          "type" = paste("Cumsum", sep = ""))
            
            cumsum_data$Consent <- cumsum(plot_data$Consent)
            
            names(cumsum_data) <- c("Year-Week", "Start", "End", "Consent", "type")
            
            plot_data <- rbind(plot_data, cumsum_data)
            
            fig <- plotly::plot_ly(plot_data,
                    x = ~Start,
                    y = ~Consent,
                    color = ~type,
                    type = "scatter",
                    mode = "markers+lines",
                    text = ~Consent) %>%
              plotly::add_text(textposition = "top") %>%
              plotly::layout(yaxis = list(title = "Frequency"),
                    xaxis = list(title = paste("Time Weeks", data_last_sync_date, sep = "\\n\\n")),
                    font = list(family = "Assistant")
                    )
          }
          else if(c(values) %in% "Weekly" & length(c(values)) == 1){
            
            plotData <- defaultMetricsDfForChart[
              defaultMetricsDfForChart$Start >= filters$filterDateFrom &
                defaultMetricsDfForChart$Start <= filters$filterDateTo,]
            
            fig <- plotly::plot_ly(plotData,
                    x = ~Start,
                    y = ~Consent,
                    type = "scatter",
                    mode = "markers+lines",
                    text = ~Consent) %>%
              plotly::add_text(textposition = "top") %>%
              plotly::layout(yaxis = list(title = "Frequency"),
                    xaxis = list(title = paste("Time Weeks", data_last_sync_date, sep = "\\n\\n")),
                    font = list(family = "Assistant")
                    )
            
          } 
          else if(c(values) %in% "Cumulative" & length(c(values)) == 1){
            
            plotData <- defaultMetricsDfForChart[
              defaultMetricsDfForChart$Start >= filters$filterDateFrom &
                defaultMetricsDfForChart$Start <= filters$filterDateTo,]
            
            fig <- plotly::plot_ly(plotData,
                    x = ~Start,
                    y = ~cumsum(Consent),
                    type = "scatter",
                    mode = "markers+lines",
                    text = ~cumsum(Consent)) %>%
              plotly::add_text(textposition = "top") %>%
              plotly::layout(yaxis = list(title = "Frequency"),
                    xaxis = list(title = paste("Time Weeks", data_last_sync_date, sep = "\\n\\n")),
                    font = list(family = "Assistant")
                    )
          } 
          
        } else {
          
          unlistVector <- unlist(strsplit(filters$plotSpecificFilterRows,","))
          rows <- sub("^\\\\s+", "", unlistVector)
          rows <- unique(rows)
        
          metricsDataForChart <- getMetricsDfForCharts(demo_data_filtered, filters$plotSpecificFilterSingleColumn, 
                                      c(rows))
          
          if(length(c(values)) == 2){
            plot_data <- metricsDataForChart[
              metricsDataForChart$Start >= filters$filterDateFrom &
                metricsDataForChart$Start <= filters$filterDateTo,]
            
            temp_type <- plot_data$type
            
            plot_data$type <- paste("Weekly-", plot_data$type, sep = "")
            
            cumsum_data <- data.frame("Year-Week" = plot_data$`Year-Week`,
                                      "Start" = plot_data$Start,
                                      "End" = plot_data$End,
                                      "Consent" = plot_data$Consent,
                                      "type" = paste("Cumsum-", temp_type, sep = ""))
            
            cumsum_data$Consent <- cumsum(plot_data$Consent)
            
            names(cumsum_data) <- c("Year-Week", "Start", "End", "Consent", "type")
            
            both_data <- rbind(plot_data, cumsum_data)
            
            fig <- plotly::plot_ly(data = both_data, x = ~Start, y = ~Consent,
                            color = ~type,
                            type = "scatter", mode = "markers+lines",
                            text = ~Consent) %>%
              plotly::add_text(textposition = "top") %>%
              plotly::layout(yaxis = list(title = "Frequency"),
                    xaxis = list(title = paste("Time Weeks", data_last_sync_date, sep = "\\n\\n")),
                    font = list(family = "Assistant")
                    )
          }
          else if(c(values) %in% "Weekly" & length(c(values)) == 1){
            
            plotData <- metricsDataForChart[
              metricsDataForChart$Start >= filters$filterDateFrom &
                metricsDataForChart$Start <= filters$filterDateTo,]
            
            fig <- plotly::plot_ly(data = plotData, x = ~Start, y = ~Consent,
                                  color = ~type,
                                  type = "scatter", 
                                  mode = "markers+lines",
                                  text = ~Consent) %>%
              plotly::add_text(textposition = "top") %>%
              plotly::layout(yaxis = list(title = "Frequency"),
              xaxis = list(title = paste("Time Weeks", data_last_sync_date, sep = "\\n\\n")),
              font = list(family = "Assistant"))
            
          } 
          else if(c(values) %in% "Cumulative" & length(c(values)) == 1){
            
            plotData <- metricsDataForChart[
              metricsDataForChart$Start >= filters$filterDateFrom &
                metricsDataForChart$Start <= filters$filterDateTo,]
            
            fig <- plotly::plot_ly(data = plotData, x = ~Start, y = ~cumsum,
                                  color = ~type,
                                  type = "scatter", 
                                  mode = "markers+lines",
                                  text = ~cumsum) %>%
              plotly::add_text(textposition = "top") %>%
              plotly::layout(yaxis = list(title = "Frequency"),
              xaxis = list(title = paste("Time Weeks", data_last_sync_date, sep = "\\n\\n")),
              font = list(family = "Assistant"))
          } 
        }
        
        
        fig
      }, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("consent_metrics_consent_weeks_table" %in% plot_name){
      
      if(!(output_type %in% c("html"))){
        print_rtable <- "knitr::kable(metricsDataForChart)"
      } else {
        print_rtable <- "knitr::kable(metricsDataForChart)"
      }
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({
        library(knitr)
        metricsDataForChart <- getDefaultMetricsDfForChart(demo_data_filtered)',
                       print_rtable,
                       
                       '}, error=function(e) {
        return(e)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    if("consent_patient_characteristics_rtables" %in% plot_name){
      if(!(output_type %in% c("html"))){
        print_rtable <- "rtables::build_table(lyt, consentedRtableData)"
      } else {
        print_rtable <- "rtables::as_html(rtables::build_table(lyt, consentedRtableData),
            class_table = 'table table-condensed table-hover')"
      }
      
      content <- paste('```{r warning = FALSE, results = "asis"}',
                       '',
                       'filters <- filter[', count, ',]',
                       'printSummary(filters)',
                       'demo_data_filtered <- filterDataHandler(demo_data, filters)',
                       '',
                       'tryCatch({

        library(rtables)
        library(tern)
        consentedRtableData <- consentedRtable(demo_data_filtered)
      
        unlistVector <- unlist(strsplit(filters$plotSpecificFilterRows,","))
        rows <- sub("^\\\\s+", "", unlistVector)
        rows <- unique(rows)
        
        unlistVector <- unlist(strsplit(filters$plotSpecificFilterColumns,","))
        columns <- sub("^\\\\s+", "", unlistVector)
        columns <- unique(columns)
        
        lyt <- rtables::basic_table(show_colcounts = TRUE) |>
          rtables::split_cols_by(var = c(columns)) |>
          tern::summarize_vars(c(rows), var_labels = c(rows))',
                       print_rtable,
                       
                       
                       
                       '}, error=function(e) {
        return(NULL)
      })
```
', sep = '\n')
      
      content_list <- append(content_list, content)
    }
    
    count <- count + 1
  }
  
  if((output_type %in% c("html"))){
    content <- '```{css, echo=FALSE}
.rtables-titles-block {
  display: none !important;
}
```'
  content_list <- append(content_list, content)
  }

return(content_list)
}

downloadCSV <- function(input_data){
  shiny::withProgress(
    message = paste0("Preparing CSV"),
    value = 0,
    {
      shiny::incProgress(1/10)
      Sys.sleep(1)
      shiny::incProgress(5/10)
      
      #creating temporary directory
      temporary_directory <- tempdir()
      
      csv_file <- "temp_data.csv"
      temp_file_path <- file.path(temporary_directory, csv_file)
      
      write.csv(input_data, file = temp_file_path)
      return(temp_file_path)
    }
  )
}

generate_report_filter_fields <- c(filterMinAge = c('NULL'),
                                   filterMaxAge = c('NULL'),
                                   filterSex = c('NULL'),
                                   filterRace = c('NULL'),
                                   filterSiteName = c('NULL'),
                                   filterPcp = c('NULL'),
                                   plotSpecificFilter = c("NULL"),
                                   plotSpecificFilterRows = c("NULL"),
                                   plotSpecificFilterColumns = c("NULL"),
                                   filterDateFrom = c("NULL"),
                                   filterDateTo = c("NULL"),
                                   filterIncludeExcludeNAIndications = c("NULL"),
                                   filterConsentStatus = c("NULL"),
                                   filterAnatomic = c("NULL"),
                                   filterMorphology = c("NULL"),
                                   plotName = c('NULL'))
