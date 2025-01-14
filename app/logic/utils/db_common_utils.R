#* @Path app/logic/utils/db_common_utils.R

#************************ START: LOADING REQUIRED PACKAGES AND RESPECTIVE CLASSES **********************#
# Import config file to get database credentials.
box::use(config)

box::use(
  ./common_utils[cleanUpRaceCodes, replaceNANullToNoData, getMetastaticStatus],
)

box::use(
  utils[read.csv],
  readr[read_rds]
)
#************************ END: LOADING REQUIRED PACKAGES AND RESPECTIVE CLASSES **********************#


#************************ START: DATABASE CONFIGURATION / DETAILS **********************#

#************************ END: DATABASE CONFIGURATION / DETAILS ************************#

#* @Description: Fetching last sync date
#* @return: list
lastSyncDate <- function(){
  
  tryCatch({
    
    # query <- "SELECT * FROM sync_date;"
    # 
    # fetched_data <- DBI::dbGetQuery(database_connection, query)
    # 
    # if (is.null(fetched_data))
    #   return(NULL)
    
    return("2023-08-3 04:25:39")
  }, error = function(err) {
    if(config$get("show_error")){
      return(err)
    } else {
      return(NULL)
    }
  })
}

#* @Description: Fetching data from optimize DB.
#* @return: list
mainDf <- function(){
  
  tryCatch({
    
    if(config$get("read_external_datafile")){
      fetched_data <- read.csv("./data/demo_data_used_to_load_dashboard.csv")
      
      # saveRDS(overviewdf, file="./data/dataframe.rds")
    }
    
    fetched_data$race <- cleanUpRaceCodes(fetched_data$race)
    
    metastatic_status_renamed_values <- getMetastaticStatus(fetched_data)
    fetched_data$m_status <- metastatic_status_renamed_values$mstatus
    
    fetched_data$sex <- replaceNANullToNoData(fetched_data$sex)
    fetched_data$race <- replaceNANullToNoData(fetched_data$race)
    fetched_data$site_name <- replaceNANullToNoData(fetched_data$site_name)
    fetched_data$assigned_md <- replaceNANullToNoData(fetched_data$assigned_md)
    
    fetched_data$icd03_code <- replaceNANullToNoData(fetched_data$icd03_code)
    
    fetched_data$anatomic_site_code <- replaceNANullToNoData(fetched_data$anatomic_site_code)
    fetched_data$topography_code <- replaceNANullToNoData(fetched_data$topography_code)
    
    fetched_data$anatomic_site <- replaceNANullToNoData(fetched_data$anatomic_site)
    fetched_data$topography_description <- replaceNANullToNoData(fetched_data$topography_description)
    fetched_data$icdo3_description <- replaceNANullToNoData(fetched_data$icdo3_description)
    
    fetched_data$icd03_morphology_code <- sapply(strsplit(split = '-', fetched_data$icd03_code),function(i){i[1]})
    
    fetched_data$sex[fetched_data$sex == "male"] <- "Male"
    fetched_data$sex[fetched_data$sex == "female"] <- "Female"
    
    fetched_data <- fetched_data[fetched_data$site_name != "No Data Available", ]
    
    # This removes duplicated patient_uuids
    if(sum(lengths(fetched_data$patient_uuid)) > 0){
      fetched_data <- fetched_data[!duplicated(fetched_data$patient_uuid), ]
    }
    
    if(is.null(fetched_data))
      return(NULL)
    
    return(fetched_data)
  }, error = function(err) {
    if(config$get("show_error")){
      return(err)
    } else {
      return(NULL)
    }
  })
}

getICD03Codes <- function(){
  tryCatch({
    query <- "SELECT * FROM icd_o_3_hierarchy;"
    
    fetched_data <- DBI::dbGetQuery(database_connection, query)
    
    if (is.null(fetched_data))
      return(NULL)
    
    return(fetched_data)
  }, error = function(err) {
    if(config$get("show_error")){
      return(err)
    } else {
      return(NULL)
    }
  })
}

if(TRUE){
  
  overviewdf <- mainDf()
  
  uniquePcpNames <- unique(overviewdf$assigned_md)
  uniquePcpNames[uniquePcpNames == "No Data Available"] <- 'NA'
  
  unique_anatomic_codes <- unique(overviewdf$anatomic_site_code)
  
  unique_morphology_codes <- gsub('No Data Available', 'NA', unique(unique(overviewdf$icd03_morphology_code)))
  
  unique_anatomic_code_name <- gsub('No Data Available', 'NA', unique(unique(overviewdf$anatomic_site)))
  
  unique_consent_status <- unique(overviewdf$consent_status)
  
  unique_race <- gsub('No Data Available', 'NA', unique(overviewdf$race))
  
  unique_site_name <- c(unique(overviewdf$site_name), 'NA')
  
  unique_pcp_full_name <- gsub('No Data Available', 'NA', unique(overviewdf$assigned_md))
  
  data_sync_date_for_plots <- paste("Data as of", lastSyncDate(), "UTC", sep = " ")
}
