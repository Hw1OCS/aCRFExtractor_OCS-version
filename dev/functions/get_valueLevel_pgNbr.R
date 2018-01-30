#' This is the main program which does the page extraction from aCRF page, related to Value level tab. 
#'
#' @param crf_pageIn The preprocessed dataset with all text read from aCRF using pkg::pdftools.
#' @param defineOrigin_variableTab The preprocessed dataset with define specs info.
#' @param until_pgNbr Included for debugging purpose.
#'
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.worku@ocs-consulting.com>
#'
#'


get_valueLevel_pgNbr <- function (crf_pageIn = NULL,
                                  defineOrigin_valueLevelTab = NULL, 
                                  until_pgNbr = NULL) {
  
  message("Extracting page numbers for Value Level tab...")
  
  ######################################
  ## get total number of CRF pages.   ##
  ######################################
  if (!is.null(until_pgNbr)) {
    total_crf_pages <- until_pgNbr
  }
  else {
    total_crf_pages <- dim(crf_pageIn)[1]
  }
  
  ######################################
  ## Start extracting page numbers.   ##
  ######################################
  ## output holder
  ds_out <- list()
  
  ## Extract last string from "Where Clause" column
  ds_lastString <- defineOrigin_valueLevelTab %>%
    dplyr::mutate(last_string = stringr::str_match(string = WhereClause, pattern = "(EQ\\.)(.*?)-")[,3] ) %>%          ## extract a value that exist between two strings
    dplyr::mutate(row_nbr = 1:n())                 ## add row number index to dataframe
  
  ## loop through data rows in variable tab dataset and grab page numbers
  total_rows <- dim(ds_lastString)[1]
  
  ## Set progress bar
  pb <- txtProgressBar(min = 1, max = total_rows, style = 3)
  
  for (curr_row in 1:total_rows) {
    # print(curr_row)
    
    ## filter row dataset
    ds_row <- ds_lastString %>%
      dplyr::filter(dplyr::row_number(row_nbr) %in% curr_row) %>%
      as.data.frame()
    
    #####################
    ## Define query.   ##
    #####################
    ## get last string obtained from "Where Clause" column
    last_string <- ds_lastString %>%
      dplyr::filter(dplyr::row_number(row_nbr) %in% curr_row) %>%
      dplyr::pull(last_string) 
    
    ## get domain
    domain <- ds_lastString %>%
      dplyr::filter(dplyr::row_number(row_nbr) %in% curr_row) %>%
      dplyr::pull(Dataset) 
    
    ## Depending on domain, define different query
    if( stringr::str_detect(string = domain, pattern = "^((SUPP)\\w+)") ) {
      ####################################################################################
      ## e.g., RACEOTH in SUPPDM (see sample aCRF, pp 6/22)
      ## e.g., EGCLSIG=N in SUPPEG (see sample aCRF, pp 12/22)
      ## e.g., RTRINIT in SUPPQS (however, the actual domain in the dataset is SUPPQSMM, 
      ##       see sample aCRF, pp 13/22)
      ## e.g., RACE1, RACE2, etc (see sample aCRF, pp 6/22)   
      ### (RACE1)(\\=\\w+)?(\\,\\s+\\w+\\,\\s+\\w+\\.)?\\s+(in)\\s+((SUPP)\\w+)
      ### (\\w+\\,\\s+)?(RACE2)(\\=\\w+)?(\\,\\s+\\w+\\,\\s+\\w+\\.)?(\\,\\s+\\w+\\.)?
      ##  \\s+(in)\\s+((SUPP)\\w+)
      ####################################################################################
      query1 <- paste("(\\w+\\,\\s+)?(", last_string, ")(\\=\\w+)?(\\,\\s+\\w+\\,\\s+\\w+\\.)?(\\,\\s+\\w+\\.)?\\s+(in)\\s+(", "(SUPP)\\w+", ")", sep = "")  
    } 
    else {
      ####################################################################################
      ## e.g., PEORRES when PETESTCD=PE01 (see sample aCRF, pp 10/22)
      ## e.g., VSORRES / VSORRESU when VSTESTCD = SYSBP, DIABP (see sample aCRF, pp 11/22)
      ## e.g., NOT COVERED: The equal sign is sometime chopped down, so it is optional 
      ##       (see sample aCRF, pp 15/22 for "CSDD15"). The reason to ignore this case 
      ##       is because otherwise the program includes single variable (e.g., WEIGHT 
      ##       in pp. 15 for VS domain).
      ####################################################################################
      # query1 <- paste("(\\s+)?(\\=)?(\\s+)?(\\w+\\,\\s+)?(", last_string, ")", sep = "")                                                                         
      query1 <- paste("(\\s+)?(\\=)(\\s+)?(\\w+\\,\\s+)?(", last_string, ")", sep = "")                                                                         
    }
    
    ## page number holder
    pages_all <- character()
    
    #########################################################
    ## loop through the whole aCRF and grab page numbers   ##
    ## corresponding to the last string.                   ##
    #########################################################
    for (i in seq(1:total_crf_pages)) {
      
      ## filter specific page number
      crf_page <- crf_pageIn %>%
        dplyr::filter(page_nbr %in% i)
      
      ## get target column for page extraction
      crf_page_txt <- crf_page %>%
        dplyr::pull(crf_txt) %>% 
        stringr::str_trim() %>%
        stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") %>%
        unlist()
      
      ## check if SDTM variable exist in a page
      if ( any(stringr::str_detect(crf_page_txt, query1)) ) {
        pages_all <- c(pages_all, i)
      }
    }
    
    ## collapse page numbers, if there are more than one pages
    pages_all_collapse <- paste(pages_all, collapse = " ")
    
    ## add page numbers to existing data frame. Note that only one row is expected here
    ds_row$Pages <- pages_all_collapse
    
    ## add row dataset to output list
    ds_out[[curr_row]] <- ds_row
    
    # update progress bar
    setTxtProgressBar(pb, curr_row)
  }
  
  ## Close progress bar
  close(pb)
  
  #############################
  ## Combine all outputs.    ##
  #############################
  ds_out_combined <- dplyr::bind_rows(ds_out)
  
  return(ds_out_combined)
}




