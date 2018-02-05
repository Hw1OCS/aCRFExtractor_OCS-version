#' This is the main program which does the page extraction from aCRF page, related to Variable Tab. 
#'
#' @param crf_pageIn The preprocessed dataset with all text read from aCRF using pkg::pdftools.
#' @param defineOrigin_variableTab The preprocessed dataset with define origin info.
#' @param domain_list List of SDTM domains read from aCRF.
#' @param until_pgNbr Included for debugging purpose.
#'
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.worku@ocs-consulting.com>
#'
#'


get_sdtmVars_pgNbr <- function (crf_pageIn = NULL,
                                defineOrigin_variableTab = NULL, 
                                domain_list = NULL, 
                                until_pgNbr = NULL) {
  
  message("Extracting page numbers for Variable tab...")
  
  ## get total number of CRF pages
  if (!is.null(until_pgNbr)) {
    # print("Minning some pages in the aCRF file")
    total_crf_pages <- until_pgNbr
  }
  else {
    # print("Minning all pages in the aCRF file")
    total_crf_pages <- dim(crf_pageIn)[1]
  }
  
  ## create an empty dataset to store result
  crf_out <- data.frame(a = numeric(), b = character(), c = character())
  # crf_out_strangeAll <- data.frame(a = numeric(), b = character(), c = character(), d = character())
  
  ####################################################
  ## get SDTM vars and corresponding page number.   ##
  ####################################################
  # ## Set progress bar
  # pb <- txtProgressBar(min = 1, max = length(domain_list), style = 3)
  # count <- 0
  
  for (domain in domain_list) {
    print(domain)
    
    ## define patter of the annotation
    # query1 <- paste("^((", domain, ")(?!\\_)\\w+)", sep = "")
    if (domain %in% c("DM")) {
      lst_dm <- c("STUDYID", "SUBJID", "SITEID", 
                  "BRTHDTC", "AGE", "AGEU",  
                  "SEX", "RACE", "ETHNIC")            ## source: IG (https://www.w3.org/wiki/images/6/61/HCLS$$F2F$$DrugSafety$SDTM_3.1_Implementation_Guide_v1_01.pdf)
      
      tmp_query1 <- paste("^(", lst_dm, ")", sep = "")
      query1 <- paste(tmp_query1, collapse = "|")
    } else {
      query1 <- paste("^((", domain, ")(?!\\_)\\w+)", sep = "")
    }
    
    
    ## loop through the whole aCRF and grab page numbers per domain
    for (i in seq(1:total_crf_pages)) {
      
      ## filter specific page number
      crf_page <- crf_pageIn %>%
        dplyr::filter(page_nbr %in% i)
      
      ## get target column for page extraction
      crf_page_txt <- crf_page %>%
        dplyr::pull(crf_txt) %>% 
        stringr::str_trim() %>%
        stringr::str_split(pattern = "\\s+") %>%
        unlist()
      
      ## check if SDTM variable exist in a page
      # if ( any(stringr::str_detect(crf_page$crf_txt, query1)) ) {
      if ( any(stringr::str_detect(crf_page_txt, query1)) ) {
        # print(i)
        # crf_page$sdtm_vars <- ifelse(stringr::str_detect(crf_page$crf_txt, query1), 
        #                              stringr::str_extract_all(crf_page$crf_txt, query1), 
        #                              "NA")
        
        sdtm_vars <- crf_page_txt %>% 
          stringr::str_extract_all(pattern = query1) %>%
          unlist()
        
        # crf_page <- tidyr::unnest(data = crf_page, sdtm_vars)   ## unlist - hard coded (pkg: tidyr)
        
        # crf_page <- subset(crf_page[, c("page_nbr", "sdtm_vars")], crf_page$sdtm_vars != "NA")
        
        # crf_page$domain <- domain
        
        ## combine all columns
        crf_page_out <- data.frame(page_nbr = i, sdtm_vars, domain)
        
        ####################################################
        ## Keep only variables whose origin is from CRF.  ##
        ####################################################
        sdtm_vars_OriginCRF <- defineOrigin_variableTab %>%
          dplyr::filter(Dataset %in% domain) %>%
          dplyr::pull(Variable)
        
        crf_page_outFinal <- crf_page_out %>%
          dplyr::filter(sdtm_vars %in% c(sdtm_vars_OriginCRF))
        
        ## concatenate domain result
        crf_out <- rbind(crf_out, crf_page_outFinal)
      }
    }
    
    ## remove duplicate page number
    crf_out <- crf_out %>%
      dplyr::distinct(domain, sdtm_vars, page_nbr)
    
    ## remove unwanted SDTM variables (e.g., VSTESTCD)
    tmp_vars_unwanted <- crf_out %>%
      dplyr::pull(sdtm_vars) %>%
      stringr::str_extract_all(pattern = "(\\w+(CD))*$") %>%
      unlist()
    
    vars_unwanted <- tmp_vars_unwanted[tmp_vars_unwanted != ""]    ## keep non-missing string values
    
    crf_out <- crf_out %>%
      dplyr::filter(!sdtm_vars %in% c(vars_unwanted))
    
    # # update progress bar
    # setTxtProgressBar(pb, count+1)
  }
  
  # ## Close progress bar
  # close(pb)
  
  return(crf_out)
}




