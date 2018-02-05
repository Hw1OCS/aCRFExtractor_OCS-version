#' The program performs validation test for us by comparing two files, i.e., a file with manually created page numbers against the one created by the program (pkg:aCRFPageMinner).
#' 
#' @description The program assumes that an MS Excel file for manually created file, and a CSV file for the machine created file.
#'
#' @param filename_manualPages The filename of the manually created (an MS Excel) file.
#' @param tabsheet_manualPages The tabsheet of the manually created (an MS Excel) file.
#' @param filename_machinePages The filename of the machine created (a CSV) file.#'
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.worku@ocs-consulting.com>
#'
#'


perform_validation <- function(filename_manualPages = NULL, tabsheet_manualPages = NULL,
                               filename_machinePages = NULL) {

  ###########################
  ## Import input files.   ##
  ###########################
  
  ## import manual file
  defOrig_fileName <- filename_manualPages
  sheet_name <- tabsheet_manualPages
  
  defineOrigin_manual <- readxl::read_excel(path = file.path(defOrig_fileName), 
                                            sheet = sheet_name, 
                                            col_names = TRUE)
  
  ## import machine file
  defineOrigin_machine <- readr::read_csv(file = filename_machinePages, col_names = T)
  
  
  ####################################
  ## Preprocess imported dataset.   ##
  ####################################
  if ( tabsheet_manualPages == "ValueLevel" ) {
    ## -- filter manual page
    defineOrigin_manual_CRF <- defineOrigin_manual %>% 
      dplyr::filter(Origin %in% c("CRF")) %>% 
      dplyr::select(Order, Dataset, Variable, "Where Clause", Pages) 
    
    ## remove space in column names
    names(defineOrigin_manual_CRF) <- names(defineOrigin_manual_CRF) %>%
      stringr::str_replace_all(pattern = "\\s+", replacement = "")
    
    ## -- filter machine file
    defineOrigin_machine_final <- defineOrigin_machine %>%
      dplyr::select(Order, Dataset, Variable, WhereClause, Pages)
  } 
  else {
    ## filter manual page
    defineOrigin_manual_CRF <- defineOrigin_manual %>% 
      dplyr::filter(Origin %in% c("CRF")) %>% 
      dplyr::select(Order, Dataset, Variable, Pages) 
    
    ## filter machine file
    defineOrigin_machine_final <- defineOrigin_machine %>% 
      dplyr::select(Order, Dataset, Variable, Pages)
  }
  
  ################################################
  ## Compare manually created dataset against   ##
  ## machine created dataset.                   ##
  ################################################
  res_comparison <- compareDF::compare_df(df_new = defineOrigin_machine_final, 
                                          df_old = defineOrigin_manual_CRF, 
                                          group_col = c("Pages"))
  
  
  ##################################################
  ## Compare files and then export the finding.   ##
  ##################################################
  ## preprocess comparison result before export
  comp_df <- dplyr::tbl_df(res_comparison$comparison_df)
  
  chg_summary_df <- as.data.frame(res_comparison$change_summary)
  chg_summary <- t(chg_summary_df)
  colnames(chg_summary) <- rownames(chg_summary_df)
  
  out_comparison <- list(comp_df, chg_summary)
  
  ## export comparison report
  compOut_dir <- stringr::str_extract(string = filename_manualPages, pattern = "\\.\\/(.*?)\\/")
  studyid_out <- stringr::str_extract(string = filename_machinePages, pattern = "\\((.*?)\\)")
  filename_out <- file.path(paste(compOut_dir, "comparison report for ", tabsheet_manualPages, " tab ", studyid_out, "_", Sys.Date(), ".txt", sep = ""))
  
  cat(capture.output(print(out_comparison, row.names = FALSE), file=filename_out))
  
  print("")
  print("<!-- ###############################################################################  -->")
  print(paste("To see comparison report, please go to this path: ", filename_out, sep = ""))
  print("<!-- ###############################################################################  -->")
}
  