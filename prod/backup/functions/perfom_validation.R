#' The program performs validation test for us by comparing two files, i.e., a file with manually created page numbers against the one created by the program (pkg:aCRFPageMinner).
#' 
#' @description The program assumes that an MS Excel file for manually created file, and a CSV file for the machine created file.
#'
#' @param filename_manualPages The filename of the manually created (an MS Excel) file.
#' @param tabsheet_manualPages The tabsheet of the manually created (an MS Excel) file.
#' @param filename_machinePages The filename of the machine created (a CSV) file.
#' @param compOut_dir The directory name for storing the comparison report.
#'
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.worku@ocs-consulting.com>
#'
#'


perform_validation <- function(filename_manualPages = NULL, tabsheet_manualPages = NULL,
                               filename_machinePages = NULL, 
                               compOut_dir = NULL) {

  #########################################
  ## Preprocess manually created file.   ##
  #########################################
  defOrig_fileName <- filename_manualPages
  sheetVarTab_name <- tabsheet_manualPages
  
  
  ## Read Define specs with page numbers populated manually
  defineOrigin_variableTab_manual <- readxl::read_excel(path = file.path(defOrig_fileName), 
                                                        sheet = sheetVarTab_name, 
                                                        col_names = TRUE)
  
  defineOrigin_variableTab_manual_CRF <- defineOrigin_variableTab_manual %>% 
    dplyr::filter(Origin %in% c("CRF")) %>% 
    dplyr::select(Order, Dataset, Variable, Pages) 
  
  
  #######################################
  ## Preprocess machine created file   ##
  #######################################
  defineOrigin_variableTab_machine <- readr::read_csv(file = filename_machinePages, col_names = T)
  
  
  ################################################
  ## Compare manually created dataset against   ##
  ## machine created dataset.                   ##
  ################################################
  res_comparison <- compareDF::compare_df(df_new = defineOrigin_variableTab_machine, df_old = defineOrigin_variableTab_manual_CRF, group_col = c("Pages"))
  
  
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
  filename_out <- file.path(paste(compOut_dir, "/comparison report_", Sys.Date(), ".txt", sep = ""))
  cat(capture.output(print(out_comparison, row.names = FALSE), file=filename_out))
  
  print("")
  print("<!-- ###############################################################################  -->")
  print(paste("To see comparison report, please go to this path: ", filename_out, sep = ""))
  print("<!-- ###############################################################################  -->")
}
  