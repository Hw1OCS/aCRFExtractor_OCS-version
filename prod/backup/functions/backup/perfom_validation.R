
perform_validation <- function(filename_manualPages = NULL, tabsheet_manualPages = NULL,
                               ds_machinePages = NULL, 
                               compOut_dir = NULL) {

  #################################################
  ## Preprocess dataset with manually created.   ##
  #################################################
  # defOrig_fileName <- "./input files/Define specs CDISC SDTM completed - manually.xlsx"
  # sheetVarTab_name <- "Variables"
  
  defOrig_fileName <- filename_manualPages
  sheetVarTab_name <- tabsheet_manualPages
  
  
  ## Read Define specs with page numbers populated manually
  defineOrigin_variableTab_manual <- readxl::read_excel(path = file.path(defOrig_fileName), 
                                                        sheet = sheetVarTab_name, 
                                                        col_names = TRUE)
  
  defineOrigin_variableTab_manual_CRF <- defineOrigin_variableTab_manual %>% 
    dplyr::filter(Origin %in% c("CRF")) %>% 
    dplyr::select(Order, Dataset, Variable, Pages) 
  
  
  ################################################
  ## Compare manually created dataset against   ##
  ## machine created dataset.                   ##
  ################################################
  res_comparison <- compareDF::compare_df(df_new = ds_machinePages, df_old = defineOrigin_variableTab_manual_CRF, group_col = c("Pages"))
  
  # res_comparison$html_output
  
  ## preprocess comparison result before export
  comp_df <- dplyr::tbl_df(res_comparison$comparison_df)
  
  chg_summary_df <- as.data.frame(res_comparison$change_summary)
  chg_summary <- t(chg_summary_df)
  colnames(chg_summary) <- rownames(chg_summary_df)
  
  out_comparison <- list(comp_df, chg_summary)
  
  ## export comparison report
  # filename_out <- file.path(paste("./../../../04 Output/comparison report_", Sys.Date(), ".txt", sep = ""))
  filename_out <- file.path(paste(compOut_dir, "/comparison report_", Sys.Date(), ".txt", sep = ""))
  cat(capture.output(print(out_comparison, row.names = FALSE), file=filename_out))
  
  print("")
  print("<!-- ###############################################################################  -->")
  print(paste("To see comparison report, please go to this path: ", filename_out, sep = ""))
  print("<!-- ###############################################################################  -->")
}
  