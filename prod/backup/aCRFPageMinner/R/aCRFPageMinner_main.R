#' Extracts page numbers of SDTM variables available in manually created aCRF file. The output is used for 
#' creation of Define XML file.
#' 
#' The input files (e.g., studyid, aCRF file, and Define Origin file) are taken from a user via console.
#'
#' @examples
#' ## library(aCRFPageMinner)
#' ## run_aCRFPageMinner()
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.worku@ocs-consulting.com>
#'
#'

run_aCRFPageMinner <- function() {
  
  ## -------------------------------------------------------- ##
  ## <!-- Configure dependent packages.                 -->   ##
  ## -------------------------------------------------------- ##
  ## pdftools to read a pdf file (e.g., aCRF)
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Please install pdftools: install.packages('pdftools'); library(pdftools)")
  }
  
  ## readxl to read an MS Excel file (e.g., Define specs)
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Please install readxl: install.packages('readxl'); library(readxl)")
  }
  
  ## stringr for patter matching
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Please install stringr: install.packages('stringr'); library(stringr)")
  }
  
  ## tidyr for unnesting
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Please install tidyr: install.packages('tidyr'); library(tidyr)")
  }
  
  ## magrittr to use pipe operator
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Please install magrittr: install.packages('magrittr'); library(magrittr)")
  }
  
  ## purrr to convert multiple variables type from factor to character
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Please install purrr: install.packages('purrr'); library(purrr)")
  }
  
  ## plyr to sort dataframe, both ascending and descending
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("Please install plyr: install.packages('plyr'); library(plyr)")
  }
  
  ## tidyselect
  if (!requireNamespace("tidyselect", quietly = TRUE)) {
    stop("Please install tidyselect: install.packages('tidyselect'); library(tidyselect)")
  }
  
  ## compareDF to compare two dataframe
  if (!requireNamespace("compareDF", quietly = TRUE)) {
    stop("Please install compareDF: install.packages('compareDF'); library(compareDF)")
  }
  
  ## -- load all required libraries, after all pkgs are installed
  if (!requireNamespace("pacman", quietly = TRUE)) {
    stop("Please install pacman: install.packages('pacman'); library(pacman)")
  }
  
  pacman::p_load(pdftools, stringr, tidyr, magrittr, purrr, plyr, tidyselect, compareDF)
  
  
  ## Fill studyId info
  # studyid <- readline(prompt = "Enter StudyId: ")
  assign(x = "studyid", value = readline(prompt = "Enter StudyId: "), envir = .GlobalEnv)
  
  
  ## ---------------------------------------------- ##
  ## <!-- Step 1. Import required input files.  --> ##
  ## ---------------------------------------------- ##
  ###########################
  ## Import aCRF pdf file. ##
  ###########################
  print("Import aCRF pdf file. Thanks!")
  aCRF_fileName <- file.choose()
  pdf_fileName <- aCRF_fileName
  # assign(x = "aCRF_fileName", value = file.choose(), envir = .GlobalEnv)
  
  # pdf_fileName = "./input files/blankcrf.pdf"
  
  ## read aCRF pdf file
  pages <- pdftools::pdf_info(pdf_fileName)$pages
  crf_raw <- pdftools::pdf_text(pdf_fileName)
  
  
  ###############################
  ## Import Define specs file. ##
  ###############################
  print("Import Define specs which is an MS Excel (i.e., xls/xlsx) file. Thanks!")
  defOrig_fileName <- file.choose()
  # defOrig_fileName <- "./input files/Define specs CDISC SDTM completed_without page numbers.xlsx"
  # assign(x = "defOrig_fileName", value = file.choose(), envir = .GlobalEnv)
  
  sheetVarTab_name <- "Variables"
  
  defineOrigin_variableTab <- readxl::read_excel(path = file.path(defOrig_fileName), 
                                                 sheet = sheetVarTab_name, 
                                                 col_names = TRUE)
  
  ## Set output destination
  assign(x = "output_dir", value = dirname(defOrig_fileName), envir = .GlobalEnv)
  
  # ## load library
  # source("./dev/functions/load_library.R")
  # 
  # ## load custom source codes
  # source("./dev/functions/preprocess_crf_fn.R")
  # source("./dev/functions/get_sdtmVars_pgNbr_fn.R")
  # source("./dev/functions/collapse_pgNbrs.R")
  # source("./dev/functions/perfom_validation.R")
  
  
  ## ------------------------------------------------------------- ##
  ## <!-- Step 2: Preprocess imported aCRF and Define specs.   --> ##
  ## ------------------------------------------------------------- ##
  ## Preprocess aCRF data
  crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw)
  
  ## Preprocess Define specs data
  defineOrigin_variableTab_preprocess <- defineOrigin_variableTab %>%
    dplyr::select(Order, Dataset, Variable, Origin, Pages) %>%
    dplyr::filter(Origin %in% c("CRF"))
  
  ## Get list of domains that was obtained in aCRF
  domain_DefOrigin_VarTab_filtered <- defineOrigin_variableTab_preprocess %>%
    dplyr::distinct(Dataset) %>%
    dplyr::pull()
  
  
  ## ---------------------------------------- ##
  ## <!-- Step 3: Extract page numbers.   --> ##
  ## ---------------------------------------- ##
  crf_out_varTab <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, 
                                       defineOrigin_variableTab = defineOrigin_variableTab_preprocess,
                                       domain_list = domain_DefOrigin_VarTab_filtered)
  
  
  ## ---------------------------------------------------- ##
  ## <!-- Step 4: Create output dataset for export.   --> ##
  ## ---------------------------------------------------- ##
  
  
  
  ######################################################
  ## Get Order variable generated from Pinnacle 21.   ##
  ## - It will be used to sort final output dataset.  ##
  ######################################################
  # defineOrigin_variableTab_Order <- defineOrigin_variableTab %>%
  defineOrigin_variableTab_Order <- defineOrigin_variableTab_preprocess %>%
    dplyr::select(Order, Dataset, Variable) %>%
    dplyr::rename(tmp_Order = Order) %>%
    dplyr::mutate(Order = as.numeric(tmp_Order)) %>%
    dplyr::select(Order, Dataset, Variable)
  
  # crf_out_varTab_collapsed_rename <- crf_out_varTab_collapsed_mdf %>%
  crf_out_varTab_order <- crf_out_varTab %>%
    purrr::map_if(is.factor, as.character) %>%
    dplyr::as_data_frame() %>%
    dplyr::rename(Dataset = domain) %>%
    dplyr::rename(Variable = sdtm_vars)                             ## rename column name: New = Old
  
  # crf_out_varTab_merged <- crf_out_varTab_order %>%
  crf_out_varTab_order_merged <- crf_out_varTab_order %>%
    dplyr::inner_join(defineOrigin_variableTab_Order,
                      by = c("Dataset", "Variable"))
  
  #########################################################
  ## Collapse number of pages by Domain and Variable.    ##
  #########################################################
  crf_out_varTab_collapsed <- collapse_pgNbrs(dsin = crf_out_varTab_order_merged, 
                                              domain_list = domain_DefOrigin_VarTab_filtered)
  
  ## Get final output dataset
  crf_out_varTab_final <- crf_out_varTab_collapsed %>%
    dplyr::select(Order, Dataset, Variable, Pages)              ## rearrange columns, i.e., bring Order to the first column
  
  ## export final dataset
  # filename_out <- file.path(paste("./../../../04 Output/page numbers for Variables tab_", Sys.Date(), ".csv", sep = ""))
  tmp_filename <- paste("page numbers for Variables tab (studyid = ", studyid, ")_", Sys.Date(), ".csv", sep = "")
  filename_out <- file.path(output_dir, tmp_filename)
  
  readr::write_csv(x = crf_out_varTab_collapsed, path = filename_out)
  
  print("")
  print("<!-- #################################################################################################  -->")
  print(paste("To see the extracted page numbers from aCRF, please go to this path: ", filename_out, sep = ""))
  print("<!-- #################################################################################################  -->")
  
  
  #########################################################
  ## Perform validation by comparison manually created   ##
  ## page numbers and the one created by the program.    ##
  #########################################################
  # perform_validation(filename_manualPages = "./input files/Define specs CDISC SDTM completed - manually.xlsx", 
  #                    tabsheet_manualPages = "Variables", 
  #                    ds_machinePages = crf_out_varTab_final, compOut_dir = "../../../04 Output")
  
  
}