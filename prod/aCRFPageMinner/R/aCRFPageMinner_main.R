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

  ## dplyr for data manipulation
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install dplyr: install.packages('dplyr'); library(dplyr)")
  }

  ## readr for reading/writing data
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Please install readr: install.packages('readr'); library(readr)")
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

  ## Read Variables tab of Define specs file.
  sheetVarTab_name <- "Variables"
  defineOrigin_variableTab <- readxl::read_excel(path = file.path(defOrig_fileName),
                                                 sheet = sheetVarTab_name,
                                                 col_names = TRUE)

  ## Read Value level tab of Define specs file.
  sheetValueLevelTab_name <- "ValueLevel"
  defineOrigin_valueLevelTab <- readxl::read_excel(path = file.path(defOrig_fileName),
                                                   sheet = sheetValueLevelTab_name,
                                                   col_names = TRUE)

  ## Set output destination
  assign(x = "output_dir", value = dirname(defOrig_fileName), envir = .GlobalEnv)


  ## ------------------------------------------------------------- ##
  ## <!-- Step 2: Preprocess imported aCRF and Define specs.   --> ##
  ## ------------------------------------------------------------- ##
  ## -- Preprocess aCRF data
  crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw)

  ## -- Preprocess Variable tab dataset
  defineOrigin_variableTab_preprocess <- defineOrigin_variableTab %>%
    dplyr::select(Order, Dataset, Variable, Origin, Pages) %>%
    dplyr::filter(Origin %in% c("CRF"))

  ## Get list of domains that was obtained in aCRF from Variable Tab dataset
  domain_DefOrigin_VarTab_filtered <- defineOrigin_variableTab_preprocess %>%
    dplyr::distinct(Dataset) %>%
    dplyr::pull()

  ## -- Preprocess Value level tab dataset
  defineOrigin_valueLevelTab_preprocess <- defineOrigin_valueLevelTab %>%
    dplyr::select(Order, Dataset, Variable, "Where Clause", Origin, Pages) %>%
    dplyr::filter(Origin %in% c("CRF"))

  ## remove spaces from column names
  names(defineOrigin_valueLevelTab_preprocess) <- names(defineOrigin_valueLevelTab_preprocess) %>%
    stringr::str_replace_all(pattern = "\\s+", replacement = "")

  ## ------------------------------------------------------------------ ##
  ## <!-- Step 3a: Extract page numbers from Variable Tab dataset.  --> ##
  ## ------------------------------------------------------------------ ##
  message("pkg::aCRFPageMinner started extracting page numbers for both tabs: ", date())

  crf_out_varTab <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess,
                                       defineOrigin_variableTab = defineOrigin_variableTab_preprocess,
                                       domain_list = domain_DefOrigin_VarTab_filtered)


  ## ---------------------------------------------------------------------- ##
  ## <!-- Step 3b: Extract page numbers from Value level Tab dataset.   --> ##
  ## ---------------------------------------------------------------------- ##
  crf_out_valueLevelTab <- get_valueLevel_pgNbr(crf_pageIn = crf_preprocess,
                                                defineOrigin_valueLevelTab = defineOrigin_valueLevelTab_preprocess)


  ## ---------------------------------------------------- ##
  ## <!-- Step 4: Create output dataset for export.   --> ##
  ## ---------------------------------------------------- ##

  ######################################################
  ## Get Order variable generated from Pinnacle 21.   ##
  ## - It will be used to sort final output dataset.  ##
  ######################################################
  # defineOrigin_variableTab_Order <- defineOrigin_variableTab %>%
  defineOrigin_variableTab_Order <- defineOrigin_variableTab_preprocess %>%
    dplyr::select(Order, Dataset, Variable, Origin) %>%
    dplyr::rename(tmp_Order = Order) %>%
    dplyr::mutate(Order = as.numeric(tmp_Order)) %>%
    dplyr::select(Order, Dataset, Variable, Origin)

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

  ###############################################
  ## Export final dataset for Variable Tab.    ##
  ###############################################
  ## Reorder columns, bring Order column to first column
  crf_out_varTab_final <- crf_out_varTab_collapsed %>%
    dplyr::select(Order, Dataset, Variable, Origin, Pages)

  ## export output file
  tmp_fname_varTab <- paste("page numbers for Variable Tab (studyid = ", studyid, ")_", Sys.Date(), ".csv", sep = "")
  fname_varTab_out <- file.path(output_dir, tmp_fname_varTab)

  readr::write_csv(x = crf_out_varTab_final, path = fname_varTab_out)             ## rearrange columns, i.e., bring Order to the first column

  #################################################
  ## Export final dataset for Value Level Tab.   ##
  #################################################
  tmp_fname_valueLevelTab <- paste("page numbers for Value Level Tab (studyid = ", studyid, ")_", Sys.Date(), ".csv", sep = "")
  fname_valueLevel_out <- file.path(output_dir, tmp_fname_valueLevelTab)

  readr::write_csv(x = crf_out_valueLevelTab, path = fname_valueLevel_out)

  print("")
  print("<!-- ##############################################################################  -->")
  print(paste("To see extracted page numbers of aCRF, please follow the link below: "))
  print(paste("## Variable Tab output file: ", fname_varTab_out, sep = ""))
  print(paste("## Value Level Tab output file: ", fname_valueLevel_out, sep = ""))
  print("<!-- ##############################################################################  -->")
  print("")
}
