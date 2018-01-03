
## load library
library(pdftools)     ## to read a pdf file (e.g., aCRF)
library(readxl)       ## to read an MS Excel file (e.g., Define specs)
library(stringr)      ## for patter matching
library(tidyr)        ## for unnesting
library(magrittr)     ## to use pipe operator

## load custom source codes
source("./dev/functions/preprocess_crf_fn.R")
source("./dev/functions/get_sdtmVars_pgNbr_fn.R")

## ---------------------------------------------- ##
## <!-- Step 1. Import required input files.  --> ##
## ---------------------------------------------- ##
###########################
## Import aCRF pdf file. ##
###########################
# print("Import aCRF pdf file. Thanks!")
# aCRF_fileName <- file.choose()
# pdf_fileName <- aCRF_fileName

pdf_fileName = "./input files/blankcrf.pdf"

## read aCRF pdf file
pages <- pdftools::pdf_info(pdf_fileName)$pages
crf_raw <- pdftools::pdf_text(pdf_fileName)


###############################
## Import Define specs file. ##
###############################
# print("Import Define specs which is an MS Excel (i.e., xls/xlsx) file. Thanks!")
# defOrig_fileName <- file.choose()
defOrig_fileName <- "./input files/Define specs CDISC SDTM completed_without page numbers.xlsx"
sheetVarTab_name <- "Variables"

defineOrigin_variableTab <- readxl::read_excel(path = file.path(defOrig_fileName), 
                                               sheet = sheetVarTab_name, 
                                               col_names = TRUE)

## ------------------------------------------------------------- ##
## <!-- Step 2: Preprocess imported aCRF and Define specs.   --> ##
## ------------------------------------------------------------- ##
## Preprocess aCRF data
crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw)

## Preprocess Define specs data
defineOrigin_variableTab_preprocess <- defineOrigin_variableTab %>%
  dplyr::select(Order, Dataset, Variable, Origin, Pages) %>%
  dplyr::filter(Origin %in% c("CRF"))

## Get list of domains
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

####################################################
## Combine page numbers per domain and variable.  ##
####################################################
crf_out_varTab_collapsed <- crf_out_varTab %>%
  dplyr::group_by(domain, sdtm_vars) %>%
  dplyr::summarise(Pages = paste(page_nbr, collapse = " "))     ## use space as a delimitor / separator for the collapse

###################################################
## Get Order variable generated from Pinnacle.   ##
###################################################
## Get Order column for sorting purpose
defineOrigin_variableTab_Order <- defineOrigin_variableTab %>%
  dplyr::select(Order, Dataset, Variable) %>%
  dplyr::rename(tmp_Order = Order) %>%
  dplyr::mutate(Order = as.numeric(tmp_Order)) %>%
  dplyr::select(Order, Dataset, Variable)

crf_out_varTab_collapsed_rename <- crf_out_varTab_collapsed %>%
  dplyr::mutate_if(is.factor, as.character) %>%                   ## change datatype from factor to character (all at once)
  dplyr::rename(Dataset = domain) %>%
  dplyr::rename(Variable = sdtm_vars)                             ## rename column name: New = Old
  
crf_out_varTab_merged <- crf_out_varTab_collapsed_rename %>%
  dplyr::inner_join(defineOrigin_variableTab_Order,
                    by = c("Dataset", "Variable"))
  
## Order dataset by Pinnacle's Order variable
## not working...
crf_out_varTab_final <- crf_out_varTab_merged %>%
  dplyr::group_by(Dataset, Variable, Pages) %>%
  dplyr::arrange(Order, .by_group = TRUE)                        ## order dataframe using multiple columns
  
