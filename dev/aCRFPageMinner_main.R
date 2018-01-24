
## load library
source("./dev/functions/load_library.R")

## load custom source codes
source("./dev/functions/preprocess_crf_fn.R")
source("./dev/functions/get_sdtmVars_pgNbr_fn.R")
source("./dev/functions/collapse_pgNbrs.R")

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


## export final dataset
filename_out <- file.path(paste("./../../../04 Output/page numbers for Variables tab_", Sys.Date(), ".csv", sep = ""))

readr::write_csv(x = crf_out_varTab_collapsed, path = filename_out)


# ## Collapse page numbers per domain and variable
# crf_out_varTab_collapsed <- crf_out_varTab %>%
#   dplyr::group_by(domain, sdtm_vars) %>%
#   dplyr::summarise(Pages = paste(page_nbr, collapse = " "))     ## use space as a delimitor / separator for the collapse
# 
# ## change data type of factor variables to character (all at once)
# crf_out_varTab_collapsed_mdf <- crf_out_varTab_collapsed %>%
#   purrr::map_if(is.factor, as.character) %>%
#   dplyr::as_data_frame()
  

## Order dataset by Pinnacle's Order variable
## not working...
# crf_out_varTab_final <- crf_out_varTab_merged %>% 
  # dplyr::group_by(Dataset, Variable, Pages) %>% 
  # dplyr::arrange(Order, .by_group = TRUE)                        ## order dataframe using multiple columns
  # dplyr::arrange(Dataset, Variable, Pages, as.numeric(Order)) 

## check if dataframe is grouped
# dplyr::is.grouped_df(crf_out_varTab_merged)

# crf_out_varTab_final <- crf_out_varTab_merged %>%
#   plyr::arrange(Dataset, Variable, Order)

# crf_out_varTab_df <- as.data.frame(crf_out_varTab_merged)
# crf_out_varTab_final <- crf_out_varTab_df[with(crf_out_varTab_df,
#                                                order(Dataset, Variable, Pages, Order)), ]

# crf_out_varTab_final <- crf_out_varTab_merged %>%
#   dplyr::arrange(Dataset, Variable, Order)
  