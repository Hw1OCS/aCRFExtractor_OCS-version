
defOrig_fileName <- "./input files/Define specs CDISC SDTM completed.xlsx"
sheetVarTab_name <- "Variables"


## Read Define specs with page numbers populated manually
defineOrigin_variableTab_manual <- readxl::read_excel(path = file.path(defOrig_fileName), 
                                                      sheet = sheetVarTab_name, 
                                                      col_names = TRUE)

defineOrigin_variableTab_manual_CRF <- defineOrigin_variableTab_completed %>% 
  dplyr::select(Order, Dataset, Variable, Origin, Pages) %>%
  dplyr::filter(Origin %in% c("CRF"))

## Get list of domains
domain_manual <- defineOrigin_variableTab_manual_CRF %>%
  dplyr::distinct(Dataset) %>%
  dplyr::pull()
  