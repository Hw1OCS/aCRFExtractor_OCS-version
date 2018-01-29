#' Extracts page numbers of SDTM variables available in manually created aCRF file. The output is used for creation of Define XML file.
#' The input files (e.g., studyid, aCRF file, and Define Origin file) are taken from a user via console
#'
#' @examples
#' ## library(aCRFExtractor)
#' ## run_aCRFExtractor()
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.x.worku@gsk.com>
#'
#'


run_aCRFExtractor <- function() {

  ## -------------------------------------------------------- ##
  ## <!-- Configure dependent packages.                 -->   ##
  ## -------------------------------------------------------- ##
  ## pdftools
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Please install pdftools: install.packages('pdftools'); library(pdftools)")
  }

  ## stringr
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Please install stringr: install.packages('stringr'); library(stringr)")
  }

  ## tidyr
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Please install tidyr: install.packages('tidyr'); library(tidyr)")
  }

  ## xlsx
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    stop("Please install xlsx: install.packages('xlsx'); library(xlsx)")
  }

  ## sqldf
  if (!requireNamespace("sqldf", quietly = TRUE)) {
    stop("Please install sqldf: install.packages('sqldf'); options(gsubfn.engine = 'R'); library(sqldf)")
  }

  ## -- load all required libraries, after all pkgs are installed
  if (!requireNamespace("pacman", quietly = TRUE)) {
    stop("Please install pacman: install.packages('pacman'); library(pacman)")
  }
  pacman::p_load(pdftools, stringr, tidyr, xlsx, sqldf)


  ## Fill studyId info
  # studyid <- readline(prompt = "Enter StudyId: ")
  assign(x = "studyid", value = readline(prompt = "Enter StudyId: "), envir = .GlobalEnv)

  ## Import aCRF pdf file
  print("Import aCRF pdf file. Thanks!")
  aCRF_fileName <- file.choose()
  # assign(x = "aCRF_fileName", value = file.choose(), envir = .GlobalEnv)

  ## Import defineOrigin file
  print("Import Define Origin draft xls/xlsx file. Thanks!")
  defOrig_fileName <- file.choose()                             ## the current version of the package assumes VariableTab
  # assign(x = "defOrig_fileName", value = file.choose(), envir = .GlobalEnv)

  ## Ask user to specify output destination
  # print("Where do you want to save the output file (select a folder)?")
  # output_dir <- tk_choose.dir(default = "",
  #                             caption = "Where do you want to save the output file (select a folder)?")

  # output_dir <- dirname(defOrig_fileName)
  assign(x = "output_dir", value = dirname(defOrig_fileName), envir = .GlobalEnv)

  ##
## Environmental settings
# studyid <- "115158"
# studyid <- "115649"
# location_source <- "/Users/workuhm/Desktop/aCRF_files/"
# location_source <- paste("/Users/workuhm/Desktop/aCRF_files/", studyid, "/", sep = "")

## Define the aCRF PDF file
# pdf_fileName = paste(location_source, "blankcrf_ES_20170609.pdf", sep = "")
# pdf_fileName = paste(location_source, "blankcrf.pdf", sep = "")
pdf_fileName <- aCRF_fileName

## load source files
source("./functions/load_library.R")

source("./functions/preprocess_crf_fn.R")
source("./functions/get_sdtmVars_pgNbr_fn.R")
# source("./functions/get_pgsSpecialVars_fn.R")
source("./functions/get_finalaCRFpages_fn.R")
source("./functions/get_linkPages_fn.R")
# source("./functions/get_processParentLinkPage_fn.R")
source("./functions/get_processParentLinkPage1_fn.R")
source("./functions/get_processParentLinkPage2_fn.R")
source("./functions/chk_sdtmVars_against_defOrigin.R")
source("./functions/db_linkpages.R")
source("./functions/get_pgsSpecialVars_main_fn.R")
# source("./functions/db_pageSections.R")


## -------------------------------------------------------- ##
## <!-- Get MFS specs.                                -->   ##
## -------------------------------------------------------- ##
# fileMFS_name <- paste(location_source, "SDTM_SPECIFICATION_", studyid, ".XLSX", sep = "")
# sheetMFSMapping_name <- "Mapping"
#
# MFS_mappingTab <- read.xlsx(file = fileMFS_name, sheetName = sheetMFSMapping_name)
# MFS_mappingTab2 <- MFS_mappingTab[, c("SDTM_DS", "SDTM_VAR", "SPECIFICATION", "FUNCTION")]



## -------------------------------------------------------- ##
## <!-- Get the Define Origin file.                   -->   ##
## -------------------------------------------------------- ##
# fileDefOrigin_name <- paste(location_source, studyid, "_DEFINE_ORIGIN", ".xlsx", sep = "")
# sheetVarTab_name <- "Variable"
# sheetVLMTab_name <- "VLM"

fileDefOrigin_name <- defOrig_fileName
# sheetVarTab_name <- defOrig_sheetName
sheetVarTab_name <- "Variable"

defineOrigin_variableTab <- xlsx::read.xlsx(file = fileDefOrigin_name, sheetName = sheetVarTab_name)
# defineOrigin_variableTab <- read_excel(path = fileDefOrigin_name, sheet = "Variable", col_names = T)
defineOrigin_variableTab <- as.data.frame(defineOrigin_variableTab)

# defineOrigin_VLMTab <- read.xlsx(file = fileDefOrigin_name, sheetName = sheetVLMTab_name)

## keep main variables
defineOrigin_variableTab2 <- defineOrigin_variableTab[,c("Domain", "Variable", "Codelist", "Origin")]
# defineOrigin_VLMTab2 <- defineOrigin_VLMTab[,c("Domain", "Variable", "VLM_variable", "VLM_value")]

# ## remove codelist records and others
defineOrigin_variableTab3 <- defineOrigin_variableTab2[!(defineOrigin_variableTab2$Codelist %in% c("MedDRA", "GSKDD", "DOMAIN")), ]

## --------------------------- ##
## VERSION-2: Only CRF Page.   ##
## 23-Sep-2017.                ##
## ----------------------------##
defineOrigin_variableTab3 <- defineOrigin_variableTab3[defineOrigin_variableTab3$Origin %in% c("CRF Page"), ]


## <!-- Get variables not in aCRF as mentoined in Define Origin file
otherOrigins_varTab1 <- subset(defineOrigin_variableTab3, !(Origin) %in% c("CRF Page"))
otherOrigins_varTab2 <- otherOrigins_varTab1[order(otherOrigins_varTab1[, 2]), ]         ## ORDER function works only with numeric value for column definition
otherOrigins_varTab3 <- otherOrigins_varTab2[!duplicated(otherOrigins_varTab2[, "Variable"]), ]


## ------------------------------------------------------------------- ##
## <!-- Get variables related to SUPP&domain and RELREC domain.   -->  ##
## ------------------------------------------------------------------- ##
domains_orig <- as.character(unique(otherOrigins_varTab3$Domain))
domain_unexpected <- as.character(unlist(stringr::str_extract_all(domains_orig, "^(SUPP\\w+)|^(RELREC)")))

sdtmVars_supp_relrec <- otherOrigins_varTab3 %>%
  dplyr::filter(Domain %in% domain_unexpected) %>%
  dplyr::mutate(Variable = as.character(Variable)) %>%
  dplyr::pull(Variable)

sdtmVars_supp_relrec <- unique(sdtmVars_supp_relrec)

assign(x = "sdtmVars_supp_relrec", value = sdtmVars_supp_relrec, envir = .GlobalEnv)


## select domains
# sel_domain <- "AE"
#
# dsinOrig <- defineOrigin_variableTab3[defineOrigin_variableTab3$Domain == sel_domain, ]
# dsinaCRF <- crf_out[crf_out$domain == sel_domain, ]

## ------------------------------------------------------------------- ##
## <!-- Get list of Domains as specified in Define Origin file.   -->  ##
## ------------------------------------------------------------------- ##
## VariableTab
domain_DefOrigin <- as.character(unique(defineOrigin_variableTab3$Domain))
domain_DefOrigin_supps <- as.character(domain_DefOrigin[!is.na(unlist(str_extract(string = domain_DefOrigin, pattern = "^SUPP\\w+")))])
domain_DefOrigin_Tdomains <- as.character(domain_DefOrigin[!is.na(unlist(str_extract(string = domain_DefOrigin, pattern = "^T\\w+")))])

# domain_DefOrigin_VarTab_filtered <- domain_DefOrigin[!(domain_DefOrigin %in% c(domain_DefOrigin_supps, domain_DefOrigin_Tdomains, "SE", "RELREC"))]
assign(x = "domain_DefOrigin_VarTab_filtered", value = domain_DefOrigin[!(domain_DefOrigin %in% c(domain_DefOrigin_supps, domain_DefOrigin_Tdomains, "SE", "RELREC"))], envir = .GlobalEnv)

## VLMTab
# domain_DefOrigin_VLMTab <- as.character(unique(defineOrigin_VLMTab2$Domain))
# domain_DefOrigin_VLMTab2 <- as.character(unlist(str_extract(domain_DefOrigin_VLMTab, "[^T]*")))
# domain_DefOrigin_VLMTab2 <- ifelse(str_detect(domain_DefOrigin_VLMTab, "^T"),
#                                    "NA",
#                                    domain_DefOrigin_VLMTab)
# domain_DefOrigin_VLMTab_filtered <- domain_DefOrigin_VLMTab2[!(domain_DefOrigin_VLMTab2 %in% c("IS", "NA"))]
# domain_DefOrigin_VLMTab_filtered


# query_tmp <- paste(domain_DefOrigin_supps, domain_DefOrigin_Tdomains, sep = "")
# # query_domain <- paste("c(", query_tmp, ")", sep = "")
# defineOrigin_variableTab4 <- defineOrigin_variableTab3[!(defineOrigin_variableTab3$Domain %in% c(query_tmp)), ]
# unique(defineOrigin_variableTab4$Domain)

## ---------------------------------------------- ##
## <!-- Import aCRF pdf file and parse it     --> ##
## ---------------------------------------------- ##
##pdf_info(pdf_fileName)

pages <- pdftools::pdf_info(pdf_fileName)$pages
crf_raw <- pdftools::pdf_text(pdf_fileName)
##crf_txt2 <- pdf_render_page(pdf_fileName)

## ---------------------------------------------- ##
## <!-- Pre-process 1: Clean the meesy data.  --> ##
## ---------------------------------------------- ##
# crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw, def_OS = "windows")
# crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw, def_OS = "mac")
crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw)

## ---------------------------------------------------------- ##
## <!-- Pre-process 2: Extract domain specific variables. --> ##
## ---------------------------------------------------------- ##
# crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM"), until_pgNbr = "51")
# crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM", "AE"))
# crf_out_batch1 <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("MH", "AE", "CM", "DM"))
crf_out_varTab <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = domain_DefOrigin_VarTab_filtered)


## get SDTM vars without domain name
# crf_out$Variable <- unlist(str_trim(str_extract(crf_out$sdtm_vars, "[^.]*$")))

## remove duplicates
# crf_out_unique <- crf_out[!duplicated(crf_out[, c("page_nbr", "Variable")]), c("page_nbr", "domain", "Variable")]
# crf_out_VarTab_unique <- crf_out_varTab[!duplicated(crf_out_varTab[, c("page_nbr", "Variable")]), c("page_nbr", "sdtm_vars", "domain", "Variable")]

crf_out_varTab <- crf_out_varTab %>% dplyr::arrange(page_nbr, domain, Variable, sdtm_vars)
crf_out_VarTab_unique <- crf_out_varTab[!duplicated(crf_out_varTab[, c("page_nbr", "sdtm_vars")]), ]     ## 23-Sep-2017


## collapse page numbers
# delim <- paste("(\"", ", ", "\")", sep = "")
# query_pgNbr <- paste(paste("SELECT A.*, group_concat(page_nbr SEPARATOR ", delim, " AS page_nbr_concat ", sep = ""),
#                      "FROM qwe AS A", sep = "")



## ---------------------------------------------------------- ##
## <!-- Pre-process 3: Extract link page numbers.         --> ##
## ---------------------------------------------------------- ##
# linkPages <- get_linkPages(dsin = crf_preprocess, debugme = 40)
linkPages <- get_linkPages(dsin = crf_preprocess, dset_defOrigin = defineOrigin_variableTab3)

## -- remove unexpected patterns such as IS.VISITNUM, IS.USUBJID
linkPages2 <- linkPages[!(stringr::str_detect(as.character(linkPages$sdtm_vars), "^\\w{2}\\.(VISITNUM)|^\\w{2}\\.(USUBJID)")), ]
# linkPages2 <- linkPages2[!linkPages2$Variable %in% c("EPOCH"), ]

## -- fill missing domains
linkPages3 <- dplyr::tbl_df(linkPages2) %>%
  # dplyr::filter(!Variable %in% c("EPOCH")) %>%
  dplyr::filter(!Variable %in% c("EPOCH", "CONFIDENTIAL", "QVAL")) %>%
  dplyr::filter(!(stringr::str_detect(domain, "__"))) 

# linkPages_unique <- linkPages3[!duplicated(linkPages3[, c("Variable", "pgNbr_aCRF")]), c(4,5,1,2)]
# linkPages_unique <- linkPages3[!duplicated(linkPages3[, c("Variable", "pgNbr_aCRF")]), c(4,5,1,2)]

## -- remove duplicated values
linkPages4 <- linkPages3 %>% dplyr::arrange(page_nbr, domain, Variable, sdtm_vars)

## -- remove domain name for STUDYID from SDTM_VARS variable (e.g., CE.STUDYID OR )

# linkPages_unique <- linkPages4[!duplicated(linkPages4[, c("page_nbr", "sdtm_vars")]), ]
# linkPages_unique <- linkPages4 %>%
#   dplyr::distinct(page_nbr, domain, Variable)

# linkPages_unique <- linkPages_unique %>% 
#   dplyr::arrange(page_nbr, domain, desc(Variable))


# ## ------------------------------------------------------------------- ##
# ## 23-Sep-2017: Keep STUDYID/VISITNUM obtained from linkpages.         ##
# ## ------------------------------------------------------------------- ##
# ## -- studyID
# linkPages_studyID <- dplyr::tbl_df(linkPages_unique) %>%
#   dplyr::filter(Variable %in% c('STUDYID'))
# 
# ## remove duplicate
# # linkPages_studyID <- linkPages_studyID %>% dplyr::arrange(page_nbr, domain, Variable, sdtm_vars)
# # linkPages_studyID_unique <- linkPages_studyID[!duplicated(linkPages_studyID[, c("page_nbr", "Variable")]), ]
# 
# 
# ## -- visitnum
# linkPages_visitnum <- linkPages_unique %>%
#   dplyr::filter(Variable %in% c('VISITNUM'))
# 
# ## remove duplicate
# linkPages_visitnum <- linkPages_visitnum %>% dplyr::arrange(page_nbr, domain, Variable, sdtm_vars)
# linkPages_visitnum_unique <- linkPages_visitnum[!duplicated(linkPages_visitnum[, c("page_nbr", "Variable")]), ]


## -- 23-Sep-2017: flagged out
# colnames(linkPages_unique) <- c("page_nbr", "sdtm_vars", "domain", "Variable")    


## --------------------------------------------------------- ##
## <!-- Get page numbers for STUDYID and VISITNUM.       --> ##
## --------------------------------------------------------- ##

# ## -- Step 0. Get all section info
# sections_db <- db_pageSections(crf_preprocess = crf_preprocess, 
#                                domain_list = domain_DefOrigin_VarTab_filtered)

## -- Step 1. Get all pages with link pages
linkPages_db <- db_linkPages(crf_preprocess = crf_preprocess, 
                             domain_list = domain_DefOrigin_VarTab_filtered)

## add domain info to linkPages database
linkPages_studyID <- linkPages4 %>%
  dplyr::filter(Variable %in% c("STUDYID")) %>%
  dplyr::distinct(page_nbr, Variable, domain, pgNbr_parent) %>%
  dplyr::select(c(2, 1, 3, 4))                       ## rearrange columns of a dataframe

linkPages_visitnum <- linkPages4 %>%
  dplyr::filter(Variable %in% c("VISITNUM")) %>%
  dplyr::distinct(page_nbr, Variable, domain, pgNbr_parent) %>%
  dplyr::select(c(2, 1, 3, 4))                       ## rearrange columns of a dataframe

#####################################################################
## Add page numbers for STUDYID/VISITNUM from grand-parent pages.  ##
#####################################################################
linkPages_studyID_grandpa <- sqldf::sqldf("SELECT 
                                           FROM   linkPages_studyID A 
                                                  linkPages_db      B 
                                           ON A.page_nbr = B.")

# linkPages_db2 <- linkPages_db %>%
#   dplyr::inner_join(linkPages_unique, by = 'page_nbr')

# linkPages_db2 <- sqldf::sqldf('SELECT A.*, B.domain AS domain_b, B.page_nbr AS page_nbr_B 
#                                FROM        linkPages_db AS A
#                                INNER JOIN  linkPages_db       AS B
#                                ON A.page_nbr = B.pgNbr_parent AND
#                                A.domain   = B.domain')

## -- Step 2. Start grabbing special variables (STUDYID/VISITNUM)
## STUDYID
# pgs_studyId_all <- get_pgsSpecialVars_main(variable_special = "STUDYID", 
#                                            domain_list = domain_DefOrigin_VarTab_filtered, 
#                                            crf_out_VarTab_unique = crf_out_VarTab_unique, 
#                                            crf_preprocess = crf_preprocess, 
#                                            # defineOrigin_variableTab_mainSDTM2 = defineOrigin_variableTab_mainSDTM2, 
#                                            linkPages_all = linkPages_db)

pgs_studyId_all <- get_pgsSpecialVars_main(variable_special = "STUDYID",
                                           domain_list = domain_DefOrigin_VarTab_filtered,
                                           dsin_crf = crf_preprocess, 
                                           linkPages_special = linkPages_studyID)

## VISITNUM
# pgs_visitnum_all <- get_pgsSpecialVars_main(variable_special = "VISITNUM", 
#                                             domain_list = domain_DefOrigin_VarTab_filtered, 
#                                             crf_out_VarTab_unique = crf_out_VarTab_unique, 
#                                             crf_preprocess = crf_preprocess, 
#                                             # defineOrigin_variableTab_mainSDTM2 = defineOrigin_variableTab_mainSDTM2, 
#                                             linkPages_all = linkPages_db)

pgs_visitnum_all <- get_pgsSpecialVars_main(variable_special = "VISITNUM",
                                            domain_list = domain_DefOrigin_VarTab_filtered,
                                            dsin_crf = crf_preprocess, 
                                            linkPages_special = linkPages_visitnum)


## --------------------------------------------- ##
## <!-- Combine main table with link table.  --> ##
## --------------------------------------------- ##
# crf_out_DM <- get_finalaCRFpages(dsin_crf = crf_out_unique[crf_out_unique$domain == "DM", ])
crf_out_VarTab_final <- get_finalaCRFpages(dsin_crf = crf_out_VarTab_unique, 
                                           dsin_linkPages = linkPages_unique, 
                                           domain_list = domain_DefOrigin_VarTab_filtered)

# test <- sqldf("SELECT A.*, group_concat(page_nbr SEPARATOR) AS page_nbr_concat
#                       FROM qwe AS A")

# ddply(qwe, .(page_nbr), summarize, C = toString(C))

## save file
# fileout_name <- paste(location_source, "sdtmVars_aCRF_all_", studyid, ".xlsx", sep = "")
# sheetout_name <- paste("sdtmVars_aCRF_all", studyid, sep = "")
#
# write.xlsx(x = crf_out_VarTab_final, file = fileout_name, sheetName = sheetout_name,
#            row.names = FALSE, col.names = TRUE)




## ------------------------------------------------------------------------------- ##
## <!-- Keep page numbers of SDTM variables only found in Define Origin file.  --> ##
## ------------------------------------------------------------------------------- ##
defineOrigin_variableTab_mainSDTM <- subset(defineOrigin_variableTab3, 
                                            Domain %in% c(domain_DefOrigin_VarTab_filtered))

defineOrigin_variableTab_mainSDTM2 <- sqldf::sqldf("SELECT A.*, B.page_nbr_concat
                                                   FROM defineOrigin_variableTab_mainSDTM AS A
                                                   LEFT JOIN crf_out_VarTab_final AS B
                                                   ON A.Domain = B.domain AND A.Variable = B.Variable")



## ----------------------------- ##
## <!-- Flag other Origins  -->  ##
## ----------------------------- ##
## 23-Sep-2017: flagged out since the current version is based on only CRF Page Origin

## Derived variables 
# defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "Derived" &
#                                                                         !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
#                                                                       "Origin is Derived in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
#                                                                       "")
# ## Assigned variables
# defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "Assigned" &
#                                                                         !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
#                                                                       "Origin is Assigned in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
#                                                                       defineOrigin_variableTab_mainSDTM2$flg_further_check_needed)
# ## eDT variables
# defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "eDT" &
#                                                                         !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
#                                                                       "Origin is eDT in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
#                                                                       defineOrigin_variableTab_mainSDTM2$flg_further_check_needed)
# ## Protocol variables
# defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "Protocol" &
#                                                                         !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
#                                                                       "Origin is Protocol in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
#                                                                       defineOrigin_variableTab_mainSDTM2$flg_further_check_needed)




## ---------------------------------------------------- ##
## <!-- Combine main dataset and STUDYID dataset.   --> ##
## ---------------------------------------------------- ##
# defineOrigin_variableTab_mainSDTM3 <- sqldf("SELECT A.*, B.pg_nbr_concat AS pg_nbr_concat_B, B.flg_further_check_needed
#                                              FROM defineOrigin_variableTab_mainSDTM2 AS A
#                                              LEFT JOIN pgs_studyid_all AS B
#                                              ON A.Domain = B.Domain AND A.Variable = B.Variable")

defineOrigin_variableTab_mainSDTM3 <- sqldf::sqldf("SELECT A.*, B.pg_nbr_concat AS pg_nbr_concat_B, B.flg_further_check_needed AS flg_further_check_needed_B,
                                            C.pg_nbr_concat AS pg_nbr_concat_C, C.flg_further_check_needed AS flg_further_check_needed_C
                                            FROM defineOrigin_variableTab_mainSDTM2 AS A
                                            LEFT JOIN pgs_studyid_all AS B
                                            ON A.Domain = B.Domain AND A.Variable = B.Variable
                                            LEFT JOIN pgs_visitnum_all AS C
                                            ON A.Domain = C.Domain AND A.Variable = C.Variable")

## add studyid info
defineOrigin_variableTab_mainSDTM3$page_nbr_concat2 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_B),
                                                              defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_B,
                                                              defineOrigin_variableTab_mainSDTM3$page_nbr_concat)
defineOrigin_variableTab_mainSDTM3$flg_further_check_needed2 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_B),
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_B,
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed)

## add visitnum info
defineOrigin_variableTab_mainSDTM3$page_nbr_concat3 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_C),
                                                              defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_C,
                                                              defineOrigin_variableTab_mainSDTM3$page_nbr_concat2)
defineOrigin_variableTab_mainSDTM3$flg_further_check_needed3 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_C),
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_C,
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed2)

# defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, !names(defineOrigin_variableTab_mainSDTM3) %in% c("pg_nbr_concat", "pg_nbr_concat_B")]
# defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, c("Domain", "Variable", "Codelist", "page_nbr_concat2", "flg_further_check_needed")]
# defineOrigin_variableTab_mainSDTM4 <- sqldf("SELECT A.Domain, A.Variable, A.Codelist, A.page_nbr_concat2 AS page_nbr_concat, A.flg_further_check_needed
#                                              FROM defineOrigin_variableTab_mainSDTM3")

defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, c("Domain", "Variable", "Codelist", "Origin", "page_nbr_concat3", "flg_further_check_needed3")]
# defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, c("Domain", "Variable", "Codelist", "page_nbr_concat2", "flg_further_check_needed")]



## Export updated Define Origin file
# fileout_name <- paste(location_source, "defineOrigin_variableTab_pageNumbers_", studyid, ".xlsx", sep = "")
# sheetout_name <- paste("defineOrigin_variableTab_pageNumbers", studyid, sep = "")

# write.xlsx(x = defineOrigin_variableTab_mainSDTM4, file = fileout_name, sheetName = sheetout_name,
#            row.names = FALSE, col.names = TRUE)


## export result to default directory, the same place as Define Origin file
fileOut_name <- paste("defineOrigin_variableTab_pageNumbers_", studyid, "_",
                      str_replace_all(format(Sys.time(), "%b %d %Y"), " ", "_"),
                      ".xls", sep = "")

xlsx::write.xlsx(x = defineOrigin_variableTab_mainSDTM4, file = file.path(output_dir, fileOut_name), sheetName = studyid,
                 row.names = FALSE, col.names = TRUE)

# write_excel_csv(x = pgs_aCRF_out, path = fileOut_name, col_names = T)
# write_csv(x = pgs_aCRF_out, path = file.path(output_dir, fileOut_name), col_names = T)     ## powered by pkg::readr
# write.table(x = pgs_aCRF_out, file = file.path(output_dir, fileOut_name), row.names = F, col.names = T)

# write_csv(x = defineOrigin_variableTab_mainSDTM4,
#           path = file.path(output_dir, fileOut_name),
#           col_names = T, na = "")                                     ## powered by pkg::readr

print("")
print("<!-- *************************************************  -->")
print(paste("The output file (", fileOut_name, ") is saved in directory: ", output_dir, ". Please check!",
            sep = ""))
print("<!-- *************************************************  -->")

# return(defineOrigin_variableTab_mainSDTM4)

}