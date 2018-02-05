
########################
## Install package.   ##
########################
# install.packages("P:/OCS/000048 - aCRFPageMinner/03 Programs/R/aCRFExtractor_OCS-version/prod/aCRFPageMinner_0.1.0.zip", repos = NULL, type = "win.binary")

## load library
library(aCRFPageMinner)

###################################
## Run app using dummy study.    ##
###################################
aCRFPageMinner::run_aCRFPageMinner()


########################################################################
## Perfom validation by comparing the file with manually created      ##
## page numbers against the one produced by the program.              ##
########################################################################

## 1. Validate output for Variable Tab 
aCRFPageMinner::perform_validation(filename_manualPages = "./input files/Define specs CDISC SDTM completed - manually.xlsx", 
                                   tabsheet_manualPages = "Variables", 
                                   filename_machinePages = "./input files/page numbers for Variable Tab (studyid = A1B2C3)_2018-02-05.csv")

## 2. Validate output for ValueLevel Tab 
aCRFPageMinner::perform_validation(filename_manualPages = "./input files/Define specs CDISC SDTM completed - manually.xlsx", 
                                   tabsheet_manualPages = "ValueLevel", 
                                   filename_machinePages = "./input files/page numbers for Value Level Tab (studyid = A1B2C3)_2018-02-05.csv")
