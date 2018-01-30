
##############################
## Testing aCRFPageMinner.  ##
##############################

## load package
# source("./dev/functions/load_library.R")
source("./dev/functions/aCRFPageMinner_main.R")


## run main function
run_aCRFPageMinner()

## perform validation
## 1. Variable tab result
perform_validation(filename_manualPages = "./input files/Copy of Define specs CDISC SDTM completed - manually.xlsx", 
                   tabsheet_manualPages = "Variables", 
                   filename_machinePages = "./input files/page numbers for Variable Tab (studyid = 1234567)_2018-01-30.csv")

## 2. Value Level tab result
perform_validation(filename_manualPages = "./input files/Copy of Define specs CDISC SDTM completed - manually.xlsx", 
                   tabsheet_manualPages = "ValueLevel", 
                   filename_machinePages = "./input files/page numbers for Value Level Tab (studyid = 1234567)_2018-01-30.csv")


