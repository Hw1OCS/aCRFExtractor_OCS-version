#' The program splits raw data read from aCRF file using page delimiter depending which operating system is the package is running from. 
#' It can handle both Windows and Mac systems. 
#'
#'
#' @param crf_txt_raw The raw info read from aCRF file.
#'
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.worku@ocs-consulting.com>
#'
#'


preprocess_crf <- function(crf_txt_raw = NULL) {
  
  crf_page2 <- data.frame(a=numeric(), b=character())
  count <- 0
  
  for (txtin in crf_txt_raw) {
    
    count <- count + 1
    sys_name <- tolower(Sys.info()["sysname"])
    
    # if (!(str_detect(def_OS, "windows"))) {
    if (!str_detect(sys_name, "windows")) {
      txtin_splt <- unlist(strsplit(as.character(txtin), "\n"))
    }
    else {
      txtin_splt <- unlist(strsplit(as.character(txtin), "\r\n"))
    }
    
    txtin_splt <- cbind(count, txtin_splt)
    
    crf_page2 <- rbind(crf_page2, txtin_splt)
  }
  
  crf_page2 <- as.data.frame(crf_page2)
  colnames(crf_page2) <- c("page_nbr", "crf_txt")
  
  return(crf_page2)
  
}