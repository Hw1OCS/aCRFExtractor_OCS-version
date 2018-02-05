#' The program collapse page numbers that belong to the same family (wrt Dataset and Variable columns). 
#'
#' @param dsin The preprocessed dataset with all page numbers for each domain extracted from aCRF file.
#' @param domain_list List of SDTM domains read from aCRF.
#'
#'
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.worku@ocs-consulting.com>
#'
#'


collapse_pgNbrs <- function(dsin = NULL, domain_list = NULL) {
  
  ## create an empty dataset to store result
  ds_final_out <- data.frame(a = character(), b = character(), c = numeric(), d = character())
  
  for (domain_sel in domain_list) {
    # print(domain_sel)
    
    ## filter dataset
    ds_flt <- dsin %>%
      dplyr::filter(Dataset %in% domain_sel)
    
    ## Collapse page numbers per domain and variable
    ds_final <- ds_flt %>%
      dplyr::group_by(Dataset, Variable, Order, Origin) %>%
      dplyr::summarise(Pages = paste(page_nbr, collapse = " ")) %>%    ## use space as a delimitor / separator for the collapse
      dplyr::arrange(Order)
    
    ## concatenate domain result
    ds_final_out <- rbind(ds_final_out, as.data.frame(ds_final))
  }
  
  return(ds_final_out)
}