# Gets RID -----
#' @title Gets RID
#' @description This function is used to extract the RID from the original PTID format of SITEID_S_RID.
#' @param IDVar Participant ID variable with the format SITEID_S_RID; XXX_S_XXX.
#' @param type Whether to get RID or SIETID
#' @return A vector with the same size as IDVar with specified ID type format.
#' @examples
#' \dontrun{
#'   rid <- get_siterid(IDVar = c("001_S_1234", "111_S_99999"), type = "RID")
#'   siteid <- get_siterid(IDVar = c("001_S_1234", "111_S_99999"), type = "SITEID")
#' }
#' @keywords Get IDs
#' @rdname get_siterid
#' @export
#' @importFrom rlang arg_match0
#' @importFrom stringr str_c str_detect str_remove_all str_extract
get_siterid <- function(IDVar, type = "RID") {
  
  library(stringr)
  library(rlang)
  
  rlang::arg_match0(arg = type, values = c("RID", "SITEID"))
  # To check the ID variable contains at least 10 characters
  num_char <- nchar(IDVar)
  ps_nchar <- nchar("XXX_S_X")
  if (all(num_char >= ps_nchar) == FALSE) stop(stringr::str_c(toString(IDVar[num_char < 7]), " contain(s) less than ", ps_nchar, " characters"))
  # To check if the pattern is similar to XXX_S_XXXX
  ps_pattern <- "[0-9]{3}_S_[0-9]{4}"
  pattern_status <- stringr::str_detect(string = IDVar, pattern = ps_pattern)
  if (all(pattern_status) == FALSE) stop(stringr::str_c(IDVar[pattern_status == FALSE], " does not match XXX_S_XXXX patterns"))
  
  if (type %in% "RID") {
    id <- stringr::str_remove_all(string = IDVar, pattern = "[0-9]{3}_S_")
  } 
  if (type %in% "SITEID"){
    id <- stringr::str_extract(string = IDVar, pattern = "[0-9]{3}_S")
    id <- stringr::str_remove_all(string = id, "_S")
  }
  
  return(id)
}
