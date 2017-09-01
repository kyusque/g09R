#' extract the summary area from gaussian09 log file
#'
#' @param file file path
#' @importFrom dplyr %>%
#' @importFrom readr read_file_raw
#' @return summary list
#' @export
#' @examples
#' library(dplyr)
#' dir(".") %>%
#'  grep(".out", ., value = TRUE) %>%
#'   lapply(
#'     function(x){
#'       res <- read_g09output(x)
#'       tryCatch(
#'         c(res$Title ,res$Summary$`CCSD(T)`),
#'         error = function(e) NA
#'       )
#'     }
#'   )
#'
#'
#'
read_g09output <- function(file){

  raw <- read_file_raw(file)

  if(0 == length(raw %>% tail(1000) %>% rawToChar %>% grep("Normal termination", .)))
    return(NA)

  #for ircmax calculation
  if(0 != length(raw %>% tail(1000) %>% rawToChar %>% grep("This type of calculation cannot be archived.", .)))
    return(NA)

  #extract the summary area
  #read the file backwards lines by lines
  i <- 0
  while(TRUE){
    i <- i + 1
    strings <- tail(raw, i * 10^3) %>% rawToChar %>% gsub("\\r", "", .) %>% strsplit("\n")
    if(0 == length(start <- strings[[1]] %>% grep("1\\\\1\\\\",.)))
      next()
    end <- strings[[1]] %>% grep("@",.)
    sumstr <- strings[[1]][start:end] %>% paste(collapse = "") %>% gsub(" ", "", .)
    break()
  }

  #organize the summary area
  temp <- sumstr %>%
  {function(x) strsplit(x, "\\\\\\\\")[[1]]}() %>%
    strsplit("\\\\")

  result <- list(Info = temp[[1]],
                 Root = temp[[2]],
                 Title = temp[[3]],
                 ChageMulti = strsplit(temp[[4]][1], ",")[[1]],
                 Atoms = temp[[4]][-1],
                 Summary = {
                   temp[[5]] %>% strsplit("=") %>% do.call(rbind, .) %>%
                   {function(x){value <- x[,2] %>% lapply(function(y) y); names(value) <- x[,1]; value}}()
                 }
  )
  return(result)
}
