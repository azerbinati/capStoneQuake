#' Title
#'
#' @param filename
#'
#' @return a data.frame with raw NOAA data
#' @export
#' @importFrom readr read_delim
#' @examples
#'  \dontrun{
#' readRawNOAA(file.path('~',"signif.txt"))
#' }
readRawNOAA <- function(filename)
{
  readr::read_delim(filename,delim='\t')
}



#' titolize
#'
#' This function make every first letter in uppercase of a string, function is vectorized.
#'
#' @param x characters, or list of characters.
#'
#' @return a mixed case string, or a list of mixed case sting if x was a list
#'
#'
#' @examples titolize("my string is lowercase")
#' @export
titolize <- function(x) {
  x<-tolower(x)
  s <- strsplit(x, " ")
  mapply(s, FUN= function(x) {paste(toupper(substring(x, 1, 1)), substring(x, 2),        sep = "", collapse = " ")})
}

#' Tidy NOAA
#'
#' this function is the utility responsabile for clean up the raw data file into a specifically designed data.frame
#' for fourther
#'
#' @param rawNOAA
#'
#' @return a data.frame with LNG/LAT in double, date column as Date object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eq_clean_data(readRawNOAA(filetabdelimited))
#' }
#' @importFrom dplyr mutate filter
eq_clean_data <- function(rawNOAA)
{
  stopifnot( class( rawNOAA ) %in% c("data.frame","tbl_df", "tbl" ))
  stopifnot( class( rawNOAA$DAY ) %in% c("integer"))
  stopifnot( class( rawNOAA$MONTH ) %in% c("integer"))
  stopifnot( class( rawNOAA$YEAR ) %in% c("integer"))


  rawNOAA%>%dplyr::filter(!is.na(DAY) & !is.na(MONTH) & YEAR>0) %>% #remove a.c. events and unknown dates
    dplyr::mutate(date =as.Date(paste(YEAR,MONTH,DAY,sep='/'))) %>% ##create a date column of type Date
    dplyr::mutate(LATITUDE=as.double(LATITUDE),LONGITUDE=as.double(LONGITUDE)) %>%  #cast LNG LAT to double
  dplyr::mutate(EQ_PRIMARY=as.double(EQ_PRIMARY),DEATHS=as.double(DEATHS))  #cast MAG e DEATHS to double



}

#' eq_location_clean
#'
#' this function take in the NOAA data.frame and process the column LOCATION_NAME.
#' The column will get the first country removed and convert the rest to mixed case. IE, the value "ITALY: SICILY" will
#' be converted in "Sicily".
#'
#' @param rawNOAA
#'
#' @return the original data.frame with LOCATION_NAME mutated
#'
#' @export
#' @importFrom dplyr mutate
#'
#' @examples
#'  \dontrun{
#'  eq_location_clean(rawNOAA)
#'  }
#'
eq_location_clean<-function(rawNOAA)
{
  stopifnot( class( rawNOAA ) %in% c("data.frame","tbl_df", "tbl" ))
  stopifnot( class( rawNOAA$LOCATION_NAME  ) %in% c("character"))

  rawNOAA%>% dplyr::mutate(LOCATION_NAME=trimws (substr(LOCATION_NAME,start =  nchar(COUNTRY)+2, stop=nchar(LOCATION_NAME)))) %>%
    dplyr::mutate(LOCATION_NAME  = titolize(LOCATION_NAME))
}
