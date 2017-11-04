
#' Read in the NOAA earthquake raw data file
#'
#' This function reads in a tab-delimited NOAA earthquake file.
#'
#' @param filename A character string giving the name of the file
#'
#' @return This function returns a \code{tbl_df} object.
#'
#' @note An error message is printed if the file is not found.
#'
#' @importFrom readr read_delim
#' @importFrom dplyr tbl_df
#'
#' @examples
#' my_raw_data <- eq_read_data(filename="NOAA_earthquakes.txt")
#'
#' @export
eq_read_data <- function(filename) {
  full_file <- system.file("extdata",filename,package="earthquake")
  if(!file.exists(full_file))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_delim(file=full_file, delim="\t", progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Clean up an earthquake location string
#'
#' This function cleans up the local location of an earthquake.
#' If there are multiple locations separated by commas, each 
#' location name is cleaned and the locations are recombined into a 
#' string. Cleaning involves removing extra white space, 
#' converting to title case, and removing location names that are 
#' only directions, e.g., "e", "east of", etc. This function 
#' is used by eq_clean_data(). 
#' 
#' @param local_loc_str A character string giving the locations 
#'                      separated by commas.
#'
#' @return This function returns a string containing the cleaned up 
#'  location.
#'
#' @importFrom stringr str_trim str_to_lower str_to_title
#'
#' @examples
#' clean_local_location("CUZCO,COLLAO,LIMA")
#' 
clean_local_location <- function(local_loc_str) {
  ## Remove these if they constitute the whole LocalLocation string:
  remove_these <- c("e","w","n","s",
                    "e.","w.","n.","s.",
                    "e of","w of","n of","s of",
                    "e. of","w. of","n. of","s. of",
                    "east of","west of","north of","south of",
                    "sw","ne","se","nw",
                    "sw of", "ne of", "se of", "nw of")
  remove_these <- paste0("^",remove_these,"$")
  remove_these <- paste(remove_these, collapse="|")
  
  
  local_loc_vec <- unlist(strsplit(local_loc_str, ","))
  local_loc_vec <- stringr::str_trim(local_loc_vec)
  
  ## If any local locations exactly equal an entry in remove_these,
  ## remove them. Convert to lower case first. Afterwards, 
  ## remove empty strings from the vector.
  local_loc_vec <- stringr::str_to_lower(local_loc_vec)
  local_loc_vec <- sub(remove_these,"",local_loc_vec)
  local_loc_vec <- local_loc_vec[!is.na(local_loc_vec)]

  ## Convert to title case and 
  local_loc_vec <- stringr::str_to_title(local_loc_vec)

  ## Recombine the locations into a single string:
  clean_local_loc_str <- paste(local_loc_vec, collapse=", ")
  
  return(clean_local_loc_str)
}


#' Clean up a raw NOAA earthquake data file
#'
#' This function cleans up a NOAA earthquake file. Missing months 
#' and days are filled in with 1's. Some BC dates are approximate.
#'
#' @param filename A character string giving the name of the file
#'
#' @return This function returns a \code{tbl_df} object.
#'
#' @note An error message is printed if the file is not found.
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_if
#' @importFrom stringr str_to_title str_trim
#'
#' @examples
#' my_clean_data <- eq_clean_data(filename="NOAA_earthquakes.txt")
#'
#' @export
eq_clean_data <- function(filename) {
  one <- function(x) { return(1) }

  dat <- eq_read_data(filename)
 
  ## Thanks to Discussion Forum participants for ideas about how to 
  ## handle BC dates.
  dat$AD <- TRUE
  dat$AD[dat$YEAR >= 0] <- FALSE

  dat <- dat %>%
    dplyr::mutate(EQ_MAG_MW = as.numeric(EQ_MAG_MW),
                  EQ_MAG_MS = as.numeric(EQ_MAG_MS),
                  EQ_MAG_MB = as.numeric(EQ_MAG_MB),
                  EQ_MAG_MFA = as.numeric(EQ_MAG_MFA),
                  TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
                  Latitude = as.numeric(LATITUDE),
                  Longitude = as.numeric(LONGITUDE))

  ## Create approximate date fields that replace missing months and
  ## days with 1.
  ## Work with Country and leave COUNTRY intact.
  dat <- dat %>%
    dplyr::mutate(
      posYear = abs(YEAR),
      approxMonth = MONTH,
      approxDay = DAY,
      approxMonth = purrr::map_if(approxMonth, is.na, one),
      approxDay = purrr::map_if(approxDay, is.na, one),
      positiveDATE = as.Date(paste(posYear,approxMonth,approxDay,sep="-")),
      DATE = positiveDATE, 
      tmpDateDiff = as.numeric(as.Date("0000-01-01") - positiveDATE),
      dateDiff = as.Date(tmpDateDiff, origin = "0000-01-01"),
      Country = stringr::str_to_title(COUNTRY))

  neg_year <- dat$AD
  dat$DATE[neg_year] <- dat$dateDiff[neg_year]

  ## Remove temporary columns used for date manipulation:
  dat <- dat %>%
  dplyr::select(-posYear, -tmpDateDiff, -dateDiff)
  
  ## Create a field containing the "local" location of the earthquake.
  dat <- dat %>%
    dplyr::mutate(
      LocalLocation = LOCATION_NAME,
      ## Remove anything in parentheses or brackets:
      LocalLocation = gsub("\\(.+?\\)","",LocalLocation), # nongreedy
      LocalLocation = gsub("\\[.+?\\]","",LocalLocation), # nongreedy

      ## Multiple countries are separated by semicolons.
      ## Retain only the first (primary) country:
      LocalLocation = sub("(.*?)(;)(.*)","\\1",LocalLocation), #nongreedy

      ## Remove everything that comes before the last colon:
      LocalLocation = gsub("(.+:)(.*)","\\2",LocalLocation), #greedy
      LocalLocation = stringr::str_trim(LocalLocation))

  ## Further clean up the local locations:
  dat$LocalLocation <- sapply(dat$LocalLocation, clean_local_location)

  dat <- dat %>%
    dplyr::mutate(
      LocalLocation = gsub("Of ","of ",LocalLocation),
      
      ## Fix capitalization for NE, SW, NW, and SE:
      LocalLocation = gsub("Ne |Ne. ","NE ",LocalLocation),
      LocalLocation = gsub("Sw |Sw. ","SW ",LocalLocation),
      LocalLocation = gsub("Nw |Nw. ","NW ",LocalLocation),
      LocalLocation = gsub("Se |Se. ","SE ",LocalLocation),

      LocalLocation = gsub("^Ne$","NE",LocalLocation),
      LocalLocation = gsub("^Sw$","SW",LocalLocation),
      LocalLocation = gsub("^Nw$","NW",LocalLocation),
      LocalLocation = gsub("^Se$","SE",LocalLocation),
      
      ## Fix some very specific issues:
      LocalLocation = sub("E Coast of","E Coast",LocalLocation),
      LocalLocation = sub("N-Central","N Central",LocalLocation),
      LocalLocation = sub("Me\\xico","Mexico",LocalLocation,fixed=TRUE),
      LocalLocation = sub("Damage & Injuries In Every Dept.","",
                          LocalLocation))

  ## Replace any missing or blank LocalLocation values with the country:
  dat <- within(dat, {
    LocalLocation[is.na(LocalLocation)] <- Country[is.na(LocalLocation)]
    LocalLocation[LocalLocation == ""] <- Country[LocalLocation == ""]
  })

  dat$Country <- factor(dat$Country)

  return(dat)
}
