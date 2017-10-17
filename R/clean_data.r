
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
#' my_raw_data <- noaa_read(filename="signif.txt")
#' 
#' @export
noaa_read <- function(filename) {
  full_file <- system.file("extdata",filename,package="earthquake")
  if(!file.exists(full_file))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_delim(file=full_file, delim="\t", progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Clean up a raw NOAA earthquake data file
#' 
#' This function cleans up a NOAA earthquake file.
#' 
#' @param filename A character string giving the name of the file
#' 
#' @return This function returns a \code{tbl_df} object.
#' 
#' @note An error message is printed if the file is not found.
#' 
#' @importFrom tools toTitleCase
#' 
#' @examples
#' my_clean_data <- eq_clean_data(filename="signif.txt")
#' 
#' @export
eq_clean_data <- function(filename) {
  dat <- noaa_read(filename)

  ## Keep track of whether the year is BC or AD:
  dat$AD <- TRUE
  dat$AD[dat$YEAR < 0] <- FALSE
  dat$YEAR <- abs(dat$YEAR)

  dat <- within(dat, {
    ## Create approximate date fields that replace missing values:
    approxMonth <- MONTH
    approxMonth[is.na(approxMonth)] <- 1

    approxDay <- DAY
    approxDay[is.na(approxDay)] <- 1

    DATE <- as.Date(paste(YEAR, approxMonth, approxDay, sep="-"))

    ## Convert lat and long to numeric:
    Latitude <- as.numeric(LATITUDE)
    Longitude <- as.numeric(LONGITUDE)

    ## Work with Country and leave COUNTRY intact.
    ## Similarly for LocalLocation and LOCATION_NAME.
    Country <- tools::toTitleCase(tolower(COUNTRY))
    LocalLocation <- LOCATION_NAME
  })

  ## Remove things in parentheses or brackets:
  dat$LocalLocation <- gsub("\\(.+?\\)", "", dat$LocalLocation)
  dat$LocalLocation <- gsub("\\[.+?\\]", "", dat$LocalLocation)

  ## Remove these if they constitute the whole local location string:
  remove_these <- c("e","w","n","s",
                    "e.","w.","n.","s.",
                    "E.","W.","N.","S.",
                    "e of","w of","n of","s of",
                    "east of","west of","north of","south of",
                    "sw","ne","se","nw",
                    "sw of", "ne of", "se of", "nw of")
  dat$LocalLocation <- sapply(dat$LocalLocation, function(x) {
    ## Split each entry by ";" to separate countries.
    ## If there are multiple countries, just keep the first as the primary.
    countries <- unlist(strsplit(x, ";"))
    primary_country <- countries[1]

    ## Now remove the country at the front.
    ## There can be up to three entries: country, state/province, city.
    ## Keep the last entry, which should be the city (or most local place 
    ##  name).
    tmp <- unlist(strsplit(primary_country, ":"))
    local_loc <- tmp[length(tmp)]
    local_loc <- trimws(local_loc, which="both")

    ## For the primary country, split by "," to separate cities.
    local_loc <- sapply(unlist(strsplit(local_loc, ",")), function(name) {
      tmp <- tolower(trimws(name, which="both"))
      if(tmp %in% remove_these) {
        ## Remove any lone directions:
        ret <- NA
      } else {
        ## Fix any spacing around commas and convert to title case.
        ret <- tools::toTitleCase(tmp)

        ## Manually fix direction capitalization:
        ret <- sub("^e ","E ",ret)
        ret <- sub("^s ","S ",ret)
        ret <- sub("^n ","N ",ret)
        ret <- sub("^w ","W ",ret)

        ret <- sub("^Ne |^Ne. |^ne ","NE ",ret)
        ret <- sub("^Sw |^Sw. |^sw ","SW ",ret)
        ret <- sub("^Nw |^Nw. |^nw ","NW ",ret)
        ret <- sub("^Se |^Se. |^se ","SE ",ret)
      }
      return(ret)
    })
    local_loc <- local_loc[!is.na(local_loc)]
    local_loc <- paste(local_loc, collapse=", ")
    return(local_loc)
  })

  ## Replace any missing or blank names with the country:
  dat <- within(dat, {
    LocalLocation[is.na(LocalLocation)] <- Country[is.na(LocalLocation)]
    LocalLocation[LocalLocation == ""] <- Country[LocalLocation == ""]
  })
  any(is.na(dat$LocalLocation))
  any(dat$LocalLocation == "")

  ## Fix a few remaining issues:
  dat$LocalLocation <- sub("E Coast of","E Coast", dat$LocalLocation)
  dat$LocalLocation <- sub("n-Central","N Central", dat$LocalLocation)
  dat$LocalLocation <- sub("n Cagayan","N Cagayan", dat$LocalLocation)
  dat$LocalLocation <- sub("Near s","Near S", dat$LocalLocation)
  dat$LocalLocation <- sub("Near e","Near E", dat$LocalLocation)
  dat$LocalLocation <- sub("Me\\xico","Mexico", dat$LocalLocation, fixed=TRUE)

  return(dat)
}
