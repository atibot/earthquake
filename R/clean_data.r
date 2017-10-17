
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
#' my_data <- fars_read(filename="accident_2013.csv.bz2")
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
#' my_data <- fars_read(filename="accident_2013.csv.bz2")
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

    approxDate <- as.Date(paste(YEAR, approxMonth, approxDay, sep="-"))

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




# #' Generate FARS data filename
# #' 
# #' This function generates the name of an FARS data file for a given 
# #' year. The resulting filename follows the pattern 
# #' \code{accident_YEAR.csv.bz2}, where YEAR is the year value passed to 
# #' the function.
# #' 
# #' @param year A numeric constant giving the year for which a filename 
# #' should be generated. Should be in four-digit format, e.g., 2013.
# #' Can alternatively be a string, e.g., "2013".
# #' 
# #' @return This function returns a string giving the filename. The string 
# #' follows this pattern: \code{accident_YEAR.csv.bz2}
# #' 
# #' @examples
# #' my_file <- make_filename(year=2013)
# #' 
# #' @export
# make_filename <- function(year) {
  # year <- as.integer(year)
  # sprintf("accident_%d.csv.bz2", year)
# }


# #' Create a list of FARS datasets separated by year
# #' 
# #' This function reads in FARS datasets for select years and returns 
# #' each dataset as a separate element in a list.
# #' 
# #' @param years A numeric vector of years for which data are desired.
# #' Can alternatively be a list of numeric values. The years should be 
# #' in four-digit format, e.g., 2013. Can alternatively be a vector of 
# #' strings, e.g., c("2013","2014","2015")
# #' 
# #' @return This function returns a list. Each element of the list is 
# #' a \code{tbl_df} object showing the month and year columns of a  
# #' FARS dataset corresponding to a given year.
# #' 
# #' @note If an error occurs while trying to read in a FARS dataset, a 
# #' warning is given and the list element for that year is NULL. This 
# #' function uses the functions \code{make_filename} and \code{fars_read}.
# #' 
# #' @importFrom dplyr mutate_ select_
# #' @importFrom magrittr "%>%"
# #' 
# #' @examples
# #' my_list <- fars_read_years(years=2013:2015)
# #' 
# #' @export
# fars_read_years <- function(years) {
  # lapply(years, function(year) {
    # file <- make_filename(year)
    # tryCatch({
      # dat <- fars_read(file)
      # dplyr::mutate_(dat, year = ~year) %>% 
                # dplyr::select_(.dots = c("MONTH", "year"))
    # }, error = function(e) {
      # warning("invalid year: ", year)
      # return(NULL)
    # })
  # })
# }


# #' Create frequency table of accidents by month and year
# #' 
# #' This function uses FARS datasets to calculate the number of 
# #' accidents by month and year, and displays the results as a table.
# #'
# #' @param years A numeric vector of years to include in the frequency 
# #' table. Can alternatively be a list of numeric values. The years should  
# #' be in four-digit format, e.g., 2013. Can alternatively be a vector of 
# #' strings, e.g., c("2013","2014","2015")
# #' 
# #' @return This function returns a \code{tbld_df} object showing the 
# #' number of accidents by month and year.
# #' 
# #' @importFrom dplyr bind_rows group_by_ summarize_ n
# #' @importFrom tidyr spread_
# #' @importFrom magrittr "%>%"
# #' 
# #' @examples
# #' fars_summarize_years(years=2013:2015)
# #' 
# #' @export
# fars_summarize_years <- function(years) {
  # dat_list <- fars_read_years(years)
  # dplyr::bind_rows(dat_list) %>% 
      # dplyr::group_by_(~year, ~MONTH) %>% 
      # dplyr::summarize_(n = "n()") %>%
      # tidyr::spread_(key_col = 'year', value_col = 'n')
# }


# #' Plot a map showing locations of accidents
# #' 
# #' This function uses FARS datasets to show a map of where accidents 
# #' have occurred in a specified state and year.
# #' 
# #' @param state.num A numeric constant giving the state id
# #' @param year A numeric constant giving the year in four-digit 
# #' format, e.g., 2013. Can alternatively be a string, e.g., "2013".
# #' 
# #' @return This function plots a map and does not return anything.
# #' 
# #' @note The function stops if state.num is not a valid state id.
# #' If there are no accidents, a message is printed to the console.
# #' This function uses the functions \code{make_filename} and 
# #' \code{fars_read}.
# #' 
# #' @importFrom dplyr filter_
# #' @importFrom maps map
# #' @importFrom graphics points
# #' 
# #' @examples
# #' fars_map_state(state.num=1, year=2013)
# #' 
# #' @export
# fars_map_state <- function(state.num, year) {
  # filename <- make_filename(year)
  # data <- fars_read(filename)
  # state.num <- as.integer(state.num)

  # if(!(state.num %in% unique(data$STATE)))
    # stop("invalid STATE number: ", state.num)
  # data.sub <- dplyr::filter_(data, ~STATE == state.num)
  # if(nrow(data.sub) == 0L) {
    # message("no accidents to plot")
    # return(invisible(NULL))
  # }
  # is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  # is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  # with(data.sub, {
    # maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              # xlim = range(LONGITUD, na.rm = TRUE))
    # graphics::points(LONGITUD, LATITUDE, pch = 46)
  # })
# }
