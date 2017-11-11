

# earthquake


This is an R package for investigating data on significant earthquakes 
around the world extending back to 2150 B.C. The data set was provided by 
the National Geophysical Data Center/World Data Service* and is available 
through the National Oceanic and Atmospheric Administration (NOAA) 
[website](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).

A description of the data, as stated on the NOAA website, is as follows:

  ```
  "The Significant Earthquake Database contains information on destructive 
  earthquakes from 2150 B.C. to the present that meet at least one of the 
  following criteria: Moderate damage (approximately $1 million or more), 
  10 or more deaths, Magnitude 7.5 or greater, Modified Mercalli 
  Intensity X or greater, or the earthquake generated a tsunami."
  ```

## Installation
The earthquake package can be downloaded from GitHub using the `devtools` 
package: 

  ```
  devtools::install_github("lmitchell4/earthquake")
  ```

## Data
A copy of the entire data set (downloaded October 2017) is provided with 
the earthquake package in a file called `NOAA_earthquakes.txt`.

## Functions
This package includes functions for reading in and cleaning the data set, 
and creating timeline and map plots of earthquakes. Here are brief 
summaries of the primary functions:

library(earthquake)
context("Function output")

`eq_clean_data` - This function reads in and clean the data set that comes 
with the package.

`eq_timeline` - This function creates a timeline plot of earthquakes. 

`eq_map` - This function creates an interactive map of earthquake epicenters 
using the JavaScript [leaflet]( http://leafletjs.com) library. 

## References
*National Geophysical Data Center / World Data Service (NGDC/WDS): 
Significant Earthquake Database. National Geophysical Data Center, 
NOAA. doi:10.7289/V5TD9V7K

## License

This project is released under [GPL-3 License](https://github.com/lmitchell4/earthquake/blob/master/LICENSE).
