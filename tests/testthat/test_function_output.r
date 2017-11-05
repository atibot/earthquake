library(earthquake)
context("Function output")

## eq_read_data
test_that("eq_read_data is a tbl_df with specific columns", {
  eqDataRaw <- eq_read_data(filename="NOAA_earthquakes.txt")
  expect_is(eqDataRaw, "tbl_df")

  eqDataRawNames <- names(eqDataRaw)
  checkNames <- c("YEAR","MONTH","DAY","COUNTRY","TOTAL_DEATHS",
                  "EQ_PRIMARY","EQ_MAG_MW","EQ_MAG_MS","EQ_MAG_MB",
                  "EQ_MAG_ML","EQ_MAG_MFA","TOTAL_DEATHS","LATITUDE",
                  "LONGITUDE")

  expect_true(all(checkNames %in% eqDataRawNames))
})

## clean_local_location
test_that("clean_local_location returns the correct string", {
  equals(clean_local_location("CUZCO,COLLAO,LIMA"), "Cuzco, Collao, Lima")
})

## eq_clean_data
test_that("eq_clean_data is a tbl_df with specific columns", {
  eqDataClean <- eq_clean_data("NOAA_earthquakes.txt")
  expect_is(eqDataClean, "tbl_df")

  eqDataCleanNames <- names(eqDataClean)
  checkNames <- c("DATE","LocalLocation","LocalLocation","Country",
                  "TOTAL_DEATHS","EQ_PRIMARY","Latitude","Longitude")

  expect_true(all(checkNames %in% eqDataCleanNames))
})

## Multiple tests need this:
eqDataClean <- eq_clean_data("NOAA_earthquakes.txt")
eqDataShort <- eqDataClean %>%
  dplyr::filter(Country == "Mexico" & YEAR >= 2000)

## GeomTimeline
test_that("GeomTimeline is a Geom", {
  equals(class(GeomTimeline), "Geom")
})

## geom_timeline
test_that("geom_timeline is a ggplot", {
  testPlot <- ggplot2::ggplot(eqDataShort) +
    geom_timeline(ggplot2::aes(x=DATE, y=Country, fill=TOTAL_DEATHS, 
                   size=EQ_PRIMARY))
  expect_is(testPlot, "gg")
  expect_is(testPlot, "ggplot")
})

## geomTimelineLabel
test_that("geomTimelineLabel is a Geom", {
  equals(class(geomTimelineLabel), "Geom")
})

## geom_timeline_label
test_that("geom_timeline_label is a ggplot", {
  testPlot <- ggplot2::ggplot(eqDataShort) +
    geom_timeline_label(ggplot2::aes(x=DATE, y=Country, fill=TOTAL_DEATHS,
                        size=EQ_PRIMARY, label=LocalLocation))
  expect_is(testPlot, "gg")
  expect_is(testPlot, "ggplot")
})

## eqTheme
test_that("eqTheme is a ggplot theme", {
  expect_is(eqTheme(), "gg")
  expect_is(eqTheme(), "theme")
})

## eq_timeline
test_that("eq_timeline is a ggplot", {
  testPlot <- eq_timeline(dataset=eqDataShort, label=TRUE)
  expect_is(testPlot, "gg")
  expect_is(testPlot, "ggplot")
})

## eq_create_label
test_that("eq_create_label returns correct string", {
  equals(eq_create_label(data.frame(LocalLocation="x", EQ_PRIMARY=5, 
                              TOTAL_DEATHS=0)), 
          paste0("<div><div><strong>Location:</strong>x</div>",
                 "<div><strong>Magnitude:</strong>5</div>",
                 "<div><strong>Total deaths:</strong></div></div>"))
})

## eq_map
test_that("eq_map returns correct string", {
  testPlot <- eqDataShort %>% 
      dplyr::mutate(popup_text = eq_create_label(.)) %>% 
      eq_map(annot_col = "popup_text")
  expect_is(testPlot, "leaflet")
  expect_is(testPlot, "htmlwidget")
})
