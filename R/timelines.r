
#' This class is used to generate earthquake timeline plots for
#' \code{geom_timeline}.
#'
#' This class creates grobs need to visualize earthquakes by date and
#' country. This class extends \code{Geom} and is used by
#' \code{geom_timeline}. A point is plotted for each unique earthquake.
#'
#' @param x A vector of dates on which earthquakes occurred.
#' @param y A factor vector giving the country in which each earthquake
#'          occurred (optional).
#' @param shape A numeric vector giving the point shape to be used.
#' @param size A numeric vector representing the size of each point.
#' @param colour A string vector controlling the color of the point borders.
#' @param fill A string vector controlling the fill color of the points.
#' @param alpha A numeric constant in the interval [0,1] controlling the
#'              transparency of the wind radii chart. This parameter has a
#'              default value of 0.4.
#' @param stroke A numeric vector controlling the border with of the point.
#'
#' @return This returns a tree of grid objects.
#'
#' @importFrom ggplot2 ggproto aes Geom
#' @importFrom grid pointsGrob unit gpar gList segmentsGrob gTree
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
  required_aes = c("x"),
  optional_aes = c("y"),
  default_aes = ggplot2::aes(y = 1, shape = 21, size = 1, colour = "black",
                              fill = "black", alpha = 0.4, stroke = 0.5,),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    ## Transform the data first
    coords <- coord$transform(data, panel_scales)

    # ## Let's print out the structure of the 'coords' object
    # str(coords)

    ## Construct a grid grob:
    pts <- grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      size = grid::unit(coords$size, "mm"),
      gp = grid::gpar(
        col = coords$colour,
        fill = coords$fill,
        alpha = coords$alpha
      )
    )

    ## Create horizontal lines:
    uniq_y <- sort(unique(coords$y))
    n <- length(uniq_y)

    xmin <- min(coords$x)
    xmax <- max(coords$x)

    saveList <- grid::gList()
    for(i in seq(n)) {
      saveList[[i]] <- grid::segmentsGrob(
        x0 = xmin,
        x1 = xmax,
        y0 = uniq_y[i],
        y1 = uniq_y[i],
        gp = grid::gpar(lwd = 2, col = "gray")
      )
    }

    saveList[[n + 1]] <- pts
    grid::gTree(children = saveList)
  }
)

#' Create a timeline plot of earthquakes
#'
#' This function creates a plot showing earthquakes by date and country. 
#' It uses the GeomTimeline geom.
#'
#' @param x A vector of dates on which earthquakes occurred.
#' @param y A factor vector giving the country in which each earthquake
#'          occurred (optional).
#' @param shape A numeric vector giving the point shape to be used.
#' @param size A numeric vector representing the size of each point.
#' @param colour A string vector controlling the color of the point borders.
#' @param fill A string vector controlling the fill color of the points.
#' @param alpha A numeric constant in the interval [0,1] controlling the
#'              transparency of the wind radii chart. This parameter has a
#'              default value of 0.4.
#' @param stroke A numeric vector controlling the border with of the point.
#'
#' @return This function adds an earthquake timeline plot to the current
#'         ggplot.
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' eq_data <- eq_clean_data("NOAA_earthquakes.txt")
#' eq_data <- subset(eq_data, Country == "Mexico" & YEAR >= 2000)
#'
#' ggplot2::ggplot(eq_data) +
#'   geom_timeline(ggplot2::aes(x=DATE, y=Country, fill=TOTAL_DEATHS, 
#'                  size=EQ_PRIMARY))
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' This class is used to generate earthquake timeline plots with l
#' location labels for \code{geom_timeline_label}.
#'
#' This class creates grobs need to visualize earthquakes by date, country,
#' and city or region. This class extends \code{Geom} and is used by
#' \code{geom_timeline_label}. A point is plotted for each unique earthquake.
#'
#' @param x A vector of dates on which earthquakes occurred.
#' @param y A factor vector giving the country in which each earthquake
#'          occurred (optional).
#' @param shape A numeric vector giving the point shape to be used.
#' @param size A numeric vector representing the size of each point.
#' @param colour A string vector controlling the color of the point borders.
#' @param fill A string vector controlling the fill color of the points.
#' @param alpha A numeric constant in the interval [0,1] controlling the
#'              transparency of the wind radii chart. This parameter has a
#'              default value of 0.4.
#' @param stroke A numeric vector controlling the border with of the point.
#' @param label A string vector containing the label that should be used
#'              for each earthquake; this is usually a city or region name.
#' @param n_max A integer constant. If n_max is used, only the n_max
#'              largest earthquakes, as determined by the value of size,
#'              are given labels. The default behavior is to label all
#'              earthquakes.
#'
#' @return This returns a tree of grid objects.
#'
#' @importFrom ggplot2 ggproto aes Geom draw_key_point
#' @importFrom grid pointsGrob unit gpar gList segmentsGrob gTree textGrob
#'
#' @export
geomTimelineLabel <- ggplot2::ggproto("geomTimelineLabel", ggplot2::Geom,
  required_aes = c("x"),
  optional_aes = c("y","label"),
  default_aes = ggplot2::aes(y = 1, shape = 21, size = 1, colour = "black",
                              fill = "black", alpha = 0.4, stroke = 0.5,
                              label=""),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord, n_max) {
    ## Transform the data first
    coords <- coord$transform(data, panel_scales)

    # ## Let's print out the structure of the 'coords' object
    # str(coords)

    ## Create list for holding grobs:
    saveList <- grid::gList()

    ## Create horizontal lines:
    uniq_y <- sort(unique(coords$y))
    n <- length(uniq_y)

    xmin <- min(coords$x)
    xmax <- max(coords$x)

    for(i in seq(n)) {
      saveList[[i]] <- grid::segmentsGrob(
        x0 = xmin,
        x1 = xmax,
        y0 = uniq_y[i],
        y1 = uniq_y[i],
        gp = grid::gpar(lwd = 2, col = "gray")
      )
    }

    ## Create grob for points:
    pts <- grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      size = grid::unit(coords$size, "mm"),
      gp = grid::gpar(
        col = coords$colour,
        fill = coords$fill,
        alpha = coords$alpha
      )
    )
    saveList[[length(saveList) + 1]] <- pts

    ## Dataset for labels:
    if(is.null(n_max) || n_max > 0) {
      if(is.null(n_max)) {
        coords_label <- coords
      } else {
        coords_sort <- coords[order(coords$size, decreasing=TRUE), ]
        coords_label <- coords_sort[seq(n_max), ]
      }

      ## Create vertical lines:
      ## Suppose height of vertical line is always 1/6 of the space
      ## that each country has available.
      space_per_country <- 1/n
      v_line_height <- space_per_country/5
      for(i in seq(nrow(coords_label))) {
        saveList[[length(saveList) + 1]] <- grid::segmentsGrob(
          x0 = coords_label$x[i],
          x1 = coords_label$x[i],
          y0 = coords_label$y[i],
          y1 = coords_label$y[i] + v_line_height,
          gp = grid::gpar(lwd = 2, col = "gray")
        )
      }

      ## Create grob for labels:
      lbls <- grid::textGrob(coords_label$label, x = coords_label$x,
                      y = coords_label$y + v_line_height,
                      just = "centre", hjust = 0, vjust = 0, rot = 45,
                      check.overlap = FALSE, default.units = "npc",
                      name = NULL, gp = grid::gpar(), vp = NULL)
      saveList[[length(saveList) + 1]] <- lbls
    }

    grid::gTree(children = saveList)
  }
)

#' Create a timeline plot of earthquakes with labels showing the
#' city or region.
#'
#' This function creates a plot showing earthquakes by date, country, and
#' city or region. It is similar to geom_timeline, but has the option to
#' add labels showing the city or region in which the earthquake took
#' place. It uses the geomTimelineLabel geom.
#'
#' @param x A vector of dates on which earthquakes occurred.
#' @param y A factor vector giving the country in which each earthquake
#'          occurred (optional).
#' @param shape A numeric vector giving the point shape to be used.
#' @param size A numeric vector representing the size of each point.
#' @param colour A string vector controlling the color of the point borders.
#' @param fill A string vector controlling the fill color of the points.
#' @param alpha A numeric constant in the interval [0,1] controlling the
#'              transparency of the wind radii chart. This parameter has a
#'              default value of 0.4.
#' @param stroke A numeric vector controlling the border with of the point.
#' @param label A string vector containing the label that should be used
#'              for each earthquake; this is usually a city or region name.
#' @param n_max A integer constant. If n_max is used, only the n_max
#'              largest earthquakes, as determined by the value of size,
#'              are given labels. The default behavior is to label all
#'              earthquakes.
#'
#' @return This function adds an earthquake timeline plot with labels to
#'         the current ggplot.
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' eq_data <- eq_clean_data("NOAA_earthquakes.txt")
#' eq_data <- subset(eq_data, Country == "Mexico" & YEAR >= 2000)
#'
#' ggplot2::ggplot(eq_data) +
#'   geom_timeline_label(ggplot2::aes(x=DATE, y=Country, fill=TOTAL_DEATHS,
#'                           size=EQ_PRIMARY, label=LocalLocation))
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE, n_max = NULL, ...) {
  ggplot2::layer(
    geom = geomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

#' Create a theme for earthquake timeline plots.
#'
#' This function creates a ggplot theme for earthquake timeline plots.
#' It is intended to be used with geom_timeline and geom_timeline_label.
#'
#' @return This function changes the theme of a ggplot.
#'
#' @importFrom ggplot2 theme_classic theme element_blank
#' 
#' @examples
#' eq_data <- eq_clean_data("NOAA_earthquakes.txt")
#' eq_data <- subset(eq_data, Country == "Mexico" & YEAR >= 2000)
#'
#' ggplot2::ggplot(eq_data) +
#'   geom_timeline_label(ggplot2::aes(x=DATE, y=Country, fill=TOTAL_DEATHS,
#'                           size=EQ_PRIMARY, label=LocalLocation)) + 
#'   eqTheme()
#' @export
eqTheme <- function() {
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())
}

#' Create a timeline plot of earthquakes quickly
#'
#' This function facilitates creating an earthquake timeline plot with
#' or without location labels using a custom theme.
#'
#' @param dataset A data frame of earthquake data, as returned by
#'                eq_clean_data.
#' @param xmin A date object giving a lower bound for the dates represented 
#'             in the plot. If xmin is set, the dataset is subsetted to 
#'             exclude dates before xmin.
#' @param xmax A date object giving an upper bound for the dates 
#'             represented in the plot. If xmax is set, the dataset is 
#'             subsetted to exclude dates after xmax.
#' @param label A Boolean indicating whether labels showing the local
#'              location of the earthquake should be included in the plot.
#'
#' @return This function creates an an earthquake timeline plot where the
#'         fill color represents the total number of deaths and the point
#'         size represents the magnitude.
#'
#' @importFrom ggplot2 ggplot aes labs
#'
#' @examples
#' eq_data <- eq_clean_data("NOAA_earthquakes.txt")
#' eq_data <- subset(eq_data, Country == "Mexico" & YEAR >= 2000)
#'
#' eq_timeline(dataset=eq_data, label=TRUE)
#'
#' @export
eq_timeline <- function(dataset, xmin = NA, xmax = NA, label = FALSE) {
  if(!is.na(xmin)) {
    dataset <- subset(dataset, dataset$DATE >= xmin)
  }
  if(!is.na(xmax)) {
    dataset <- subset(dataset, dataset$DATE <= xmax)
  }

  if(label) {
    ggplot2::ggplot(dataset) +
      geom_timeline_label(ggplot2::aes_(x=~DATE, y=~Country, 
                          fill=~TOTAL_DEATHS, size=~EQ_PRIMARY, 
                          label=~LocalLocation)) +
      ggplot2::labs(fill="# deaths", size="Magnitude") +
      eqTheme()
  } else {
    ggplot2::ggplot(dataset) +
      geom_timeline(ggplot2::aes_(x=~DATE, y=~Country, fill=~TOTAL_DEATHS,
                    size=~EQ_PRIMARY)) +
      ggplot2::labs(fill="# deaths", size="Magnitude") +
      eqTheme()
  }
}
