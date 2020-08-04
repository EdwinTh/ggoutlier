#' Create a histogram in with outlier bins.
#'
#' Wrapper around `ggplot2::geom_histogram` in which
#'   data points that are considered outliers be binned
#'   together. The tick marks under the outlier bins show
#'   the entire range of the outliers and the special nature
#'   of the outlier bins can be made apperant by giving them
#'   a different fill.
#'
#' @param x A data frame containing the variable to plot.
#' @param var_name Character indicating which col in `x` to use.
#' @param cut_off_floor Numeric that indicates what the floor
#'   value of the regular range is. All values below this are
#'   grouped in the outlier bin on the low end.
#' @param cut_off_ceiling Numeric that indicates what the ceiling
#'   value of the regular range is. All values above this are
#'   grouped in the outlier bin on the high end.
#' @param col Character that will be used at the col
#'   argument of `ggplot2::geom_histogram`.
#' @param fill Character that will be used at the fill
#'   argument of `ggplot2::geom_histogram` for the regular range.
#' @param fill_outlier_bins Character that will be used at the fill
#'   argument of `ggplot2::geom_histogram` for the outlier bins.
#' @param binwidth Numeric that will be used at the binwith
#'   argument of `ggplot2::geom_histogram`
#'
#' @return An object of class `ggplot`.
#'
#' @examples
#' ggoutlier_hist(mtcars, "disp", cut_off_floor = 100)
#' ggoutlier_hist(mtcars, "disp", cut_off_ceiling = 300)
#' ggoutlier_hist(mtcars, "disp", cut_off_floor = 100, cut_off_ceiling = 300)
#' ggoutlier_hist(mtcars, "disp", cut_off_floor = 100, fill = "firebrick")
#' ggoutlier_hist(mtcars, "disp", cut_off_floor = 100, binwidth = 10)
#' ggoutlier_hist(mtcars, "disp", cut_off_floor = 100, fill_outlier_bins = "gold")
#'
#' # Note that the result is a regular ggplot object
#' #   that you can postprocess the way you want.
#' p <- ggoutlier_hist(mtcars, "disp", cut_off_floor = 100, cut_off_ceiling = 300)
#' p + ggplot2::ggtitle("A histogram with binned outliers") +
#'   ggplot2::ylab("Nr of occurences")
#' @export
ggoutlier_hist <- function(x,
                            var_name,
                            cut_off_floor     = NA_real_,
                            cut_off_ceiling   = NA_real_,
                            col               = "black",
                            fill              = "cornflowerblue",
                            fill_outlier_bins = "forestgreen",
                            binwidth          = NULL) {

  hist_input_checking(x,
                      var_name,
                      cut_off_floor,
                      cut_off_ceiling,
                      col,
                      fill,
                      fill_outlier_bins,
                      binwidth)

  printing_min_max <- get_printing_min_max(x, var_name)

  regular_x <- filter_regular_x(x, var_name, cut_off_floor, cut_off_ceiling)

  plot_obj <- ggplot2::ggplot(regular_x, ggplot2::aes_string({{var_name}})) +
    ggplot2::geom_histogram(col = col, fill = fill, binwidth = binwidth)

  if (!is.na(cut_off_ceiling)) {
    x_ceil            <- x[x[[var_name]] >= cut_off_ceiling, var_name, drop = FALSE]
    x_ceil[, 1]       <- cut_off_ceiling
    ticks_for_ceiling <- update_tickmarks_ceiling(plot_obj, cut_off_ceiling,
                                                  printing_min_max[2])
    plot_obj <- plot_obj +
      ggplot2::geom_histogram(data = x_ceil,
                              fill = fill_outlier_bins,
                              col = col,
                              binwidth = binwidth) +
      ggplot2::scale_x_continuous(breaks = ticks_for_ceiling$tick_positions,
                                  labels = ticks_for_ceiling$tick_labels)
  }

  if (!is.na(cut_off_floor)) {
    x_floor         <- x[x[[var_name]] <= cut_off_floor, var_name, drop = FALSE]
    x_floor[, 1]    <- cut_off_floor
    ticks_for_floor <- update_tickmarks_floor(plot_obj, cut_off_floor,
                                              printing_min_max[1])
    plot_obj <-
      suppressMessages(
        plot_obj +
          ggplot2::geom_histogram(data = x_floor,
                                  fill = fill_outlier_bins,
                                  col = col, binwidth = binwidth) +
          ggplot2::scale_x_continuous(breaks = ticks_for_floor$tick_positions,
                                      labels = ticks_for_floor$tick_labels)
      )
  }
  plot_obj
}


hist_input_checking <- function(x,
                                var_name,
                                cut_off_floor,
                                cut_off_ceiling,
                                col,
                                fill,
                                fill_outlier_bins,
                                binwidth) {

  stopifnot(is.data.frame(x),
            is.character(var_name),
            is.numeric(cut_off_floor),
            is.numeric(cut_off_ceiling),
            is.character(col),
            is.character(fill),
            is.character(fill_outlier_bins))

  if (!var_name %in% colnames(x)) {
    stop(sprintf("%s is not a column in x", var_name))
  }

  if ( is.na(cut_off_floor) & is.na(cut_off_ceiling)) {
    stop("Neither cut_off_floor, nor cut_off_ceiling are specified")
  }

  if ( !is.na(cut_off_floor) && !is.na(cut_off_ceiling) ) {
    if (cut_off_floor >= cut_off_ceiling) {
      stop("cut_off_floor should be smaller than cut_off_ceiling")
    }
  }

  if (!is.na(cut_off_floor)) {
    if (cut_off_floor < min(x[[var_name]])) {
      stop(sprintf("cut_off_floor lower than the lowest value in %s",
                   var_name))
    }
    if (cut_off_floor > max(x[[var_name]])) {
      stop(sprintf("cut_off_floor higher than the highest value in %s",
                   var_name))
    }
  }

  if (!is.na(cut_off_ceiling)) {
    if (cut_off_ceiling < min(x[[var_name]])) {
      stop(sprintf("cut_off_ceiling lower than the lowest value in %s",
                   var_name))
    }
    if (cut_off_ceiling > max(x[[var_name]])) {
      stop(sprintf("cut_off_ceiling higher than the highest value in %s",
                   var_name))
    }
  }
}

get_printing_min_max <- function(x, var_name) {

  var <- x[[var_name]]
  c(min = round(min(var, na.rm = TRUE), 1),
    max = round(max(var, na.rm = TRUE), 1))
}

filter_regular_x <- function(x,
                             var_name,
                             cut_off_floor = NA_real_,
                             cut_off_ceiling = NA_real_) {
  x_var <- x[[var_name]]
  ind   <- rep(TRUE, length(x_var))
  if (!is.na(cut_off_floor)) ind[x_var <= cut_off_floor] <- FALSE
  if (!is.na(cut_off_ceiling)) ind[x_var >= cut_off_ceiling] <- FALSE
  x[ind, var_name, drop = FALSE]
}

update_tickmarks_ceiling <- function(plot_obj,
                                     cut_off,
                                     max_print) {
  ranges <- suppressMessages(
    ggplot2::ggplot_build(plot_obj)$layout$panel_params[[1]]$x)
  label_to_add <- sprintf("(%s , %s)", round(cut_off, 1), max_print)
  tick_positions <- ranges$get_breaks()
  tick_labels    <- ranges$get_labels()
  if (overlap_ceiling(tick_positions, cut_off)) {
    tick_positions <- tick_positions[-length(tick_positions)]
    tick_labels    <- tick_labels[-length(tick_labels)]
  }
  return(list(tick_positions = c(tick_positions, cut_off),
              tick_labels    = c(tick_labels, label_to_add)))
}

overlap_ceiling <- function(positions, cut_off) {
  positions <- positions[!is.na(positions)]
  n <- length(positions)
  ticks_dif <- positions[n] - positions[n-1]
  (cut_off - positions[n]) / ticks_dif < 0.25
}

update_tickmarks_floor <- function(plot_obj,
                                   cut_off,
                                   min_print) {
  ranges <- suppressMessages(
    ggplot2::ggplot_build(plot_obj)$layout$panel_params[[1]]$x)
  label_to_add <- sprintf("(%s , %s)", min_print, round(cut_off, 1))
  tick_positions <- ranges$get_breaks()
  tick_labels    <- ranges$get_labels()

  if (overlap_floor(tick_positions, cut_off)) {
    tick_positions <- tick_positions[-1]
    tick_labels    <- tick_labels[-1]
  }
  return(list(tick_positions = c(cut_off, tick_positions),
              tick_labels    = c(label_to_add, tick_labels)))
}

overlap_floor <- function(positions, cut_off) {
  positions <- positions[!is.na(positions)]
  ticks_dif <- positions[2] - positions[1]
  (positions[1] - cut_off) / ticks_dif < 0.25
}
