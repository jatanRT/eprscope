
#
#' Plot Voltammogram or Chronoamperogram from (Spectro)Electrochemical Experiment
#'
#'
#' @family EPR Spectroelectrochemistry
#'
#'
#' @description
#'   A short description...
#'
#'
#'
#' @inheritParams plot_EPR_Specs
#' @param data.voltamm Data frame (table) object including required columns like \code{Potential}
#'   (\eqn{E}) and/or \code{Time} (\eqn{t}) as well as \code{Current} (\eqn{I}). Even though an arbitrary
#'   column label can be used, the best option is to use labels e.g. like \code{E_V}, \code{E_mV}, \code{time_s},
#'   \code{I_uA} or \code{I_mA}.
#' @param x Character string pointing to \code{x}-axis/column quantity, in the original \code{data.voltamm},
#'   like \strong{potential} (e.g. \code{x = "E_V"}, \code{x = "E_mV"}) or \strong{time} (e.g. \code{x = "time_s"},
#'   \code{x = "time_min"} or \code{x = "time_ms"}). \strong{Default}: \code{x = "E_V"} (potential in volts).
#' @param x.unit Character string pointing to \code{x}-quantity unit. There are following units available:
#'   \code{x.unit = "V"}, \code{x.unit = "mV"} (in case of voltammetry, \code{x} corresponds to potential)
#'   as well as \code{x.unit = "s"}, \code{x.unit = "ms"} or \code{x.unit = "min"} (in case of chronoamperometry,
#'   \code{x} corresponds to time). \strong{Default}: \code{x.unit = "V"}.
#' @param Current Character string indicating the \code{Current}(\eqn{I})-axis/column quantity in the original
#'   \code{data.voltamm} object. \strong{Default}: \code{Current = "I_ua"} (current in \eqn{\text{µA}}).
#' @param Current.unit Character string pointing to \code{Current} quantity unit like \code{Current.unit = "uA"}
#'   (microamps, \strong{default}), \code{Current.unit = "A"}, \code{Current.unit = "mA"} and \code{Current.unit = "nA"}.
#' @param xlim Numeric vector of the \code{x}-quantity visual border limits, e.g. like \code{xlim = c(-100,400)}
#'   (potential in "mV", in case of voltammetry) or \code{xlim = c(0,1000)} (time in seconds,
#'   in case of chronoamperometry). \strong{Default}: \code{xlim = NULL} actually defining the entire \code{x}-range
#'   from the original dataset.
#' @param Ilim Numeric vector characterizing the visual border limits of the current, e.g. like \code{Ilim = c(-0.05,0.2)},
#'   corresponding to current in milliamps. \strong{Default}: \code{Ilim = NULL} defining the entire current (\eqn{I}) range
#'   from the original dataset.
#' @param line.color Character string related to line color of the voltammogram / chronoamperoggram. All \pkg{ggplot2} compatible
#'   colors are allowed, e.g. like \code{line.color = "blue"}. \strong{Default}: \code{}
#' @param ref.electrode Character string ...TBC...
#' @param plot.interact Logical, ...TBC...
#' @param ticks Character string ...the axis ticks pointing either
#'
#'
#' @return Graphical representation (list object) of voltammogram or chronoamperogram either in static
#'   form by \pkg{ggplot2} (\code{plot.interact = FALSE}) or in interactive form by \pkg{plotly}
#'   (\code{plot.interact = TRUE}).
#'
#'
#' @examples
#' \dontrun{
#' TBC
#' TBC
#' }
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 geom_path
plot_ECh_VoC_amperogram <- function(data.voltamm,
                                    x = "E_V", ## can be "time_s", "time_min" or "time_ms" as well
                                    x.unit = "V", ## can be "mV" or "s" or "min" or "ms" as well
                                    Current = "I_uA",
                                    Current.unit = "uA", ## can be "nA", "mA" and "A" as well
                                    xlim = NULL,
                                    Ilim = NULL,
                                    line.color = "darkviolet",
                                    line.width = 0.75,
                                    theme.basic = "theme_gray",
                                    axis.title.size = 15,
                                    axis.text.size = 14,
                                    border.line.color = "black",
                                    border.line.width = 0.5,
                                    ref.electrode = NULL,
                                    plot.interact = FALSE,
                                    ticks = "out",
                                    grid = TRUE){
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Define limits if `xlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.x.region <- c(
   min(data.voltamm[[x]]),
   max(data.voltamm[[x]])
   )
   xlim <- xlim %>% `if`(
     is.null(xlim),
     data.x.region,
     .
   )
  ## Define limits for `I` (current,similarly like in case of `x` above)
  ## 10% of the `max` below the `min` and 10% above the `max
  data.I.region <- c(
   min(data.voltamm[[Current]]),
   max(data.voltamm[[Current]])
   )
   data.I.region.2 <- c(
     data.I.region[1] - (data.I.region[2] * 0.1),
     data.I.region[2] * 1.1
   )
   Ilim <- Ilim %>% `if`(
     is.null(Ilim),
     data.I.region.2,
     .
   )
  #
  ## Voltammogram borders for the visualization (see 'coord_cartesian')
  x.start <- xlim[1]
  x.end <- xlim[2]
  #
  ## Time unit conversion
  if (x.unit != "V" || x.unit != "mV"){
    if (x.unit == "min"){
      data.voltamm[[x]] <- data.voltamm[[x]] * 60
      ## rename "x" column
      data.voltamm <- data.voltamm %>%
        dplyr::rename(dplyr::all_of(c("time_s" = "time_min")))
    }
    if (x.unit == "ms"){
      data.voltamm[[x]] <- data.voltamm[[x]] * 0.001
      ## rename "x" column
      data.voltamm <- data.voltamm %>%
        dplyr::rename(dplyr::all_of(c("time_s" = "time_ms")))
    }
  }
  #
  ## x Label conditions + definitions =>
  ## reference electrode condition
  ref.electrode.condition <- ifelse(!is.null(ref.electrode),TRUE,FALSE)
  ## Definitions
  if (!is.null(ref.electrode)) {
    x.label.ref <- bquote(italic(E)~~"("~.(x.unit)~")"~~~italic(vs)~~~.(ref.electrode))
    x.label.ref.html <- paste0(
      "<i>E</i> (",
      x.unit,
      ")",
      "  <i>vs</i>  ",
      ref.electrode
      )
  }
  ## Conditions (V)
  if (x.unit == "V") {
  x.plot.limits <- c(x.start - 0.1, x.end + 0.1)
  ## label for interactive graph/plot
  if (isTRUE(plot.interact)) {
    ## reference electrode
    x.label.html <- switch(2 - ref.electrode.condition,
      x.label.ref.html,
      paste0("<i>E</i> (", x.unit, ")")
      )
    } else {
    ## reference electrode
    x.label <- switch(2 - ref.electrode.condition,
      x.label.ref,
      bquote(italic(E)~~"("~V~")")
      )
    }
  }
  ## Conditions (mV) =>
  if (x.unit == "mV"){
    x.plot.limits <- c(x.start - 100,x.end + 100)
    ## label for interactive graph/plot
    if (isTRUE(plot.interact)){
      ## reference electrode
      x.label.html <- switch(2-ref.electrode.condition,
                             x.label.ref.html,
                             paste0("<i>E</i> (",x.unit,")"))
    } else {
      ## reference electrode
      x.label <- switch(2-ref.electrode.condition,
                        x.label.ref,
                        bquote(italic(E)~~"("~m~V~")"))
    }
  }
  ## Conditions (s)
  if (x.unit == "s"){
    x.plot.limits <- c(x.start - 10,x.end + 10)
    if (!is.null(ref.electrode)){
      cat("To display the chronoamperogram",
          "no reference electrode is required.",
          sep = "\n")
    } else {
      if (isTRUE(plot.interact)){
        x.label.html <- paste0("<i>Time</i> (",x.unit,")")
      } else {
        x.label <- bquote(italic(Time)~~"("~s~")")
      }
    }
  }
  ## y (Current/I) Label Condition function (incl. interactive plot labels) =>
  yLabel <- function(unit,interact = FALSE){
    if (unit == "A"){
      if (isFALSE(interact)){
        return(bquote(italic(Current)~~"("~A~")"))
      } else {
        return("<i>Current</i>  (A)")
      }
    }
    if (unit == "uA"){
      if (isFALSE(interact)){
        return(bquote(italic(Current)~~"("~mu~A~")"))
      } else {
        return("<i>Current</i>  (&#181; A)")
      }
    }
    if (unit == "mA"){
      if (isFALSE(interact)){
        return(bquote(italic(Current)~~"("~m~A~")"))
      } else {
        return("<i>Current</i>  (m A)")
      }
    }
    if (unit == "nA"){
      if (isFALSE(interact)){
        return(bquote(italic(Current)~~"("~n~A~")"))
      } else {
        return("<i>Current</i>  (n A)")
      }
    }
  }
  if (isFALSE(plot.interact)){
    ## General labels for `{ggplot2}`
    Labs <- labs(x = x.label,y = yLabel(unit = Current.unit))
    #
    ## plot theme OUT-TICKS
    theme_out_ticks <- theme(
      axis.ticks.length = unit(6, "pt"),
      axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
      axis.text.y = element_text(margin = margin(4, 6, 6, 0, unit = "pt"), size = axis.text.size),
      axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
      axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
      panel.border = element_rect(
        color = border.line.color,
        linewidth = border.line.width,
        fill = NA
      )
    )
    #
    ## plot theme IN-TICKS
    theme_in_ticks <- theme(
      axis.ticks.length = unit(-6, "pt"),
      axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
      axis.text.y = element_text(margin = margin(4, 6, 6, 0, unit = "pt"), size = axis.text.size),
      axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
      axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
      panel.border = element_rect(
        color = border.line.color,
        linewidth = border.line.width,
        fill = NA
      )
    )
    #
    ## plot theme without grid
    theme.Nogrid <- theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
    ## grid condition
    nogrid.cond <- ifelse(grid,FALSE,TRUE)
    #
    ## x,y ticks of the upper and right axis, respectively:
    axis_x_duplicate <-
      scale_x_continuous(sec.axis = dup_axis(name = "",
                                             labels = NULL))
    axis_y_duplicate <-
      scale_y_continuous(sec.axis = dup_axis(name = "",
                                             labels = NULL))
    #
  }
  ## basic voltammogram plot
  basic.voltammogram <-
    ggplot(data = data.voltamm,
           aes(x = .data[[x]],y = .data[[Current]])) +
    geom_path(linewidth = line.width,color = line.color) +
    coord_cartesian(xlim = x.plot.limits,ylim = Ilim)
  #
  ## ALL `GGPLOT2` THEMES TOGETHER
  if (isFALSE(plot.interact)){
    if (theme.basic == "theme_gray"){
      if (ticks == "out"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          {if(nogrid.cond)theme.Nogrid} +
          theme_out_ticks
      }
      if (ticks == "in"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          {if(nogrid.cond)theme.Nogrid} +
          theme_in_ticks +
          axis_x_duplicate +
          axis_y_duplicate
      }
    }
    if (theme.basic == "theme_bw"){
      if (ticks == "out"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_bw() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_out_ticks
      }
      if (ticks == "in"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_bw() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_in_ticks +
          axis_x_duplicate +
          axis_y_duplicate
      }
    }
    if (theme.basic == "theme_light"){
      if (ticks == "out"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_light() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_out_ticks +
          theme(panel.border = element_blank()) ## panel border re-definition
      }
      if (ticks == "in"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_light() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_in_ticks +
          axis_x_duplicate +
          axis_y_duplicate +
          theme(panel.border = element_blank())
      }
    }
    if (theme.basic == "theme_linedraw"){
      if (ticks == "out"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_linedraw() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_out_ticks
      }
      if (ticks == "in"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_linedraw() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_in_ticks +
          axis_x_duplicate +
          axis_y_duplicate
      }
    }
    if (theme.basic == "theme_classic"){
      if (ticks == "out"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_classic() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_out_ticks +
          theme(panel.border = element_blank()) ## panel border re-definition
      }
      if (ticks == "in"){
        plot.iv.it <-
          basic.voltammogram +
          Labs +
          theme_classic() +
          {if(nogrid.cond)theme.Nogrid} +
          theme_in_ticks +
          axis_x_duplicate +
          axis_y_duplicate +
          theme(panel.border = element_blank())
      }
    }
  }
  #
  ## Conditions for interactive/static plots
  if (isTRUE(plot.interact)){
    plot.iv.it.interact <-
      plotly::ggplotly(basic.voltammogram) %>%
        plotly::layout(
          plot_bgcolor = "#e5ecf6",
          xaxis = list(
            title = list(
              text = x.label.html,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size),
            gridcolor = "#ffff",
            linecolor = plotly::toRGB("black"),
            linewidth = 1.2, showline = TRUE, mirror = TRUE
          ),
          yaxis = list(
            title = list(
              text = yLabel(
                unit = Current.unit,
                interact = TRUE
              ),
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size),
            gridcolor = "#ffff",
            linecolor = plotly::toRGB("black"),
            linewidth = 1.2, showline = TRUE, mirror = TRUE
          )
        )
    #
    ## the entire interactive (`plotly`)
    return(plot.iv.it.interact)
    #
  } else {
    ## the entire static`ggplo2` plot
    return(plot.iv.it)
  }
  #
}
