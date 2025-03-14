#'
#' Transferred Charge and Number of Electrons from Chronoamperogram
#'
#'
#' @family EPR Spectroelectrochemistry
#'
#'
#' @description
#'   Evaluation of the transferred charge and the corresponding number of electrons
#'   from \href{https://doi.org/10.1515/pac-2018-0109}{chronoamperogram} related to electrochemical
#'   experiment, performed simultaneously with the EPR time series measurement or independently of the latter.
#'   To acquire charge, the input \eqn{I} \emph{vs} \eqn{time} relation (coming from the
#'   \code{data.at}) is integrated by the \code{\link[pracma:trapz]{pracma::cumtrapz}} function.
#'   \strong{Prior to integration a capacitive current correction must be done}, especially if it is relatively
#'   high in comparison to \href{https://doi.org/10.1515/pac-2018-0109}{faradaic} one. Afterwards,
#'   the number of electrons is calculated by Faraday's law (see details). The output plot can be visualized
#'   either as \eqn{Q(N_{\text{e}})} \emph{vs} \eqn{t} (time) or as \eqn{Q(N_{\text{e}})} \emph{vs} \eqn{E}
#'   (potential, if available in the input \code{data.at}).
#'
#'
#' @details
#'   When quantitative EPR is carried out along with electrochemistry simultaneously,
#'   one can easily compare the number of radicals with the number of transferred electrons.
#'   Number of radicals (\eqn{N_{\text{R}}}) are evaluated from quantitative measurements (see also
#'   \code{\link{quantify_EPR_Abs}}), whereas number of transferred electrons (\eqn{N_{\text{e}}}) is related
#'   to charge (\eqn{Q}), according to:
#'   \deqn{N_{\text{e}} = (Q\,N_{\text{A}})/F}
#'   where \eqn{N_{\text{A}}} stands for the Avogadro's number and \eqn{F} for the Faraday's constants.
#'   Both are obtained by the \code{constans::syms$na} and the \code{constants::syms$f}, respectively,
#'   using the \href{https://r-quantities.github.io/constants/}{constants} package
#'   (see \code{\link[constants]{syms}}). If both numbers match (\eqn{N_{\text{R}} = N_{\text{e}}}),
#'   it reveals the presence of one-electron oxidation/reduction, while different numbers may point
#'   to a more complex mechanism (such as comproportionation, follow-up reactions, multiple electron transfer).
#'
#'
#' @references
#'   Bard AJ, Faulkner LR, White HS (2022). \emph{Electrochemical methods: Fundamentals and Applications},
#'   3rd edition, John Wiley and Sons, Inc., ISBN 978-1-119-33405-7, \url{https://www.wiley.com/en-us/9781119334064}.
#'
#'   Pingarrón JM, Labuda J, Barek J, Brett CMA, Camões MF, Fojta M, Hibbert DB (2020).
#'   “Terminology of Electrochemical Methods of Analysis (IUPAC Recommendations 2019).”
#'   \emph{Pure Appl. Chem.}, \strong{92}(4), 641–694, \url{https://doi.org/10.1515/pac-2018-0109}.
#'
#'   Neudeck A, Petr A, Dunsch L (1999). “The redox mechanism of Polyaniline Studied by Simultaneous
#'   ESR–UV–vis Spectroelectrochemistry.” \emph{Synth. Met.}, \strong{107}(3), 143–158,
#'   \url{https://doi.org/10.1016/S0379-6779(99)00135-6}.
#'
#'   Hans W. Borchers (2023). \emph{pracma: Practical Numerical Math Functions}.
#'   R package version 2.4.4, \url{https://cran.r-project.org/web/packages/pracma/index.html}.
#'
#'
#' @inheritParams plot_ECh_VoC_amperogram
#' @param data.at Data frame (table) object, including required columns like \code{Time} (\eqn{t}),
#'   \code{Current} (\eqn{I}). Even though an arbitrary column label can be used, the best option
#'   is to use labels such as \code{time_s}, \code{I_uA} or \code{I_mA}. Optionally, column related
#'   to potential (\eqn{E}) may be present as well for the transferred charge (\eqn{Q}) or number
#'   of electrons (\eqn{N_{\text{e}}}) \emph{vs} \eqn{E} visualization (see also the arguments
#'   \code{E}, \code{E.unit} and \code{ref.electrode}).
#' @param time Character string, pointing to \code{time}-axis/column header in the original \code{data.at}.
#'   \strong{Default}: \code{time = "time_s"} (time in seconds).
#' @param time.unit Character string, pointing to \code{time}-quantity unit. There are following units available:
#'   \code{time.unit = "s"} (\strong{default}), \code{time.unit = "ms"}, \code{time.unit = "us"} (microseconds),
#'   \code{time.unit = "ns"} or \code{time.unit = "min"}.
#' @param Current Character string, indicating the \code{Current}(\eqn{I})-axis/column quantity name
#'   in the original \code{data.at} object. \strong{Default}: \code{Current = "I_A"} (current in \eqn{\text{A}}).
#' @param Current.unit Character string, pointing to \code{Current} quantity unit like \code{Current.unit = "uA"}
#'   (microamps) \code{Current.unit = "A"} (\strong{default}), \code{Current.unit = "mA"}
#'   and \code{Current.unit = "nA"}.
#' @param tlim Numeric vector of the \code{time}-quantity lower and upper limit, e.g. \code{xlim = c(5,400)}
#'   (time in seconds. \strong{Default}: \code{tlim = NULL} actually setting the entire \code{time} interval
#'   from the original dataset.
#' @param E Character string, referring to \eqn{E}(potential) column name within the input \code{data.at}
#'   dataset. \strong{Default}: \code{E = NULL}, corresponding to situation, when one doesn't want to
#'   visualize transferred charge (or number of electrons) \emph{vs} \eqn{E}.
#' @param E.unit Character string, setting the potential unit (see \code{E} argument), usually
#'   \code{E.unit = "mV"} or \code{E.unit = "V"}. \strong{Default}: \code{E.unit = NULL}, corresponding
#'   to situation, when one doesn't want to visualize transferred charge (or number of electrons)
#'   \emph{vs} \eqn{E}.
#' @param Ne.output Logical. Should be the number of transferred electrons (\code{Ne}) presented within
#'   the plot ? \strong{Default}: \code{Ne.output = TRUE}.
#' @param separate.plots Logical. By \strong{default}, both relations: \eqn{Q(N_{\text{e}})} \emph{vs}
#'   \eqn{t,E} (time or potential) are shown in one plot (\code{separate.plots = FALSE}). One can separate
#'   \eqn{N_{\text{e}}} \emph{vs} \eqn{t,E} and \eqn{Q} \emph{vs} \eqn{t,E} into individual plots setting
#'   up the \code{separate.plots = TRUE}.
#'
#'
#' @return
#'   List containing the following elements, depending on \code{separate.plots}:
#'   \enumerate{
#'   \item If \code{separate.plots = FALSE}
#'     \describe{
#'     \item{df}{Original \code{data.at} data frame object with the following additional
#'     columns/variables: \code{Q_C} (charge in coulombs), \code{Q_mC} (charge in millicoulombs,
#'     if the maximum charge \eqn{\leq 0.099\,\text{C}}) and \code{Ne} (number of transferred
#'     electrons, if \code{Ne.output = TRUE}).}
#'     \item{plot}{Side-by-side plot object (list) of \eqn{N_{\text{e}}} \emph{vs} \eqn{t,E}
#'     as well as \eqn{Q} \emph{vs} \eqn{t,E}.}
#'     }
#'
#'   \item If \code{separate.plots = TRUE}
#'     \describe{
#'     \item{df}{Original \code{data.at} data frame object with the following additional
#'     columns/variables: \code{Q_C} (charge in coulombs), \code{Q_mC} (charge in millicoulombs,
#'     if the maximum charge \eqn{\leq 0.099\,\text{C}}) and \code{Ne} (number of transferred
#'     electrons, if \code{Ne.output = TRUE}).}
#'     \item{plot.Ne}{Plot object (list) of \eqn{N_{\text{e}}} \emph{vs} \eqn{t,E}.}
#'     \item{plot.Q}{Plot object (list) of \eqn{Q} \emph{vs} \eqn{t,E}.}
#'     }
#'   }
#'
#'
#' @examples
#' ## loading package built-in example file =>
#' ## `.txt` file generated by the IVIUM potentiostat software
#' triarylamine.path.cv <-
#' load_data_example(file = "Triarylamine_ECh_CV_ivium.txt")
#' ## the data frame contains following variables:
#' ## time, desired potential, current and the actual/applied
#' ## potential
#' triarylamine.data.cv <-
#'   data.table::fread(file = triarylamine.path.cv,
#'     skip = 2,
#'     col.names = c("time_s",
#'                   "E_V_des", # desired potential
#'                   "I_A",
#'                   "E_V_app") # applied potential
#'   )
#' #
#' ## simple chronoamperogram plot
#' plot_ECh_VoC_amperogram(data.vat = triarylamine.data.cv,
#'   x = "time_s",
#'   x.unit = "s",
#'   Current = "I_A",
#'   Current.unit = "A",
#'   ticks = "in"
#'  )
#' #
#' ## transferred charge and the number of electrons
#' ## with default parameters
#' triarylamine.data.QNe <-
#'   eval_ECh_QNe_chronoamp(data.at = triarylamine.data.cv)
#' #
#' ## data frame preview
#' triarylamine.data.QNe$df
#' #
#' ## graphical representation
#' triarylamine.data.QNe$plot
#'
#'
#'
#' @export
#'
#'
eval_ECh_QNe_chronoamp <- function(data.at,
                                   ## time can be "time_min","time_ms","time_us" ,"time_ns" as well
                                   time = "time_s",
                                   time.unit = "s", ## can be "min","ms","us","ns"
                                   tlim = NULL,
                                   Current = "I_A",
                                   Current.unit = "A", ## can be "nA", "mA" and "uA" as well
                                   E = NULL, ## potential
                                   E.unit = NULL, ## potential unit V, mV
                                   ref.electrode = NULL,
                                   Ne.output = TRUE,
                                   separate.plots = FALSE){
  #
  ## 'Temporary' processing variables
  . <- NULL
  Q_C <- NULL
  Q_mC <- NULL
  Ne <- NULL
  #
  ## Define limits if `tlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.at.time.region <- c(
    min(data.at[[time]]),
    max(data.at[[time]])
  )
  tlim <- tlim %>% `if`(
    is.null(tlim),
    data.at.time.region, .
  )
  #
  ## Time unit conversion with a new column
  if (time.unit == "s"){
    ## rename time column
    time <- time %>% `if`(time != "time_s","time_s", .)
    ## take the data as it is
    data.at <- data.at
  } else if (time.unit == "ms") {
    data.at[["time_s"]] <- data.at[[time]] * 0.001
    tlim <- tlim * 0.001 ## selected time region in seconds
  } else if (time.unit == "us") {
    data.at[["time_s"]] <- data.at[[time]] * 1e-6
    tlim <- tlim * 1e-6
  } else if (time.unit == "ns") {
    data.at[["time_s"]] <- data.at[[time]] * 1e-9
    tlim <- tlim * 1e-9
  } else if (time.unit == "min") {
    data.at[["time_s"]] <- data.at[[time]] * 60
    tlim <- tlim * 60
  }
  #
  ## Current unit conversion with a new column
  if (Current.unit == "A"){
    ## rename column
    Current <- Current %>% `if`(Current != "I_A","I_A", .)
    ## take the data as it is
    data.at <- data.at
  } else if (Current.unit == "mA") {
    data.at[["I_A"]] <- data.at[[Current]] * 0.001
  } else if (Current.unit == "uA") {
    data.at[["I_A"]] <- data.at[[Current]] * 1e-6
  } else if (Current.unit == "nA") {
    data.at[["I_A"]] <- data.at[[Current]] * 1e-9
  }
  #
  ## constants to evaluate transf. charge and number of electrons
  ## Avogadro's
  N_A <- constants::syms$na
  ## Faraday's
  Faraday <- constants::syms$f
  #
  ## evaluate charge by the I-t (Current-time) integration
  data.at <- data.at %>%
    dplyr::filter(dplyr::between(.data[["time_s"]],tlim[1],tlim[2])) %>%
    dplyr::mutate(Q_C = pracma::cumtrapz(.data[["time_s"]],.data[["I_A"]])[, 1])
  #
  ## charge re-scale and transfer it into `mC`
  ## `mC` condition
  Q_mC_condition <- ifelse(max(data.at[["Q_C"]]) <= 0.099,TRUE,FALSE)
  Q_unit_string <- switch(2-Q_mC_condition,"mC","C")
  #
  if (Q_mC_condition) {
    data.at <- data.at %>%
      dplyr::mutate(Q_C = abs(min(.data$Q_C) - .data$Q_C)) %>%
      ## a capacitive current correction must be performed
      ## prior to Q or Ne evaluation !!
      dplyr::mutate(Q_mC = .data$Q_C * 1e3)
  } else {
    data.at <- data.at %>%
      dplyr::mutate(Q_C = abs(min(.data$Q_C) - .data$Q_C))
  }
  #
  ## Faraday's Law, calculating number of electrons
  if (isFALSE(Ne.output)){
    data.at <- data.at
  } else {
    data.at <- data.at %>%
      ## Ne = (Q * NA) / F
      dplyr::mutate(Ne = (.data$Q_C * N_A) / Faraday)
  }
  #
  ## plots depending on Potential
  if (!is.null(E)){
    if (is.null(E.unit)) {
      stop(" Potential unit is set to `NULL`!\n
           Please, define the `E.unit` by character string !! ")
    } else {
      ## plot Charge and E
      plot.Q.vs.E <-
        data.at %>%
        ggplot(aes(x = .data[[E]],
                   y = switch(2-Q_mC_condition, .data$Q_mC, .data$Q_C))) +
        geom_path(linewidth = 0.75,color = "darkred") +
        labs(x = bquote(italic(E)~~"("~.(E.unit)~")"~~~italic(vs)~~~.(ref.electrode)),
             y = bquote(italic(Transferred)~~~italic(Q)~~~"("~.(Q_unit_string)~")")) +
        plot_theme_In_ticks() # +
      # ggplot2::ggtitle(label = bquote(italic(Q)~~italic(vs)~~italic(E)))
      #
      ## plot Ne and Potential
      if (isTRUE(Ne.output)){
        plot.Ne.vs.E <-
          data.at %>%
          ggplot(aes(x = .data[[E]],
                     y = .data$Ne)) +
          geom_path(linewidth = 0.75,color = "blue") +
          labs(x = bquote(italic(E)~~"("~.(E.unit)~")"~~~italic(vs)~~~.(ref.electrode)),
               y = bquote(italic(Transferred)~~~italic(N)[e])) +
          plot_theme_In_ticks() # +
        # ggplot2::ggtitle(label = bquote(italic(N)[e]~~italic(vs)~~italic(E)))
        #
        ## entire plot
        complete.plot <-
          patchwork::wrap_plots(plot.Q.vs.E,
                                plot.Ne.vs.E,
                                nrow = 1)
      } else {
        complete.plot <- plot.Q.vs.E
      }
    }
    #
    ## Potential is not included (now TIME)=>
  } else {
    if (!is.null(E) || !is.null(E.unit)) {
      stop("Charge and Ne are evaluated from `chronoamperogram` !\n
           Please, set both `E` arguments (E + E.unit) to `NULL` !! ")
    } else {
      ## plot Charge and Time
      plot.Q.vs.t <-
        data.at %>%
        ggplot(aes(x = .data[["time_s"]],
                   y = switch(2-Q_mC_condition, .data$Q_mC, .data$Q_C))) +
        geom_line(linewidth = 0.75,color = "darkred") +
        coord_cartesian(xlim = tlim) +
        labs(x = bquote(italic(Time)~~"("~s~")"),
             y = bquote(italic(Transferred)~~~italic(Q)~~~"("~.(Q_unit_string)~")")) +
        plot_theme_In_ticks() # +
      # ggplot2::ggtitle(label = bquote(italic(Q)~~italic(vs)~~italic(t)))
      #
      ## plot Ne and time
      if (isTRUE(Ne.output)){
        plot.Ne.vs.t <-
          data.at %>%
          ggplot(aes(x = .data[["time_s"]],
                     y = .data$Ne)) +
          geom_line(linewidth = 0.75,color = "blue") +
          coord_cartesian(xlim = tlim) +
          labs(x = bquote(italic(Time)~~"("~s~")"),
               y = bquote(italic(Transferred)~~~italic(N)[e])) +
          plot_theme_In_ticks() # +
        # ggplot2::ggtitle(label = bquote(italic(N)[e]~~italic(vs)~~italic(t)))
        #
        ## entire plot
        complete.plot <-
          patchwork::wrap_plots(plot.Q.vs.t,
                                plot.Ne.vs.t,
                                nrow = 1)
      } else {
        complete.plot <- plot.Q.vs.t
      }
    }
  }
  #
  if (isTRUE(separate.plots)) {
    ## potential condition
    potential.cond <- ifelse(!is.null(E),TRUE,FALSE)
    #
    result <- list(plot.Ne = switch(2-potential.cond,plot.Ne.vs.E,plot.Ne.vs.t),
                   plot.Q = switch(2-potential.cond,plot.Q.vs.E,plot.Q.vs.t),
                   df = data.at)
    #
  } else {
    result <- list(plot = complete.plot,df = data.at)
  }
  #
  return(result)
  #
}
