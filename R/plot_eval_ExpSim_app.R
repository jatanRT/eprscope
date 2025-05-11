
#
#' Interactive Application to Plot and Evaluate CW Isotropic EPR Spectra
#'
#'
#' @family Shiny Interactive Applications
#'
#'
#' @description
#'   Launch this app in order to quickly check recorded EPR spectra with their instrumental parameters.
#'   Additionally, you may try to interactively simulate the isotropic continuous wave (CW) EPR spectra
#'   using the simple graphical user interface based on the \href{https://shiny.posit.co/}{R Shiny} and several
#'   functions from the package (see \code{Details}).
#'
#'
#' @details
#'   Based on the acquisition EPR spectrum \code{origin}, in graphical user interface one can load a spectrum
#'   and its instrumental/experimental parameters by the \code{\link{readEPR_Exp_Specs}} as well as
#'   by the \code{\link{readEPR_params_tabs}} functions. An EPR is immediately depicted in the interactive
#'   \code{{plotly}} format by \code{\link{plot_EPR_Specs2D_interact}}. The magnetic flux density \eqn{B}
#'   of such EPR spectrum can be changed from \code{mT} to \code{G} and to \code{g}-values. Moreover,
#'   if the compound/radical structure is already known, one can display its structure by entering
#'   its corresponding \code{SMILES} code, using the \code{\link{draw_molecule_by_rcdk}}.
#'   Uploaded \code{ASCII} data (spectrum + parameters) are applied to interactively simulate
#'   the corresponding experimental isotropic EPR spectrum by the \code{\link{eval_sim_EPR_iso}} and to visualize
#'   it by the \code{\link{present_EPR_Sim_Spec}}. Additionally, one can also print and download
#'   the corresponding data frame/table in several formats (\code{.csv},\code{.pdf} or \code{.xlsx})
#'   and thus use the data not only in \code{R} environment but also in other software tools.
#'   All instantaneously presented EPR spectra (either after loading the ASCII data or after simulation)
#'   can be also exported in formats like \code{.png}, \code{.jpeg} as well as \code{.pdf}.
#'
#'
#' @examples
#' \dontrun{
#' ## run the app just by the following
#' ## command in R console:
#' plot_eval_ExpSim_app()
#' #
#' ## this is an examples of the raw
#' ## shiny R code to run the app,
#' ## code example from the `SERVER` part:
#' server <- function(input, output,session) {
#' ## redefinition of `norm.vec.add`
#' norm_vec <- reactiveValues(val = NULL)
#' observe({
#'  if (isTRUE(input$normVec)){
#'    norm_vec$val <- as.numeric(unlist(strsplit(input$vecNorm,",")))
#'  } else {
#'    norm_vec$val <- NULL
#'  }
#' })
#' #
#' ## Load experimental spectrum data frame
#' expr_data <- reactive({
#' #
#' req(input$ASCIIfile)
#' #
#'  spectr.data <-
#'    readEPR_Exp_Specs(
#'      path_to_ASC = input$ASCIIfile$datapath,
#'      col.names = switch(
#'        3-origin.cond(orig = input$origin),
#'        c("index","B_G", "dIepr_over_dB"),
#'        c("B_G", "dIepr_over_dB"),
#'        c("B_mT","dIepr_over_dB")
#'      ),
#'      x.unit = switch(
#'        3-origin.cond(orig = input$origin),
#'        "G",
#'        "G",
#'        "mT"
#'      ),
#'      qValue = input$qValue,
#'      origin = input$origin,
#'      norm.vec.add = norm_vec$val
#'    )
#'  spectr.data
#' })
#' #
#' # -------------------- INTERACTIVE SPECTRUM ---------------------
#' #
#' output$plot <- plotly::renderPlotly({
#'  #
#'  ## add g-Value condition:
#'  if (isTRUE(input$gval)) {
#'    expr_data_g <- expr_data() %>%
#'      dplyr::mutate(g_Value =
#'                      eval_gFactor(
#'                        nu.val = input$MWnuGHz,
#'                        B.val = .data$B_mT
#'                      ))
#'  }
#' ...})
#'  }
#' }
#'
#'
#' @export
#'
#'
plot_eval_ExpSim_app <- function() {
  #
  appDir <-
    system.file(
      "shinyApp01",
      package = "eprscope"
    )
  #
  if (appDir == "") {
    stop(
      "Could not find `shinyApp01`. Try to reinstall the `{eprscope}` !! ", call. = FALSE
    )
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}
