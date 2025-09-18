#'
#' EPR Intensity Multiplet Prediction for Interactions of Electron with Selected Nucleus/Nuclei
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   What is the expected EPR intensity pattern for a group of equivalent nuclei? One may use this function for a quick
#'   prediction/visualization of EPR spectrum multiplets (without a specific hyperfine splitting). Its central code
#'   (computation of binomial/multinomial coefficients) is implemented in the \code{\link{eval_sim_EPR_iso}} to provide
#'   simulations of isotropic EPR spectra. The theoretical intensities/coefficients are returned either as a vector
#'   or as a barplot using the \code{\link[ggplot2]{geom_bar}}.
#'
#'
#' @param nucle_us_i Character string, pointing to specific nucleus/nuclei in the form like \code{"14N"} or \code{"2H"}.
#'   If \code{nucle_us_i = "1H"}, i.e. interaction with proton(s) is considered as \strong{default} one. Based on the string,
#'   the characteristic nuclear spin quantum number \code{I} is taken from the \code{\link{isotopes_ds}} dataset. If one wants
#'   to define \code{I} in a more general way, just put the \code{nucle_us_i = NULL} and define the desired \code{I}
#'   by the corresponding argument (see below).
#' @param I Numeric value, pointing to nuclear spin quantum number of proposed nucleus/nuclei interacting with unpaired electron.
#'   \code{I} must be specified in the form like \code{I = 0.5} (for spin 1/2), \code{I = 1.5} (for spin 3/2) or \code{I = 1}.
#'   The \strong{default} value, \code{I = NULL}, is applied only in such case when the \code{I} is defined
#'   via the \code{nucle_us_i} argument (see above).
#' @param N.nuclei Numeric value (integer), corresponding to number of interacting equivalent nuclei (within a group).
#'   \strong{Default}: \code{N.nuclei = 1}.
#'
#'
#' @returns A list consisting of:
#'   \describe{
#'   \item{intensity.vec}{A named vector of binomial/multinomial coefficients related to theoretical intensities
#'   of isotropic EPR spectrum when the coupling between unpaired electron and the surrounding nuclei is present.
#'   Coefficient/Vector names correspond to total spin quantum numbers (e.g. for one \code{"14N"} nucleus, the names
#'   are represented by the \code{c("-1","0","1")} vector).}
#'   \item{plot}{GGplot2 object/list as a graphical representation of the \code{intensity.vec} list component.}
#'   }
#'
#' @examples
#' ## intensity multiplet for methyl group
#' ## (3 x 1H equivalent nuclei)
#' methyl.multipl <-
#'   plot_eval_EPRtheo_mltiplet(
#'     N.nuclei = 3
#'   )
#' #
#' ## vector of predicted
#' ## intensities/coefficients
#' methyl.multipl$intensity.vec
#' #
#' ## graphical repsentation
#' methyl.multipl$plot
#' #
#' ## intensity multiplet for 2 x 14N
#' ## in TMPD (Wuster's blue) radical cation
#' tmpd.14N.multipl <-
#'   plot_eval_EPRtheo_mltiplet(
#'     I = 1,
#'     N.nuclei = 2
#'   )
#' #
#' tmpd.14N.multipl$plot
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 geom_bar scale_x_continuous
plot_eval_EPRtheo_mltiplet <- function(nucle_us_i = "1H",I = NULL,N.nuclei = 1) {
  #
  ## Temporary processing variables
  . <- NULL
  #
  ## conditions for `nucle_us_i` and I
  if (length(nucle_us_i) > 1) {
    stop(" Please specify ONLY INDIVIDUAL CHARACTER STRING (not a vector)\n
         for the proposed nucleus/nuclei interacting with the unpaired electron !")
  }
  #
  if (is.null(I)) {
    if (!is.null(nucle_us_i)) {
      ## actuall spin quantum number from the "nuclei" dataset
      I <- eprscope::isotopes_ds %>%
        dplyr::filter(.data$Isotope == nucle_us_i) %>%
        dplyr::pull(.data$Spin)
      I <- unname(I)
    } else {
      stop(" Nucleus/Nuclei (`nucle_us_i`) and spin quantum number (`I`)\n
         are not defined. At least one of the arguments must be properly\n
         set and the other one assigned to `NULL`!")
    }
  } else {
    if (!is.null(nucle_us_i)) {
      ## redefinition of nucleus/nuclei string (expecting mistakes:-))
      nucle_us_i <- NULL
    } else {
      I <- I
    }
  }
  #
  ## number of nuclei
  h <- N.nuclei
  #
  ## including the spin quantum numbers
  ## as names for the vector components (see below)
  ## in order to be sure that the pattern corresponds
  ## to the right mI
  #
  ## not total mI value range:
  mI_values <- seq(-I ,I ,by = 1)
  ## ...and the total:
  mI_values_total <- seq(- (I * h), (I * h), by = 1)
  mI_range_total <- length(mI_values_total)
  #
  ## initialize intensity multiplet pattern (with names):
  intensity_M <- rep(0,mI_range_total)
  names(intensity_M) <- mI_values_total
  #
  ## recursive function
  ## (see e.g. https://www.geeksforgeeks.org/recursive-functions-in-r-programming/,
  ## or https://data-flair.training/blogs/r-recursive-function/)
  ## to handle combinatorics:
  combinations_fn <- function(depth, remain, actual_Sum, actual_combo) {
    if (depth > length(mI_values)) {
      if (remain == 0) {
        # calculate multinomial coefficient
        coeff <- factorial(h) / prod(factorial(actual_combo))
        # add to appropriate intensity `int`
        int <- as.character(actual_Sum)
        intensity_M[int] <<- intensity_M[int] + coeff
        ## double arrow for loop (state maintaining)
      }
      return()
    }
    #
    for (count in 0:min(remain, h)) {
      new_combo <- actual_combo
      new_combo[depth] <- count
      combinations_fn(
        depth + 1,
        remain - count,
        actual_Sum + (count * mI_values[depth]),
        new_combo
      )
    }
  }
  # starting recursive calculation
  combinations_fn(1, h, 0, rep(0,length(mI_values)))
  #
  ## create data frame form the `intensity_M`
  nucleu.us.i.df <-
    data.frame(
      Total_Spin_QuantN <- as.numeric(names(intensity_M)),
      Intensity <- unname(intensity_M)
    )
  #
  ## size of the intensity value annotations within the bar plot
  ## depending on number of nuclei
  if (N.nuclei <= 4) {
    size.coeff.bar.plot = 6
  } else if (N.nuclei <= 10 & N.nuclei >= 4) {
    size.coeff.bar.plot = 5
  } else if (N.nuclei > 10 & N.nuclei <= 16) {
    size.coeff.bar.plot = 4
  } else if (N.nuclei > 16) {
    size.coeff.bar.plot = 3
  }
  #
  ## extracting strings from `nucle_us_i` for plot title
  nucle.us.i.num <- unlist(
    stringr::str_extract(nucle_us_i,pattern = "[[:digit:]]+")
  )
  nucle.us.i.char <- unlist(
    stringr::str_extract(nucle_us_i,pattern = "[[:alpha:]]+")
  )
  #
  ## craate bar plot for illustration
  nucleu.us.i.plot <-
    ggplot(
      data = nucleu.us.i.df,
      aes(x = Total_Spin_QuantN,y = Intensity)
    ) +
    geom_bar(
      stat = "identity",
      color = "darkgray",
      fill = factor(Intensity),#"blue",
      alpha = 0.54,
      linewidth = 0.75
    ) +
    geom_text(
      label = Intensity,
      vjust = `if`(N.nuclei >= 4,0.2,1.4),
      color = `if`(N.nuclei >= 4,"black","white"),
      size = size.coeff.bar.plot,
      fontface = "bold"
    ) +
    scale_x_continuous(breaks = nucleu.us.i.df$Total_Spin_QuantN) +
    labs(
      title = switch(
        2 - is.null(nucle_us_i),
        bquote(Theoretical ~ EPR ~ Intensity ~ Pattern ~ "for" ~~
                 .(N.nuclei) ~ {}%*%{} ~"("~ I ~{}=={}~ .(I) ~ ")"),
        bquote(Theoretical ~ EPR ~ Intensity ~ Pattern ~ "for" ~~
                 .(N.nuclei) ~ {}%*%{} ~ {}^.(nucle.us.i.num)*.(nucle.us.i.char)),
      ),
      x = bquote(italic(Total ~~ Spin ~~ Quantum ~~ Number ~~ of ~~ the ~~
                          Equiv. ~~ Nuclei ~~ Group)),
      y = bquote(italic(Intensity)~~"("~p.d.u.~")")
    ) + theme(
      panel.border = element_rect(
        fill = NA,
        color = "darkgray",
        linewidth = 1.6
      ),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 14)
    )
  #
  return(
    list(
      intensity.vec = intensity_M,
      plot = nucleu.us.i.plot
    )
  )
}
