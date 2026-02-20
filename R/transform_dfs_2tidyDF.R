#
#' Transformation of Several Data Frame Objects/Variables into one Tidy (Long Table) Form
#'
#'
#' @description
#'   Taking several data frame objects/variables from the (local/Global) R environment and transforming them into
#'   one with the \href{https://r4ds.hadley.nz/data-tidy.html}{long table/tidy form}. This is especially handy
#'   if a quick comparison of similar plots/EPR spectra in one graph panel is required. Difference between
#'   the \code{\link{readEPR_Exp_Specs_multif}} and the actual function lies in the object input/origin.
#'   While the \code{\link{readEPR_Exp_Specs_multif}} takes the original files/data, coming from spectrometer,
#'   the \code{transform_dfs_2tidyDF} collects temporary processed/stored data frame objects/variables
#'   created meanwhile in the R environment.
#'
#'
#' @param ... Data frame variable names (separated by comma \code{,}) to be transformed into long
#'   table/tidy form in order to quickly compare plots/EPR spectra in one graph panel (see also \code{Examples}).
#'   The number of data frame variables is not limited, however for the series of \eqn{\geq 10-12}
#'   objects/spectra, the graph may look cluttered, depending on complexity of EPR spectra.
#' @param df.names Character string vector (in the form of e.g. \code{c("Spectr_01","Spectr_02")}
#'   or \code{c("283 K","273 K","263 K","253 K")}), describing the individual data frames/plots in the returned
#'   tidy/long table and corresponding to series of plots/spectra (be aware of the data frames order).
#' @param which.coly.norm Character string, pointing to name of the column (in all data frame
#'   objects) to be normalized, by the \code{norm.vec} vector. This column actually corresponds to quantity/variable
#'   presented on graph as \emph{y}-axis. \strong{Default}: \code{which.coly.norm = "dIepr_over_dB"} (that is the
#'   derivative intensity in CW EPR spectra).
#' @param norm.vec Numeric vector, consisting of normalization (division) constants (see also
#'   the \code{which.coly.norm} argument) for each of the data frame objects/variables defined by the ellipsis
#'   \code{...} argument. \strong{Default}: \code{norm.vec = rep(1, times = length(df.names))}, i.e. all normalization
#'   constants equal to \code{1}. If needed, the vector may be redefined (please, be aware of the data frames order).
#' @param var2nd.series Character string, pointing to name of the second variable which is "common denominator"
#'   for the series of \code{df.names} vector elements like \code{var2nd.series = "Temperature"}
#'   or \code{var2nd.series = "Spectrum"} (\strong{default}).
#'
#'
#' @returns
#'   Tidy (long form) data frame/table object/variable, carrying all the individual inputs, defined by the \code{...}
#'   argument and ready for the overlay/offset plot to compare the data in desired series.
#'
#'
#' @examples
#' ## compare TMPD*+ EPR spectrum with that of aminoxyl,
#' ## first, define the path and variable for TMPD*+ data
#' tmpd.data.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' tmpd.data.df <-
#'   readEPR_Exp_Specs(
#'     tmpd.data.path,
#'     col.names = c("B_G","dIepr_over_dB"),
#'     qValue = 3500,
#'     origin = "winepr"
#'    )
#' #
#' ## preview
#' head(tmpd.data.df)
#' #
#' ## second, do the same fo rthe  aminoxyl data
#' aminoxyl.data.path <-
#'   load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data.df <-
#'   readEPR_Exp_Specs(
#'     aminoxyl.data.path,
#'     qValue = 2100
#'   )
#' #
#' ## preview
#' aminoxyl.data.df %>% head()
#' #
#' ## ...own transformation
#' spectra.data.tidy.df <-
#'   transform_dfs_2tidyDF(
#'     tmpd.data.df,
#'     aminoxyl.data.df,
#'     df.names = c("TMPD","Aminoxyl"),
#'     norm.vec = c(5e+3,1e-5),
#'     var2nd.series = "Radical"
#'   )
#' #
#' ## preview
#' head(spectra.data.tidy.df)
#' #
#' ## plotting both EPR spectra together
#' plot_EPR_Specs(
#'   data.spectra = spectra.data.tidy.df,
#'   x = "B_G",
#'   x.unit = "G",
#'   var2nd.series = "Radical",
#'   var2nd.series.slct.by = 1,
#'   xlim = c(3400,3600),
#'   line.colors = c("darkorange","blue2"),
#'   legend.title = "Radical"
#' )
#' #
#' ## ...and the interactive preview (B in mT)
#' plot_EPR_Specs2D_interact(
#'   data.spectra = spectra.data.tidy.df,
#'   line.colors = c("darkorange","blue2"),
#'   var2nd.series = "Radical",
#'   legend.title = "Radical"
#' )
#'
#'
#' @export
#'
#'
transform_dfs_2tidyDF <- function(
    ..., ## all data frame variable names separated by `,`
    df.names, ## character vector string `c("a","b","c","d")`
    which.coly.norm = "dIepr_over_dB", ## char. string of column name to be normalized
    norm.vec = rep(1,times = length(df.names)),
    var2nd.series = "Spectrum"
  ) {
  #
  ## list arguments and their names
  argumn.dfs.list <- list(...)
  names(argumn.dfs.list) <- df.names
  #
  ## checking lengths of the `df.names` and `argumn.dfs.list`
  if (length(argumn.dfs.list) != length(df.names)) {
    stop(" The number of data frame objects does not correspond\n
         to that of the `df.names` !! Please, verify the length\n
         of both. ")
  }
  #
  ## check the obejct class of each `argumn.dfs.list` element
  if (isFALSE(all(sapply(argumn.dfs.list, inherits, what = "data.frame")))) {
    stop(" None of the input objects/variables inherits the `data.frame`\n
         structure !! Please, check out each of the input objects by the `class()`\n
         or `is.data.frame()` functions. ")
  }
  #
  ## checking whether the `which.coly.norm` column name
  ## is present in all data frame objects within the list
  if (!all(
    unlist(
      lapply(
        argumn.dfs.list,
        function(x) {
          any(grepl(which.coly.norm,colnames(x)))
        }
      )
    )
  )) {
    stop(" The column name/header defined by the `which.coly.norm` argument\n
         is not present in any of the data frame objects !! Be sure that all\n
         data frames contain the same column name defined above. ")
  }
  #
  ## normalization
  argumn.dfs.list.new <-
    Map(
      function(u,v) {
        u <- u %>%
          dplyr::mutate(!!rlang::quo_name(which.coly.norm) :=
                          .data[[which.coly.norm]] / v)
      },
      argumn.dfs.list,
      norm.vec
    )
  #
  ## previous list is not required anymore
  rm(argumn.dfs.list)
  #
  ## create tidy data frame
  tidy.df <- dplyr::bind_rows(argumn.dfs.list.new,.id = var2nd.series)
  ## remove index column if present
  if (any(grepl("index",colnames(tidy.df)))){
    tidy.df$index <- NULL
  }
  #
  return(tidy.df)
}
