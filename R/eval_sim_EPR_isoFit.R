#'
#' Least-Squares Fitting of Isotropic EPR spectra by Simulations
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Fitting of the simulated spectrum onto the experimental one represents an important step in the analysis
#'   of EPR spectra. Parameters of the simulated spectrum like \eqn{g_{\text{iso}}}; coupling constants
#'   (in \code{MHz}) \eqn{A_{\text{iso}}} for each group of equivalent nuclei; linewidth
#'   (either \eqn{\Delta B_{\text{pp}}} or \eqn{FWHM} depending on the \code{lineSpecs.form} argument);
#'   spectral baseline (see the \code{baseline.correct}) and finally the intensity (multiplication coefficient)
#'   are optimized by the methods listed in \code{\link{optim_for_EPR_fitness}}.
#'   The \code{lineG.content} corresponding parameter is the only one,
#'   which needs to be varied "manually". In order to control the optimization/fitting process,
#'   by similar interactive way like in \href{https://easyspin.org/easyspin/documentation/userguide_fitting.html}{EasySpin},
#'   a \href{https://www.rstudio.com/products/shiny/}{Shiny app}
#'   and/or \href{https://gganimate.com/}{{gganimate}} visualization is under development.
#'
#'
#' @note
#'   In order to guess the intensity multiplication constant (please, refer to the \code{optim.params.init}
#'   argument), one might compare the intensities of the experimental (\code{expr}) and simulated (\code{sim})
#'   EPR spectrum by one of the interactive or static plot functions (e.g. \code{\link{plot_EPR_Specs}}
#'   or \code{\link{plot_EPR_Specs2D_interact}}) as well as by the \code{\link{eval_sim_EPR_iso}}. Accordingly,
#'   \strong{the initial intensity multiplication constant} can be estimated as the ratio
#'   \strong{max(\code{expr} intensity)/max(\code{sim} intensity)}.
#'
#'
#' @inheritParams eval_gFactor_Spec
#' @param data.spectr.expr Data frame object/table, containing the experimental spectral data the with magnetic flux density
#'   (\code{"B_mT"} or \code{"B_G"}) and the intensity (see the \code{Intensity.expr} argument) columns.
#' @param Intensity.expr Character string, pointing to column name of the experimental EPR intensity within
#'   the original \code{data.spectr.expr}. \strong{Default}: \code{dIepr_over_dB}.
#' @param Intensity.sim Character string, pointing to column name of the simulated EPR intensity within the related output
#'   data frame. \strong{Default}: \code{Intensity.sim = "dIeprSim_over_dB"}.
#' @param nuclear.system.noA List or nested list \strong{without estimated hyperfine coupling constant values},
#'   such as \code{list("14N",1)} or \code{list(list("14N", 2),list("1H", 4),list("1H", 12))}. The \eqn{A}-values
#'   are already defined as elements of the \code{optim.params.init} argument/vector. If the EPR spectrum
#'   does not display any hyperfine splitting, the argument definition reads \code{nuclear.system.noA = NULL} (\strong{default}).
#' @param baseline.correct Character string, referring to baseline correction of the simulated/fitted spectrum.
#'   Corrections like \code{"constant"} (\strong{default}), \code{"linear"} or \code{"quadratic"} can be applied.
#' @param lineG.content Numeric value between \code{0} and \code{1}, referring to content of the \emph{Gaussian} line form.
#'   If \code{lineG.content = 1} (\strong{default}) it corresponds to "pure" \emph{Gaussian} line form
#'   and if \code{lineG.content = 0} it corresponds to \emph{Lorentzian} one. The value from (0,1)
#'   (e.g. \code{lineG.content = 0.5}) represents the linear combination (for the example above,
#'   with the coefficients 0.5 and 0.5) of both line forms => so called \emph{pseudo-Voigt}.
#' @param lineSpecs.form Character string, describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (i.e. \code{"absorption"} which can be used as well) line form
#'   of the analyzed EPR spectrum/data.
#' @param optim.method Character string (vector), setting the optimization method(s) gathered within
#'   the \code{\link{optim_for_EPR_fitness}}. \strong{Default}: \code{optim.method = "neldermead"}. Additionally,
#'   several consecutive methods can be defined like \code{optim.method = c("levenmarq","neldermead")}, where
#'   the best fit parameters from the previous method are used as input for the next one. In such case, the output
#'   is \code{list} with the elements/vectors from each method, in order to see the progress of the optimization.
#' @param optim.params.init Numeric vector with the initial parameter guess (elements) where the \strong{first five
#'   elements are immutable}
#'   \enumerate{
#'   \item g-value (g-factor)
#'
#'   \item \strong{G}aussian linewidth
#'
#'   \item \strong{L}orentzian linewidth
#'
#'   \item baseline constant (intercept)
#'
#'   \item intensity multiplication constant
#'
#'   \item baseline slope (only if \code{baseline.correct = "linear"} or \code{baseline.correct = "quadratic"}),
#'   if \code{baseline.correct = "constant"} it corresponds to the \strong{first HFCC} (\eqn{A_1})
#'
#'   \item baseline quadratic coefficient (only if \code{baseline.correct = "quadratic"}),
#'   if \code{baseline.correct = "constant"} it corresponds to the \strong{second HFCC} (\eqn{A_2}),
#'   if \code{baseline.correct = "linear"} it corresponds to the \strong{first HFCC} (\eqn{A_1})
#'
#'   \item additional HFCC (\eqn{A_3}) if \code{baseline.correct = "constant"} or if
#'   \code{baseline.correct = "linear"} (\eqn{A_2}), if \code{baseline.correct = "quadratic"} it corresponds
#'   to the \strong{first HFCC} (\eqn{A_1})
#'
#'   \item ...additional HFCCs (\eqn{A_k...}, each vector element is reserved only for one \eqn{A})
#'   }
#'   DO NOT PUT ANY OF THESE PARAMETERS to \code{NULL}. If the lineshape is expected to be pure
#'   \strong{L}orentzian or pure \strong{G}aussian then put the corresponding vector element to \code{0}.
#' @param optim.params.lower Numeric vector (with the same element order like \code{optim.params.init})
#'   with the lower bound constraints. \strong{Default}: \code{optim.params.lower = NULL} which actually
#'   equals to \eqn{g_{\text{init}} - 0.001}, \eqn{0.8\,\Delta B_{\text{G,init}}},
#'   \eqn{0.8\,\Delta B_{\text{L,init}}}, baseline intercept initial constant \eqn{- 0.001},
#'   intensity multiplication initial constant \eqn{= 0.8\,\text{init}}, baseline initial slope \eqn{- 5} (in case
#'   the \code{baseline.correct} is set either to \code{"linear"} or \code{"quadratic"}) and finally,
#'   the baseline initial quadratic coefficient \eqn{- 5} (in case the \code{baseline.correct} is set to
#'   \code{"quadratic"}). Lower limits of all hyperfine coupling constant (HFCCs) are set to \eqn{0.875\,A_{\text{init}}}.
#' @param optim.params.upper Numeric vector (with the same element order like \code{optim.params.init})
#'   with the upper bound constraints. \strong{Default}: \code{optim.params.upper = NULL} which actually
#'   equals to \eqn{g_{\text{init}} + 0.001}, \eqn{1.2\,\Delta B_{\text{G,init}}},
#'   \eqn{1.2\,\Delta B_{\text{L,init}}}, baseline intercept initial constant \eqn{+ 0.001},
#'   intensity multiplication initial constant \eqn{= 1.2\,\text{init}}, baseline initial slope \eqn{+ 5} (in case
#'   the \code{baseline.correct} is set either to \code{"linear"} or \code{"quadratic"}) and finally,
#'   the baseline initial quadratic coefficient \eqn{+ 5} (in case the \code{baseline.correct} is set to
#'   \code{"quadratic"}). Upper limits of all HFCCs are set to \eqn{1.125\,A_{\text{init}}}.
#' @param Nmax.evals Numeric value, maximum number of function evaluations and/or iterations.
#'   The only one method, limited by this argument, is \code{\link[minpack.lm]{nls.lm}}, where
#'   \code{Nmax.evals = 1024}. Higher \code{Nmax.evals} may extremely extend the optimization
#'   time, therefore the \strong{default} value reads \code{Nmax.evals = 512}. However, the \code{"pswarm"}
#'   method requires at least the default or even higher values.
#' @param tol.step Numeric value, describing the smallest optimization step (tolerance) to stop the optimization.
#'   \strong{Default}: \code{tol.step = 5e-7}.
#' @param pswarm.size Numeric value, which equals to particle swarm size (i.e. number of particles),
#'   if \code{method = "pswarm"}. The \strong{default} value (\code{pswarm.size = NULL}) actually
#'   corresponds to \code{floor(10+2*sqrt(length(x.0)))} (for \code{SPSO2007}, see the \code{pswarm.type}
#'   argument), e.g. to optimize 8 parameters, number of particles = 15. For the \code{SPSO2011}
#'   the default number of particles equals to \code{40}.
#' @param pswarm.diameter Numeric value, corresponding to diameter of the particle swarm search space
#'   (in case \code{method = "pswarm"}). The \strong{default} value (\code{pswarm.diameter = NULL})
#'   refers to the Euclidean distance, defined as:
#'   \deqn{\sqrt{\sum_k\,(\text{optim.params.upper}[k] - \text{optim.params.lower}[k])^2}}
#' @param pswarm.type Character string, setting the type/version of particle swarm algorithm
#'   if \code{method = "pswarm"}. There are two types available: \code{pswarm.type = "SPSO2007"}
#'   and \code{pswarm.type = "SPSO2011"}. The latter introduced an adaptive random topology,
#'   which allows the swarm to dynamically adjust its communication structure.
#'   This helps in maintaining diversity in the swarm and improves the algorithm's ability
#'   to escape local optima. This type generally offers better performance on larger multidimensional spaces
#'   than the \code{pswarm.type = "SPSO2007"}, which uses a more static topology. Details may be found
#'   in the \code{References} of the \code{\link{optim_for_EPR_fitness}}.
#'   \strong{Default}: \code{pswarm.type = NULL} (actually corresponding to \code{"SPSO2007"},
#'   that performs slightly better on smaller scales such as simulations of EPR spectra).
#' @param check.fit.plot Logical, whether to return overlay plot with the initial simulation + the best simulation
#'   fit + experimental spectrum (including residuals in the lower part of the plot,
#'   \code{check.fit.plot = TRUE}, \strong{default}) or with the following three spectra
#'   (\code{check.fit.plot = FALSE}): 1. experimental, 2. the best simulated one with the baseline fit
#'   and 3. the best simulated spectrum with the baseline fit subtracted. The latter two are offset for clarity,
#'   within the plot.
#' @param output.list.final Logical. If \code{TRUE}, \code{list} with the following components will be exclusively returned:
#'   1. optimized parameters from the best fit (together with the minimum sum of residual squares)
#'   and 2. data frame of the final best simulated spectrum with and without the baseline fit (see \code{Value}).
#'   Such output will be applied for the more complex optimization/fitting (which is currently under development),
#'   as stated in the \code{Description}, therefore, the \strong{default} value reads \code{output.list.final = FALSE}.
#' @param ... additional arguments specified (see also \code{\link{optim_for_EPR_fitness}}).
#'
#'
#' @return Optimization/Fitting procedure results in vector or data frame or list depending on the \code{check.fit.plot}
#'   and \code{output...} arguments.
#'   \enumerate{
#'   \item If \code{check.fit.plot = TRUE} or \code{check.fit.plot = FALSE}, the result corresponds
#'   to list with the following components:
#'   \describe{
#'   \item{plot}{Visualization of the experimental as well as the best fitted EPR simulated spectra.
#'   If \code{check.fit.plot = TRUE}, the overlay plot consists of the initial simulation + the best simulation
#'   fit + experimental spectrum, including residuals in the plot lower part. Whereas, if \code{check.fit.plot = FALSE},
#'   following three spectra are available: 1. experimental, 2. the best simulated one with the baseline fit
#'   and 3. the best simulated spectrum with the baseline fit subtracted. The latter two are offset for clarity.}
#'   \item{best.fit.params}{Vector of the best (final) fitting (optimized) parameters, for each corresponding
#'   \code{optim.method}, to simulate the experimental EPR spectrum, see also description of the \code{optim.params.init}.}
#'   \item{df}{Tidy data frame (table) with the magnetic flux density and intensities of the experimental,
#'   the best simulated/fitted, as well as the initially simulated EPR spectrum and residuals
#'   (if \code{check.fit.plot = TRUE}), or wide data frame with the following variables / columns
#'   (for \code{check.fit.plot = FALSE}): magnetic flux density, intensity of the experimental
#'   spectrum, intensity of the best simulated one (including the baseline fit), residual intensity and finally,
#'   the best simulated spectrum intensity without the baseline fit.}
#'   \item{sum.LSQ.min}{The minimum sum of the residual square vector after the least-square
#'   procedure.}
#'   \item{N.evals}{Number of iterations/function evaluations completed before termination.
#'   If the \code{pswarm} optimization algorithm is included in \code{optim.method}, the \code{N.evals}
#'   equals to vector with the following elements: number of function evaluations, number of iterations (per one particle)
#'   and the number of restarts.}
#'   \item{N.converg}{Vector or simple integer code indicating the successful completion
#'   of the optimization/fit. In the case of \code{"levenmarq"} method, the vector elements coincide with
#'   the sum of residual squares at each iteration. If the \code{optim.method = "pswarm"} is applied, one of the following
#'   codes can be returned: \code{0}: algorithm terminated by reaching the absolute tolerance,
#'   \code{1}: maximum number of function evaluations reached, \code{2}: maximum number of iterations reached,
#'   \code{3}: maximum number of restarts reached, \code{4}: maximum number of iterations without improvement reached.
#'   For all the other remaining methods (coming from \code{{nloptr}} package), the integers have to be positive
#'   to indicate the successful convergence.}
#'   }
#'
#'   \item If \code{output.list.final = TRUE}, the function exclusively returns list with the two components,
#'   which will be applied for the more complex optimization/fitting (which is currently under development).
#'   \describe{
#'   \item{params}{Vector, corresponding to the best fitting (optimized) parameters (related
#'   to the \code{optim.params.init} argument, see also list above) + minimum sum of the residual squares,
#'   after the (final) \code{optim.method} procedure.}
#'   \item{df}{Data frame including the final best simulated spectrum with and without the baseline fit.}
#'   }
#'   }
#'
#'
#' @examples
#' ## loading built-in example dataset which is simple
#' ## EPR spectrum of the aminoxyl radical:
#' aminoxyl.data.path <-
#'   load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data <-
#'   readEPR_Exp_Specs(aminoxyl.data.path,
#'                     qValue = 2100)
#' #
#' ## EPR spectrum simulation fit with "Nelder-Mead"
#' ## optimization method with `check.fit.plot = FALSE`:
#' tempo.test.sim.fit.a <-
#'   eval_sim_EPR_isoFit(data.spectr.expr = aminoxyl.data,
#'     nu.GHz = 9.806769,
#'     lineG.content = 0.5,
#'     optim.method = "neldermead",
#'     nuclear.system.noA = list("14N",1),
#'     baseline.correct = "linear",
#'     optim.params.init =
#'       c(2.006, # g-value
#'         4.8, # G Delta Bpp
#'         4.8, # L Delta Bpp
#'         0, # intercept (constant) lin. baseline
#'         0.016, # Sim. intensity multiply
#'         1e-6, # slope lin. baseline
#'         49), # A in MHz
#'     check.fit.plot = FALSE
#'   )
#' ## OUTPUTS:
#' ## best fit parameters:
#' tempo.test.sim.fit.a$best.fit.params
#' #
#' ## spectrum plot with experimental spectrum,
#' ## simulated one with the linear baseline fit
#' ## and simulated one with the linear baseline
#' ## fit subtracted:
#' tempo.test.sim.fit.a$plot
#' #
#' ## minimum sum of squared residuals:
#' tempo.test.sim.fit.a$sum.LSQ.min
#' #
#' ## number of evaluations / iterations:
#' tempo.test.sim.fit.a$N.evals
#' #
#' ## convergence, in this case it is represented
#' ## by the integer code indicating the successful
#' ## completion (it must be > 0):
#' tempo.test.sim.fit.a$N.converg
#' #
#' ## preview of data frame including all EPR spectra:
#' head(tempo.test.sim.fit.a$df)
#' #
#' ## similar EPR spectrum simulation fit with "particle swarm"
#' ## optimization algorithm and `check.fit.plot = TRUE` option
#' ## as well as user defined bound constraints:
#' tempo.test.sim.fit.b <-
#'   eval_sim_EPR_isoFit(data.spectr.expr = aminoxyl.data,
#'     nu.GHz = 9.806769,
#'     lineG.content = 0.4,
#'     optim.method = "pswarm",
#'     nuclear.system.noA = list("14N",1),
#'     baseline.correct = "constant",
#'     optim.params.init = c(2.006,4.8,4.8,0,1.1e-2,49),
#'     optim.params.lower = c(2.0048,4.4,4.4,-1e-4,9e-3,45),
#'     optim.params.upper = c(2.0072,5.2,5.2,1e-4,1.5e-2,53),
#'     check.fit.plot = TRUE
#'   )
#' ## OUTPUTS:
#' ## best fit parameters:
#' tempo.test.sim.fit.b$best.fit.params
#' #
#' ## quick simulation check by plotting the both
#' ## simulated and the experimental EPR spectra
#' ## together with the initial simulation
#' ## and the residuals (differences between the
#' ## experiment and the best fit)
#' tempo.test.sim.fit.b$plot
#' #
#' ## fitting of the aminoxyl EPR spectrum
#' ## by the combination of the 1. "Levenberg-Marquardt"
#' ## and 2. "Nelder-Mead" algorithms
#' tempo.test.sim.fit.c <-
#'   eval_sim_EPR_isoFit(aminoxyl.data,
#'                       nu.GHz = 9.86769,
#'                       lineG.content = 0.5,
#'                       optim.method = c("levenmarq",
#'                                        "neldermead"),
#'                       nuclear.system.noA = list("14N",1),
#'                       baseline.correct = "constant",
#'                       optim.params.init = c(2.0060,
#'                                             4.8,
#'                                             4.8,
#'                                             0,
#'                                             7e-3,
#'                                             49),
#'                       check.fit.plot = FALSE
#'                     )
#' ## OUTPUTS:
#' ## best fit parameters for both procedures within a list:
#' tempo.test.sim.fit.c$best.fit.params
#' #
#' ## compare the results with the example in the `readMAT_params_file`,
#' ## corresponding to the best fit from `Easyspin`
#' #
#' ## `N.converg` also consists of two components
#' ## each corresponding to result of the individual
#' ## optimization method where the "levenmarq" returns
#' ## the sum of squares at each iteration, therefore the 1st
#' ## component is vector and the 2nd one is integer code
#' ## as already stated above:
#' tempo.test.sim.fit.c$N.converg
#'
#'
#' @export
#'
#'
#' @importFrom stats median
#' @importFrom dplyr rowwise
eval_sim_EPR_isoFit <- function(data.spectr.expr,
                                Intensity.expr = "dIepr_over_dB",
                                Intensity.sim = "dIeprSim_over_dB",
                                nu.GHz,
                                B.unit = "G",
                                nuclear.system.noA = NULL,
                                baseline.correct = "constant", ## "linear" or "quadratic"
                                lineG.content = 0.5,
                                lineSpecs.form = "derivative",
                                optim.method = "neldermead", ## also two consecutive methods as vector
                                optim.params.init,
                                optim.params.lower = NULL,
                                optim.params.upper = NULL,
                                Nmax.evals = 512,
                                tol.step = 5e-7,
                                pswarm.size = NULL,
                                pswarm.diameter = NULL,
                                pswarm.type = NULL,
                                check.fit.plot = TRUE,
                                output.list.final = FALSE,
                                ...){
  #
  ## 'Temporary' processing variables
  . <- NULL
  Residuals <- NULL
  Simulation_NoBasLin <- NULL
  Spectrum <- NULL
  ## delete index column if present
  if (any(grepl("index", colnames(data.spectr.expr)))) {
    data.spectr.expr$index <- NULL
  }
  ## instrumental parameters except the microwave frequency must be read from
  ## the experimental data. It cannot be done by the same way like in simulation
  ## because the relevant instrum. params. like Bsw (B.SW) and Bcf (B.CF) differs
  ## from those presented in `.DSC` and `.par`. The reason is the Teslameter. If it'is
  ## in ON state the measured B values (can be slightly, i.e. usually approx. 1-3 G)
  ## different from those measured by the Hall probe or from the spectrum parameter
  ## settings. If the Teslameter is in ON state the measured values are automatically
  ## written into the text ASCII file. Therefore, to properly compare the simulated
  ## and experimental spectrum these parameters must be extracted form
  ## the original experimental ASCII (`.txt` or `.asc`) ASCII data file =>
  B.cf <- stats::median(data.spectr.expr[[paste0("B_",B.unit)]])
  B.sw <- max(data.spectr.expr[[paste0("B_",B.unit)]]) -
    min(data.spectr.expr[[paste0("B_",B.unit)]])
  N.points <- nrow(data.spectr.expr)
  mw.GHz <- nu.GHz
  ## therefore => the named vector
  instrum.params <- c(Bcf = B.cf,Bsw = B.sw,Npoints = N.points,mwGHz = mw.GHz)
  #
  ## Conditions for G-L content and the corresponding linewidth
  if (lineG.content == 1 & optim.params.init[3] != 0){
    stop(" Spectral lineshape is defined as pure Gaussian. Please, put the Lorentzian\n
         linewidth element, corresponding to 3rd `optim.params.int`, to `0` ! ")
  }
  if (lineG.content == 1 & optim.params.init[2] == 0){
    stop(" Spectral lineshape is defined as pure Gaussian. Therefore, the corresponding\n
         linewidth (`optim.params.init[2]`) must be DIFFERENT FROM `0` ! ")
  }
  ## ...the same for Lorentz =>
  if (lineG.content == 0 & optim.params.init[2] != 0){
    stop(" Spectral lineshape is defined as pure Lorentzian. Please, put the Gaussian\n
         linewidth element, corresponding to 2nd `optim.params.int`, to `0` ! ")
  }
  if (lineG.content == 0 & optim.params.init[3] == 0){
    stop(" Spectral lineshape is defined as pure Lorentzian. Therefore,the corresponding\n
         linewidth (`optim.params.init[3]`) must be DIFFERENT FROM `0` ! ")
  }
  #
  ## Define the length of `nuclear.system.noA` similarly as in simple simulation
  ## check if the list is nested (several groups) or simple (only one group)
  nested_list <- any(sapply(nuclear.system.noA, is.list))
  if (isFALSE(nested_list)){
    ## redefinition of `nuclear.system.noA` list to calculate the spectra without
    ## any additional conditions just by simple =>
    nuclear.system.noA <- list(nuclear.system.noA)
  } else {
    nuclear.system.noA <- nuclear.system.noA
  }
  #
  ## condition to switch among three values
  ## <==> baseline approximation
  baseline.cond.fn <- function(baseline.correct){
    if (baseline.correct == "constant" || baseline.correct == "Constant"){
      return(0)
    } else if (baseline.correct == "linear" || baseline.correct == "Linear"){
        return(1)
    } else if(baseline.correct == "quadratic" || baseline.correct == "Quadratic"){
        return(2)
    }
  }
  #
  # ==================== FUNCTIONS TO PARAMETRIZE SIMULATIONS =====================
  #
  ## ....... by arguments/parameters
  ## based on `optim.method` and the corresponding argument
  ## therefore => AS a FITNESS FUNCTIONS THEY MUST BE DEFINED SEPARATELY !!
  ## OTHERWISE THE OPTIMIZATION WON'T WORK
  fit_sim_params_par <- function(data,
                                 nucs.system,
                                 Intensity.sim,
                                 lineG.content,
                                 baseline,
                                 B.unit,
                                 par){
    #
    ## definition of the 1st param. => g-value
    g.var <- par[1]
    ## definition of the additional params. like line-width
    ## and GL-line content/contribution
    gB.width.var <- par[2]
    lB.width.var <- par[3]
    #
    ## A.vars based on `nucs.system`, such system (definition at the beginning)
    ## must contain only nucleus character string and the corresponding number
    ## of nuclei within the group because As will be varied
    if (is.null(nucs.system)){
      #
      ## function to simulate EPR spectra WITHOUT NUCLEI INTERACTION
      sim_epr_iso_df_noNucs <- function(GL.linewidth,G.line.content){
        #
        ## `GL.linewidth` is list
        sim.df <-
          eval_sim_EPR_iso(g.iso = g.var,
                           B.unit = B.unit,
                           instrum.params = instrum.params,
                           natur.abund = FALSE,
                           nuclear.system = NULL,
                           lineSpecs.form = lineSpecs.form,
                           lineGL.DeltaB = GL.linewidth,
                           lineG.content = G.line.content,
                           Intensity.sim = Intensity.sim)$df
        #
        return(sim.df)
      }
      #
      if (lineG.content == 0 & gB.width.var == 0){
        sim.fit.df <-
          sim_epr_iso_df_noNucs(GL.linewidth = list(NULL,lB.width.var),
                                G.line.content = 0
          )
       #
      } else if(lineG.content == 1 & lB.width.var == 0){
        sim.fit.df <-
          sim_epr_iso_df_noNucs(GL.linewidth = list(gB.width.var,NULL),
                                G.line.content = 1
          )
        #
      } else{
        sim.fit.df <-
          sim_epr_iso_df_noNucs(GL.linewidth = list(gB.width.var,lB.width.var),
                                G.line.content = lineG.content
         )
      }
      #
    } else {
      #
      ## Define the length of `nucs.system` similarly as in simple simulation
      ## check if the list is nested (several groups) or simple (only one group)
      nested_list <- any(sapply(nucs.system, is.list))
      if (isFALSE(nested_list)){
        ## redefinition of `nucs.system` list to calculate the spectra without
        ## any additional conditions just by simple =>
        nucs.system <- list(nucs.system)
      } else{
        nucs.system <- nucs.system
      }
      ## what is the length of the list (how many nuclear groups)
      nucle_us_i <- sapply(1:length(nucs.system), function(e) nucs.system[[e]][[1]])
      #
      ## adding parameters As to nested list
      ## the first par[1,2,3,4,5,...] is reserved for g,linewidths, baseline and intensity
      ## `A.var` should be explicitly expressed by corresp. x0 elements
      A.var <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                      par[8:(7+length(nucle_us_i))],
                      par[7:(6+length(nucle_us_i))],
                      par[6:(5+length(nucle_us_i))]
                      )
      #
      nucs.system.new <- c()
      for (j in seq(nucs.system)) {
        nucs.system.new[[j]] <- c(nucs.system[[j]],A.var[j])
        nucs.system.new[[j]] <- as.list(nucs.system.new[[j]])
      }
      #
      ## function to simulate EPR spectra WITH MUCLEI INTERACTION
      sim_epr_iso_df_Nucs <- function(GL.linewidth,G.line.content){
        #
        ## `GL.linewidth` is list
        sim.df <-
          eval_sim_EPR_iso(g.iso = g.var,
                           B.unit = B.unit,
                           instrum.params = instrum.params,
                           natur.abund = TRUE,
                           nuclear.system = nucs.system.new,
                           lineSpecs.form = lineSpecs.form,
                           lineGL.DeltaB = GL.linewidth,
                           lineG.content = G.line.content,
                           Intensity.sim = Intensity.sim)$df
        #
        return(sim.df)
      }
      #
      ## evaluating simulated intensity like before (in the case of `nucs.system = NULL`)
      if (lineG.content == 0 & gB.width.var == 0){
        #
        sim.fit.df <-
          sim_epr_iso_df_Nucs(GL.linewidth = list(NULL,lB.width.var),
                              G.line.content = 0
          )
        #
      } else if (lineG.content == 1 & lB.width.var == 0){
        sim.fit.df <-
          sim_epr_iso_df_Nucs(GL.linewidth = list(gB.width.var,NULL),
                              G.line.content = 1
          )
        #
      } else{
        sim.fit.df <-
          sim_epr_iso_df_Nucs(GL.linewidth = list(gB.width.var,lB.width.var),
                              G.line.content = lineG.content
         )
      }
      #
    }
    if (baseline == "constant"){
      ## Intensity = a + b*Intensity
      data[[Intensity.sim]] <- par[4] + (par[5] * sim.fit.df[[Intensity.sim]])
    }
    if (baseline == "linear"){
      ## Intensity = a + b*Intensity + c*B (B = "magnetic flux density")
      data[[Intensity.sim]] <- par[4] + (par[5] * sim.fit.df[[Intensity.sim]]) +
        (par[6] * sim.fit.df[[paste0("Bsim_",B.unit)]])
    }
    if (baseline == "quadratic"){
      ## Intensity = a + b*Intensity + c*B + d*B^2
      data[[Intensity.sim]] <- par[4] + (par[5] * sim.fit.df[[Intensity.sim]]) +
        (par[6] * sim.fit.df[[paste0("Bsim_",B.unit)]]) +
        (par[7] * (sim.fit.df[[paste0("Bsim_",B.unit)]])^2)
    }
    #
    return(data[[Intensity.sim]])
    #
  }
  #
  ## the second function with `x0`
  fit_sim_params_x0 <- function(data,
                                nucs.system,
                                Intensity.sim,
                                lineG.content,
                                baseline,
                                B.unit,
                                x0){
    #
    ## definition of the 1st param. => g-value
    g.var <- x0[1]
    ## definition of the additional params. like line-width
    ## and GL-line content/contribution
    gB.width.var <- x0[2]
    lB.width.var <- x0[3]
    #
    ## A.vars based on `nucs.system`, such system (definition at the beginning)
    ## must contain only nucleus character string and the corresponding number
    ## of nuclei within the group because As will be varied
    if (is.null(nucs.system)){
      #
      ## function to simulate EPR spectra WITHOUT NUCLEI INTERACTION
      sim_epr_iso_df_noNucs <- function(GL.linewidth,G.line.content){
        #
        ## `GL.linewidth` is list
        sim.df <-
          eval_sim_EPR_iso(g.iso = g.var,
                           B.unit = B.unit,
                           instrum.params = instrum.params,
                           natur.abund = FALSE,
                           nuclear.system = NULL,
                           lineSpecs.form = lineSpecs.form,
                           lineGL.DeltaB = GL.linewidth,
                           lineG.content = G.line.content,
                           Intensity.sim = Intensity.sim)$df
        #
        return(sim.df)
      }
      #
      if (lineG.content == 0 & gB.width.var == 0){
        sim.fit.df <-
          sim_epr_iso_df_noNucs(GL.linewidth = list(NULL,lB.width.var),
                                G.line.content = 0
          )
        #
      } else if(lineG.content == 1 & lB.width.var == 0){
        sim.fit.df <-
          sim_epr_iso_df_noNucs(GL.linewidth = list(gB.width.var,NULL),
                                G.line.content = 1
          )
        #
      } else{
        sim.fit.df <-
          sim_epr_iso_df_noNucs(GL.linewidth = list(gB.width.var,lB.width.var),
                                G.line.content = lineG.content
          )
      }
      #
    } else {
      #
      ## Define the length of `nucs.system` similarly as in simple simulation
      ## check if the list is nested (several groups) or simple (only one group)
      nested_list <- any(sapply(nucs.system, is.list))
      if (isFALSE(nested_list)){
        ## redefinition of `nucs.system` list to calculate the spectra without
        ## any additional conditions just by simple =>
        nucs.system <- list(nucs.system)
      } else{
        nucs.system <- nucs.system
      }
      ## what is the length of the list (how many nuclear groups)
      nucle_us_i <- sapply(1:length(nucs.system), function(e) nucs.system[[e]][[1]])
      #
      ## adding parameters As to nested list
      ## the first par[1,2,3,4,5,...] is reserved for g,linewidths, baseline and intensity
      ## `A.var` should be explicitly expressed by corresp. x0 elements
      A.var <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                      x0[8:(7+length(nucle_us_i))],
                      x0[7:(6+length(nucle_us_i))],
                      x0[6:(5+length(nucle_us_i))]
      )
      #
      nucs.system.new <- c()
      for (j in seq(nucs.system)) {
        nucs.system.new[[j]] <- c(nucs.system[[j]],A.var[j])
        nucs.system.new[[j]] <- as.list(nucs.system.new[[j]])
      }
      #
      ## function to simulate EPR spectra WITH MUCLEI INTERACTION
      sim_epr_iso_df_Nucs <- function(GL.linewidth,G.line.content){
        #
        ## `GL.linewidth` is list
        sim.df <-
          eval_sim_EPR_iso(g.iso = g.var,
                           B.unit = B.unit,
                           instrum.params = instrum.params,
                           natur.abund = TRUE,
                           nuclear.system = nucs.system.new,
                           lineSpecs.form = lineSpecs.form,
                           lineGL.DeltaB = GL.linewidth,
                           lineG.content = G.line.content,
                           Intensity.sim = Intensity.sim)$df
        #
        return(sim.df)
      }
      #
      ## evaluating simulated intensity like before (in the case of `nucs.system = NULL`)
      if (lineG.content == 0 & gB.width.var == 0){
        #
        sim.fit.df <-
          sim_epr_iso_df_Nucs(GL.linewidth = list(NULL,lB.width.var),
                              G.line.content = 0
          )
        #
      } else if (lineG.content == 1 & lB.width.var == 0){
        sim.fit.df <-
          sim_epr_iso_df_Nucs(GL.linewidth = list(gB.width.var,NULL),
                              G.line.content = 1
          )
        #
      } else{
        sim.fit.df <-
          sim_epr_iso_df_Nucs(GL.linewidth = list(gB.width.var,lB.width.var),
                              G.line.content = lineG.content
          )
      }
      #
    }
    if (baseline == "constant"){
      ## Intensity = a + b*Intensity
      data[[Intensity.sim]] <- x0[4] + (x0[5] * sim.fit.df[[Intensity.sim]])
    }
    if (baseline == "linear"){
      ## Intensity = a + b*Intensity + c*B (B = "magnetic flux density")
      data[[Intensity.sim]] <- x0[4] + (x0[5] * sim.fit.df[[Intensity.sim]]) +
        (x0[6] * sim.fit.df[[paste0("Bsim_",B.unit)]])
    }
    if (baseline == "quadratic"){
      ## Intensity = a + b*Intensity + c*B + d*B^2
      data[[Intensity.sim]] <- x0[4] + (x0[5] * sim.fit.df[[Intensity.sim]]) +
        (x0[6] * sim.fit.df[[paste0("Bsim_",B.unit)]]) +
        (x0[7] * (sim.fit.df[[paste0("Bsim_",B.unit)]])^2)
    }
    #
    return(data[[Intensity.sim]])
    #
  }
  #
  # ====================== PARAMETER GUESSES =======================
  #
  ## initial parameter guesses for the optimization and definition
  ## g values
  limits.params1a <- optim.params.init[1] - 0.001
  limits.params1b <- optim.params.init[1] + 0.001
  ## linewidths
  limits.params2 <- optim.params.init[2] * 0.2
  limits.params3 <- optim.params.init[3] * 0.2
  ## constant
  limits.params4a <- optim.params.init[4] - 0.001
  limits.params4b <- optim.params.init[4] + 0.001
  ## intensity multiplication coeff.
  limits.params5 <- optim.params.init[5] * 0.2
  #
  lower.limits <- c(limits.params1a,
                    optim.params.init[2] - limits.params2,
                    optim.params.init[3] - limits.params3,
                    limits.params4a,
                    optim.params.init[5] - limits.params5)
  lower.limits <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                         c(lower.limits,-5,-5),
                         c(lower.limits,-5),
                         lower.limits
                         )
  upper.limits <- c(limits.params1b,
                    optim.params.init[2] + limits.params2,
                    optim.params.init[3] + limits.params3,
                    limits.params4b,
                    optim.params.init[5] + limits.params5)
  upper.limits <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                         c(upper.limits,5,5),
                         c(upper.limits,5),
                         upper.limits
  )
  #
  if (is.null(nuclear.system.noA)){
    lower.limits <- lower.limits
    upper.limits <- upper.limits
  } else {
    ## individual optimization limits for As
    A.lower.limits <- c()
    A.upper.limits <- c()
    if (baseline.correct == "constant"){
      for (a in 6:(5+length(nuclear.system.noA))){
        A.lower.limits[a-5] <- optim.params.init[a] - (optim.params.init[a] * 0.125)
        A.upper.limits[a-5] <- optim.params.init[a] + (optim.params.init[a] * 0.125)
      }
    }
    if (baseline.correct == "linear"){
      for (a in 7:(6+length(nuclear.system.noA))){
        A.lower.limits[a-6] <- optim.params.init[a] - (optim.params.init[a] * 0.125)
        A.upper.limits[a-6] <- optim.params.init[a] + (optim.params.init[a] * 0.125)
      }
    }
    if (baseline.correct == "quadratic"){
      for (a in 8:(7+length(nuclear.system.noA))){
        A.lower.limits[a-7] <- optim.params.init[a] - (optim.params.init[a] * 0.125)
        A.upper.limits[a-7] <- optim.params.init[a] + (optim.params.init[a] * 0.125)
      }
    }
    #
    ## actual `limits`
    lower.limits <- c(lower.limits,A.lower.limits)
    upper.limits <- c(upper.limits,A.upper.limits)
  }
  optim.params.lower <- optim.params.lower %>%
    `if`(is.null(optim.params.lower), lower.limits, .)
  optim.params.upper <- optim.params.upper %>%
    `if`(is.null(optim.params.upper), upper.limits, .)
  #
  # ================== GENERAL FUNCTION FOR OPTIMIZATION ====================
  #
  ## because it depends on method (`method`), function (`fun`)
  ## and initial params (`x.0`), define it accordingly
  optim_fn <- function(fun,method,x.0){
    optim.list <- optim_for_EPR_fitness(x.0 = x.0,
                                        fn = fun,
                                        method = method,
                                        lower = optim.params.lower,
                                        upper = optim.params.upper,
                                        data = data.spectr.expr,
                                        nucs.system = nuclear.system.noA,
                                        Intensity.sim = Intensity.sim,
                                        lineG.content = lineG.content,
                                        baseline = baseline.correct,
                                        B.unit = B.unit,
                                        Nmax.evals = Nmax.evals,
                                        tol.step = tol.step,
                                        pswarm.size = pswarm.size,
                                        pswarm.diameter = pswarm.diameter,
                                        pswarm.type = pswarm.type,
                                        ...)
    #
    return(optim.list)
  }
  #
  ## =========== OWN OPTIMIZATION which can be performed also WITH SEVERAL =========
  ## =========== CONSECUTIVE METHODS DEPENDING on the `optim.method` ===============
  ## ============================== VECTOR LENGTH ==================================
  #
  optimization.list <- c()
  best.fit.params <- c()
  optim.params.init.list <- list()
  optim.params.init.list[[1]] <- optim.params.init
  for (m in seq(optim.method)) {
    if (optim.method[m] == "levenmarq"){
      ## LSQ or DIFF. FUNCTIONS
      ## "levelnmarq" is defined by residuals, NOT by sum of the residual squares !!
      min_residuals_lm <- function(data,
                                   nucs.system,
                                   Intensity.sim,
                                   lineG.content,
                                   baseline,
                                   B.unit,
                                   par){
        with(data,data[[Intensity.expr]] -
                 fit_sim_params_par(data,
                                    nucs.system,
                                    Intensity.sim,
                                    lineG.content,
                                    baseline,
                                    B.unit,
                                    par))
      }
      #
      optimization.list[[m]] <- optim_fn(fun = min_residuals_lm,
                                         method = "levenmarq",
                                         x.0 = optim.params.init.list[[m]])
    }
    if (optim.method[m] == "pswarm"){
      ## LSQ FUNCTION
      min_residuals_ps <- function(data,
                                   nucs.system,
                                   Intensity.sim,
                                   lineG.content,
                                   baseline,
                                   B.unit,
                                   par){
        with(data,sum((data[[Intensity.expr]] -
                         fit_sim_params_par(data,
                                            nucs.system,
                                            Intensity.sim,
                                            lineG.content,
                                            baseline,
                                            B.unit,
                                            par))^2))
      }
      #
      optimization.list[[m]] <- optim_fn(fun = min_residuals_ps,
                                         method = "pswarm",
                                         x.0 = optim.params.init.list[[m]])
    }
    if (optim.method[m] == "slsqp" || optim.method[m] == "neldermead" ||
        optim.method[m] == "crs2lm" || optim.method[m] == "sbplx" ||
        optim.method[m] == "cobyla" || optim.method[m] == "lbfgs") { ## with `else` it doesn't work
      ## LSQ FUNCTION
      min_residuals_nl <- function(data,
                                   nucs.system,
                                   Intensity.sim,
                                   lineG.content,
                                   baseline,
                                   B.unit,
                                   x0){
        with(data,sum((data[[Intensity.expr]] -
                         fit_sim_params_x0(data,
                                           nucs.system,
                                           Intensity.sim,
                                           lineG.content,
                                           baseline,
                                           B.unit,
                                           x0))^2))
      }
      #
      optimization.list[[m]] <- optim_fn(fun = min_residuals_nl,
                                         method = optim.method[m],
                                         x.0 = optim.params.init.list[[m]])
    }
    #
    ## best parameters as input (`optim.params.init.list`) for the next cycle
    ## if several subsequent `optim.method` applied
    best.fit.params[[m]] <- optimization.list[[m]]$par
    if (length(optim.method) > 1){
      if (m < length(optim.method)) {
        optim.params.init.list[[m+1]] <- best.fit.params[[m]]
      }
    }
    #
  }
  # =================== THE BEST PARAMS. VECTOR ====================
  #
  ## The best system is the last one from the `best.fit.params` =>
  ## therefore it correspond to `best.fit.params[[length(optim.method)]]`
  ## to simulate and display the spectrum
  #
  ## "best" (i.e. including best As) nuclear system
  if (is.null(nuclear.system.noA)){
    nucs.system.best <- NULL
  } else{
    A.best <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                     best.fit.params[[length(optim.method)]][8:(7+length(nuclear.system.noA))],
                     best.fit.params[[length(optim.method)]][7:(6+length(nuclear.system.noA))],
                     best.fit.params[[length(optim.method)]][6:(5+length(nuclear.system.noA))]
                     )
    A.best <- round(A.best,digits = 3)
    nucs.system.best <- c()
    for (j in seq(nuclear.system.noA)) {
      nucs.system.best[[j]] <- c(nuclear.system.noA[[j]],A.best[j])
      nucs.system.best[[j]] <- as.list(nucs.system.best[[j]])
    }
  }
  #
  ## Conditions for linewidths obtained from `best.fit.params`
  #
  ## ============ FUNCTION TO SIMULATE SPECTRA WITH THE FINAL (BEST FIT) PARAMS ============
  #
  sim_epr_iso_df_final <- function(GL.linewidth){
    #
    ## `GL.linewidth` is list
    sim.df <-
      eval_sim_EPR_iso(g.iso = best.fit.params[[length(optim.method)]][1],
                       B.unit = B.unit,
                       instrum.params = instrum.params,
                       natur.abund = TRUE,
                       nuclear.system = nucs.system.best,
                       lineSpecs.form = lineSpecs.form,
                       lineGL.DeltaB = GL.linewidth,
                       lineG.content = lineG.content,
                       Intensity.sim = Intensity.sim)$df

  }
  #
  if (best.fit.params[[length(optim.method)]][2] == 0){
    ## best simulated spectrum data frame
    best.fit.df <-
      sim_epr_iso_df_final(
        GL.linewidth = list(NULL,
                            best.fit.params[[length(optim.method)]][3])
      )
    #
  } else if (best.fit.params[[length(optim.method)]][3] == 0){
    ## best simulated spectrum data frame
    best.fit.df <-
      sim_epr_iso_df_final(
        GL.linewidth = list(best.fit.params[[length(optim.method)]][2],
                            NULL)
      )
    #
  } else{
    ## best simulated spectrum data frame
    best.fit.df <-
      sim_epr_iso_df_final(
        GL.linewidth = list(best.fit.params[[length(optim.method)]][2],
                            best.fit.params[[length(optim.method)]][3])
      )
  }
  #
  ## best simulated Intensity and add the `Intensity.sim` to experimental
  # spectrum data based on the baseline.correct condition
  ## first of all the intensity part which depends on `baseline.correct`
  Intens.baseline.switch <-
    switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
           ((best.fit.params[[length(optim.method)]][6] * best.fit.df[[paste0("Bsim_",B.unit)]]) +
              (best.fit.params[[length(optim.method)]][7] * (best.fit.df[[paste0("Bsim_",B.unit)]])^2)),
           (best.fit.params[[length(optim.method)]][6] * best.fit.df[[paste0("Bsim_",B.unit)]]),
           0
    )
  ## add the overall simulated intensity including baseline
  data.spectr.expr[[Intensity.sim]] <-
    best.fit.params[[length(optim.method)]][4] +
    (best.fit.params[[length(optim.method)]][5] * best.fit.df[[Intensity.sim]]) +
    Intens.baseline.switch
  #
  ## ======================== DATA & PLOTTING =============================
  #
  ## final data  new frame and rename columns
  data.sim.expr <- data.spectr.expr %>%
    dplyr::select(dplyr::all_of(c(paste0("B_",B.unit),
                                  Intensity.expr,Intensity.sim))) %>%
    dplyr::rename_with(~ c("Experiment","Simulation"),
                       dplyr::all_of(c(Intensity.expr,Intensity.sim)))
  #
  rm(data.spectr.expr) ## not required anymore
  #
  ## calculating `rowwise` difference between expr. and sim. spectra
  data.sim.expr.resid <- data.sim.expr %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Residuals = .data$Experiment - .data$Simulation)
  ## select only `B` and `Residuals`
  data.sim.expr.resid <- data.sim.expr.resid %>%
    dplyr::select(dplyr::all_of(c(paste0("B_",B.unit),"Residuals")))
  #
  ## results (incl. comparison of experimental and simulated spectra)
  ## depending on `check.fit.plot` which shows only the overlay spectra and best
  ## fitting parameters. Otherwise the entire list (see below) will be returned.
  if (isTRUE(check.fit.plot)){
    ## transformation into long table ("tidy") format for visualization
    data.sim.expr.long <- data.sim.expr %>%
      tidyr::pivot_longer(!dplyr::all_of(paste0("B_",B.unit)),
                          names_to = "Spectrum",
                          values_to = Intensity.expr) %>%
      dplyr::arrange(.data$Spectrum)
    #
  } else {
    ## for plotting both spectra as publication ready => spectra will be offset
    ## for clarity => recalculate the intensity => shift the simulated intensity
    ## down below by factor of difference between `max()` and `min()`
    Int.diff <- max(data.sim.expr$Experiment) - min(data.sim.expr$Experiment)
    data.sim.expr.long <- data.sim.expr %>%
      ## offset for clarity
      dplyr::mutate(!!rlang::quo_name("Simulation") := .data$Simulation - (0.9 * Int.diff)) %>%
      ## simulation without baseline
      dplyr::mutate(Simulation_NoBasLin = (best.fit.params[[length(optim.method)]][5] *
                      best.fit.df[[Intensity.sim]]) - (1.9 * Int.diff)) %>%
      tidyr::pivot_longer(!dplyr::all_of(paste0("B_",B.unit)),
                          names_to = "Spectrum",
                          values_to = Intensity.expr) %>%
      dplyr::arrange(.data$Spectrum)
  }
  #
  ## Adding residuals and `pure` simulation (without baseline)
  ## to the overall data frame which will be returned
  ## in the case of `check.fit.plot = FALSE`
  data.sim.expr$Residuals <- data.sim.expr.resid$Residuals
  data.sim.expr$Simulation_NoBasLin <-
    best.fit.params[[length(optim.method)]][5] * best.fit.df[[Intensity.sim]]
  #
  # ---------------- add initial simulation only if `check.fit.plot = TRUE` ------------------
  #
  ## Add initial (corresponding to `optim.params.init`) simulation to spectra
  #
  ## ...first of all, create initial `nuclear.system` =>
  if (isTRUE(check.fit.plot)) {
    if (!is.null(nuclear.system.noA)){
      A.init <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                       optim.params.init[8:(7+length(nuclear.system.noA))],
                       optim.params.init[7:(6+length(nuclear.system.noA))],
                       optim.params.init[6:(5+length(nuclear.system.noA))]
      )
      A.init <- round(A.init,digits = 3)
      nucs.system.init <- c()
      for (j in seq(nuclear.system.noA)) {
        nucs.system.init[[j]] <- c(nuclear.system.noA[[j]],A.init[j])
        nucs.system.init[[j]] <- as.list(nucs.system.init[[j]])
      }
    } else {
      nucs.system.init <- NULL
    }
    ##...initial simulation data frame + 1.) extract and create list for DeltaBs
    DeltaB.init <- c()
    DeltaB.init <- Map(function(p,q) {
      DeltaB.init[[p]] <- optim.params.init[q] %>%
        `if`(optim.params.init[q] == 0, NULL, .)
    },
    c(1,2),
    c(2,3)
    )
    sim.df.init <-
      eval_sim_EPR_iso(g.iso = optim.params.init[1],
                       B.unit = B.unit,
                       instrum.params = instrum.params,
                       natur.abund = TRUE,
                       nuclear.system = nucs.system.init,
                       lineSpecs.form = lineSpecs.form,
                       lineGL.DeltaB = DeltaB.init,
                       lineG.content = lineG.content,
                       Intensity.sim = Intensity.expr)$df
    ## `Intensity.expr` due to `data.sim.expr.long` (see its creation above)
    #
    ## intensity based on baseline
    Intens.init.baseline.switch <-
      switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
             ((optim.params.init[6] * sim.df.init[[paste0("Bsim_",B.unit)]]) +
                (optim.params.init[7] * (sim.df.init[[paste0("Bsim_",B.unit)]])^2)),
             (optim.params.init[6] * sim.df.init[[paste0("Bsim_",B.unit)]]),
             0
      )
    ## finally, the initial sim intensity =>
    ## from `optim.params.init` (
    sim.df.init[[Intensity.expr]] <-
      optim.params.init[4] +
      (optim.params.init[5] * sim.df.init[[Intensity.expr]]) +
      Intens.init.baseline.switch
    #
    ## and rename and select only required columns
    sim.df.init <- sim.df.init %>%
      dplyr::select(
        dplyr::all_of(c(paste0("Bsim_",B.unit),Intensity.expr))
        ) %>%
      dplyr::rename_with(~ c(paste0("B_",B.unit)),
                         dplyr::all_of(c(paste0("Bsim_",B.unit)))
    )
    #
    ## create a column "Spectrum" (repeat "Init_Sim")
    ## in order to `bind_rows` with `data.sim.expr.long` =>
    sim.df.init[["Spectrum"]] <-
      rep("Simulation_Init",times = nrow(sim.df.init))
    #
    ## finally, `bind_rows` => both data frames together
    data.sim.expr.long <-
      data.sim.expr.long %>%
      dplyr::bind_rows(sim.df.init) %>%
      dplyr::arrange(.data$Spectrum)
  }
  #
  # ----------------------------------------------------------------------
  #
  ## plotting all spectra
  ## condition to present the intensity =>
  ylab <- switch(2-grepl("deriv|Deriv",lineSpecs.form),
                 bquote(d * italic(I)[EPR] ~ "/" ~ d * italic(B) ~ ~"(" ~ p.d.u. ~ ")"),
                 bquote(italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")")
  )
  #
  if (isTRUE(check.fit.plot)){
    ## display both overlay spectra (upper part) and residuals
    ## (lower part) in 1 col. by `{patchwork}`
    plot.sim.expr.upper <- ggplot(data = data.sim.expr.long) +
      geom_line(aes(x = .data[[paste0("B_",B.unit)]],
                    y = .data[[Intensity.expr]],
                    color = .data$Spectrum),
                linewidth = 0.75) +
      scale_color_manual(values = c("darkcyan","magenta","#5D5DFF"), #674EA7 #7F6000 #351C75 #4C4CFF
                         labels = c("Experiment\n",
                                    "Best\nSimulation Fit\n",
                                    "Initial\nSimulation")
                         ) +
      labs(title = "EPR Simulation Fit",
           color = NULL,
           x = NULL,
           y = ylab) +
      plot_theme_In_ticks() +
      theme(legend.text = element_text(size = 13))
      #
      plot.sim.expr.lower <- ggplot(data = data.sim.expr.resid) +
        geom_line(aes(x = .data[[paste0("B_",B.unit)]],
                      y = Residuals),
                  color = "black",
                  linewidth = 0.75,
                  alpha = 0.75) +
        labs(title = "Residuals (Experiment - Best Simulation Fit)",
             x = bquote(italic(B)~~"("~.(B.unit)~")"),
             y = switch(2-grepl("deriv|Deriv",lineSpecs.form),
                        bquote(Diff. ~ ~ d*italic(I)[EPR]~ ~"/"~ ~d*italic(B) ~ ~ ~ "(" ~ p.d.u. ~ ")"),
                        bquote(Diff. ~ ~ italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")")
            )
          ) +
        plot_theme_In_ticks()
      #
      ## entire plot
      plot.sim.expr <-
        patchwork::wrap_plots(plot.sim.expr.upper,
                              plot.sim.expr.lower,
                              ncol = 1)
      #
  } else {
    plot.sim.expr <- ggplot(data = data.sim.expr.long) +
      geom_line(aes(x = .data[[paste0("B_",B.unit)]],
                    y = .data[[Intensity.expr]],
                    color = .data$Spectrum),
                linewidth = 0.75) +
      scale_color_manual(values = c("darkcyan","magenta","darkblue"),
                         labels = c("Experiment\n",
                                    "Simulation\n+Baseline Fit\n",
                                    "Simulation\n-Baseline Fit")) +
      labs(color = NULL,
           x = bquote(italic(B)~~"("~.(B.unit)~")"),
           y = ylab) +
      plot_theme_NoY_ticks() +
      theme(legend.text = element_text(size = 13))
  }
  #
  ## ==================== BASIC OPTIMIZATION INFORMATION/STATISTICS ======================
  #
  ## final list components depending on method
  min.LSQ.sum <- c()
  N.evals <- c()
  N.converg <- c()
  for(m in seq(optim.method)){
    if (optim.method[m] == "levenmarq"){
      min.LSQ.sum[[m]] <-
        optimization.list[[m]]$deviance ## The min sum of the squared residual vector.
      # fn.min <- optimization.list$fvec ## The result of the last `fn` evaluation; i.e. the residuals.
      N.evals[[m]] <-
        optimization.list[[m]]$niter ## The number of iterations/evaluations completed before termination.
      N.converg[[m]] <-
        optimization.list[[m]]$rsstrace ## Sum of squares at each iteration.
    }
    if (optim.method[m] == "pswarm"){
      min.LSQ.sum[[m]] <-
        optimization.list[[m]]$value ## The value of `fn` corresponding to best `par`.
                                     ## because `fn` is sum of squares
      N.evals[[m]] <-
        optimization.list[[m]]$counts ## A three-element vector containing the number of function
                                      ## evals., the number of iterations, and the number of restarts.
      N.converg[[m]] <-
        optimization.list[[m]]$convergence ## An integer code. `0` indicates that the algorithm
                                           ## terminated by reaching the absolute tolerance; otherwise:
                                           ## `1` Maximum number of function evaluations reached.
                                           ## `2` Maximum number of iterations reached.
                                           ## `3` Maximum number of restarts reached.
                                           ## `4` Maximum number of iterations without improvement reached.

    }
    if (optim.method[m] == "slsqp" || optim.method[m] == "neldermead" ||
        optim.method[m] == "crs2lm" || optim.method[m] == "sbplx" ||
        optim.method[m] == "cobyla" || optim.method[m] == "lbfgs"){ ## with `else` it doesn't work
      min.LSQ.sum[[m]] <-
        optimization.list[[m]]$value ## the function value corresponding to `par`.
                                     ## because function is sum of squares
      N.evals[[m]] <-
        optimization.list[[m]]$iter ## number of (outer) iterations, see `Nmax.evals`.
      N.converg[[m]] <-
        optimization.list[[m]]$convergence ## integer code indicating successful completion (> 0)
                                           ## or a possible error number (< 0).
    }
  }
  #
  ## ================================= RESULTS =============================
  #
  if (isTRUE(check.fit.plot)) {
    ## finally add residuals to `data.sim.expr.long`
    data.sim.expr.resids <- data.sim.expr.resid %>%
      dplyr::rename_with(~ c(Intensity.expr),dplyr::all_of("Residuals"))
    #
    data.sim.expr.resids[["Spectrum"]] <-
      rep("Residuals",times = nrow(data.sim.expr.resids))
    #
    data.sim.expr.long <-
      data.sim.expr.long %>%
      dplyr::bind_rows(data.sim.expr.resids) %>%
      dplyr::arrange(.data$Spectrum)
  }
  #
  # return list, vector, data frame ...
  if (isTRUE(output.list.final)) {
    ## final optimized parameters (+ min.LSQ) from the last `optim.method`
    ## if `length(optim.method) > 0`
    result.vec <- c(
      best.fit.params[[length(optim.method)]],
      min.LSQ.sum[[length(optim.method)]]
    )
    ## final data frame
    if (isTRUE(check.fit.plot)){
      result.df <- data.sim.expr.long %>%
        dplyr::filter(Spectrum == "Simulation") %>%
        dplyr::select(!dplyr::all_of(c("Spectrum")))
    } else {
      result.df <- data.sim.expr %>%
        dplyr::select(dplyr::all_of(c(paste0("B_",B.unit),
                                      "Simulation",
                                      "Simulation_NoBasLin")))
    }
    #
    result <- list(params = result.vec,df = result.df)
    #
  } else {
      ## final list components (switching between `check.fit.plot` condition)
      result <- list(
        plot = plot.sim.expr,
        best.fit.params = best.fit.params,
        df = switch(2-check.fit.plot,
                    data.sim.expr.long,
                    data.sim.expr),
        sum.LSQ.min = min.LSQ.sum,
        N.evals = N.evals,
        N.converg = N.converg
      )
    }
  #
  return(result)
  #
}
