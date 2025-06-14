% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readEPR_params_slct.R
\name{readEPR_params_slct_sim}
\alias{readEPR_params_slct_sim}
\title{Read the Selected Instrumental Parameters Required for EPR Simulations}
\usage{
readEPR_params_slct_sim(path_to_dsc_par, origin = "xenon", B.unit = "G")
}
\arguments{
\item{path_to_dsc_par}{Character string, path (also provided by \code{\link[base]{file.path}})
to \code{.DSC/.dsc} or \code{.par} (depending on OS, see the \code{origin} argument)
\code{text} files including all instrumental parameters and provided by the EPR machine.}

\item{origin}{Character string, corresponding to software used to acquire EPR spectra.
The files are slightly different depending on whether
they were recorded by the "WinEpr",\code{origin = "winepr"}, "Xenon"
(\strong{default}: \code{origin = "xenon"}) or by the "Magnettech" (ESR5000 [11-0422],
\code{origin = "magnettech"}).}

\item{B.unit}{Character string, pointing to unit of magnetic flux density which is the output
"unit", \code{"G"} ("Gauss") or \code{"mT"} ("millitesla"), for \code{"sweep width"}
and \code{"central field"} (see also the \code{\link{eval_sim_EPR_iso}}).
\strong{Default}: \code{B.unit = "G"}.}
}
\value{
List consisting of:
  \describe{
  \item{Bcf}{Central field (magnetic flux density, \emph{B}) value in \code{B.unit}.}
  \item{Bsw}{Sweep width (magnetic flux density, \emph{B}, experimental range) value in \code{B.unit}.}
  \item{Npoints}{Number of points (spectral resolution).}
  \item{mwGHz}{Microwave frequency value in \code{GHz}.}
  }
}
\description{
Reading the \code{.DSC/.dsc} or \code{.par} file to extract the important parameters like
  "sweep width", "central field", "number of points" as well as "microwave frequency"
  which are are required for the simulations of EPR spectra (see also
  the \code{\link{eval_sim_EPR_iso}} function).
}
\examples{
## loading `.par` (`WinEPR`) parameter file example
TMPD_radCat_par_path <-
  load_data_example(file = "TMPD_specelchem_accu_b.par")
#
## `B` parameters in `mT`
readEPR_params_slct_sim(TMPD_radCat_par_path,
                        origin = "winepr",
                        B.unit = "mT")
#
## loading `.dsc` (`Magnettech`) parameter
## file example
AcridineRad.params.path <-
  load_data_example("AcridineDeriv_Irrad_365nm.dsc")
readEPR_params_slct_sim(AcridineRad.params.path,
                        origin = "magnettech")


}
\seealso{
Other Data Reading: 
\code{\link{readEPR_Exp_Specs}()},
\code{\link{readEPR_Exp_Specs_kin}()},
\code{\link{readEPR_Exp_Specs_multif}()},
\code{\link{readEPR_Sim_Spec}()},
\code{\link{readEPR_param_slct}()},
\code{\link{readEPR_params_slct_kin}()},
\code{\link{readEPR_params_slct_quant}()},
\code{\link{readEPR_params_tabs}()},
\code{\link{readEPR_solvent_props}()},
\code{\link{readMAT_params_file}()}
}
\concept{Data Reading}
