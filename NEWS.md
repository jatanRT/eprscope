# eprscope 0.1.11 (1st public release)

## 2024-08-29

### Bug Fixes/Critical Updates

* reading of the spectral time series within the `readEPR_Exp_Specs`
  function

* reading parameter strings from 'parameter' files within the `readEPR_param_slct`
  function

* the *g*-value scale on *x*-axis within the plotting functions is now properly
  displayed
  
* if the related `time(series)` unit is converted in relevant data frames, 
  the corresponding column is renamed in order to inherit the new unit 
  (like `time_s`), accordingly
  
* now, the elements in `names` argument, within the `readEPR_Exp_Specs_multif`,
  can also contain characters like `c("a","b","c")`, not just numbers
  like `c("250","260","270")`
  
* fixed visualization of legend labels within the `plot_EPR_Specs` function 

* natural abundance of interacting nuclei to calculate the intensities
  within the `eval_sim_EPR_iso` and related functions/examples 

### Updates

* polynomial to fit the distribution of (*B*<sub>1</sub>,*B*<sub>m</sub>) 
  within the cavity was extended in `quantify_EPR_Abs`. Now, the degrees from
  5 to 12 are available
  
* additionally, the above-corresponding (*B*<sub>1</sub>,*B*<sub>m</sub>) 
  theoretical distribution calculation was removed for now, and will be addressed
  in the future

* intensity, *B*, and the `time.series` column indices arguments, in general reading
  function `readEPR_Exp_Specs`, were renamed (using `.id`) in order to clearly
  point to headers in `col.names` 
  
* `magnettech` option added to reading functions to read the EPR spectra
  and parameters from these instruments, additionally, the 'general' reading
  function `readEPR_Exp_Specs` was updated to read files, from
  the most common EPR instruments, more easily by adding the auxiliary 
  `data.structure` argument 
  
* the optimization algorithms, other than `diff-levenmarq`, were removed
  from the `eval_kinR_EPR_modelFit` for now, additional optimization
  methods will be implemented later on
  
* many function documentations updated + several examples added +
  code-cleaning (by `styler` package) done in many functions + 
  `README`, `DESCRIPTION` and vignettes updated, accordingly
  
* vignette to describe the EPR simulations and fitting was removed, for now
  (will be added later), however, the important points from that vignette
  are summarized in the relevant function documentations
  
* stoichiometric coefficient notation as well as reaction model scheme
  for consecutive reactions within the `eval_kinR_ODE_model` updated/extended
  
* option to use Tesla (`T`) unit, now available in all *g*-value evaluations 

* `line.type` option added to all relevant plotting functions

* calculation of confidence interval by the `eval_interval_cnfd_tVec` simplified

* to read the EPR instrumental parameters by `readEPR_params_slct`,
  added option to read several parameters simultaneously (vectorized input)
  
* customized plot themes, like `plot_theme_In_ticks`, were simplified,
  now, there is no need to add opposite axis-ticks by additional theme layer,
  those function can now also include additional arguments from general
  `theme()` function

### New Functions/Files/Vignettes  

* perinaphthenyl (PNT) ENDOR spectral data added to package database
  for documentation examples (in plotting and reading functions)
  
* setting up the continuous integrations like `R CMD check` 
  and `pkgdown` (in order to build the site)  via github actions
  
* `CONTRIBUTING.md` added to package/site  

* new vignette for datasets added

* new family functions for EPR spectroelectrochemistry, including
  the function to plot voltammograms and chronoamperograms 
  (`plot_ECh_VoC_amperogram`) as well as to calculate the transferred charge
  and the corresponding number of electrons from chronoamperogram
  (`eval_ECh_QNe_chronoamp`)
  
* new dataset (`Triarylamine_radCat_decay_a`) as an example of time 
  series experimental data + new datasets coming from `magnettech`
  instrument: `AcridineDeriv_Irrad_365nm`
  
# eprscope 0.1.10

## 2024-01-24

### Bug Fixes/Critical Updates

* calculation of normalization constant `quantify_EPR_Norm_const` ⇒
  number of points added to calculated norm. constant by unitless receiver gain
  
* kinetic models by ODE ⇒ `eval_kinR_ODE_model` + `eval_kinR_EPR_modelFit`
  now consider partial reaction orders and correct formulas for rates
  ⇒ derivations divided by stoichiometric coefficients according
  to IUPAC recommendations (see also https://doi.org/10.1021/ed083p510) +
  additionally, now the kinetic parameters can be optimized
  not only by the Levenberg-Marquardt algorithm but also by those including within 
  the `optim_for_EPR_fitness` (in case if the partial reaction orders
  are considered)
  
* `eval_kinR_ODE_model` can now compare/plot experimental data and model
  in order to 'play' with (manually optimize) partial reaction orders
  and/or stoichiometric coefficients to fit the experimental data e.g. like
  integrals or concentrations *vs.* time. Additionally, the A <-- R --> B kinetic model
  was removed (also from `eval_kinR_EPR_modelFit`) simply, because it "doubles"
  the R --> B model. The model can be added later on.
  
* several bugs in `quantify_EPR_Abs` (e.g. like concentration calculation + default temperature
  definition) were fixed. The function was completely rewritten. Moreover, now the quantification possesses
  user's/instrument's defined polynomial (orders from 6 to 11) fitting of the spatial distribution
  of *B*<sub>1</sub> and *B*<sub>m</sub> as well as point sample calibration factor as arguments
  in order to be more flexible. Finally, the theoretical *B*<sub>1</sub> and *B*<sub>m</sub> distribution
  (see e.g. https://www.sciencedirect.com/science/article/pii/S1090780797912489) 
  can be considered as well in order to estimate the radical concentration. 
  
* remedy for the Breit-Rabi calculations of energies/frequencies/Bs in simulation => 
  now the g-value for each level is corrected and *B* are calculated by the fixed-point iterations
  (see e.g https://doi.org/10.1016/j.jmr.2005.08.013). Simulations are not limited by
  the number of equivalent nuclei groups anymore (6 could be used up to now). 
  Right now any number of groups maybe used in the actual simulation functions. All simulations 
  are also continuously checked by the package tests/examples.
  
* `eval_sim_EPR_isoFit` now contains the option not only to fit the *pseudo*-Voight lines but also pure 
  Lorentzian or pure Gaussian onto the experimental EPR spectra
  
### Updates

* `README` ⇒ pkg. badges initiated + usage examples added + `DESCRIPTION` + vignette demonstrating
  the basic functionality of the package
  
* several documentations and examples

* `draw_molecule_by_rcdk` ⇒ renamed and focused just on one molecule +
  added option to place label of the molecule onto an arbitrary position of output image +
  documentation + examples
  
* increased accuracy of ENDOR frequencies in nuclei `isotopes_ds` dataset

* couple of variables/arguments (e.g. like `B` <-> `B.val`) renamed in order to be consistent 
  throughout the package
  
* to select/remove the columns from `data frame` objects by `{dplyr}` the `dplyr::all_of()` function 
  is now mostly applied in the package code
  
* extended tolerance to find half of the max. intensity in order to evaluate FWHM

* functions to read the EPR spectra like `readEPR_Exp_Specs`, `readEPR_Exp_Specs_kin` 
  as well as `readEPR_Exp_Specs_multif` were simplified and now contain the option to take additional 
  arguments from the essential `data.table::fread()` which are not predefined in those functions 
  in order to be more flexible upon reading
  
### New Functions/Files/Vignettes  

* added files required to run examples as well as those incl. in vignette(s) ⇒ they are related 
  to EPR spectra of electrochemically generated tetramethyl-phenylenediamine (TMPD) radical cation

* smoothing of EPR spectra by `{npreg}` package ⇒ `smooth_EPR_Spec_by_npreg` which is based 
  on the evaluation of polynomial splines
  
* fitting of the experimental EPR spectra by simulations ⇒ `eval_sim_EPR_isoFit` where several 
  optimization methods can be applied ⇒ all incl. in `optim_for_EPR_fitness` + baseline correction 
  (either "constant" or "linear" or "quadratic") is now included in the fitting procedure
  
* new tutorial vignette for simulations `EPR_Simulations` was initiated/set up

* testing environment by the `{testthat}` pkg. set up and initiated + added tests for simulations 
  and conversions
  
* added files/spectral data corresponding to an EPR spectrum of aminoxyl radical 
  for examples and testing  

# eprscope 0.1.9

## 2023-10-18

### Bug Fixes

* main function to read EPR spectral data (`readEPR_Exp_Specs`) is now forced to use
  `file` argument ⇒ that is `data.table::fread(file = path_to_ASC)`
  
* conversion of `time` to `var` within a cyclic change in `convert_time2var`

* changed tolerance to find intensity values around `0` in *g*-factor evaluation 
  (`eval_gFactor_Spec`) in order to take into account different spectral data resolutions
  
* general integration function (`eval_integ_EPR_Spec`) now includes the proper scaling
  of sigmoid integrals + fixed `Intensity` bug in vector output for the integrated 
  form of EPR spectra

* temperature parameter in instrumental `.DSC` or `.par` files is sometimes missing ⇒ 
  now the `readEPR_params_tabs` checks whether the temperature parameter is present 
  and creates the table accordingly (with or without the temperature)

### Updates

* `README`, `DESCRIPTION` and "Introduction"" `vignette`

* documentation references set up

* Code for the absolute quantification of paramag. centers/radicals was simplified
  within the `quantify_EPR_abs` function.
  
* Function to draw molecules was rewritten to read both  `.sdf` files and `smiles` characters +
  it was renamed to `draw_molecules_by_rcdk`.
  
* Reading the simulated spectra (`readEPR_Sim_Spec`) has now the option to read `ASCII` data from 
  various sources like ⇒ "easyspin", "xenon", "simfonia" (winepr) or "csv".
  
* Instrumental parameters (related to recording of EPR spectra, `readEPR_params_tab`) 
  can be converted into interactive table based on [{DT} package](https://rstudio.github.io/DT/).
  
* Both `solvents` and `isotopes` datasets were renamed to `..._ds`.

* several documentations and examples

* The `Intensity` selection based on different character strings is now replaced 
  by the `lineSpecs.form` argument throughout the corresponding functions.
  
* The `simulation` section/family was renamed to `Simulation and Optimization`.

* Presentation of simulated and experimental spectra (`presentEPR_Sim_Spec`) has now option
  to display overlayed spectra.

### New Functions

* read properties of solvents (from the corresponding datasets) ⇒ `readEPR_solvent_props`

* convert magnetic flux density (*B*) values according to input-output units ⇒ `convert_B`

* simulation of isotropic EPR spectra in both derivative & integrated spectral forms ⇒ 
  `eval_sim_EPR_iso`
  
* simulation of isotropic EPR spectra corresponding to linear **combination** of several 
  components evaluated by the latter function ⇒ `eval_sim_EPR_iso_combo` 

* read instrumental parameters required for the simulations that is now implemented
  in both sim. functions ⇒ `readEPR_params_slct_sim`

# eprscope 0.1.8

## 2023-05-15

* Added functions to evaluate radical kinetics incl. fitting procedures to experimental data
  (integral intensities).

* Added basic package data structure and vignettes. Package data now include the isotope database,
  `isotope_db`, to gather the basic properties of nuclei mainly for analysis in EPR/ENDOR spectroscopy. 
  For such purpose the new function `eval_nu_ENDOR` was written. It provides calculation of ENDOR/Larmor
  frequencies for selected nuclei at specific saturation magnetic flux densities, *B*<sub>sat</sub>.

* All functions (incl. documentation) are now linked by sections/families.

# eprscope 0.1.7

## 2023-04-25

* First tagging after main updates in `quantify_EPR_sim` function. Additionally, the main reading functions, 
  `readEPR_...` have been completely rewritten.
