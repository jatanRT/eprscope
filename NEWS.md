# eprscope 0.1.13

## 2025-03-22

### Bug Fixes/Critical Updates

* minimum sum of residual squares (`min.rss`), coming from the final list 
  of `eval_sim_EPR_isoFit`, is now identical to that of calculated from the final 
  data frame `df` of the column/spectrum `Residuals`
  
* intensity (multiplet) pattern related to just one group of equivalent nuclei
  is properly displayed when running the `eval_sim_EPR_iso` + right now the 
  function has no limitations, regarding the number of nuclei (within a group) 
  or their corresponding spin quantum number *I* (calculation of multiplets,
  multinomial coefficients, was significantly updated using a recursive function)
  
* bibliography (`.bib`) template, when running the `create_qmdReport_proj`,
  now possesses the right name inherited from the `wd.subdir.name` 
  
* fixed bug in `plot_EPR_Specs` where the g-value scale was not properly 
  displayed; now, if used either with `plot_theme_NoY_ticks`
  or `plot_theme_In_ticks`, the back-ticks on the opposite axis are shown
  as expected
  
* several fixes and/or updates in documentation (functions, including 
  `Examples` + vignettes)   

### Updates

* the maximum number of components in `quantify_EPR_Sim_series` was increased 
  from `6` to `10`; however one should be aware of such a high number
  of radicals when describing the EPR spectrum "envelope" (the sum of all
  components) because all the components/radicals must mirror the chemical
  reality of that mixture
  
* data frame output from the `eval_sim_EPR_isoFit` 
  when `output.list.forFitSp = FALSE` was replaced by the plot/spectrum, 
  depending on the `check.fit.plot` argument, in order to be ready 
  for the new complex fitting function (currently under development);
  this is also the reason why the list of suggested packages
  in the `DESCRIPTION` has been updated
  
* code in several functions &rarr; cleaned up in order to be more readable

* right now, all functions based on optimization of EPR simulation parameters,
  like `optim_for_EPR_fitness`, `eval_simEPR_isoFit`, `quantify_EPR_sim_series`,
  possess an option to display messages and progress of the optimization/fitting
  procedure (including the elapsed time), within the R console, for better
  interactivity; `Examples` of those functions have been updated accordingly
  
* the `Blim` argument (defining the magnetic flux density, *B* region as a vector)
  was added/updated in several functions which return plots/spectra   

### New Functions/Files/Vignettes

* R Shiny application with simple user interface (UI): `plot_eval_ExpSim_app`, 
  providing not only interactive visualization of an CW EPR spectrum,
  and its corresponding instrumental parameters but also the simulation
  in isotropic regime; all graphs/spectra and data frames/tables can be also
  exported to common formats like `.csv`, `.pdf`, `.png`, `.jpeg` and excel

# eprscope 0.1.12

## 2024-12-05

### Bug Fixes/Critical Updates

* `README` documentation &rarr; fixed/updated, because it did not contain 
  the information about the *JDK* installation as well as that about the essential 
  R packages + couple of learning or tutorial resources and comments were added
  
* several fixes and/or updates in documentation (functions + vignettes)  

* the default intensity multiplication coefficient (parameter) boundaries 
  within the `eval_sim_EPR_isoFit` function are fixed and correctly set up +
  extended default boundaries for hyperfine coupling constants (*A*) added

* fixed visualization of an EPR spectrum fit within the `eval_sim_EPR_isoFit` +
  the initial simulation (see updates) also includes a baseline correction 
  (now it should also work for the consecutive `optim.method` (vector))

* increased decimal places of the magnetic flux density (*B*) and intensity
  by the reading of ASCII files, using the `read_EPR_Specs` and related functions
  
* fixed issue with the `origin` argument definition (regarding the spectrometer 
  software labels/names) in reading functions

* initial `qvar`s for the `"(a=1)A + (r=1)R --> [k1] B"` model reaction 
  (see also updates) are now correctly set up

* the minimum sum of residual squares (RSS) value argument was renamed 
  (`min.LSQ.sum` &rarr; `min.rss`) and unified for all relevant 
  fitting functions
  
* tube diameter within the `quantify_EPR_Abs` is now correctly defined  

* fixed issue with the integral scale within the `eval_integ_EPR_Spec`

### Updates

* code in several functions &rarr; cleaned up in order to be more readable,
  this also includes renaming of several arguments in those functions +
  `messages` and `stop`s added on couple of places, within the code,
  for the sake of interactivity/awareness of operations
  
* reorganization of files and folders related to vignettes

* code for the integration of EPR spectra (see the `eval_integ_EPR_Spec` function) 
  was simplified and supplemented with the option to integrate spectra having
  the [B] = T units, this also applies to the `quantify_EPR_Sim_series` function 
  
* the initial simulated EPR spectrum was added to output graphics 
  of the `eval_sim_EPR_isoFit` value/list (for the case of `check.fit.plot = TRUE`) 
  in order to compare it with the final/best fit as well as 
  with the experimental EPR spectrum   

* number of letters in kinetic reaction schemes were decreased 
  in order to be more readable + the last scheme (see also bug fixes above)
  as well as the time duration and resolution were simplified in order 
  to better work with the numeric ODE integration (by the `deSolve::ode` function)

* option to select a method for the numeric ODE integration 
  (argument `solve.ode.method`) was added to both key functions analyzing 
  the radical kinetics
  
* added option to select a specific implementation of the `pswarm` ("particle swarm") 
  optimization algorithm/method, within the `optim_for_EPR_fitness` &rarr; either
  `“SPSO2007”` (for small particle spaces and swarm sizes) or `“SPSO2011”` 
  (for larger ones) can be set up + internal function updated (not provided 
  as a user option), the number of "informants" (related to particles) 
  was slightly increased by the corresponding exponent, *k = 6* 
  (see also documentation for the `pso::psoptim`)
  
* value/output list of the `eval_sim_EPR_isoFit` was extended to exclusively 
  include vector of the optimized parameters (together with the `min.rss`)
  and data frame of the best EPR simulation fit in order to be ready for the new
  complex fitting function (currently under development)
  
* simple residual analysis (plots) added to the `eval_kinR_EPR_modelFit` 
  output/value list, including the Q-Q ("quantile-quantile") plot 
  (to check weather residuals are normally distributed) 
  as well as the Residual plot (residuals *vs* predicted/fitted `qvarR`s,
  e.g. concentration) in order to check the kinetic model fit
  and precision of the optimized kinetic parameters

### New Functions/Files/Vignettes

* `create_qmdReport_proj` &rarr; create file-folder structure 
  of an EPR project to create a reproducible Quarto report (please, 
  also refer to the https://quarto.org/ website)

* peak picking in EPR/ENDOR spectrum: `eval_peakPick_Spec`, 
  with the selection of positive or negative intensity peaks for both derivative 
  as well as integrated forms of an EPR/ENDOR spectrum
  
* evaluate activation parameters of an elementary radical reaction,
  obtained by the "Eyring fit" of the experimental *k vs T* dependence,
  based on the essential transition state theory (TST) &rarr;
  `eval_kinR_Eyring_GHS`

# eprscope 0.1.11 (1st public release)

## 2024-08-29

### Bug Fixes/Critical Updates

* fixed reading of spectral time series within the `readEPR_Exp_Specs`
  function

* reading parameter strings from 'parameter' files within the `readEPR_param_slct`
  function fixed

* the *g*-value scale on *x*-axis within the plotting functions is now properly
  displayed
  
* if the related `time(series)` unit is converted in relevant data frames, 
  the corresponding column is renamed in order to inherit the new unit 
  (like `time_s`) accordingly
  
* now, the elements in `names` argument, within the `readEPR_Exp_Specs_multif`,
  can also contain characters like `c("a","b","c")`, not just numbers,
  e.g. `c("250","260","270")`
  
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
