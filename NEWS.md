# eprscope 0.1.9

## 2023-10-18

### Bug Fixes

* main function to read EPR spectral data (`readEPR_Exp_Specs`) is now forced to use
  `file` argument ⇒ that is `data.table::fread(file = path_to_ASC)`
  
* conversion of `time` to `var` within a cyclic change in `convert_time2var`

* changed tolerance to find intensity values arround `0` in *g*-factor evaluation 
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
