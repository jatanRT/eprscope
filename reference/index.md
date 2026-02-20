# Package index

## Built-In Datasets

- [`isotopes_ds`](https://jatanrt.github.io/eprscope/reference/isotopes_ds.md)
  : Nuclear Isotope Data Frame (Dataset) with ENDOR Frequencies
- [`solvents_ds`](https://jatanrt.github.io/eprscope/reference/solvents_ds.md)
  : Solvent Properties Data Frame (Dataset) for EPR/ENDOR

## Conversions and Corrections

- [`convert_A_MHz_2a()`](https://jatanrt.github.io/eprscope/reference/convert_A_MHz_2a.md)
  : Convert Coupling Constants into Splitting Ones.
- [`convert_B()`](https://jatanrt.github.io/eprscope/reference/convert_B.md)
  : Conversion of Magnetic Flux Density
- [`convert_a_mT_2A()`](https://jatanrt.github.io/eprscope/reference/convert_a_mT_2A.md)
  : Convert Splitting Constants into Coupling Ones.
- [`convert_time2var()`](https://jatanrt.github.io/eprscope/reference/convert_time2var.md)
  : Convert Time \\t\\ into Variable Linearly Depending on \\t\\.
- [`correct_time_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md)
  : Time Correction for the Experimental CW EPR Time Series.

## Data Reading

- [`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md)
  : Read the Experimental ASCII or other Text-Based EPR Data.

- [`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md)
  : Read and Process Spectral Data of Time Dependent CW EPR Experiments

- [`readEPR_Exp_Specs_multif()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md)
  : Load Several/Multiple EPR Data/Spectra Files Simultaneously

- [`readEPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/readEPR_Sim_Spec.md)
  : Read the ASCII Data of a Simulated EPR Spectrum

- [`readEPR_param_slct()`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md)
  : Read the Selected EPR Instrumental Parameters and Information

- [`readEPR_params_slct_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_kin.md)
  : Read the Selected Instrumental Parameters of EPR Time Series
  Experiment

- [`readEPR_params_slct_quant()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_quant.md)
  : Read the Selected Instrumental Parameters Relevant to EPR
  Quantitative Analysis

- [`readEPR_params_slct_sim()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_sim.md)
  : Read the Selected Instrumental Parameters Required for EPR
  Simulations

- [`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md)
  : Read the EPR Instrumental Parameters and Information for Tabular
  Outputs

- [`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md)
  :

  Reading Solvent Properties from the `solvents_ds` Dataset

- [`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)
  :

  Reading EPR Simulation Parameters and Information from the *MATLAB*
  `.mat` File

## EPR Spectroelectrochemistry

- [`eval_ECh_QNe_chronoamp()`](https://jatanrt.github.io/eprscope/reference/eval_ECh_QNe_chronoamp.md)
  : Transferred Charge and Number of Electrons from Chronoamperogram
- [`plot_ECh_VoC_amperogram()`](https://jatanrt.github.io/eprscope/reference/plot_ECh_VoC_amperogram.md)
  : Plot Voltammogram or Chronoamperogram from the
  (Spectro)Electrochemical Experiment

## Evaluations

- [`eval_DeltaXpp_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_DeltaXpp_Spec.md)
  : Calculation of EPR/ENDOR Spectrum Linewidth ( e.g. \\\Delta
  B\_{pp}\\)
- [`eval_FWHMx_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_FWHMx_Spec.md)
  : Evaluating Full Width at Half-Maximum (FWHM) from the Integrated EPR
  Spectrum
- [`eval_extremeX_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_extremeX_Spec.md)
  : Find Intensity Extremes within the EPR/ENDOR Spectrum
- [`eval_gFactor()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor.md)
  : Basic Calculation of \\g\\-Factor
- [`eval_gFactor_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_Spec.md)
  : Calculation of \\g\\-factor ("Position") from the EPR Spectrum/Data
- [`eval_interval_cnfd_tVec()`](https://jatanrt.github.io/eprscope/reference/eval_interval_cnfd_tVec.md)
  : Confidence Interval of a Vector or Data Frame Column
- [`eval_kinR_Eyring_GHS()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_Eyring_GHS.md)
  : Reaction Activation Parameters Obtained by the Essential Transition
  State Theory
- [`eval_nu_ENDOR()`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md)
  : ENDOR/Larmor Frequency of Specific Nuclei
- [`eval_peakPick_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_peakPick_Spec.md)
  : Peak Picking of EPR/ENDOR Spectra

## Evaluations and Quantum Chemistry

- [`eval_gFactor_QCHcomp()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_QCHcomp.md)
  : Calculation of \\g\\-factor from the Quantum Chemical Computational
  Output
- [`rearrange_aAiso_QCHcomp()`](https://jatanrt.github.io/eprscope/reference/rearrange_aAiso_QCHcomp.md)
  : Rearrangement of \\A\_{iso}\\/\\a\_{iso}\\ from the Quantum Chemical
  (QCH) Computations
- [`rearrange_aAiso_QCHorgau()`](https://jatanrt.github.io/eprscope/reference/rearrange_aAiso_QCHorgau.md)
  : Rearrangement of \\A\_{iso}\\/\\a\_{iso}\\ from the Gaussian & ORCA
  Computations

## Evaluations and Quantification

- [`eval_integ_EPR_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_integ_EPR_Spec.md)
  : Integration of EPR Spectrum/Data for Quantitative Analysis
- [`eval_kinR_EPR_modelFit()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md)
  : Radical Kinetic Models Fitted onto Experimental Data
- [`eval_kinR_ODE_model()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_ODE_model.md)
  : Quantitative EPR Kinetic Model Profiles by Numeric Solution of the
  ODE.
- [`quantify_EPR_Abs()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Abs.md)
  : Absolute Quantification of Radicals/Spins
- [`quantify_EPR_Norm_const()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Norm_const.md)
  : Normalization Constant Calculation for the Quantitative EPR Analysis

## Shiny Interactive Applications

- [`plot_eval_ExpSim_app()`](https://jatanrt.github.io/eprscope/reference/plot_eval_ExpSim_app.md)
  : Interactive Application to Plot and Evaluate CW Isotropic EPR
  Spectra

## Simulations and Optimization

- [`eval_ABIC_forFit()`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md)
  : General Ranking of Models/Fits Using the AIC and BIC Metrics
- [`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md)
  : Simulation of Isotropic EPR Spectra
- [`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
  : Least-Squares Fitting of Isotropic EPR Spectra by Simulations
- [`eval_sim_EPR_isoFit_space()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md)
  : Explore the Hyperspace of Initial EPR Simulation Parameters
  (Searching for the Best Fit)
- [`eval_sim_EPR_iso_combo()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md)
  : Simulation of Isotropic EPR Spectra Consisting of Several Components
- [`optim_for_EPR_fitness()`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md)
  : General Function for Non-Linear Optimization/Fitting of EPR
  Parameters/Data
- [`plot_eval_EPRtheo_mltiplet()`](https://jatanrt.github.io/eprscope/reference/plot_eval_EPRtheo_mltiplet.md)
  : EPR Intensity Multiplet Prediction for Interactions of Electron with
  Selected Nucleus/Nuclei
- [`plot_eval_RA_forFit()`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md)
  : General Diagnostics for Models/Fits by Simple Residual Analysis
- [`quantify_EPR_Sim_series()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md)
  : Quantify EPR Simulated Spectral Components in the Experimental
  Series
- [`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)
  : Smoothing and Fitting of an EPR Spectrum by Splines

## Visualizations and Graphics

- [`draw_molecule_by_rcdk()`](https://jatanrt.github.io/eprscope/reference/draw_molecule_by_rcdk.md)
  :

  Draw Molecule by [rcdk](https://github.com/CDK-R/cdkr) Defined by
  SMILES or SDF

- [`plot_EPR_Specs()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs.md)
  : Essential Plotting of EPR/ENDOR Spectrum/Spectra

- [`plot_EPR_Specs2D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs2D_interact.md)
  : Interactive Plot (incl. Zooming, Data Reading...etc) of EPR Spectra

- [`plot_EPR_Specs3D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs3D_interact.md)
  : Interactive 3D Surface and 2D Contour Plots for the Series of EPR
  Spectra

- [`plot_EPR_Specs_integ()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs_integ.md)
  :

  Plotting Integrated Forms of EPR Spectra Acquired by the
  `eval_integ_EPR_Spec`

- [`plot_EPR_present_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_present_interact.md)
  :

  Present/Save Interactive Plot for Publication in `.html`,`.pdf` or
  `.docx`

- [`plot_labels_xyz()`](https://jatanrt.github.io/eprscope/reference/plot_labels_xyz.md)
  : Labels for Various Plots (Spectroscopy, EPR, Voltammetry,...etc)

- [`plot_layout2D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_layout2D_interact.md)
  : Additional Layout for the Interactive 2D Plots

- [`plot_theme_In_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_In_ticks.md)
  :

  Custom `ggplot2` Theme with Axis Ticks Oriented Inside the Panel

- [`plot_theme_NoY_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_NoY_ticks.md)
  :

  Custom `ggplot2` Theme without `Y` Axis Ticks

- [`plot_theme_Out_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_Out_ticks.md)
  :

  Custom `ggplot2` Theme with Axis Ticks Oriented Outside the Panel

- [`present_EPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/present_EPR_Sim_Spec.md)
  : A Comparison of the Experimental and Simulated Forms of EPR Spectra

## Non-Categorized

- [`create_qmdReport_proj()`](https://jatanrt.github.io/eprscope/reference/create_qmdReport_proj.md)
  : Basic Quarto Reproducible Project File/Folder Structure for EPR
  Reports
- [`load_data_example()`](https://jatanrt.github.io/eprscope/reference/load_data_example.md)
  : Loading the Built-In Data Files for Package Examples
- [`transform_dfs_2tidyDF()`](https://jatanrt.github.io/eprscope/reference/transform_dfs_2tidyDF.md)
  : Transformation of Several Data Frame Objects/Variables into one Tidy
  (Long Table) Form
