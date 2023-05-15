# eprscope 0.1.8

## 2023-05-15

* Added functions to evaluate radical kinetics incl. fitting procedures to experimental data
  (integral intensities).

* Added basic package data structure and vignettes. Package data now include the isotope database,
  `isotope_db`, to gather the basic properties of nuclei mainly for analysis in EPR/ENDOR spectroscopy. 
  For such purpose the new function `eval_nu_ENDOR` was written. It provides calculation of ENDOR/Larmor
  frequencies for selected nuclei at specific saturation magnetic flux densities, *B*.

* All functions (incl. documentation) are now linked by sections.

# eprscope 0.1.7

## 2023-04-25

* First tagging after main updates in `quantify_EPR_sim` function. Additionally, the main reading functions, 
  `readEPR_...` have been completely rewritten.
