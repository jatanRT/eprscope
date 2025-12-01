# Solvent Properties Data Frame (Dataset) for EPR/ENDOR

Data frame summarizing the most important solvent properties for
EPR/ENDOR which are required for variable temperature (VT) experiments
and EPR spectroelectrochemistry.

## Usage

``` r
solvents_ds
```

## Format

A data frame with 46 rows and 10 variables/columns:

- Solvent:

  Character, solvent name.

- Formula:

  Character, ponting to molecular formula.

- MW:

  Numeric, pointing to relative molecular weight.

- Boiling_Point_oC:

  Numeric, corresponding to **boling** point in **°C**.

- Melting_Point_oC:

  Numeric, corresponding **melting** point in **°C**.

- Density_gmL:

  Numeric, corresponding to density in \\\text{g}\\\text{mL}^{-1}\\.

- Solubility_g100gW:

  Character, pointing to solubility in water expressed in
  \\\text{g}\\(100\\\text{g}~\text{of}~\text{H}\_2\text{O})^{-1}\\. 2.
  Solubility of THF in water is rather complex.

- Dielectric_Const:

  Character, corresponding to relative permittivity.

- Flash_Point_oC:

  Numeric, pointing to flash point in **°C**.

- Viscosity_cp:

  Character, corresponding to solvent dynamic viscosity in
  \\\text{cp}\equiv 1\\\text{mPa}\\\text{s}\\. The values were collected
  from
  [Sigma-Aldrich](https://www.sigmaaldrich.com/deepweb/assets/sigmaaldrich/marketing/global/documents/614/456/labbasics_pg144.pdf)
  for 20°C; [PubChem NCBI](https://pubchem.ncbi.nlm.nih.gov/) for 20°C
  and 25°C and from [ACCU DYNE
  TEST](https://www.accudynetest.com/visc_table.html) for 20°C, 25°C or
  30°C.

## Source

<https://organicchemistrydata.org/solvents/>

<https://www.sigmaaldrich.com/deepweb/assets/sigmaaldrich/marketing/global/documents/614/456/labbasics_pg144.pdf>

<https://pubchem.ncbi.nlm.nih.gov/>

<https://www.accudynetest.com/visc_table.html>

## Details

The main properties were collected from the [Division of Organic
Chemistry of the ACS](https://organicchemistrydata.org/solvents/) and
the
[Sigma-Aldrich](https://www.sigmaaldrich.com/deepweb/assets/sigmaaldrich/marketing/global/documents/614/456/labbasics_pg144.pdf).
Additional resources (e.g. for viscosities) are [PubChem
NCBI](https://pubchem.ncbi.nlm.nih.gov/) and [ACCU DYNE
TEST](https://www.accudynetest.com/visc_table.html). Besides that, the
polarity of solvents (expressed by the relative permitivity
`Dielectric_Const`) is important parameter to decide which tube/cell has
to be used for an experiment at specific temperature (unless the
measurements performed directly in liquid \\\text{N}\_2\\) =\> for the
polar solvents use capillaries or special "flat" cells (e.g. for EPR
spectroelectrochemistry), while for the less polar solvents, common
quartz tubes (with the i.d. of \\(2-4)\\\text{mm}\\) can be applied. See
also
[`vignette("datasets")`](https://jatanrt.github.io/eprscope/articles/datasets.md).

## See also

Other Built-In Datasets:
[`isotopes_ds`](https://jatanrt.github.io/eprscope/reference/isotopes_ds.md)
