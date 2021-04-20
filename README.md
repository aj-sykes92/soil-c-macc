Analysis explanatory notes
================
Dr Alasdair Sykes
04/09/2020

## Overview

Repository for code used to develop simulations underlying soil C
marginal abatement cost curve paper.

The goal of this analysis is to build and implement a version of the
IPCC (2019) steady-state model for soil carbon sequestration in global
croplands. This model will initially provide the basis for a series of
soil-based mitigation cost-effectiveness simulations focusing on arable
crop production in the United Kingdom, with the ultimate goal being the
creation of a soil carbon marginal abatement crop curve for UK
agriculture.

The model is implemented spatially, being run at 5-arc-minute (approx
10km) grid cell resolution, and over a temporal scale of 136 years, from
1961 to 2097, in annual timesteps (note some data preprocessing is, by
necessity, conducted month-wise). There is also provision to run the
model over multiple stochastic simulations up to *n = 1000*.

Documentation for the IPCC model can be found at
<https://www.ipcc-nggip.iges.or.jp/public/2019rf/vol4.html> under
Chapter 5 (Cropland), and in the GitHub repository from which the R
package for this implementation is installed,
<https://github.com/aj-sykes92/soilc.ipcc>.

## File management

‘Light’ data used by the model (e.g. model parameters, simple model
inputs) are stored as part of the repository and version controlled
alongside the source code. Large data files are stored outside the
repository and referred to using a custom function `find_onedrive()`.
This function finds the Microsoft OneDrive cloud storage folder which
has been used to date to store and manage large files (which mostly
consist of spatial data). On the author’s machine, `find_onedrive()` is
defined in the global .Rprofile, but this is optional. Using
`find_onedrive()` works like this:

`find_onedrive(dir = "onedrive-subdirectory", path = "some-file.eg")`

returns a character string which looks like this:

`"user/path/to/onedrive/onedrive-subdirectory/some-file.eg"`

As such, other contributors than the author may define their own
`find_onedrive()` function and use it as a single access point to access
large files from their own management system.

The majority of this analysis is designed to be conducted on a ‘normal’
laptop or desktop, but certain stages (see Scripts) require that some
quite large files (i.e. \>40 GB) be produced. The computer named HAL at
SRUC King’s Buildings was used for this purpose.

## Repo schema

  - **climate-anomaly-preprocessing.R** Script to process UKCP climate
    anomaly records and predictions data from .ncdf format into .rds
    with relevant variables extracted (temperature and precipitation).
    This script is possible to run on a standard-spec computer, but best
    done on SRUC-HAL/something with similar power. The output .rds data
    is approximately 2 \* 1.7 GB in size.

  - **model-data-setup-1.R** Script to process the results of
    *climate-anomaly-preprocessing.R* into a spatial format with
    historical spatial climate data from CRU-TS v4.03 brought in as the
    basis of the join. The combination of spatial data, a month-wise
    temporal dimension, and multiple (n = 1000) samples from the UKCP
    data makes for a very large (40 GB) dataset. This output is saved
    into storage space on SRUC-HAL and a smaller file (n = 1) is written
    to cloud storage for use in model development. This script can only
    be run on SRUC-HAL/similar.

  - **model-data-setup-2.R** Script using the toy-size output from
    *model-data-setup-1.R* as the basis for further model input data
    development. This script adds physical soil data and crop data
    (processed in *wheat-data-preprocessing.R*) to the model input data
    object. Implements stochastic estimate of future wheat yield using
    predictions from Ray et al. (2013) and yield response curve method
    from Holland et al. (2019).

  - **model-data-setup-3.R** Script using the output from
    *model-data-setup-2.R* as the basis for stochastic manure addition
    estimate. Manure application rates are estimated based on historical
    data and dressing percentages from FAOstat and the British Survey of
    Fertiliser Practice.

  - **model-data-setup-4.R** Script using the output from
    *model-data-setup-3.R* as the basis for stochastic tillage
    estimation. Tillage practices are estimated based on Eurostat
    tillage practice statistics.

  - **model-data-setup-5.R** Script using the output from
    *model-data-setup-4.R* as the basis for existing cover crop uptake
    estimation. Cover cropping practices are defined based on combined
    information held in *parameter-data/cover-crop-parameters.csv*.

  - **model-scenario-functions.R** Script where functions are defined to
    build soil C sequestration scenarios.

  - **combined-measure-args.xlsx** Excel file containing human readable
    definitioned of each C sequestration scenario as defined by Carmen
    Medina, plus processable translations used directly in the modelling
    process.

  - **01-model-setup-and-run.R** Script where IPCC (2019) steady state C
    model is set up and run for all scenarios. The model functions
    themselves are from the R package `soilc.ipcc`, which is installed
    from [this GitHub
    repository](https://github.com/aj-sykes92/soilc.ipcc).

  - **02-output-generation.R** Script where output data and summaries
    from *01-model-setup-and-run.R* are created and written.

  - `crop-preprocessing` **folder** Folder containing preprocessing
    scripts for crop data.

  - `parameter-data` **folder** Folder containing parameter data (in
    .csv format) used by the IPCC C model functions.

  - `deprecated` **folder** Folder containing old/no-longer-used
    documentation and scripts.

  - `model-runs` **folder** Folder containing summarised model outputs
    (see section *Outputs*.

## Outputs

This subdirectory holds summarised, human-readable outputs from the
model runs. The full output data is too large to version control and is
output directly to Cloud (OneDrive) storage; please see *File
Management* section.

In this version controlled output subdirectory are two .csv files:

### `c-sequestration-summary.csv`

This contains a full summary of the sequestration by each measure over
each project-defined timescale, as well as formal measure definitions
and descriptions. Outputs are all in tonnes C, absolute or annual as
described by each variable name.

### `interaction-factor-summary.csv`

This contains a summary of the interaction factors between measures
calculated using the C model itself. The data provides:

1.  The combined scenario ID
2.  The calculated 20-year C stock for the scenario
3.  The C sequestration calculated with all measures implemented
    simultaneously in the scenario
4.  The equivalent C sequestration resulting from summed separate
    implementations of each measure
5.  The interaction factor, effectively the ratio of (3) / (4)

## References

<div id="refs" class="references">

<div id="ref-Holland2019">

Holland, J.E., White, P.J., Glendining, M.J., Goulding, K.W.T., McGrath,
S.P., 2019. Yield responses of arable crops to liming – An evaluation of
relationships between yields and soil pH from a long-term liming
experiment. European Journal of Agronomy 105, 176–188.

</div>

<div id="ref-IPCC2019">

IPCC, 2019. 2019 Refinment to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories.

</div>

<div id="ref-Ray2013">

Ray, D.K., Mueller, N.D., West, P.C., Foley, J.A., 2013. Yield Trends
Are Insufficient to Double Global Crop Production by 2050. PLoS ONE 8.

</div>

</div>
