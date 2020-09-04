Analysis explanatory notes
================
Dr Alasdair Sykes
04/09/2020

## Overview

The goal of this analysis is to build and implement a version of the
IPCC (2019) steady-state model for soil carbon sequestration in global
croplands. This model will initially provide the basis for a series of
soil-based mitigation cost-effectiveness simulations focusing on arable
crop production in the United Kingdom, with the ultimate goal being the
creation of a soil carbon marginal abatement crop curve for UK
agriculture.

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

### Scripts

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
    (processed in *wheat-data-preprocessing.R*) to the model inputs.

### Data

#### `manure-data` folder

#### `parameter-data` folder

## References

<div id="refs" class="references">

<div id="ref-IPCC2019">

IPCC, 2019. 2019 Refinment to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories.

</div>

</div>
