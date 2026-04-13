# suction-furs-minimal-reproduction

Minimal R reproduction script for the main figures based on ESM_2.xlsx.

## Overview

This repository contains a minimal R script to reproduce the main study figures from the final manually verified 109-study coded dataset.

It supports figure-level minimal reproducibility only and does not reproduce the full Web of Science retrieval, screening, dual-review full-text verification, or manual audit workflow.

## Repository contents

- `reproduce_figures_from_ESM_2.R`

## Required input

Before running the script, place the following file in the `data/` directory:

- `data/ESM_2.xlsx`

Worksheet name:

- `Study-level coding`

## Note

The current manuscript and supplementary package refer to the final manually verified study-level coding workbook as `ESM_2.xlsx`.

Earlier internal file naming used `Supplementary_Data_File_2.xlsx` in prior versions. The current repository version uses `ESM_2.xlsx` as the expected workbook name.

The required input workbook is provided separately through the journal submission system for editorial and peer-review purposes and is not mirrored in this public repository.

This repository is therefore a **code-only minimal reproduction repository**.

## Output

The script generates the following files in the `results/` directory:

- `results/Fig_1.png`
- `results/Fig_1.tiff`
- `results/Fig_1.pdf`
- `results/Fig_2.png`
- `results/Fig_2.tiff`
- `results/Fig_2.pdf`
- `results/Supplementary_Fig_S3.png`
- `results/Supplementary_Fig_S3.tiff`
- `results/Supplementary_Fig_S3.pdf`
- `results/Supplementary_Table_S3.csv`

## Environment used

- R version: 4.5.0
- Platform: x86_64-w64-mingw32/x64
- OS: Windows 11 x64

## Main packages

- `here`
- `readxl`
- `ggplot2`
- `dplyr`
- `tidyr`
- `stringr`
- `patchwork`
- `ggrepel`
- `scales`
- `rlang`
- `cowplot`
- `ragg`
- `tibble`

## Run

Run the script from the project root:

```r
source("reproduce_figures_from_ESM_2.R")

```
## Scope
This script supports figure-level minimal reproducibility from the final coded dataset only. It does not reproduce the full Web of Science retrieval, screening, dual-review full-text verification, or manual audit workflow.
