# suction-furs-minimal-reproduction
Minimal R reproduction script for the main figures based on Supplementary Data File 2.
suction-furs-minimal-reproduction
# Minimal reproduction of the main figures

This repository contains a minimal R script to reproduce the two main figures from the final 109-study coded dataset.

## Files

- `minimal_reproduction_from_supp_data_file_2.R`

## Required input

Place the following file in the same folder as the R script before running:

- `Supplementary Data File 2_Study-level coding.xlsx`

Worksheet name:

- `Study-level coding`

## Note

The required input file is provided through the journal submission system for editorial and peer-review purposes and is not mirrored in this public repository.

This repository is therefore a **code-only minimal reproduction repository**.

## Output

The script generates:

- `minimal_reproduction_results/Figure_1_Combined.png`
- `minimal_reproduction_results/Figure_1_Combined.tiff`
- `minimal_reproduction_results/Figure_1_Combined.pdf`
- `minimal_reproduction_results/Figure_2_Combined.png`
- `minimal_reproduction_results/Figure_2_Combined.tiff`
- `minimal_reproduction_results/Figure_2_Combined.pdf`

## Environment used

- R version: 4.5.0
- Platform: x86_64-w64-mingw32/x64
- OS: Windows 11 x64

## Main packages

- `readxl`
- `readr`
- `ggplot2`
- `dplyr`
- `tidyr`
- `stringr`
- `forcats`
- `patchwork`
- `ggrepel`
- `scales`
- `rlang`
- `cowplot`
- `ragg`

## Run

```r
source("minimal_reproduction_from_supp_data_file_2.R")
```
## Scope
This script supports figure-level minimal reproducibility from the final coded dataset only. It does not reproduce the full Web of Science retrieval, screening, or manual audit workflow.
