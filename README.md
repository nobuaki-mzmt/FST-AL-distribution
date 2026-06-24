# README

This repository provides access to the data and source code used for the manuscript,  

**Four decades of inland invasion by the Formosan subterranean termite in Alabama: expansion associated with transportation infrastructure**.  

**Xing Ping Hu, Nobuaki Mizumoto**  

Accepted at TBD  
Paper DOI: TBD  
Preprint DOI: 10.32942/X21H4M  

## Table of Contents

- `FSTrecords.csv` - Raw distribution records of *Coptotermes formosanus* by Alabama county.
- `processing.R` - R script that downloads, cleans, and formats all variable data (population, road, rail, climate) and generates the analysis-ready datasets used by `output.R`.
- `output.R` - R script to generate plots and statistical results.
- `source.R` - R script with helper functions sourced by `processing.R` and `output.R`. All outputs will be generated in `output/`

## Data sources

`processing.R` compiles several external datasets, downloaded automatically where possible:

- **County boundaries & roads/rails** - Retrieved via the `tigris` package (US Census Bureau TIGER/Line shapefiles), including primary/secondary roads and rail lines for Alabama, 2011-2025.
- **2000 road/rail counts** - Census TIGER/Line 2000 (`TGR`) files downloaded directly from `https://www2.census.gov/geo/tiger/tiger2k/al/`, parsed from `.RT1` records by Census Feature Class Code (CFCC).
- **Population estimates** - Historical county population from NHGIS (`nhgis0002_ts_nominal_county.csv`, downloaded from [NHGIS](https://www.nhgis.org/)) for 1980-2020, combined with 2024 estimates from the `tidycensus::get_estimates()` function.
- **Climate data** - Monthly average and minimum temperature by county, downloaded from NOAA NCEI Climate Divisional Database (`climdiv-tmpccy` and `climdiv-tmincy` files, https://www.ncei.noaa.gov/pub/data/cirs/climdiv/), 1980-2025.

Processed/intermediate outputs are cached as `.rda` files in `data_fmt/` (`alabama_census.rda`, `df_alabama_climate.rda`, `df_surv.rda`).

## Session information
```
R version 4.5.2 (2025-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Full package versions are recorded in `renv.lock`.
```

## Contact
Nobuaki Mizumoto, nzm0095@auburn.edu
