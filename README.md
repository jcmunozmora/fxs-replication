# Replication Package: "When Do Property Rights Reduce Illicit Crops? Market Connectivity as the Binding Constraint"

**Authors:** Eduard Martínez-González (ICESI), María Margarita López-Uribe (Universidad de los Andes), Juan C. Muñoz-Mora (EAFIT)  
**Journal:** *Journal of Public Economics* (under review)  
**Date:** June 2026

---

## Overview

This package contains the code and data required to replicate all tables and figures in the paper. We study the *Formalizar para Sustituir* (FxS) program — a land titling initiative implemented in Colombia after the 2016 peace agreement with FARC — and estimate its effect on coca cultivation using a staggered difference-in-differences design at the 1 km × 1 km grid level over 2011–2021.

**Key findings:**
- Land titling (FxS) reduces coca by 0.25–0.38 ha per grid (42–64% of pre-treatment mean)
- The effect fades within two years in areas without road access
- Grids with both FxS titles and *Placa-Huella* tertiary road improvements show a combined reduction of 65–128% of the pre-treatment mean, with sustained effects

---

## Data availability

| Dataset | Availability | Source |
|---------|-------------|--------|
| `panel_grids_regressions.rds` | **Included** — anonymized version (see note below) | Constructed from sources below |
| SIMCI coca detection grids | Request via UNODC-Colombia | [SIMCI](https://www.unodc.org/colombia/es/simci/simci.html) |
| ANT land titling records | Request via ANT | [Agencia Nacional de Tierras](https://www.ant.gov.co/) |
| Suomi-NPP night lights | Public | [NOAA/NASA](https://www.ngdc.noaa.gov/eog/viirs.html) |
| Sentinel-2 (building prediction) | Public | [ESA Copernicus](https://scihub.copernicus.eu/) |
| SECOP contracting records | Public | [Colombia Compra](https://www.colombiacompra.gov.co/) |
| PDET road projects | Public | [ART](https://www.renovacionterritorio.gov.co/) |
| Global Forest Change | Public | [Hansen et al. (2013)](https://www.globalforestwatch.org/) |

**Anonymization note.** The deposited dataset (`data/panel_grids_regressions.rds`, 680 KB) has been anonymized to protect program beneficiaries before public release. Three changes were applied relative to the working dataset:

1. **`id_grid` anonymized** — the original UNODC grid identifiers map 1:1 to GPS coordinates via the UNODC shapefile. Grid IDs have been replaced with randomly assigned anonymous integers (consistent within the dataset so that panel fixed-effects estimation is unaffected).
2. **`cod_vereda` removed** — 8-digit DANE vereda codes identifying sub-municipal administrative units (~200 inhabitants). Not used in any regression.
3. **`n_predios_in_grid` removed** — exact count of titled parcels per grid. 122 grids (49.6% of FxS grids) contain exactly one titled parcel; combined with the grid location, this could identify individual beneficiary households. The binary indicator `d_predio_in_grid` (which is used in the regressions) is retained.

All 81 remaining variables (including all variables used in the published regressions) are present in the deposited file. The underlying SIMCI and ANT records contain parcel-level identifiers that must be requested directly from the respective agencies; they are not reproduced here.

---

## Requirements

- **R** ≥ 4.3.0
- Install all required packages by running:

```r
source("code/00_packages.R")
```

Key packages: `fixest`, `did` (Callaway-Sant'Anna), `bacondecomp`, `HonestDiD`, `ggplot2`, `dplyr`, `tidyr`, `rio`, `here`.

---

## Repository structure

```
fxs_2026/
├── README.md                    ← this file
├── run_all.R                    ← master replication script
├── data/
│   └── panel_grids_regressions.rds   ← main analysis dataset (772 grids × 2001–2021)
├── code/
│   ├── 00_packages.R            ← package installation
│   ├── 04_main_regressions/
│   │   ├── 01_event_study_crops.R     → Figure 4 (main event study)
│   │   ├── 02_main_reg_table.R        → Table 2 (TWFE main results)
│   │   ├── 03_staggered_table.R       → Table 3 (Callaway-Sant'Anna)
│   │   └── 04_staggered_event_study.R → Figure 5 (C-S group-time ATTs)
│   ├── 05_mechanism/
│   │   ├── 01_event_study_mechanism.R → Figure 8 (mechanism event studies)
│   │   ├── 02_mechanism_table.R       → Table 4 (economic activity)
│   │   └── 03_enforcement_table.R     → Table A3 (enforcement)
│   └── 06_roads/
│       ├── 01_roads_main_table.R      → Table 5 (triple difference)
│       ├── 02_placa_huella_mechanism.R → Tables A4–A5
│       └── 03_roads_mechanism.R       → Tables A6–A7
└── output/
    ├── tables/                  ← .tex files for all tables
    └── figures/                 ← .png files for all figures
```

**Note:** Scripts in `05_mechanism/` that require the household survey (441 FxS beneficiaries, baseline 2016 / endline 2019) and scripts in `08_robustness/` requiring the full SECOP procurement database are documented but not executable from this package. Those results are noted in `run_all.R`.

---

## Replication instructions

Run all scripts from the `fxs_2026/` directory:

```bash
cd fxs_2026
Rscript run_all.R
```

Or interactively in R:

```r
setwd("fxs_2026/")
source("run_all.R")
```

Output tables (`.tex`) and figures (`.png`) are written to `output/tables/` and `output/figures/`. The script reports a summary of which scripts completed successfully.

**Estimated runtime:** < 10 minutes on a modern laptop (main bottleneck: Callaway-Sant'Anna estimation with bootstrapped SEs).

---

## Dataset: `panel_grids_regressions.rds`

Unit of observation: **1 km × 1 km grid × year** (2001–2021).  
Full panel: 46,788 grid-year observations × 84 variables.  
Main analysis sample (`sample_control_1 == 1`, years 2011–2021): 8,492 observations, 772 grids (246 FxS + 526 control).

### Key variables

| Variable | Description |
|----------|-------------|
| `id_grid` | Grid identifier |
| `year` | Calendar year |
| `crops` | Coca hectares in grid (= % of 100-ha grid covered by coca) |
| `treat_staggered` | Time-varying treatment: 1 if grid has FxS title AND year ≥ treatment year |
| `d_treated_1` | Time-invariant: 1 if FxS grid (main sample) |
| `d_control_1` | Time-invariant: 1 if neighbor control grid (main sample) |
| `sample_control_1` | = 1 for main analysis sample (treated + immediate neighbors) |
| `sample_control_2` | = 1 for ORIP robustness sample (treated + not-yet-titled ORIP grids) |
| `d_vias_pdet` | = 1 if grid has PDET *Placa-Huella* road project within 1 km |
| `nl_harm` | Log of harmonic-mean night light intensity (Suomi-NPP VIIRS) |
| `building` | Count of 10 m² Sentinel-2 pixels classified as built-up (2016–2021) |
| `loss_cover` | Cumulative primary forest loss pixels (Global Forest Change) |
| `codmpio` | Municipality code (DIVIPOLA) |
| `provincia` | Province/subregion name |
| `year_conf` | Year of first FxS title delivery (0 = never treated) |

---

## Correspondence

Juan C. Muñoz-Mora — jcmunozmora@gmail.com  
Eduard Martínez-González — [contact]

---

## License

Code: [MIT License](https://opensource.org/licenses/MIT)  
Data: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) — derived from public and restricted sources as noted above.
