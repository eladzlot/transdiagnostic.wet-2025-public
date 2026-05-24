# Transdiagnostic Written Exposure Therapy (WET) — 2026

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20366472.svg)](https://doi.org/10.5281/zenodo.20366472)

This repository contains the data, scripts, materials, and supplementary documentation for "Transdiagnostic Written Exposure Therapy: Piloting an Online Intervention." The study investigates the efficacy and mechanisms of Written Exposure Therapy (WET) for transdiagnostic anxiety in an online setting.

A time-stamped, immutable snapshot of this repository is archived on Zenodo at [https://doi.org/10.5281/zenodo.20366471](https://doi.org/10.5281/zenodo.20366471) (concept DOI; always resolves to the latest version).

## Contents

### Manuscript

Folder: [`docs/`](docs/). Contains R Markdown source files that compile to the manuscript, including all models and calculations.

- [`docs/output/wet.pdf`](docs/output/wet.pdf) — rendered manuscript.
- [`docs/output/wet.docx`](docs/output/wet.docx) — rendered manuscript (Word format, for journal submission).

### Scripts

Folder: [`scripts/`](scripts/). All analysis scripts used to process data, compute results, and generate visualisations.

- Files prefixed `tables.` produce the manuscript tables.
- Files prefixed `figures.` produce the manuscript figures.
- Files prefixed `model.` define and run the growth, change, and mechanism models. Models are fit via a wrapper around the `rethinking` package, defined in [`scripts/model.helpers.R`](scripts/model.helpers.R).
- Files prefixed `run.` execute model batches for processes and mechanisms.
- [`scripts/tidy.R`](scripts/tidy.R) and [`scripts/load_data.R`](scripts/load_data.R) handle data loading and reshaping.

### Data

Folder: [`data/`](data/). Processed datasets used in the analyses presented in the manuscript.

- [`data/CODEBOOK.md`](data/CODEBOOK.md) — variable-level documentation for every data file. Read this first.
- [`data/data_long.csv`](data/data_long.csv) — long-format dataset, one row per item response per participant per timepoint.
- [`data/demographics.data.csv`](data/demographics.data.csv) — demographic information and baseline OASIS/WSAS totals per participant.
- [`data/pid.condition.csv`](data/pid.condition.csv) — id-to-condition crosswalk.
- [`data/screening.csv`](data/screening.csv) — data from the pre-randomization screening pool (n = 375).
- [`data/diagnostics/`](data/diagnostics/) — outputs of the preregistered outlier-diagnostic procedure (see the diagnostics supplement).

Participant identifiers in all data files are serial numbers, not real Prolific IDs, to protect participant privacy.

### Supplementary materials

Folder: [`supplementary/`](supplementary/).

**Materials presented to participants** ([`supplementary/materials/`](supplementary/materials/)):

- [`MATERIALS.pdf`](supplementary/materials/MATERIALS.pdf) — navigation document for all participant-facing materials. Includes a cross-reference table showing what participants saw at each timepoint, instructions and response anchors for every measure used, and citations to source publications.
- [`consent.pdf`](supplementary/materials/consent.pdf) — consent form text.
- [`ctsi.pdf`](supplementary/materials/ctsi.pdf) — Core Threat Structured Interview (intervention component).
- [`instructions.wet.pdf`](supplementary/materials/instructions.wet.pdf) — Written Exposure Therapy psychoeducation and writing instructions.
- [`instructions.niw.pdf`](supplementary/materials/instructions.niw.pdf) — Neutral Imagery Writing psychoeducation and writing instructions.

**Analytic supplements**:

- [`supplementary/models.pdf`](supplementary/models.pdf) — detailed description of all statistical models.
- [`supplementary/processes/processes.pdf`](supplementary/processes/processes.pdf) — detailed outcomes of all candidate-mechanism (process) models.
- [`supplementary/diagnostics_outliers.pdf`](supplementary/diagnostics_outliers.pdf) — preregistered measurement-level WAIC and PSIS outlier diagnostics across all 27 fitted models.
- [`supplementary/engagement/engagement.manual.pdf`](supplementary/engagement/engagement.manual.pdf) — coding manual for engagement.
- [`supplementary/judges/coding.impact.md`](supplementary/judges/coding.impact.md) — coding scheme for qualitative analysis of impact and skill-transfer responses.

The preregistration for this study is available on OSF at [https://osf.io/2cehr](https://osf.io/2cehr) ([DOI 10.17605/OSF.IO/2CEHR](https://doi.org/10.17605/OSF.IO/2CEHR)).

## How to use

1. **Clone the repository:**
   ```bash
   git clone https://github.com/eladzlot/transdiagnostic.wet-2025-public.git
   ```

2. **Install dependencies.** The scripts depend on:
   - `tidyverse`
   - `here`
   - `psych`
   - `papaja`
   - `rethinking`

   For installing `rethinking`, see the [installation guide](https://github.com/rmcelreath/rethinking/tree/master). The remaining packages can be installed with:
   ```r
   install.packages(c("tidyverse", "here", "psych", "papaja"))
   ```

3. **Reproduce the manuscript.** Knit [`docs/wet.Rmd`](docs/wet.Rmd) in RStudio (or via `rmarkdown::render()`) to regenerate the output document. The first knit will fit all Bayesian models from scratch, which is slow; cached fits are stored in `data/models/` on subsequent runs.

## Citation

If you use this repository in your work, please cite:

> Zlotnick, E., Sorka, H., Barzilay, S., & Huppert, J. D. (2026). Transdiagnostic Written Exposure Therapy: Piloting an Online Intervention. *Clinical Psychological Science*.

## License Overview

### Code
The code in this repository is licensed under the [MIT License](LICENSE). You are free to use, modify, and distribute the code, provided proper attribution is given.

### Data
The data are under a restricted-use [license](data/LICENSE):
- **Embargo**: These materials are private and cannot be used without explicit permission until the associated publication.
- **Post-Embargo License**: After publication, these materials will be licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 (CC BY-NC-SA)](https://creativecommons.org/licenses/by-nc-sa/4.0/) license.
- **Attribution**: Proper attribution is required for any use.

### Manuscript License
The manuscript files in this repository are private and under embargo until the associated work is officially published. During the embargo period, they may not be used, shared, or distributed without explicit permission. Post-publication use of the manuscript will be subject to publisher policies and agreements.

## Contact

For questions or feedback, please contact [Elad Zlotnick](mailto:elad.zlotnick@mail.huji.ac.il).
