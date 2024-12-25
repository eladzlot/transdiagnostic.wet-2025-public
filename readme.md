# Transdiagnostic Written Exposure Therapy (WET) - 2025

This repository contains the data, scripts, and supplementary materials for the study "Transdiagnostic Written Exposure Therapy: Piloting an Online Intervention." The study investigates the efficacy and mechanisms of Written Exposure Therapy (WET) for transdiagnostic anxiety in an online setting.

## Contents

### 1. **Scripts**
- **Folder:** [`scripts/`](scripts/)
- Contains all analysis scripts used to process data, compute results, and generate visualizations for the manuscript.
  - Tables: files with the `tables` prefix include the code for creating tables.
  - Figures: files with the `figures` prefix include the code for creating figures.
  - Models: files with the `models` prefix include the code for describing and running growth or mechanism models. All models are run with a wrapper to the rethinking package defined in [`model.helpers`](scripts/model.helpers.R).

### 2. **Supplementary Materials**
- **Folder:** [`supplementary/`](supplementary/)
- Includes:
  - Detailed outcomes of all [process models](supplementary/process.pdf).
  - Detailed outcomes of all [mechanism models](supplementary/mechanisms.pdf).

### 3. **Data**
- **Folder:** [`data/`](data/)
- Contains processed datasets used in the analyses presented in the manuscript.
- **Note:** Raw data is not available to ensure participant confidentiality. Only de-identified and aggregated data are included.

### 4. **Manuscript**
- **Folder:** [`docs/`](docs/)
* Contains Rmd files used to produce the manuscript including all models and calculations.
- The full manuscript of the study is included for reference.
  - **File:** [`wet.pdf`](docs/output/wet.pdf)
  - **File:** [`wet.docx`](docs/output/wet.docx)


## How to Use

1. Clone the repository:
```bash
git clone https://github.com/eladzlot/transdiagnostic.wet-2025-public.git
```

2. Install Dependencies
Ensure all required R packages are installed. The scripts depend on the following packages:
   - `tidyverse`
   - `here`
   - `psych`
   - `papaja`
   - `rethinking` 

  For installing `rethinking` see the [installation manual](https://github.com/rmcelreath/rethinking/tree/master). You can install the rest of these packages in R using:
```R
install.packages(c("tidyverse", "here", "psych", "papaja"))
```

3. Knit the Document  
   Knit the file `docs/wet.Rmd` in RStudio or using another method of your choice to generate the output document.

## Citation

If you use this repository in your work, please cite:
```
Elad Zlotnick, Hila Sorka, Snir Barzilay, Jonathan D. Huppert (2025). 
Transdiagnostic Written Exposure Therapy: Piloting an Online Intervention. 
[Paper submitted for publication]
```

## License Overview

### Code
The code in this repository is licensed under the [MIT License](LICENSE). You are free to use, modify, and distribute the code, provided proper attribution is given.

### Data 
The data are under a restricted-use [license](data/LICENSE):
- **Embargo**: These materials are private and cannot be used without explicit permission until the associated publication.
- **Post-Embargo License**: After publication, these materials will be licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 (CC BY-NC-SA)](https://creativecommons.org/licenses/by-nc-sa/4.0/) license.
- **Attribution**: Proper attribution is required for any use.

### Manuscript License
The manuscript files in this repository are private and under embargo until the associated work is officially published.
During the embargo period, they may not be used, shared, or distributed without explicit permission.

Post-publication use of the manuscript will be subject to publisher policies and agreements. See [manuscript/LICENSE](manuscript/LICENSE) for details.

## Contact
For questions or feedback, please contact [Elad Zlotnick](mailto:elad.zlotnick@mail.huji.ac.il).
