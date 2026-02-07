# Disturbance and landscape characteristics interactively drive dispersal strategies in continuous and fragmented metacommunities

*This repository contains the code and data to reproduce the analyses and figures in the paper:*

**Gelber, S., Tietjen, B., & May, F. (2026). Disturbance and landscape characteristics interactively drive dispersal strategies in continuous and fragmented metacommunities.**

## Authors

- Stav Gelber, Britta Tietjen, Felix May

## Repository Structure

- `Model/`: Source code of the individual-based simulation model.
  - `src/`: Core model functions (birth, death, dispersal, disturbance, etc.)
  - `parameters.R`: Model parameters and configuration.
  - `cluster_model_run.R`: Script used to run simulations on an HPC cluster.
  - `run_model.R`: Script to run the model locally.
  - `run_model.sh`: SLURM batch script for cluster submission.
- `R/`: R scripts to reproduce the figures from model output.
  - `generate_figures.R`: Main script to produce all manuscript and appendix figures.
  - `process_sim.R`, `make_plots_boxplot.R`, `make_plots_timeSeries.R`: Helper functions.
  - `figures/`: Final manuscript figures (Figures 2--7, S1--S4).
- `data-raw/`: Raw model output (see Data Availability below).

## Data Availability

The raw simulation output (~15 GB) is too large for GitHub and is archived on Zenodo:

> [Zenodo DOI link -- to be added upon upload]

To reproduce the figures, download the data from Zenodo and place the contents into the `data-raw/` directory, maintaining the subfolder structure (e.g., `data-raw/96/`, `data-raw/101/`, etc.).

## Reproducing the Figures

1. Clone this repository.
2. Download the simulation data from Zenodo into `data-raw/`.
3. Install required R packages:
   ```r
   install.packages(c("ggplot2", "patchwork", "ggdist", "dplyr", "gtools", "viridis",
                       "gridExtra", "grid", "knitr"))
   ```
4. From the repository root, run:
   ```r
   source("R/generate_figures.R")
   ```

## Reproducing the Simulations

1. The exact parameter settings used for each simulation are included in the raw data (`*_static_parameters.csv` and `*_varaying_parameters.csv` files in each `data-raw/` subfolder).
2. Adjust `Model/parameters.R` accordingly.
3. Run the model locally with `Model/run_model.R`, or on a SLURM cluster with:
   ```bash
   sbatch Model/run_model.sh
   ```

## License

This project is licensed under the GNU General Public License v3.0 -- see [LICENSE.md](LICENSE.md).

## Related Publications

- Gelber, S., Blowes, S. A., Chase, J. M., Huth, A., Schurr, F. M., Tietjen, B., Zeller, J. W., & May, F. (2025). Geometric and demographic effects explain contrasting fragmentation-biodiversity relationships across scales. *Oikos*, 2025(7), e10778. https://doi.org/10.1111/oik.10778

## Contact

Stav Gelber -- stav.gelber@fu-berlin.de
