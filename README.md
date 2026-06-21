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
  - `generate_figures.R`: Top-level driver. Run this to produce every manuscript and appendix figure.
  - `figure1_landscape.R`: Builds Figure 1 (conceptual landscapes) directly from the landscape generator.
  - `figures_main.R`: Builds the per-kernel figure sets (Figures 2--7, S1--S6) from the raw simulation output.
  - `figures_kernel_comparison.R`: Builds the log-normal vs exponential kernel comparison (Figures S7--S10).
  - `figures/`: Final manuscript figures (Figures 1--7 and S1--S10).
- `data-raw/`: Raw model output (see Data Availability below and `data-raw/README.md`).

## Dispersal kernels

The main-text results use a **log-normal** dispersal kernel. As a robustness
check (Figures S7--S10), every experiment was repeated with an **exponential**
kernel of equal mean and variance. The kernel is selected in `Model/parameters.R`
via `switch$kernel_type` (`0` = log-normal, `1` = exponential). Both kernels'
raw output is archived on Zenodo (see `data-raw/README.md` for the run map).

## Data Availability

The raw simulation output is too large for GitHub and is archived on Zenodo:

> [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18519816.svg)](https://doi.org/10.5281/zenodo.18519816)

To reproduce the figures, download the data from Zenodo and unpack it into the `data-raw/` directory, keeping the per-run subfolder structure (e.g., `data-raw/300/`, `data-raw/310/`, etc.). The run-number-to-figure map is documented in `data-raw/README.md`.

## Reproducing the Figures

1. Clone this repository.
2. Download the simulation data from Zenodo into `data-raw/`.
3. Install required R packages:
   ```r
   install.packages(c("data.table", "dplyr", "ggplot2", "patchwork",
                       "viridis", "raster", "checkmate"))
   ```
4. From the repository root, run:
   ```r
   source("R/generate_figures.R")
   ```

## Reproducing the Simulations

1. The exact parameter settings used for each simulation are included in the raw data (`*_static_parameters.csv` and `*_varying_parameters.csv` files in each `data-raw/` subfolder).
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
