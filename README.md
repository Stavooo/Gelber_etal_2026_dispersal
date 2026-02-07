# Disturbance and landscape characteristics interactively drive dispersal strategies in continuous and fragmented metacommunities

*This repository contains the code and data to reproduce the analyses and figures in the paper:*

**Gelber, S., Tietjen, B., & May, F. (2026). Disturbance and landscape characteristics interactively drive dispersal strategies in continuous and fragmented metacommunities.**

## Authors

- Stav Gelber, Britta Tietjen, Felix May

## Abstract

Habitat fragmentation, driven by human activities, disrupts habitat connectivity and alters ecological processes through geometric and demographic fragmentation effects. Dispersal plays a fundamental role in shaping the distribution, abundance, and persistence of species in modified landscapes. While previous research looked at the evolution of dispersal strategies at the species level, community-level dynamics remain underexplored. Species exhibit diverse dispersal strategies to persist in modified landscapes, yet predicting how these strategies interact at the community level requires a more integrated approach. This study employed an individual-based simulation model to explore how fragmentation and other landscape characteristics influence community-level dispersal strategies. We tested the effects of varying fragmentation levels, environmental autocorrelation, habitat amount, and disturbance levels on the emerging distribution of dispersal distances within a community in modified and continuous landscapes. We hypothesised that fragmentation and other spatial patterns would significantly shape community composition, favouring particular dispersal strategies under specific environmental conditions. The findings reveal that higher disturbance levels and greater habitat amount increased the community-weighted mean of dispersal distance, while fragmentation showed only minor variation. Additionally, low autocorrelation was associated with the highest community-weighted mean of dispersal distance. These results highlight the importance of considering community-level dynamics when predicting ecosystem responses to landscape modification. By clarifying how landscape structure and disturbance shape community-level dispersal strategies, this study advances our understanding of the mechanisms underlying species persistence and community structure in modified landscapes.

## Repository Structure

- `Model/`: Source code of the individual-based simulation model
  - `src/`: Core model functions (birth, death, dispersal, disturbance, etc.)
  - `parameters.R`: Model parameters and configuration
  - `cluster_model_run.R`: Script used to run simulations on HPC cluster
  - `run_model.R`: Script to run the model locally
  - `run_model.sh`: SLURM batch script for cluster submission
- `R/`: R scripts to reproduce the figures from model output
  - `generate_figures.R`: Main script to produce all manuscript and appendix figures
  - `process_sim.R`, `make_plots_boxplot.R`, `make_plots_timeSeries.R`: Helper functions
  - `figures/`: Final manuscript figures (Figures 2--7, S1--S4)
- `data-raw/`: Raw model output (see Data Availability below)

## Data Availability

The raw simulation output (~15 GB) is too large for GitHub and is archived on Zenodo:

> [Zenodo DOI link  to be added upon upload]

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

1. Adjust parameters in `Model/parameters.R`.
2. Run locally with `Model/run_model.R`, or on a SLURM cluster with:
   ```bash
   sbatch Model/run_model.sh
   ```

## License

This project is licensed under the GNU General Public License v3.0  see [LICENSE.md](LICENSE.md).

## Acknowledgments

SG acknowledges funding from the German Research Foundation (DFG) under grant number MA 5962/1-1. We thank Selina Baldauf for her careful review of and constructive comments on the simulation model code. We also acknowledge the HPC Service of FUB-IT, Freie Universitat Berlin, for providing computing time.

## Related Publications

- Gelber, S., Blowes, S.A., Chase, J.M., Huth, A., Schurr, F.M., Tietjen, B., Zeller, J.W., & May, F. (2023). Geometric and demographic effects explain contrasting fragmentation-biodiversity relationships across scales.

## Contact

Stav Gelber  stav.gelber@fu-berlin.de
