# Disturbance and landscape characteristics interactively drive dispersal strategies in continuous and fragmented metacommunities

<!-- badges: start -->
<!-- badges: end -->

*This repository contains the code and data to reproduce the analyses and figures in the paper:*

**Disturbance and landscape characteristics interactively drive dispersal strategies in continuous and fragmented metacommunities**

## Authors

Stav Gelber, Britta Tietjen, Felix May

## Abstract

<!-- Add paper abstract here when available -->

This spatially explicit, individual-based metacommunity model investigates how disturbance regimes and landscape characteristics (habitat amount, spatial autocorrelation, and fragmentation) interact to shape dispersal strategies across species in metacommunities. The model demonstrates that optimal dispersal distances vary with environmental conditions, providing insights into how species with different dispersal abilities respond to habitat fragmentation and disturbance.

## Repository Structure

-   `Model/`: Source code to run the model simulations
    -   `src/`: Core model functions
        -   `GeDo_run.R`: Main model workflow separating geometric and demographic fragmentation effects
        -   `parameters.R`: Model parameters and configuration switches
        -   `initialize.R`, `generate_grid.R`, `generate_agents.R`, `distribute_agents.R`: Initialization functions
        -   `birth.R`, `death.R`, `immigration.R`, `disperse.R`: Core ecological processes
        -   `cookie_cutting.R`: Landscape fragmentation function
        -   `landscape.R`: Landscape generation utilities
        -   `initialize_result_files.R`: Output file initialization
    -   `run_model.R`: Script to run the model locally (single run, sequential, or parallel)
    -   `cluster_model_run.R`: Script to run the model on a computing cluster
    -   `run_model.sh`: SLURM batch script for cluster submission
    -   `Outputs/`: Directory for model output files
    
-   `R/`: R scripts to reproduce the figures in the paper
    -   `figures/`: Final manuscript figures (Figure 2-7)

-   `data-raw/`: Raw data from model simulations used to produce the figures

## Model Description

The model simulates metacommunity dynamics on spatially structured landscapes:

### Key Features

- **Spatially explicit individual-based model** tracking individual organisms across a gridded landscape
- **Species-specific dispersal strategies** with varying dispersal distances
- **Habitat heterogeneity** generated using neutral landscape models (fractal Brownian motion)
- **Fragmentation manipulation** using a "cookie-cutter" approach to separate geometric vs. demographic effects
- **Disturbance dynamics** affecting population persistence
- **Immigration** from external species pools
- **Niche-based establishment** where individual fitness depends on environmental match

### Model Processes (per time step)

1. **Birth**: Individuals reproduce based on niche fit and local density
2. **Dispersal**: Offspring disperse according to species-specific dispersal kernels
3. **Death**: Mortality influenced by niche mismatch and edge effects
4. **Immigration**: Species can immigrate from outside the landscape

### Key Parameters

- **Landscape characteristics**:
  - `hab`: Habitat amount (proportion of suitable habitat)
  - `ac`: Spatial autocorrelation (landscape smoothness)
  - `frag`: Fragmentation level (landscape configuration)
  
- **Species traits**:
  - Species-specific dispersal distances (continuous distribution)
  - Niche breadth (environmental tolerance)
  
- **Demographic parameters**:
  - Birth and death rates
  - Carrying capacity
  - Immigration rate

## Installation

### Prerequisites

The model requires R (>= 4.0.0) and the following packages:

```r
# Install required packages
install.packages(c(
  "raster",
  "data.table",
  "tibble",
  "foreach",
  "doParallel",
  "purrr",
  "vegan",
  "dplyr"
))
```

### Clone the Repository

```bash
git clone https://github.com/YOUR_USERNAME/Gelber_etal_2025_dispersal.git
cd Gelber_etal_2025_dispersal
```

## Usage

### Running the Model Locally

The `Model/run_model.R` script provides three examples:

#### 1. Single Model Run

```r
# Set working directory to repository root
setwd("path/to/Gelber_etal_2025_dispersal")

# Source the run_model script
source("Model/run_model.R")

# The script will run a single simulation and save outputs to Model/Outputs/
```

#### 2. Multiple Runs (Sequential)

Modify the `run_model.R` script to run multiple parameter combinations sequentially. Useful for testing and small-scale analyses.

#### 3. Parallel Execution

The script includes parallel processing using the `foreach` and `doParallel` packages to speed up multiple simulations.

### Running on a Computing Cluster

For large-scale simulations:

1. Adjust parameters in `Model/parameters.R`
2. Modify `Model/run_model.sh` with appropriate cluster specifications
3. Submit the job:

```bash
sbatch Model/run_model.sh
```

### Customizing Parameters

Edit `Model/parameters.R` to adjust:

- **Static parameters** (`mod_par`): Grid size, number of species, demographic rates
- **Varying parameters** (`var_par`): Combinations of habitat amount, fragmentation, autocorrelation
- **Switches** (`switch`): Enable/disable model features (immigration, disturbance, etc.)

Example parameter modification:

```r
# In parameters.R, adjust the varying parameter vectors:

# Habitat amount
habitat_percent_vector <- seq(0.2, 0.8, 0.2)

# Fragmentation
frag_factor_vector <- seq(0.1, 0.9, 0.2)

# Spatial autocorrelation
spatial_ac_vector <- seq(0.1, 0.9, 0.2)

# Create all combinations
var_par <- expand.grid(
  frag = frag_factor_vector,
  ac = spatial_ac_vector,
  hab = habitat_percent_vector,
  # ... other parameters
)
```

## Model Output

The model generates three main output files:

1. **`output_general.csv`**: Landscape-level metrics at each time step
   - Total abundance and species richness
   - Shannon diversity
   - Patch-level and sample-level averages

2. **`output_sample.csv`**: Local community composition at sample locations
   - Species presence/absence at specific grid cells
   - Recorded at beginning and end of post-fragmentation phase

3. **`output_dispersal.csv`**: Dispersal strategy-specific metrics
   - Abundance and richness grouped by dispersal distance
   - Tracked throughout the simulation

## Reproducing Paper Figures

<!-- Add instructions for reproducing figures when analysis scripts are ready -->

The figures in the paper (Figures 2-7) are located in `R/figures/`. Scripts to generate these figures from model outputs will be added to the `R/` directory.

## Citation

If you use this code or model in your research, please cite:

```
Gelber, S., Tietjen, B., & May, F. (2025). Disturbance and landscape characteristics 
interactively drive dispersal strategies in continuous and fragmented metacommunities. 
[Journal Name]. [DOI]
```

BibTeX:
```bibtex
@article{gelber2025dispersal,
  title={Disturbance and landscape characteristics interactively drive dispersal strategies in continuous and fragmented metacommunities},
  author={Gelber, Stav and Tietjen, Britta and May, Felix},
  journal={[Journal Name]},
  year={2025},
  doi={[DOI]}
}
```

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE.md](LICENSE.md) file for details.

## Contact

For questions or issues, please contact:

- Stav Gelber: stav.gelber@fu-berlin.de

## Acknowledgments

<!-- Add acknowledgments from the paper when available -->

This research was supported by [funding sources]. We thank [collaborators/reviewers] for valuable feedback on this work.

## Related Publications

- Gelber, S., Blowes, S.A., Chase, J.M., Huth, A., Schurr, F.M., Tietjen, B., Zeller, J.W., & May, F. (2023). Geometric and demographic effects explain contrasting fragmentation-biodiversity relationships across scales. [Previous related paper]
