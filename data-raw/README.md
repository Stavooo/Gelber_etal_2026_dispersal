# Model Output Data

This directory contains raw data from model simulations used to produce the figures in the paper.

## Contents

Each dataset includes:
- Model output files (CSV format)
- Parameter settings used to generate the data
- Metadata describing the simulation conditions

## File Naming Convention

Files follow the pattern:
```
[experiment_name]_[experiment_number]_rep_[replicate]_output_[type].csv
```

Where:
- `experiment_name`: Descriptive name of the experiment
- `experiment_number`: Numerical identifier for the experiment
- `replicate`: Replicate number
- `type`: One of `general`, `sample`, or `dispersal`

## Output Types

1. **`*_output_general.csv`**: Landscape-level metrics across all time steps
   - Total abundance and species richness
   - Shannon diversity index
   - Patch-level statistics
   - Sample-level statistics

2. **`*_output_sample.csv`**: Community composition at sample locations
   - Species presence/absence data
   - Spatial coordinates
   - Patch information

3. **`*_output_dispersal.csv`**: Dispersal strategy-specific metrics
   - Abundance by dispersal distance
   - Richness by dispersal distance
   - Temporal dynamics

4. **`*_static_parameters.csv`**: Fixed model parameters for the simulation

5. **`*_varying_parameters.csv`**: Parameter combinations varied across simulations

## Reproducing the Data

To reproduce the data files, run the model using:

```r
# From repository root
source("Model/run_model.R")
```

Or for large-scale cluster runs:
```bash
sbatch Model/run_model.sh
```

The output files will be generated in `Model/Outputs/` and can then be moved here for analysis.

## Data Availability

Due to file size limitations, raw simulation data may not be included in this repository. Contact the authors for access to complete datasets:

- Stav Gelber: stav.gelber@fu-berlin.de

## Analysis Scripts

R scripts to analyze these data and generate manuscript figures will be added to the `R/` directory.
