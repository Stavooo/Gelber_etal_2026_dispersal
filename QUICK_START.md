# Quick Start Guide

This guide will help you get started with the repository quickly.

## 🚀 Getting Started in 5 Minutes

### 1. Open the Project
Double-click `Gelber_etal_2025_dispersal.Rproj` to open in RStudio.

### 2. Install Required Packages
```r
# Run this in R console
install.packages(c(
  "raster", "data.table", "tibble", "foreach", 
  "doParallel", "purrr", "vegan", "dplyr"
))
```

### 3. Run a Test Simulation
```r
# Source the run script
source("Model/run_model.R")
```

This will:
- Load all model functions
- Run a single test simulation
- Save outputs to `Model/Outputs/`

## 📊 Expected Outputs

After running, you should see three CSV files in `Model/Outputs/`:
- `test_run_rep_1_output_general.csv` - Landscape-level metrics
- `test_run_rep_1_output_sample.csv` - Local community data
- `test_run_rep_1_output_dispersal.csv` - Dispersal strategy metrics

## 🔧 Customizing Parameters

Edit `Model/parameters.R` to change:

```r
# Example: Change habitat amount
habitat_percent_vector <- seq(0.2, 0.8, 0.2)

# Example: Change fragmentation levels
frag_factor_vector <- seq(0.1, 0.9, 0.2)

# Example: Number of species
mod_par$n_species <- 1000
```

## 🎯 Common Tasks

### Run Multiple Simulations
In `Model/run_model.R`, modify:
```r
n_sims <- 10  # Number of parameter combinations to run
```

### Enable Species-Specific Dispersal
In `Model/parameters.R`:
```r
switch$species_specific_disp <- 1
```

### Export Animations
```r
switch$animation_export <- 1
```

### Run on Cluster
```bash
# Edit Model/run_model.sh with your cluster settings
sbatch Model/run_model.sh
```

## 📁 File Locations

- **Model code**: `Model/src/`
- **Parameters**: `Model/parameters.R`
- **Outputs**: `Model/Outputs/`
- **Figures**: `R/figures/`
- **Documentation**: `README.md`

## ⚠️ Troubleshooting

### "Package not found" error
```r
install.packages("package_name")
```

### "File not found" error
Make sure your working directory is the repository root:
```r
setwd("C:/Stav_FU/Modelling/R/Gelber_etal_2025_dispersal")
```

### Model runs slowly
- Reduce `mod_par$n_species` (e.g., 100 instead of 1000)
- Reduce `mod_par$grid_size` (e.g., 50 instead of 200)
- Reduce `mod_par$steps_pre_frag` and `mod_par$steps_post_frag`

## 📚 Learn More

- Full documentation: `README.md`
- Parameter details: `Model/parameters.R`
- Setup summary: `SETUP_SUMMARY.md`

## 🆘 Getting Help

If you encounter issues:
1. Check the main `README.md`
2. Review parameter settings in `Model/parameters.R`
3. Contact: stav.gelber@fu-berlin.de

## 🎓 Citation

If you use this code, please cite:
```
Gelber, S., Tietjen, B., & May, F. (2025). Disturbance and landscape 
characteristics interactively drive dispersal strategies in continuous 
and fragmented metacommunities.
```

---

**Good luck with your simulations!** 🌱
