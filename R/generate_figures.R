# Generate Figures for:
# Gelber, Tietjen & May (2026)
# Disturbance and landscape characteristics interactively drive
# dispersal strategies in continuous and fragmented metacommunities
#
# This is the top-level driver. From the repository root, run:
#   source("R/generate_figures.R")
#
# It (1) builds the per-kernel figure sets and the cross-kernel comparison from
# the raw simulation output in data-raw/ (download from Zenodo first; see
# data-raw/README.md), and (2) copies the manuscript figures into R/figures/
# with their final names (fig1.png .. fig7.png, fig_s1.png .. fig_s10.png).
#
# Input  : data-raw/<run>/   raw per-rep CSVs (cached to data-raw/_cache/)
# Output : R/figures/         final figures; R/figures/_build/ intermediates
#
# Required packages: data.table, dplyr, ggplot2, patchwork, viridis, raster,
#                    checkmate (raster/checkmate are needed for Figure 1).

stopifnot(file.exists("R/generate_figures.R"))  # must be run from repo root

# --- 1. Figure 1: conceptual landscape panels (synthetic, no raw data needed) -
source("R/figure1_landscape.R")          # writes R/figures/fig1.png directly

# --- 2. Per-kernel figure sets (Figs 2-7, S1-S6) for both dispersal kernels ----
#        also populates data-raw/_cache/ used by the comparison below.
source("R/figures_main.R")

# --- 3. Cross-kernel comparison (Figs S7-S10) ---------------------------------
source("R/figures_kernel_comparison.R")

# --- 4. Copy manuscript figures into R/figures/ with their final names --------
BUILD <- file.path("R", "figures", "_build")
LN    <- file.path(BUILD, "lognormal_kernel")    # log-normal = main-text kernel
KC    <- file.path(BUILD, "kernel_comparison")
FIG   <- file.path("R", "figures")

fig_map <- c(
  # main text (log-normal kernel)
  "fig2.png"   = file.path(LN, "fig2b_boxplots2.png"),
  "fig3.png"   = file.path(LN, "fig3_felix_p.png"),
  "fig4.png"   = file.path(LN, "fig4_felix_p.png"),
  "fig5.png"   = file.path(LN, "fig_5a_felix.png"),
  "fig6.png"   = file.path(LN, "fig_6a_felix.png"),
  "fig7.png"   = file.path(LN, "fig_7a_felix.png"),
  # supplement S1-S4 (other two-variable factorials, log-normal kernel)
  "fig_s1.png" = file.path(LN, "fig_S1_felix.png"),
  "fig_s2.png" = file.path(LN, "fig_S2_felix.png"),
  "fig_s3.png" = file.path(LN, "fig_S3_felix.png"),
  "fig_s4.png" = file.path(LN, "fig_S4_felix.png"),
  # supplement S5 (sensitivity), S6 (disturbance spread x frequency)
  "fig_s5.png" = file.path(LN, "appendix_sensitivity_lines.png"),
  "fig_s6.png" = file.path(LN, "appendix_disturbance_freq_heatmap.png"),
  # supplement S7-S10 (log-normal vs exponential kernel comparison)
  "fig_s7.png"  = file.path(KC, "fig2_kernel_compare.png"),
  "fig_s8.png"  = file.path(KC, "fig3_kernel_compare.png"),
  "fig_s9.png"  = file.path(KC, "fig4_kernel_compare.png"),
  "fig_s10.png" = file.path(KC, "fig567a_kernel_compare.png")
)

for (final_name in names(fig_map)) {
  src <- fig_map[[final_name]]
  if (!file.exists(src)) {
    warning("missing build output: ", src)
    next
  }
  file.copy(src, file.path(FIG, final_name), overwrite = TRUE)
}

cat("\nDone. Manuscript figures written to", FIG, "\n")
