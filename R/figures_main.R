#!/usr/bin/env Rscript
################################################################################
# figures_main.R
#
# Builds the manuscript figures (Figs 2-7) + supplementaries S1-S6 for BOTH
# dispersal kernels, writing each kernel's complete set to its own folder under
# R/figures/_build/:
#
#   _build/exponential_kernel/   <- 200-block (exponential)
#   _build/lognormal_kernel/     <- 300-block (log-normal, main text)
#
# Each folder gets identical filenames so the two kernels are directly
# comparable side by side. Figs 5-7 carry continuous-landscape reference lines
# drawn from that kernel's own Fig-3 experiment (210 exp / 310 log).
# generate_figures.R copies the log-normal set into R/figures/ with final names.
#
# Reads the raw per-run output from data-raw/<run>/ (download from Zenodo) and
# caches a per-run summary to data-raw/_cache/summary_<run>.rds on first use.
#
# Experiment -> figure mapping:
#                         EXPONENTIAL   LOG-NORMAL
#   Fig2 row1 vary ac           200          300     continuous dist OFF
#   Fig2 row2 vary disturbance  201          301     continuous dist ON
#   Fig2 row3 vary dist_freq    204          304     continuous dist ON
#   Fig2 row4 vary frag         202          302     modified   dist OFF
#   Fig2 row5 vary hab          203          303     modified   dist OFF
#   Fig3      ac x disturbance  210          310     continuous dist ON  (+ref)
#   Fig4      frag x hab        220          320     modified   dist OFF
#   Figs5-7   4D                230          231     modified   dist ON
#   Sup       sensitivity OAT   250          350     modified   dist OFF
#   Sup       disturbance freq  260          360     continuous dist ON
#   Sup S1/S3 hab x ac x dist    -           370     modified   dist ON (frag=0.7)
#   Sup S2/S4 frag x ac x dist   -           371     modified   dist ON (hab=0.2)
#
#   S1 = hab x disturbance (ac=0.5 slice);  S3 = hab x ac (disturbance=0 slice)  [exp 370]
#   S2 = frag x disturbance (ac=0.5 slice); S4 = frag x ac (disturbance=0 slice) [exp 371]
#   S1-S4 exist for the log-normal kernel only (no exponential 270/271 runs).
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

# Raw per-run simulation output (downloaded from Zenodo into data-raw/<run>/) and
# the cached per-run summaries are read from REV; per-kernel figure sets are
# written under OUT_ROOT. generate_figures.R copies the manuscript figures from
# here to R/figures/ with their final names.
REV <- Sys.getenv("FRAG_REV", unset = "data-raw")
OUT_ROOT <- file.path("R", "figures", "_build")
dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)

# Okabe-Ito palette (identical to plot_results3.r)
okabe_ito_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

near <- function(x, v, tol = 1e-8) abs(x - v) < tol

################################################################################
# Data processing -- faithful to R/process_sim.R, robust to file size & spelling
#   cwm          = weighted.mean(species_dispersal_distance, individuals)
#   sd_dispersal = sd(species_dispersal_distance)            [UNWEIGHTED, as original]
#   present_species, habitat, fragmentation  from the general output
#   ac/frag/hab/disturbance/...               from varying_parameters
# Returns one row per (sim_id, repetition) at the final recorded step (1000).
################################################################################
process_sim_rev <- function(exp_num) {
  path <- file.path(REV, exp_num)
  disp_files <- list.files(path, pattern = "_rep_[0-9]+_output_dispersal\\.csv$", full.names = TRUE)
  gen_files  <- list.files(path, pattern = "_rep_[0-9]+_output_general\\.csv$",   full.names = TRUE)
  vp_file    <- list.files(path, pattern = "_varying_parameters\\.csv$",          full.names = TRUE)[1]
  sp_file    <- list.files(path, pattern = "_static_parameters\\.csv$",           full.names = TRUE)[1]

  static <- read.csv(sp_file)                       # columns: X (names), V1 (values)
  spf  <- as.numeric(static$V1[static$X == "steps_pre_frag"])
  sptf <- as.numeric(static$V1[static$X == "steps_post_frag"])
  final_step <- spf + sptf - 1                      # = steps[2] - 1 in the original

  vp <- as.data.frame(fread(vp_file))
  names(vp)[names(vp) == "sim_ID"] <- "sim_id"

  rep_of <- function(f) as.integer(sub(".*_rep_([0-9]+)_.*", "\\1", basename(f)))

  cwm <- rbindlist(lapply(disp_files, function(f) {
    d <- fread(f, select = c("sim_id", "step", "species_dispersal_distance", "individuals"))
    d <- d[step == final_step]
    if (nrow(d) == 0) return(NULL)
    s <- d[, .(cwm          = weighted.mean(species_dispersal_distance, individuals),
               sd_dispersal = sd(species_dispersal_distance)),
           by = .(step, sim_id)]
    s[, repetition := rep_of(f)][]
  }))

  gen <- rbindlist(lapply(gen_files, function(f) {
    g <- fread(f, select = c("sim_id", "step", "present_species", "habitat", "fragmentation"))
    g <- g[step == final_step]
    if (nrow(g) == 0) return(NULL)
    g[, repetition := rep_of(f)][]
  }))

  out <- merge(as.data.frame(gen), as.data.frame(cwm),
               by = c("step", "sim_id", "repetition"), all.x = TRUE)   # general drives (keeps extinct combos)
  out <- merge(out, vp, by = "sim_id", all.x = TRUE)
  cat(sprintf("  [process] exp %s: final_step=%d, %d sim_ids x reps, %d extinct rows (cwm NA)\n",
              exp_num, final_step, nrow(out), sum(is.na(out$cwm))))
  out
}

# Cache processed summaries so re-runs don't re-read ~10 GB of dispersal files.
CACHE <- file.path(REV, "_cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
process_cached <- function(exp_num) {
  cf <- file.path(CACHE, paste0("summary_", exp_num, ".rds"))
  if (file.exists(cf)) { cat(sprintf("  [cache] exp %s\n", exp_num)); return(readRDS(cf)) }
  res <- process_sim_rev(exp_num)
  saveRDS(res, cf)
  res
}

################################################################################
# FIGURE 2 row helper -- gray70 boxplots, outliers hidden, title on middle panel
################################################################################
fig2_row <- function(df, xvar, xlab, rowtitle) {
  df <- df[!is.na(df[[xvar]]), ]
  df[[xvar]] <- factor(df[[xvar]], levels = sort(unique(df[[xvar]])))
  base_theme <- theme_bw() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  p_cwm <- ggplot(df, aes(x = .data[[xvar]], y = cwm)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray70") +
    labs(title = NULL, x = xlab, y = "CWMDD") + base_theme
  p_rich <- ggplot(df, aes(x = .data[[xvar]], y = present_species)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray70") +
    labs(title = rowtitle, x = xlab, y = "Richness") + base_theme
  p_sd <- ggplot(df, aes(x = .data[[xvar]], y = sd_dispersal)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray70") +
    labs(title = NULL, x = xlab, y = "SDDD") + base_theme
  p_cwm + p_rich + p_sd + plot_layout(nrow = 1)
}

################################################################################
# FIGURES 5-7 builder -- faithful to plot_results3.r fig_5a..fig_7b.
# draw_ref toggles the dashed continuous-landscape reference lines.
################################################################################
make_fig567 <- function(data_101, ref2, title_suffix = "", draw_ref = TRUE) {
  hl <- function(facetvar, refs) {
    if (!draw_ref) return(NULL)
    dd <- data.frame(v = c(0.1, 0.5, 0.9), ref = refs, linetype = "Continuous landscape")
    names(dd)[1] <- facetvar
    geom_hline(data = dd, aes(yintercept = ref, linetype = linetype, group = .data[[facetvar]]),
               color = "black", show.legend = TRUE)
  }
  base_theme <- theme_bw() +
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5, size = 15),
          panel.grid.major = element_line(color = "gray90"), panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 13),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          legend.title = element_text(size = 13))
  lt_scale <- if (draw_ref) scale_linetype_manual(values = c("Continuous landscape" = "dashed")) else NULL
  ac_lab  <- labeller(ac  = c("0.1" = "autocorrelation 0.1", "0.5" = "autocorrelation 0.5", "0.9" = "autocorrelation 0.9"))
  hab_lab <- labeller(habitat = c("0.1" = "habitat 0.1", "0.5" = "habitat 0.5", "0.9" = "habitat 0.9"))
  yexp <- scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

  # 5a CWMDD, habitat on x, facet ac
  f5a <- ggplot(data_101, aes(x = as.factor(habitat), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() + hl("ac", c(ref2$cwm1, ref2$cwm2, ref2$cwm3)) +
    labs(title = paste0("Modified Landscape: Community Weighted Mean", title_suffix),
         x = "Habitat Amount", y = "CWMDD", fill = "Disturbance", linetype = NULL) +
    scale_fill_manual(values = okabe_ito_palette) + lt_scale + base_theme +
    facet_grid(cols = vars(ac), labeller = ac_lab) + yexp
  # 5b CWMDD, ac on x, facet habitat
  f5b <- ggplot(data_101, aes(x = as.factor(ac), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() + hl("habitat", c(ref2$cwm1, ref2$cwm2, ref2$cwm3)) +
    labs(title = paste0("Modified Landscape: Community Weighted Mean", title_suffix),
         x = "Autocorrelation", y = "Community Weighted Mean (CWM)", fill = "Disturbance", linetype = NULL) +
    scale_fill_manual(values = okabe_ito_palette) + lt_scale + base_theme +
    facet_grid(cols = vars(habitat), labeller = hab_lab) + yexp
  # 6a Richness, habitat on x, facet ac
  f6a <- ggplot(data_101, aes(x = as.factor(habitat), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() + hl("ac", c(ref2$richness1, ref2$richness2, ref2$richness3)) +
    labs(title = paste0("Modified Landscape: Species Richness", title_suffix),
         x = "Habitat Amount", y = "Species Richness", fill = "Disturbance", linetype = NULL) +
    scale_fill_manual(values = okabe_ito_palette) + lt_scale + base_theme +
    facet_grid(cols = vars(ac), labeller = ac_lab) + yexp
  # 6b Richness, ac on x, facet habitat
  f6b <- ggplot(data_101, aes(x = as.factor(ac), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() + hl("habitat", c(ref2$richness1, ref2$richness2, ref2$richness3)) +
    labs(title = paste0("Modified Landscape: Species Richness", title_suffix),
         x = "Autocorrelation", y = "Species Richness", fill = "Disturbance", linetype = NULL) +
    scale_fill_manual(values = okabe_ito_palette) + lt_scale + base_theme +
    facet_grid(cols = vars(habitat), labeller = hab_lab) + yexp
  # 7a SDDD, habitat on x, facet ac
  f7a <- ggplot(data_101, aes(x = as.factor(habitat), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() + hl("ac", c(ref2$sd1, ref2$sd2, ref2$sd3)) +
    labs(title = paste0("Modified Landscape: Standard Deviation of Dispersal Distance", title_suffix),
         x = "Habitat Amount", y = "SDDD", fill = "Disturbance", linetype = NULL) +
    scale_fill_manual(values = okabe_ito_palette) + lt_scale + base_theme +
    facet_grid(cols = vars(ac), labeller = ac_lab) + yexp
  # 7b SDDD, ac on x, facet habitat
  f7b <- ggplot(data_101, aes(x = as.factor(ac), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() + hl("habitat", c(ref2$sd1, ref2$sd2, ref2$sd3)) +
    labs(title = paste0("Modified Landscape: Standard Deviation of Dispersal Distance", title_suffix),
         x = "Autocorrelation", y = "SDDD", fill = "Disturbance", linetype = NULL) +
    scale_fill_manual(values = okabe_ito_palette) + lt_scale + base_theme +
    facet_grid(cols = vars(habitat), labeller = hab_lab) + yexp
  list(f5a = f5a, f5b = f5b, f6a = f6a, f6b = f6b, f7a = f7a, f7b = f7b)
}

################################################################################
# SUPPLEMENTARY S1-S4 builder -- two-variable factorial boxplot grids.
# Recreates the published supplement (originally square-kernel runs 104/105)
# with the new kernel + FFT landscape. Same 3-panel layout as Fig 4
# (CWMDD / Richness / SDDD), grouped boxplots coloured by a second factor,
# Okabe-Ito fill, a single dashed continuous-landscape reference (ref_lev,
# from this kernel's Fig-3 continuous run at ac = 0.5, no disturbance).
#   df       : processed rows already SLICED to the relevant subset
#   xvar     : x-axis factor (varying-parameter column, e.g. "hab" or "frag")
#   fillvar  : grouping factor (e.g. "disturbance" or "ac")
################################################################################
make_supp <- function(df, xvar, xlab, fillvar, filllab, ref_lev) {
  df <- df[!is.na(df[[xvar]]) & !is.na(df[[fillvar]]), ]
  df[[xvar]]    <- factor(df[[xvar]],    levels = sort(unique(df[[xvar]])))
  df[[fillvar]] <- factor(df[[fillvar]], levels = sort(unique(df[[fillvar]])))
  panel_theme <- theme_bw() +
    theme(plot.title = element_blank(),
          panel.grid.major = element_line(color = "gray90"), panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 13), legend.position = "none")
  p_cwm <- ggplot(df, aes(x = .data[[xvar]], y = cwm, fill = .data[[fillvar]])) +
    geom_boxplot(outlier.size = 0.6) + geom_hline(yintercept = ref_lev$cwm, linetype = "dashed") +
    labs(x = NULL, y = "CWMDD", fill = filllab) +
    scale_fill_manual(values = okabe_ito_palette) + panel_theme +
    theme(axis.title.x = element_blank())
  p_rich <- ggplot(df, aes(x = .data[[xvar]], y = present_species, fill = .data[[fillvar]])) +
    geom_boxplot(outlier.size = 0.6) + geom_hline(yintercept = ref_lev$richness, linetype = "dashed") +
    labs(x = xlab, y = "Richness", fill = filllab) +
    scale_fill_manual(values = okabe_ito_palette) + panel_theme
  p_sd <- ggplot(df, aes(x = .data[[xvar]], y = sd_dispersal, fill = .data[[fillvar]])) +
    geom_boxplot(outlier.size = 0.6) + geom_hline(yintercept = ref_lev$sd, linetype = "dashed") +
    labs(x = NULL, y = "SDDD", fill = filllab) +
    scale_fill_manual(values = okabe_ito_palette) + panel_theme +
    theme(axis.title.x = element_blank())
  p_cwm + p_rich + p_sd +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(tag_levels = 'a', tag_suffix = ")") &
    theme(legend.position = "top",
          legend.text = element_text(size = 13), legend.title = element_text(size = 14),
          plot.tag = element_text(size = 14, face = "bold"), plot.tag.position = c(0.02, 0.88),
          plot.margin = margin(t = 5, r = 5, b = 10, l = 5))
}

################################################################################
# PER-KERNEL BUILDER -- writes the full Fig 2-7 + 3 supplementary set to OUT.
# `e` is a named list of exp_nums for this kernel.
################################################################################
build_kernel_figures <- function(kernel_name, e) {
  OUT <- file.path(OUT_ROOT, kernel_name)
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("\n========== building %s -> %s ==========\n", kernel_name, OUT))

  d_ac   <- process_cached(e$ac)
  d_dist <- process_cached(e$dist)
  d_freq <- process_cached(e$freq)
  d_frag <- process_cached(e$frag)
  d_hab  <- process_cached(e$hab)
  d_f3   <- process_cached(e$f3)
  d_f4   <- process_cached(e$f4)
  d_567  <- process_cached(e$f567)
  d_sens <- process_cached(e$sens)
  d_df   <- process_cached(e$df)

  ## ---- FIGURE 2: 5-row boxplot grid ----------------------------------------
  r1 <- fig2_row(d_ac,   "ac",               "Landscape Autocorrelation", "Varying autocorrelation in continuous landscape")
  r2 <- fig2_row(d_dist, "disturbance",      "Disturbance Spread Rate",   "Varying disturbance spread rate in continuous landscape")
  r3 <- fig2_row(d_freq, "disturbance_freq", "Disturbance Frequency",     "Varying disturbance frequency in continuous landscape")
  r4 <- fig2_row(d_frag, "frag",             "Fragmentation Per Se",      "Varying fragmentation in modified landscape")
  r5 <- fig2_row(d_hab,  "hab",              "Habitat Amount",            "Varying habitat amount in modified landscape")
  fig2 <- r1 / r2 / r3 / r4 / r5 +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = list(paste0(letters[1:15], ")")))
  fig2 <- fig2 & theme(
    plot.tag = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0.02, 0.88),
    plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
  )
  ggsave(file.path(OUT, "fig2b_boxplots2.png"), plot = fig2, width = 14, height = 22.5, bg = "white")
  cat("[wrote] fig2b_boxplots2.png\n")

  ## ---- Reference levels from this kernel's continuous run (Fig 3 exp) -------
  ref_lev <- list(
    cwm      = mean(d_f3$cwm[near(d_f3$ac, 0.5) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    richness = mean(d_f3$present_species[near(d_f3$ac, 0.5) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    sd       = mean(d_f3$sd_dispersal[near(d_f3$ac, 0.5) & near(d_f3$disturbance, 0)], na.rm = TRUE)
  )
  ref_lev_2 <- list(
    cwm1 = mean(d_f3$cwm[near(d_f3$ac, 0.1) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    cwm2 = mean(d_f3$cwm[near(d_f3$ac, 0.5) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    cwm3 = mean(d_f3$cwm[near(d_f3$ac, 0.9) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    richness1 = mean(d_f3$present_species[near(d_f3$ac, 0.1) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    richness2 = mean(d_f3$present_species[near(d_f3$ac, 0.5) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    richness3 = mean(d_f3$present_species[near(d_f3$ac, 0.9) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    sd1 = mean(d_f3$sd_dispersal[near(d_f3$ac, 0.1) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    sd2 = mean(d_f3$sd_dispersal[near(d_f3$ac, 0.5) & near(d_f3$disturbance, 0)], na.rm = TRUE),
    sd3 = mean(d_f3$sd_dispersal[near(d_f3$ac, 0.9) & near(d_f3$disturbance, 0)], na.rm = TRUE)
  )

  ## ---- FIGURE 3 (Fig-3 exp): ac x disturbance, continuous. 3 panels --------
  data_87 <- d_f3
  left_panel <- ggplot(data_87, aes(x = as.factor(ac), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() +
    labs(x = NULL, y = "CWMDD", fill = "Disturbance") +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(plot.title = element_blank(), panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(), strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 14), axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16), axis.text = element_text(size = 13), legend.position = "none")
  middle_panel <- ggplot(data_87, aes(x = as.factor(ac), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() +
    labs(x = "Autocorrelation", y = "Richness", fill = "Disturbance") +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(plot.title = element_blank(), panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(), strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 14), axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16), axis.text = element_text(size = 13), legend.position = "none")
  right_panel <- ggplot(data_87, aes(x = as.factor(ac), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() +
    labs(x = NULL, y = "SDDD", fill = "Disturbance") +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(plot.title = element_blank(), panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(), strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 14), axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16), axis.text = element_text(size = 13), legend.position = "none")
  fig3 <- left_panel + middle_panel + right_panel +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(tag_levels = 'a', tag_prefix = "", tag_suffix = ")",
                    title = "Continuous landscape",
                    theme = theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))) &
    theme(legend.position = "top", legend.text = element_text(size = 13), legend.title = element_text(size = 14),
          plot.tag = element_text(size = 14, face = "bold"), plot.tag.position = c(0.02, 0.88),
          plot.margin = margin(t = 5, r = 5, b = 10, l = 5))
  ggsave(file.path(OUT, "fig3_felix_p.png"), plot = fig3, width = 15, height = 5, bg = "white")
  cat("[wrote] fig3_felix_p.png\n")

  ## ---- FIGURE 4 (Fig-4 exp): frag x hab, modified. dashed ref from Fig 3 ---
  data_88 <- d_f4
  left_panel <- ggplot(data_88, aes(x = as.factor(habitat), y = cwm, fill = as.factor(fragmentation))) +
    geom_boxplot() + geom_hline(yintercept = ref_lev$cwm, linetype = "dashed") +
    labs(x = NULL, y = "CWMDD", fill = "Fragmentation") +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(plot.title = element_blank(), panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(), strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 14), axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16), axis.text = element_text(size = 13), legend.position = "none")
  middle_panel <- ggplot(data_88, aes(x = as.factor(habitat), y = present_species, fill = as.factor(fragmentation))) +
    geom_boxplot() + geom_hline(yintercept = ref_lev$richness, linetype = "dashed") +
    labs(x = "Habitat Amount", y = "Richness", fill = "Fragmentation") +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(plot.title = element_blank(), panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(), strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 14), axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16), axis.text = element_text(size = 13), legend.position = "none")
  right_panel <- ggplot(data_88, aes(x = as.factor(habitat), y = sd_dispersal, fill = as.factor(fragmentation))) +
    geom_boxplot() +
    geom_hline(aes(yintercept = ref_lev$sd, linetype = "Continuous Landscape"), color = "black") +
    labs(x = NULL, y = "SDDD", fill = "Fragmentation", linetype = NULL) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous Landscape" = "dashed")) +
    theme_bw() +
    theme(plot.title = element_blank(), panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(), strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold", size = 14), axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16), axis.text = element_text(size = 13), legend.position = "none")
  fig4 <- left_panel + middle_panel + right_panel +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(tag_levels = 'a', tag_prefix = "", tag_suffix = ")",
                    title = "Modified landscape",
                    theme = theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))) &
    theme(legend.position = "top", legend.text = element_text(size = 13), legend.title = element_text(size = 14),
          plot.tag = element_text(size = 14, face = "bold"), plot.tag.position = c(0.02, 0.88),
          plot.margin = margin(t = 5, r = 5, b = 10, l = 5))
  ggsave(file.path(OUT, "fig4_felix_p.png"), plot = fig4, width = 15, height = 5, bg = "white")
  cat("[wrote] fig4_felix_p.png\n")

  ## ---- FIGURES 5-7 (Figs5-7 exp): with continuous ref lines from Fig 3 -----
  g <- make_fig567(d_567, ref_lev_2, title_suffix = "", draw_ref = TRUE)
  ggsave(file.path(OUT, "fig_5a_felix.png"), g$f5a, width = 10, height = 5, bg = "white")
  ggsave(file.path(OUT, "fig_5b_felix.png"), g$f5b, width = 10, height = 5, bg = "white")
  ggsave(file.path(OUT, "fig_6a_felix.png"), g$f6a, width = 10, height = 5, bg = "white")
  ggsave(file.path(OUT, "fig_6b_felix.png"), g$f6b, width = 10, height = 5, bg = "white")
  ggsave(file.path(OUT, "fig_7a_felix.png"), g$f7a, width = 10, height = 5, bg = "white")
  ggsave(file.path(OUT, "fig_7b_felix.png"), g$f7b, width = 10, height = 5, bg = "white")
  cat("[wrote] fig_5/6/7 a/b _felix.png\n")

  ## ---- SUP: SENSITIVITY ANALYSIS (sens exp, OAT birth/death/nb) ------------
  BASE <- list(birth_rate = 1.0, death_rate = 0.10, nb = 0.20)
  sens_agg <- d_sens %>%
    group_by(sim_id, birth_rate, death_rate, nb) %>%
    summarise(CWMDD = mean(cwm, na.rm = TRUE),
              Richness = mean(present_species, na.rm = TRUE),
              SDDD = mean(sd_dispersal, na.rm = TRUE),
              CWMDD_sd = sd(cwm, na.rm = TRUE),
              Richness_sd = sd(present_species, na.rm = TRUE),
              SDDD_sd = sd(sd_dispersal, na.rm = TRUE),
              .groups = "drop")
  base_row <- sens_agg %>%
    filter(near(birth_rate, BASE$birth_rate), near(death_rate, BASE$death_rate), near(nb, BASE$nb)) %>%
    slice(1)
  classify <- function(df, param, baseval) {
    others <- setdiff(c("birth_rate", "death_rate", "nb"), param)
    keep <- df %>% filter(near(.data[[others[1]]], BASE[[others[1]]]),
                          near(.data[[others[2]]], BASE[[others[2]]]))
    keep %>% mutate(parameter = param,
                    pct = (.data[[param]] - baseval) / baseval * 100,
                    paramval = .data[[param]])
  }
  sens_long_wide <- bind_rows(
    classify(sens_agg, "birth_rate", BASE$birth_rate),
    classify(sens_agg, "death_rate", BASE$death_rate),
    classify(sens_agg, "nb",         BASE$nb)
  ) %>% distinct(sim_id, parameter, .keep_all = TRUE)
  param_labels <- c(birth_rate = "Birth rate", death_rate = "Death rate", nb = "Niche breadth")
  param_colors <- c("Birth rate" = "#E69F00", "Death rate" = "#56B4E9", "Niche breadth" = "#009E73")
  to_metric_long <- function(df) {
    bind_rows(
      df %>% transmute(parameter, pct, paramval, metric = "CWMDD",    value = CWMDD,    sd = CWMDD_sd),
      df %>% transmute(parameter, pct, paramval, metric = "Richness", value = Richness, sd = Richness_sd),
      df %>% transmute(parameter, pct, paramval, metric = "SDDD",     value = SDDD,     sd = SDDD_sd)
    )
  }
  ml <- to_metric_long(sens_long_wide)
  ml$metric    <- factor(ml$metric, levels = c("CWMDD", "Richness", "SDDD"))
  ml$parameter <- factor(ml$parameter, levels = names(param_labels), labels = param_labels)
  # Each parameter lives on its own real-value scale, so facet metric x parameter
  # with free scales: x = actual parameter value (per column), y = metric (per row).
  base_hl <- data.frame(metric = factor(c("CWMDD", "Richness", "SDDD"),
                                        levels = c("CWMDD", "Richness", "SDDD")),
                        base = c(base_row$CWMDD, base_row$Richness, base_row$SDDD))
  base_vl <- data.frame(parameter = factor(as.character(param_labels), levels = as.character(param_labels)),
                        baseval = c(BASE$birth_rate, BASE$death_rate, BASE$nb))
  p_sens_lines <- ggplot(ml, aes(x = paramval, y = value, color = parameter, group = parameter)) +
    geom_hline(data = base_hl, aes(yintercept = base), linetype = "dashed", color = "gray50", inherit.aes = FALSE) +
    geom_vline(data = base_vl, aes(xintercept = baseval), linetype = "dotted", color = "gray60", inherit.aes = FALSE) +
    geom_line(linewidth = 0.8) +
    geom_pointrange(aes(ymin = value - sd, ymax = value + sd), size = 0.4) +
    scale_color_manual(values = param_colors, guide = "none") +
    facet_grid(metric ~ parameter, scales = "free") +
    labs(title = "Local one-at-a-time sensitivity analysis",
         subtitle = "Response vs. actual parameter value. Points = mean over 20 reps +/- 1 SD; dashed = baseline response, dotted = baseline parameter value (birth=1.0, death=0.10, niche breadth=0.20).",
         x = "Parameter value", y = "Response (mean over replicates)") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8, color = "gray35"),
          strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold"))
  ggsave(file.path(OUT, "appendix_sensitivity_lines.png"), p_sens_lines, width = 11, height = 8.5, bg = "white")
  cat("[wrote] appendix_sensitivity_lines.png\n")

  tor <- ml %>%
    group_by(metric, parameter) %>%
    summarise(low = min(value, na.rm = TRUE), high = max(value, na.rm = TRUE), .groups = "drop")
  base_tbl <- data.frame(metric = factor(c("CWMDD", "Richness", "SDDD"), levels = c("CWMDD", "Richness", "SDDD")),
                         base = c(base_row$CWMDD, base_row$Richness, base_row$SDDD))
  tor <- tor %>% left_join(base_tbl, by = "metric") %>%
    group_by(metric) %>% mutate(swing = high - low) %>% ungroup()
  ord <- tor %>% group_by(parameter) %>% summarise(s = sum(swing)) %>% arrange(s) %>% pull(parameter)
  tor$parameter <- factor(tor$parameter, levels = ord)
  p_sens_tornado <- ggplot(tor, aes(y = parameter)) +
    geom_segment(aes(x = low, xend = high, y = parameter, yend = parameter, color = parameter),
                 linewidth = 9, lineend = "butt") +
    geom_vline(data = base_tbl, aes(xintercept = base), linetype = "dashed", color = "gray30") +
    scale_color_manual(values = param_colors, guide = "none") +
    facet_wrap(~ metric, scales = "free_x") +
    labs(title = "Sensitivity tornado: response range across each parameter sweep",
         subtitle = "Bar spans min-max mean response over the OAT sweep; dashed = baseline value.",
         x = "Response (mean over replicates)", y = NULL) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8, color = "gray35"),
          strip.background = element_rect(fill = "gray85", color = "black"),
          strip.text = element_text(face = "bold"))
  ggsave(file.path(OUT, "appendix_sensitivity_tornado.png"), p_sens_tornado, width = 12, height = 3.5, bg = "white")
  cat("[wrote] appendix_sensitivity_tornado.png\n")

  ## ---- SUP: DISTURBANCE FREQUENCY x SPREAD (df exp) -- heatmaps ------------
  df_agg <- d_df %>%
    group_by(disturbance, disturbance_freq) %>%
    summarise(CWMDD = mean(cwm, na.rm = TRUE),
              Richness = mean(present_species, na.rm = TRUE),
              SDDD = mean(sd_dispersal, na.rm = TRUE), .groups = "drop")
  make_heat <- function(fillvar, title, digits = 1) {
    dd <- df_agg
    v <- dd[[fillvar]]
    rng <- range(v, na.rm = TRUE)
    denom <- if (diff(rng) > 0) diff(rng) else 1
    # white label on the dark (low) end of viridis, black on the light (high) end
    dd$.txt <- ifelse((v - rng[1]) / denom > 0.55, "black", "white")
    ggplot(dd, aes(x = factor(disturbance), y = factor(disturbance_freq), fill = .data[[fillvar]])) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = formatC(.data[[fillvar]], format = "f", digits = digits), color = .txt), size = 3) +
      scale_color_identity() +
      scale_fill_viridis_c(option = "D") +
      labs(title = title, x = "Disturbance spread probability", y = "Disturbance frequency", fill = title) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
            legend.position = "right", panel.grid = element_blank())
  }
  h_cwm  <- make_heat("CWMDD",    "CWMDD",    digits = 2)
  h_rich <- make_heat("Richness", "Richness", digits = 0)
  h_sd   <- make_heat("SDDD",     "SDDD",     digits = 2)
  fig_df <- h_cwm + h_rich + h_sd + plot_layout(nrow = 1) +
    plot_annotation(title = "Disturbance regime robustness: spread probability x event frequency (continuous landscape, ac = 0.5)",
                    theme = theme(plot.title = element_text(face = "bold", size = 13)))
  ggsave(file.path(OUT, "appendix_disturbance_freq_heatmap.png"), fig_df, width = 15, height = 4.6, bg = "white")
  cat("[wrote] appendix_disturbance_freq_heatmap.png\n")

  ## ---- SUP S1-S4: two-variable factorials (only kernels that have the runs) -
  # 370 (s13): frag = 0.7, vary hab x ac x disturbance  -> S1 (ac=0.5 slice) + S3 (dist=0 slice)
  # 371 (s24): hab  = 0.2, vary frag x ac x disturbance -> S2 (ac=0.5 slice) + S4 (dist=0 slice)
  # Single dashed reference = continuous landscape at ac=0.5, no disturbance (ref_lev).
  if (!is.null(e$s13) && !is.null(e$s24)) {
    d_s13 <- process_cached(e$s13)
    d_s24 <- process_cached(e$s24)

    s1 <- make_supp(d_s13[near(d_s13$ac, 0.5), ],          "hab",  "Habitat Amount",
                    "disturbance", "Disturbance",    ref_lev)
    ggsave(file.path(OUT, "fig_S1_felix.png"), s1, width = 15, height = 5, bg = "white")
    s3 <- make_supp(d_s13[near(d_s13$disturbance, 0), ],   "hab",  "Habitat Amount",
                    "ac",          "Autocorrelation", ref_lev)
    ggsave(file.path(OUT, "fig_S3_felix.png"), s3, width = 15, height = 5, bg = "white")
    s2 <- make_supp(d_s24[near(d_s24$ac, 0.5), ],          "frag", "Fragmentation",
                    "disturbance", "Disturbance",    ref_lev)
    ggsave(file.path(OUT, "fig_S2_felix.png"), s2, width = 15, height = 5, bg = "white")
    s4 <- make_supp(d_s24[near(d_s24$disturbance, 0), ],   "frag", "Fragmentation",
                    "ac",          "Autocorrelation", ref_lev)
    ggsave(file.path(OUT, "fig_S4_felix.png"), s4, width = 15, height = 5, bg = "white")
    cat("[wrote] fig_S1..S4_felix.png\n")
  }

  cat(sprintf("---------- %s complete ----------\n", kernel_name))
}

################################################################################
# Build both kernels.
################################################################################
exp_set <- list(
  exponential_kernel = list(ac = "200", dist = "201", freq = "204", frag = "202", hab = "203",
                            f3 = "210", f4 = "220", f567 = "230", sens = "250", df = "260"),
  lognormal_kernel   = list(ac = "300", dist = "301", freq = "304", frag = "302", hab = "303",
                            f3 = "310", f4 = "320", f567 = "231", sens = "350", df = "360",
                            s13 = "370", s24 = "371")
)

for (kname in names(exp_set)) build_kernel_figures(kname, exp_set[[kname]])

cat("\nAll figures written under:", OUT_ROOT, "\n")
cat("  exponential_kernel/  (200-block + 230)\n")
cat("  lognormal_kernel/    (300-block + 231)\n")
