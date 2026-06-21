#!/usr/bin/env Rscript
################################################################################
# figures_kernel_comparison.R
#
# Supplementary figures S7-S10 that compare the two dispersal kernels
# (exponential vs log-normal) DIRECTLY. Reads the cached per-run summaries
# written by figures_main.R (data-raw/_cache/summary_<run>.rds), so run
# figures_main.R (or generate_figures.R) first.
#
# Layout convention:
#   Fig 2  -> same 5-sweep x 3-metric boxplot grid, both kernels OVERLAID as
#             dodged boxplots coloured by kernel.
#   Fig 3+ -> combined grid: rows = response aspect (CWMDD / Richness / SDDD)
#             with a SHARED y-axis range across both kernels (fair comparison);
#             columns = kernel. Y differs between rows, shared within a row.
#
# Outputs -> revised_figures/kernel_comparison/
#   fig2_kernel_compare.png      Fig 2, kernels overlaid (fill = kernel).
#   fig3_kernel_compare.png      Fig 3 (ac x disturbance, continuous).
#   fig4_kernel_compare.png      Fig 4 (frag x hab, modified; per-kernel ref).
#   fig567a_kernel_compare.png   Figs 5-7 'a' orientation (habitat on x, facet
#                                ac); rows = metric, cols = kernel, per-ac refs.
#
# Experiment -> kernel mapping (see make_revised_figures.R header):
#   Fig2 ac/dist/freq/frag/hab : 200/201/204/202/203 (exp)  300/301/304/302/303 (log)
#   Fig3 ac x disturbance      : 210 (exp)  310 (log)
#   Fig4 frag x hab            : 220 (exp)  320 (log)
#   Figs5-7 4D                 : 230 (exp)  231 (log)
################################################################################

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(ggplot2); library(patchwork)
})

## --- locate cache / output (override input location via FRAG_REV) ------------
## Reads the per-run summary cache populated by figures_main.R. Run that script
## first (it processes both kernels' raw runs into data-raw/_cache/).
REV   <- Sys.getenv("FRAG_REV", unset = "data-raw")
CACHE <- file.path(REV, "_cache")
OUT   <- file.path("R", "figures", "_build", "kernel_comparison")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
cat("REV   =", REV, "\nCACHE =", CACHE, "\nOUT   =", OUT, "\n")

load_exp <- function(exp_num) {
  cf <- file.path(CACHE, paste0("summary_", exp_num, ".rds"))
  if (!file.exists(cf)) stop("missing cache: ", cf)
  as.data.frame(readRDS(cf))
}

okabe_ito_palette <- c("#E69F00","#56B4E9","#009E73","#F0E442",
                       "#0072B2","#D55E00","#CC79A7","#000000")
KERNEL_COLORS <- c("Exponential" = "#D55E00", "Log-normal" = "#0072B2")
KERNEL_LEVELS <- c("Exponential", "Log-normal")
near <- function(x, v, tol = 1e-8) abs(x - v) < tol

# stack two kernels' frames, adding a kernel label
stack_kernels <- function(exp_e, exp_l) {
  de <- load_exp(exp_e); de$kernel <- "Exponential"
  dl <- load_exp(exp_l); dl$kernel <- "Log-normal"
  d <- bind_rows(de, dl)
  d$kernel <- factor(d$kernel, levels = KERNEL_LEVELS)
  d
}

# three response metrics, in display (row) order
metrics <- list(
  list(y = "cwm",             lab = "CWMDD"),
  list(y = "present_species", lab = "Richness"),
  list(y = "sd_dispersal",    lab = "SDDD")
)

# continuous-landscape reference value(s) from a Fig-3 run, at disturbance = 0
ref_at  <- function(df, yvar, a) mean(df[[yvar]][near(df$ac, a) & near(df$disturbance, 0)], na.rm = TRUE)
ref_vec <- function(df, yvar)    sapply(c(0.1, 0.5, 0.9), function(a) ref_at(df, yvar, a))

################################################################################
# FIGURE 2 -- both kernels overlaid as dodged boxplots, fill = kernel.
# Free y per panel (matches the original Fig 2; only the kernel colour is new).
################################################################################
fig2_row_cmp <- function(d, xvar, xlab, rowtitle) {
  d <- d[!is.na(d[[xvar]]), ]
  d[[xvar]] <- factor(d[[xvar]], levels = sort(unique(d[[xvar]])))
  base_theme <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  mk <- function(yvar, ylab, title = NULL)
    ggplot(d, aes(x = .data[[xvar]], y = .data[[yvar]], fill = kernel)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.85,
                   position = position_dodge(width = 0.8)) +
      scale_fill_manual(values = KERNEL_COLORS) +
      labs(title = title, x = xlab, y = ylab, fill = "Dispersal kernel") + base_theme
  mk("cwm", "CWMDD") +
    mk("present_species", "Richness", rowtitle) +
    mk("sd_dispersal", "SDDD") +
    plot_layout(nrow = 1)
}

build_fig2 <- function() {
  r1 <- fig2_row_cmp(stack_kernels("200","300"), "ac",               "Landscape Autocorrelation", "Varying autocorrelation in continuous landscape")
  r2 <- fig2_row_cmp(stack_kernels("201","301"), "disturbance",      "Disturbance Spread Rate",   "Varying disturbance spread rate in continuous landscape")
  r3 <- fig2_row_cmp(stack_kernels("204","304"), "disturbance_freq", "Disturbance Frequency",     "Varying disturbance frequency in continuous landscape")
  r4 <- fig2_row_cmp(stack_kernels("202","302"), "frag",             "Fragmentation Per Se",      "Varying fragmentation in modified landscape")
  r5 <- fig2_row_cmp(stack_kernels("203","303"), "hab",              "Habitat Amount",            "Varying habitat amount in modified landscape")
  fig2 <- (r1 / r2 / r3 / r4 / r5) +
    plot_layout(ncol = 1, guides = "collect") +
    plot_annotation(tag_levels = list(paste0(letters[1:15], ")"))) &
    theme(legend.position = "top",
          legend.title = element_text(size = 13), legend.text = element_text(size = 12),
          plot.tag = element_text(size = 12, face = "bold"),
          plot.tag.position = c(0.02, 0.88),
          plot.margin = margin(t = 5, r = 5, b = 10, l = 5))
  ggsave(file.path(OUT, "fig2_kernel_compare.png"), fig2, width = 14, height = 22.5, bg = "white")
  cat("[wrote] fig2_kernel_compare.png\n")
}

################################################################################
# Shared 3-row (metric) x 2-col (kernel) grid, y shared per row.
# panel_fun(dk, m, kname, top, bottom) returns one cell ggplot (no coord set).
################################################################################
panel_theme_cmp <- function(top, bottom) theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
  panel.grid.major = element_line(color = "gray90"), panel.grid.minor = element_blank(),
  strip.background = element_rect(fill = "gray85", color = "black"),
  strip.text = element_text(face = "bold", size = 12),
  axis.title.y = element_text(size = 14), axis.text = element_text(size = 11),
  axis.title.x = if (bottom) element_text(size = 14) else element_blank()
)

build_grid <- function(d, panel_fun, refs_by_metric, w, h, fname) {
  cells <- list(); nrow_m <- length(metrics)
  for (mi in seq_along(metrics)) {
    m  <- metrics[[mi]]
    rr <- range(c(d[[m$y]], refs_by_metric[[m$y]]), na.rm = TRUE, finite = TRUE)
    pad <- diff(rr) * 0.05; if (!is.finite(pad) || pad == 0) pad <- max(abs(rr)) * 0.05 + 1e-6
    ylim <- c(rr[1] - pad, rr[2] + pad)
    for (kname in KERNEL_LEVELS) {
      dk <- d[d$kernel == kname, ]
      cells[[length(cells) + 1]] <-
        panel_fun(dk, m, kname, top = (mi == 1), bottom = (mi == nrow_m)) +
        coord_cartesian(ylim = ylim)
    }
  }
  # No baked-in figure title (titles/captions live in the manuscript text, as in
  # S1-S4). guide_area() gives the collected legend its OWN band on top; below it
  # the kernel name is each top panel's title; a)-f) tags sit at each panel's
  # top-left, next to the facets. Order top->bottom: legend, kernel title, panels.
  body <- wrap_plots(cells, ncol = 2, byrow = TRUE)
  g <- guide_area() / body +
    plot_layout(guides = "collect", heights = c(1, 28)) +
    plot_annotation(tag_levels = "a", tag_suffix = ")") &
    theme(legend.position = "top",
          legend.title = element_text(size = 13), legend.text = element_text(size = 12),
          plot.tag = element_text(size = 12, face = "bold"),
          plot.tag.position = c(0.02, 0.92))
  ggsave(file.path(OUT, fname), g, width = w, height = h, bg = "white")
  cat("[wrote]", fname, "\n")
}

################################################################################
# Reference values (per kernel) from each kernel's Fig-3 continuous run.
################################################################################
d_f3_e <- load_exp("210"); d_f3_l <- load_exp("310")
refs_fig4 <- list(
  "Exponential" = lapply(metrics, function(m) ref_at(d_f3_e, m$y, 0.5)),
  "Log-normal"  = lapply(metrics, function(m) ref_at(d_f3_l, m$y, 0.5))
)
refs567 <- list(
  "Exponential" = setNames(lapply(metrics, function(m) ref_vec(d_f3_e, m$y)), sapply(metrics, `[[`, "y")),
  "Log-normal"  = setNames(lapply(metrics, function(m) ref_vec(d_f3_l, m$y)), sapply(metrics, `[[`, "y"))
)
names(refs_fig4$Exponential) <- sapply(metrics, `[[`, "y")
names(refs_fig4$`Log-normal`) <- sapply(metrics, `[[`, "y")

refs_by_metric <- function(reflist, idx_fun) {
  setNames(lapply(metrics, function(m) c(idx_fun(reflist[["Exponential"]][[m$y]]),
                                         idx_fun(reflist[["Log-normal"]][[m$y]]))),
           sapply(metrics, `[[`, "y"))
}
rbm_fig4 <- refs_by_metric(refs_fig4, identity)
rbm_567  <- refs_by_metric(refs567,  identity)

################################################################################
# FIGURE 3 -- ac x disturbance, continuous landscape (no reference line).
################################################################################
panel_fig3 <- function(dk, m, kname, top, bottom)
  ggplot(dk, aes(x = factor(ac), y = .data[[m$y]], fill = factor(disturbance))) +
    geom_boxplot(outlier.size = 0.5) +
    scale_fill_manual(values = okabe_ito_palette) +
    labs(title = if (top) kname else NULL, x = "Autocorrelation", y = m$lab, fill = "Disturbance") +
    theme_bw() + panel_theme_cmp(top, bottom)

################################################################################
# FIGURE 4 -- frag x hab, modified landscape; per-kernel dashed continuous ref.
################################################################################
panel_fig4 <- function(dk, m, kname, top, bottom)
  ggplot(dk, aes(x = factor(habitat), y = .data[[m$y]], fill = factor(fragmentation))) +
    geom_boxplot(outlier.size = 0.5) +
    geom_hline(yintercept = refs_fig4[[kname]][[m$y]], linetype = "dashed") +
    scale_fill_manual(values = okabe_ito_palette) +
    labs(title = if (top) kname else NULL, x = "Habitat Amount", y = m$lab, fill = "Fragmentation") +
    theme_bw() + panel_theme_cmp(top, bottom)

################################################################################
# FIGS 5-7 'a' -- habitat on x, disturbance fill, facet ac; per-ac dashed refs.
# Elongated A4-portrait layout: the two kernels are STACKED vertically within
# each metric (Exponential directly above Log-normal), so the habitat x-axes
# line up for comparison and the figure fits a portrait page. y shared within
# each metric pair.
################################################################################
ac_lab <- labeller(ac = c("0.1" = "autocorrelation 0.1",
                          "0.5" = "autocorrelation 0.5",
                          "0.9" = "autocorrelation 0.9"))
build_fig567a_stacked <- function(fname, w = 8.5, h = 14.5) {
  d <- stack_kernels("230", "231")
  cells <- list()
  for (m in metrics) {
    rr <- range(c(d[[m$y]], rbm_567[[m$y]]), na.rm = TRUE, finite = TRUE)
    pad <- diff(rr) * 0.05; if (!is.finite(pad) || pad == 0) pad <- max(abs(rr)) * 0.05 + 1e-6
    ylim <- c(rr[1] - pad, rr[2] + pad)
    for (kname in KERNEL_LEVELS) {
      dk <- d[d$kernel == kname, ]
      hl <- data.frame(ac = c(0.1, 0.5, 0.9), ref = refs567[[kname]][[m$y]])
      cells[[length(cells) + 1]] <-
        ggplot(dk, aes(x = factor(habitat), y = .data[[m$y]], fill = factor(disturbance))) +
          geom_boxplot(outlier.size = 0.4) +
          geom_hline(data = hl, aes(yintercept = ref), linetype = "dashed", color = "black") +
          facet_grid(cols = vars(ac), labeller = ac_lab) +
          scale_fill_manual(values = okabe_ito_palette) +
          coord_cartesian(ylim = ylim) +
          labs(title = paste0(kname, " — ", m$lab),
               x = "Habitat Amount", y = m$lab, fill = "Disturbance") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0, face = "bold", size = 13),
                panel.grid.major = element_line(color = "gray90"), panel.grid.minor = element_blank(),
                strip.background = element_rect(fill = "gray85", color = "black"),
                strip.text = element_text(face = "bold", size = 11),
                axis.title = element_text(size = 13), axis.text = element_text(size = 10))
    }
  }
  g <- guide_area() / wrap_plots(cells, ncol = 1) +
    plot_layout(guides = "collect", heights = c(1, 55)) +
    plot_annotation(tag_levels = "a", tag_suffix = ")") &
    theme(legend.position = "top",
          legend.title = element_text(size = 13), legend.text = element_text(size = 12),
          plot.tag = element_text(size = 12, face = "bold"),
          plot.tag.position = c(0.01, 0.99))
  ggsave(file.path(OUT, fname), g, width = w, height = h, bg = "white")
  cat("[wrote]", fname, "(stacked, portrait)\n")
}

################################################################################
# Build all.
################################################################################
build_fig2()
build_grid(stack_kernels("210","310"), panel_fig3, list(),
           w = 10, h = 12, fname = "fig3_kernel_compare.png")
build_grid(stack_kernels("220","320"), panel_fig4, rbm_fig4,
           w = 10, h = 12, fname = "fig4_kernel_compare.png")
build_fig567a_stacked("fig567a_kernel_compare.png")

cat("\nAll kernel-comparison figures written under:", OUT, "\n")
