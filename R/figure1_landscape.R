#!/usr/bin/env Rscript
################################################################################
# make_fig1_landscapes.R
#
# Recreate the conceptual landscape figure (Fig. 1) using the NEW FFT-based
# landscape generator (fbm_fft, from src/landscape.R) that replaced the
# deprecated RandomFields nlm_fbm(). Layout mirrors the original Fig. 1:
#
#                          | Autocorrelation high | Autocorrelation low
#   (top, unmasked fields) |       env_high       |      env_low
#   Habitat high / Frag high |  mask |   a)   |   b)
#   Habitat high / Frag low  |  mask |   c)   |   d)
#   Habitat low  / Frag high |  mask |   e)   |   f)
#   Habitat low  / Frag low  |  mask |   g)   |   h)
#
# Environmental field  = fbm_fft(ac_amount)              (smoothness = autocorr.)
# Habitat mask         = fbm_fft(ac_amount = 1 - frag), binarised by quantile at
#                        the habitat fraction (exactly as cookie_cutting.R does).
# Colour palette: rainbow (per request), replacing the magma/inferno original.
################################################################################

source("Model/src/landscape.R")   # provides fbm_fft()

## ---- parameters (illustrative high/low levels) ------------------------------
N        <- 200
AC_HIGH  <- 0.9 ; AC_LOW  <- 0.1
HAB_HIGH <- 0.75; HAB_LOW <- 0.20
FRG_HIGH <- 0.9 ; FRG_LOW <- 0.1

PAL <- rev(rainbow(256, start = 0, end = 0.80))   # low = violet/blue, high = red

fld <- function(ac, seed) fbm_fft(gr_size = N, ac_amount = ac, raster = FALSE,
                                  rescale = TRUE, seed = seed)

## ---- fields -----------------------------------------------------------------
env_high <- fld(AC_HIGH, seed = 1)   # smooth environment
env_low  <- fld(AC_LOW,  seed = 2)   # rugged environment

# Two fragmentation fields (high frag = low autocorrelation of the mask field).
# Same field is thresholded at two habitat fractions, so within a fragmentation
# level the patch structure is shared and only habitat amount changes.
frag_field_high <- fld(1 - FRG_HIGH, seed = 11)
frag_field_low  <- fld(1 - FRG_LOW,  seed = 12)

make_mask <- function(frag_field, hab) {           # TRUE = habitat
  thr <- stats::quantile(frag_field, probs = 1 - hab, names = FALSE)
  frag_field >= thr
}
mask_hh <- make_mask(frag_field_high, HAB_HIGH)     # hab high, frag high
mask_hl <- make_mask(frag_field_low,  HAB_HIGH)     # hab high, frag low
mask_lh <- make_mask(frag_field_high, HAB_LOW)      # hab low,  frag high
mask_ll <- make_mask(frag_field_low,  HAB_LOW)      # hab low,  frag low

apply_mask <- function(env, mask) { z <- env; z[!mask] <- NA; z }

## ---- drawing helpers --------------------------------------------------------
draw_field <- function(z, letter = NULL, title = NULL) {
  par(mar = c(0.25, 0.25, if (is.null(title)) 0.25 else 1.5, 0.25))
  image(z, col = PAL, axes = FALSE, useRaster = TRUE,
        zlim = c(0, 1), xlab = "", ylab = "")
  box(col = "grey20")
  if (!is.null(title)) title(main = title, cex.main = 1.15, font.main = 1, line = 0.3)
  if (!is.null(letter)) text(0.04, 0.95, letter, adj = c(0, 1), cex = 1.1, font = 2)
}
draw_mask <- function(mask) {                       # black = habitat, white = matrix
  par(mar = c(0.25, 0.25, 0.25, 0.25))
  image(matrix(as.numeric(mask), nrow = nrow(mask)),
        col = c("white", "black"), breaks = c(-0.5, 0.5, 1.5),
        axes = FALSE, useRaster = TRUE, xlab = "", ylab = "")
  box(col = "grey20")
}
draw_label <- function(txt, cex = 1.15, font = 2) {
  par(mar = c(0, 0, 0, 0)); plot.new()
  text(0.5, 0.5, txt, srt = 90, cex = cex, font = font)
}
blank <- function() { par(mar = c(0, 0, 0, 0)); plot.new() }

## ---- assemble ---------------------------------------------------------------
# Drawing order follows the region NUMBERS in the layout matrix (1..20); the
# `0` cells (top-left corner) are left empty automatically and must NOT be drawn.
OUT_REPO <- "R/figures/fig1.png"
png(OUT_REPO, width = 1500, height = 2750, res = 200)

layout(matrix(c(
   0,  0,  0,  1,  2,
   3,  4,  5,  6,  7,
   3,  8,  9, 10, 11,
  12, 13, 14, 15, 16,
  12, 17, 18, 19, 20), nrow = 5, byrow = TRUE),
  widths  = c(0.30, 0.40, 1, 1, 1),
  heights = c(1, 1, 1, 1, 1))

draw_field(env_high, title = "Autocorrelation high")   # 1
draw_field(env_low,  title = "Autocorrelation low")    # 2
draw_label("Habitat amount high")                       # 3 (spans rows 2-3)
draw_label("Fragmentation high", cex = 1)               # 4
draw_mask(mask_hh)                                       # 5
draw_field(apply_mask(env_high, mask_hh), letter = "a)") # 6
draw_field(apply_mask(env_low,  mask_hh), letter = "b)") # 7
draw_label("Fragmentation low", cex = 1)                # 8
draw_mask(mask_hl)                                       # 9
draw_field(apply_mask(env_high, mask_hl), letter = "c)") # 10
draw_field(apply_mask(env_low,  mask_hl), letter = "d)") # 11
draw_label("Habitat amount low")                        # 12 (spans rows 4-5)
draw_label("Fragmentation high", cex = 1)               # 13
draw_mask(mask_lh)                                       # 14
draw_field(apply_mask(env_high, mask_lh), letter = "e)") # 15
draw_field(apply_mask(env_low,  mask_lh), letter = "f)") # 16
draw_label("Fragmentation low", cex = 1)                # 17
draw_mask(mask_ll)                                       # 18
draw_field(apply_mask(env_high, mask_ll), letter = "g)") # 19
draw_field(apply_mask(env_low,  mask_ll), letter = "h)") # 20

dev.off()
cat("[wrote]", OUT_REPO, "\n")
