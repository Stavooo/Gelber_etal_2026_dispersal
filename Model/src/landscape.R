# Function to generate a two-dimensional Gaussian random field with a power-law
# (fractional-Brownian-motion-like) spectrum, using FFT-based spectral synthesis.
# The field produced by fbm_fft() is inherently periodic (toroidal) — its spatial
# autocorrelation wraps seamlessly across the grid edges, which is what justifies
# using a toroidal dispersal kernel in the simulation.
#
# This replaces the earlier nlm_fbm() implementation (NLMR / RandomFields), both
# to remove the dependency on the deprecated RandomFields package and to produce
# truly toroidal landscapes.
#
# ac_amount in [0, 1] is mapped linearly to the spectral exponent alpha in
# [alpha.min, alpha.max] (default 0..3). Higher alpha gives smoother fields
# (more autocorrelation).

fbm_fft <- function(gr_size = 128,
                    ac_amount = 0.7,
                    resolution = 1,
                    alpha.min = 0,
                    alpha.max = 3,
                    seed = NULL,
                    raster = TRUE,
                    rescale = TRUE) {
  checkmate::assert_count(gr_size, positive = TRUE)
  checkmate::assert_numeric(resolution)
  checkmate::assert_numeric(ac_amount)
  checkmate::assert_true(ac_amount >= 0)
  checkmate::assert_true(ac_amount <= 1)
  checkmate::assert_logical(rescale)

  N <- gr_size
  alpha <- alpha.min + ac_amount * (alpha.max - alpha.min)

  # Build frequency grid centered on DC (FFT-shifted layout)
  fx <- ifelse(0:(N - 1) <= N / 2, 0:(N - 1), 0:(N - 1) - N)
  fy <- fx
  FX <- matrix(rep(fx, each = N), nrow = N)
  FY <- matrix(rep(fy, times = N), nrow = N)
  freq <- sqrt(FX^2 + FY^2)
  freq[1, 1] <- 1  # avoid 0/0 at DC; the DC bin is zeroed below
  amp <- 1 / (freq^alpha)

  # Generate complex white noise and shape its spectrum, then inverse FFT
  if (!is.null(seed)) {
    set.seed(seed)
  }
  noise <- matrix(rnorm(N * N), nrow = N) +
    1i * matrix(rnorm(N * N), nrow = N)
  f_field <- amp * noise
  f_field[1, 1] <- 0
  field <- Re(fft(f_field, inverse = TRUE))

  if (rescale) {
    field_min <- min(field)
    field_max <- max(field)
    if (field_max > field_min) {
      field <- (field - field_min) / (field_max - field_min)
    } else {
      field <- field * 0
    }
  }

  if (raster) {
    rast <- raster::raster(field)
    raster::extent(rast) <- c(0, ncol(rast) * resolution,
                              0, nrow(rast) * resolution)
    raster::res(rast) <- resolution
    return(rast)
  } else {
    return(field)
  }
}
