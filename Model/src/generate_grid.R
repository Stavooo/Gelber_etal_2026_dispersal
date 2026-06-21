######################## Grid Generating Function ##############################

# The function takes 4 parameters from the 'par' data.frame and generates 2
# landscapes using a fractional Brownian motion model.
# 1 grid simulates an auto correlated landscape and the other simulates
# fragmented landscape.
# The 2 grids are merged to create the final simulation space. The function
# returns a raster object.

generate_grid <- function(gr_size, ac_amount, frag_amount, hab_amount) {
  if (switch$no_ac == 1) {
    binary_grid <- raster(nrow = gr_size, ncol = gr_size, xmn = 0, xmx = gr_size, ymn = 0, ymx = gr_size, crs = NA, vals = NA, resolution = 1)
    binary_grid <- setValues(binary_grid, runif(gr_size * gr_size))
    binary_grid[binary_grid == 0] <- 0.001
  } else if (switch$no_ac == 0) {
    # Use FFT-based fractional Brownian motion (inherently periodic / toroidal).
    # Replaces the previous nlm_fbm() call that depended on the deprecated
    # RandomFields package. The toroidal landscape, combined with toroidal
    # dispersal (switch$torus == 1), produces a fully consistent torus world.
    env_grid <- fbm_fft(
      gr_size = gr_size,
      ac_amount = ac_amount,
      resolution = 1,
      raster = TRUE,
      rescale = TRUE,
      seed = NULL
    )

    binary_grid <- env_grid
    binary_grid[binary_grid == 0] <- 0.001
  } else if (switch$no_ac == 2) {
    binary_grid <- raster(nrow = gr_size, ncol = gr_size, xmn = 0, xmx = gr_size, ymn = 0, ymx = gr_size, crs = NA, vals = NA, resolution = 1)
    binary_grid <- setValues(binary_grid, 0.5)
  } else {
    print("please check no_ac switche")
  }
  return(binary_grid)
}
