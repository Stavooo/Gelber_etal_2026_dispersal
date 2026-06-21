cookie_cutting <- function(grid = grid,
                           agents = agents,
                           agents_grid = agents_grid,
                           habitat = mod_par$habitat_percent,
                           fragmentation = mod_par$frag_factor) {

  # FFT-based fractional Brownian motion produces an inherently toroidal
  # fragmentation mask, matching the toroidal environment grid and toroidal
  # dispersal. ac_amount = (1 - fragmentation): high fragmentation -> low
  # autocorrelation -> small, dispersed habitat patches (same convention as
  # the original nlm_fbm call which used fract_dim = 2 * (1 - fragmentation)).
  frag_grid <- fbm_fft(
    gr_size = mod_par$grid_size,
    ac_amount = 1 - fragmentation,
    resolution = 1,
    raster = TRUE,
    rescale = TRUE,
    seed = NULL
  )

  # Binarize: cells with value >= habitat-quantile become habitat (kept as
  # values > 1 to be overwritten by `grid`), the rest become matrix (NA).
  # Replaces landscapetools::util_binarize() which depended on a deprecated
  # package. quantile()-based binarization matches util_binarize semantics:
  # the top `habitat` fraction of values are habitat.
  thr <- stats::quantile(raster::values(frag_grid), probs = 1 - habitat,
                         na.rm = TRUE, names = FALSE)
  binary_grid <- frag_grid
  binary_grid[frag_grid >= thr] <- 2  # habitat marker (will be overwritten)
  binary_grid[frag_grid <  thr] <- 1  # matrix marker
  binary_grid[binary_grid == 1] <- NA # Subset matrix to NA


  binary_grid[binary_grid > 1] <- grid[binary_grid > 1] # Merge the layers into final space grid
  binary_grid[binary_grid == 0] <- 0.001

  delete_agents <- vector()

  clumped <- raster::clump(binary_grid, directions = 4)
  patch_matrix <- as.matrix(clumped)
  for (i in 1:nrow(agents)) {
    cur_loc <- c(agents$x_loc[i], agents$y_loc[i])
    if (is.na(binary_grid[cur_loc[1], cur_loc[2]])) {
      delete_agents <- append(delete_agents, i)
    }
  }
  agents <- agents[-c(delete_agents), ]

  # assign patch number to agent

  for (j in 1:nrow(agents)) {
    agents$patch_id[j] <- patch_matrix[agents$x_loc[j], agents$y_loc[j]]
  }
  agents_grid[is.na(binary_grid)] <- NA

  return_list <- list(grid = binary_grid, agents = agents, agents_grid = agents_grid)
  return(return_list)
}