######################## Birth function ########################################

# The function represent 1 of the 2 core processes in the dynamic model simulation.
# It generates new agents according to a set of rules.
# The function returns the updated agents list and agent raster layer
# as well as the number of successful within and between patch immigration events.

birth <- function(agents, agents_grid, grid, NB, disp, d_dis) {
  # set counters for within and between immigration events
  within_patch <- 0
  between_patch <- 0

  clumped <- raster::clump(grid, directions = 4)
  patch_matrix <- as.matrix(clumped)
  # looping through all agents
  for (i in seq_len(nrow(agents))) {
    # generate random numbers between 0-1. (Should the numbers be instead pulled from a distribution?)
    rand <- runif(3, 0, 1)

    # extract N value,niche breadth, dispersal distance, and location of current agent

    u <- species_par$n_value[species_par$species_id == agents$species_id[i]]

    # Get niche breadth. check to see if species specific niche breadth OR the function argument which comes at the moment from var_par

    if (switch$species_specific_par == 1) {
      nb <- species_par$niche_breadth[species_par$species_id == agents$species_id[i]]
    } else if (switch$species_specific_par == 0) {
      nb <- NB
    } else {
      print("check switches please")
    }

    cur_loc <- c(agents$x_loc[i], agents$y_loc[i])
    # checking the birth rate parameter
    if (rand[1] < species_par$birth_rate[species_par$species_id == agents$species_id[i]]) {
      # get dispersal rate (long vs short)
      if (switch$species_specific_par == 1) {
        dr <- species_par$dispersal_rate[species_par$species_id == agents$species_id[i]]
      } else if (switch$species_specific_par == 0) {
        dr <- disp
      } else {
        print("check switches please")
      }

      # get dispersal distance
      if (switch$species_specific_disp == 1) {
        dd <- species_par$dispersal_distance[species_par$species_id == agents$species_id[i]]
      } else if (switch$species_specific_disp == 0) {
        dd <- d_dis
      } else {
        print("check switches please")
      }

      # checking if short or long dispersal
      if (rand[2] < dr) {
        ### short dispersal###

        # a while loop to make sure that the new location is within the grid
        if (switch$same_cell_dispersal == 1) {
          while (TRUE) {
            new_loc_short <- disperse(
              cur_loc = cur_loc,
              d_sd = dd,
              d_mean = dd
            )
            if (new_loc_short[1] <= mod_par$grid_size && new_loc_short[1] >= 1 && new_loc_short[2] <= mod_par$grid_size && new_loc_short[2] >= 1) {
              break
            }
          }
        } else if (switch$same_cell_dispersal == 0) {
          while (TRUE) {
            new_loc_short <- disperse(
              cur_loc = cur_loc,
              d_sd = dd,
              d_mean = dd
            )
            if (new_loc_short[1] <= mod_par$grid_size && new_loc_short[1] >= 1 && new_loc_short[2] <= mod_par$grid_size && new_loc_short[2] >= 1 && !(new_loc_short[1] == cur_loc[1] && new_loc_short[2] == cur_loc[2])) {
              break
            }
          }
        } else {
          print("check switches please")
        }
        # DEPRECATED method to generate new location for birth (disperses outside of the grid)
        # # generate new location for birth
        # new_loc_short <- disperse(
        #   cur_loc = cur_loc,
        #   d_sd = dd,
        #   d_mean = dd
        # )
        inter_cell_subset <- collapse::fsubset(agents, x_loc == new_loc_short[1] & y_loc == new_loc_short[2])
        intra_cell <- collapse::fnrow(collapse::fsubset(inter_cell_subset, species_id == agents$species_id[i]))
        inter_cell <- collapse::fnrow(inter_cell_subset)

        if (new_loc_short[1] <= mod_par$grid_size && new_loc_short[1] >= 1 && # checking if new location is within the grid
          new_loc_short[2] <= mod_par$grid_size && new_loc_short[2] >= 1 && # checking if new location is within the grid
          !is.na(grid[new_loc_short[1], new_loc_short[2]]) && # checking if the grid cell is non-matrix (habitat)
          inter_cell < mod_par$k_inter && # checking if the cell did not reach its inter-specific carrying capacity
          intra_cell < mod_par$k_intra # checking if the cell did not reach its intra-specific carrying capacity

        ) {
          # extract environmental value at the new location and calculate survival pro. according to Gravel 2006
          e <- grid[new_loc_short[1], new_loc_short[2]]
          survival_prob <- exp((-(e - u)^2) / (2 * nb^2))

          # checking the survival probability
          if (survival_prob > rand[3]) {
            if (switch$animation_export == 1) {
              # set empty spot in raster to species number
              agents_grid[new_loc_short[1], new_loc_short[2]] <- agents_grid[cur_loc[1], cur_loc[2]]
            }

            # update agents list
            agents <- rbindlist(list(agents, list(
              max(agents$id, na.rm = TRUE) + 1,
              agents$species_id[i],
              new_loc_short[1],
              new_loc_short[2],
              patch_matrix[new_loc_short[1], new_loc_short[2]]
            )))

            # update dispersal counters
            if (patch_matrix[new_loc_short[1], new_loc_short[2]] == patch_matrix[cur_loc[1], cur_loc[2]]) {
              within_patch <- within_patch + 1
            } else {
              between_patch <- between_patch + 1
            }
          }
        }
      } else {
        ### long dispersal###
        # generate location and calculate survival prob.
        new_loc_long <- round(runif(2, 1, mod_par$grid_size))
        e <- grid[new_loc_long[1], new_loc_long[2]]
        survival_prob <- exp((-(e - u)^2) / (2 * nb^2))

        inter_cell_subset <- collapse::fsubset(agents, x_loc == new_loc_long[1] & y_loc == new_loc_long[2])
        inter_cell <- collapse::fnrow(inter_cell_subset)
        intra_cell <- collapse::fnrow(collapse::fsubset(inter_cell_subset, species_id == agents$species_id[i]))

        if (!is.na(grid[new_loc_long[1], new_loc_long[2]]) && # checking if the grid cell is non-matrix (habitat)
          inter_cell < mod_par$k_inter && # checking if the cell did not reach its inter-specific carrying capacity
          intra_cell < mod_par$k_intra # checking if the cell did not reach its intra-specific carrying capacity
        ) {
          # checking survival prob.
          if (survival_prob > rand[3]) {
            if (switch$animation_export == 1) {
              # set empty spot in raster to species number
              agents_grid[new_loc_long[1], new_loc_long[2]] <- agents_grid[cur_loc[1], cur_loc[2]]
            }
            # update dispersal counters

            if (patch_matrix[new_loc_long[1], new_loc_long[2]] == patch_matrix[cur_loc[1], cur_loc[2]]) {
              within_patch <- within_patch + 1
            } else {
              between_patch <- between_patch + 1
            }


            # update agents list
            agents <- rbindlist(list(agents, list(
              max(agents$id, na.rm = TRUE) + 1,
              agents$species_id[i],
              new_loc_long[1],
              new_loc_long[2],
              patch_matrix[
                new_loc_long[1],
                new_loc_long[2]
              ]
            )))
          }
        }
      }
    }
  }


  return_list <- list(agents = agents, agents_grid = agents_grid, within_patch = within_patch, between_patch = between_patch)
  return(return_list)
}
