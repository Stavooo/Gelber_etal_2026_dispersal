################# GeDo run Dynamic Model Function ##############################

# The function is a third iteration of the dynamic model, designed to separate
# geometric and demographic fragmentation effects by following these steps:
# 1. run the model with 100% habitat
# 2. apply the cookie-cutter approach by fragmenting the landscape
# (the effects at this time point are geometric)
# 3. keep running model and observe the demographic effects

# Functions to initialize empty result files

source("src/initialize_result_files.R")

GeDo_run <- function(mod_par,
                     var_par,
                     switch,
                     file_name = "test",
                     task_id = "no_cluster",
                     sim_id) {
  # create a vector with all species id's and get simulation steps
  species_sequence <- 1:mod_par$n_species
  steps_1 <- mod_par$steps_pre_frag
  steps_2 <- mod_par$steps_post_frag


  # Initialize output -------------------------------------------------------

  # create a data frame for species abundance and diversity results in the whole landscape
  output_all <- intialize_output_all()
  # create a data frame for species abundance and diversity results at the sample scale
  output_sample <- initialize_output_sample(species_seq = species_sequence)
  # create a data frame for species richness abundance and diversity results grouped by dispersal distance
  output_dispersal <- initialize_output_unique_dispersal()

  # Run model ---------------------------------------------------------------

  start.time.sim <- Sys.time()

  # either stochastic or not-stochastic spin-up phase. Can be switched in parameter file
  if (switch$random == 1) {
    set.seed(seed)
  }

  # initialize the model with 100% habitat
  model_start <- initialize(
    frag = 0,
    hab = 1,
    ac = var_par$ac,
    nb = var_par$nb
  )

  # extract simulation space, agents grid and agents list from the results list
  grid <- model_start$grid
  agents_grid <- model_start$agents_grid
  agents <- model_start$agents

  exp_init_grid <- grid
  # For animation, create dataframes for abundance and richness
  if (switch$animation_export == 1) {
    richness_df <- data.frame(time = 0, value = NA)
    abundance_df <- data.frame(time = 0, value = NA)
  }

  # looping through the simulation time steps
  for (i in 1:steps_1) {
    start.time <- Sys.time()

    # function that returns a vector with species presence data for the landscape
    species_count_vec <- sapply(
      species_sequence, function(sp) {
        sum(agents$species_id == sp)
      }
    )

    # recording data in the results df
    output_all <- tibble::add_row(output_all,
      sim_id = sim_id,
      step = i,
      individuals = nrow(agents),
      present_species = length(unique(agents$species_id)),
      shannon = vegan::diversity(species_count_vec, index = "shannon"),
      patch_individuals_mean = nrow(agents),
      patch_species_mean = length(unique(agents$species_id)),
      patch_shannon_mean = vegan::diversity(species_count_vec, index = "shannon"),
      sample_individuals_mean = 0,
      sample_species_mean = 0,
      sample_shannon_mean = 0,
      habitat = 1,
      fragmentation = 0,
      n_patches = 1,
      n_samples = 0,
      within_patch_dispersal = 0, # number of successful within patch dispersal events
      between_patch_dispersal = 0 # number of successful between patch dispersal events
    )

    # recording data in the dispersal results file
    if (switch$species_specific_disp == 1) {
      # get a vector of unique dispersal distances
      unique_dispersal_distances <- species_par$dispersal_distance[species_par$species_id %in% unique(agents$species_id)]

      for (d in unique_dispersal_distances) {
        output_dispersal <- tibble::add_row(output_dispersal,
          sim_id = sim_id,
          step = i,
          species_dispersal_distance = d,
          individuals = nrow(agents[agents$species_id %in% species_par$species_id[species_par$dispersal_distance == d]]),
          present_species = length(unique(agents$species_id[agents$species_id %in% species_par$species_id[species_par$dispersal_distance == d]])),
          species_id = first(species_par$species_id[species_par$dispersal_distance == d]),
          habitat = 1,
          fragmentation = 0
        )
      }
    }
    if (nrow(agents) > 0) {
      # run birth function and update agents list and grid
      for (b in 1:mod_par$n_birth) {
        step1 <- birth(
          agents = agents,
          agents_grid = agents_grid,
          grid = grid,
          NB = var_par$nb,
          disp = var_par$disp,
          d_dis = var_par$disp_dist
        )
        agents <- step1$agents
        agents_grid <- step1$agents_grid
      }
      wp <- step1$within_patch
      bp <- step1$between_patch

      # run death function and update agents list and grid
      step2 <- death(
        agents = agents,
        agents_grid = agents_grid,
        grid = grid,
        edge_fac = var_par$edge
      )
      agents <- step2$agents
      agents_grid <- step2$agents_grid

      # run immigration function and update agent list and grid
      if (switch$immigration == 1) {
        step3 <- immigration(agents, agents_grid, grid)
        agents <- step3$agents
        agents_grid <- step3$agents_grid
      }

      # run disturbance function and update agent list and grid
      if (switch$disturbance == 1) {
        step4 <- disturbance(agents, grid, var_par$disturbance)
        agents <- step4$agents
      }

      # run environment variation function and update the grid
      if (switch$env_var == 1) {
        agents_grid <- environment_variation(grid)
      }
    } else {
      print("ALL DEAD :(")
    }

    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    if (switch$print_agents == 1) {
      print(paste("step ", i, " took ", time.taken, " with ", nrow(agents), " agents", sep = ""))
    }
    if (switch$animation_export == 1) { # export png to animation folder in case switch is turned on
      # png(paste("Outputs/animation/", sprintf("%04d", i), "_animation_plot.png", sep = ""), width = 600, height = 640, pointsize = 10)
      # plot(t(flip(grid, 1)), col = gray.colors(21), legend = FALSE, axes = FALSE) # plotting the simulation space
      # plot(t(flip(agents_grid, 1)), breaks = breaks_agents, col = rainbow(mod_par$n_species), add = T, axes = F) # adding the agents raster
      # plot(t(flip(grid, 1)), col = gray.colors(21), breaks = breaks_space, legend.only = T, horizontal = T, ) # adding legend of the simulation space
      # text(x = 0, y = mod_par$grid_size + mod_par$grid_size / 10, adj = c(0, 1), labels = paste("Time Step = ", i, sep = ""), cex = 2, xpd = T)

      # dev.off()
      # update abundance and richness data frames
      richness_df <- rbind(richness_df, data.frame(time = i, value = length(unique(agents$species_id))))
      abundance_df <- rbind(abundance_df, data.frame(time = i, value = nrow(agents)))

      flipped <- t(flip(grid, 1))
      grid_df <- as.data.frame(rasterToPoints(flipped))

      # Rename the columns to 'x', 'y', and 'value'
      names(grid_df) <- c("x", "y", "value")

      # # calculate abundance
      #       agents_plot <- agents %>%
      #         group_by(x_loc, y_loc) %>%
      #         summarise(n = n())

      # # number of species
      # agents_plot <- agents %>%
      #   group_by(x_loc, y_loc) %>%
      #   summarise(n = n_distinct(species_id))

      # only dominant species
      agents_plot <- agents %>%
        group_by(x_loc, y_loc, species_id) %>%
        count() %>%
        group_by(x_loc, y_loc) %>%
        slice_max(n, n = 1)

      p1 <- ggplot() +
        geom_raster(data = grid_df, aes(x = x, y = y, fill = value)) +
        geom_point(data = agents_plot, aes(x = x_loc - 0.5, y = y_loc - 0.5, color = species_id), size = 3.5) +
        scale_fill_gradient(low = "white", high = "black") +
        scale_color_viridis(option = "turbo", n = 100) +
        theme_minimal() +
        ggtitle(paste0("Time ", strrep("•", i))) + # replace 'i' with red dots
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(size = 25)) +
        theme(legend.position = "none")

      # Assuming 'richness' and 'abundance' are data frames with columns 'time' and 'value'
      p2 <- ggplot(richness_df, aes(x = time, y = value)) +
        geom_line(size = 3) +
        theme_minimal() +
        ggtitle("Species Richness Over Time") +
        theme(axis.title.y = element_blank(), plot.title = element_text(size = 20)) +
        scale_x_continuous(limits = c(0, steps_1 + steps_2)) +
        scale_y_continuous(breaks = seq(0, mod_par$n_species, by = 10), limits = c(0, mod_par$n_species))

      p3 <- ggplot(abundance_df, aes(x = time, y = value)) +
        geom_line() +
        theme_minimal() +
        ggtitle("Abundance Over Time") +
        theme(axis.title.y = element_blank(), plot.title = element_text(size = 20)) +
        scale_x_continuous(limits = c(0, steps_1 + steps_2)) +
        scale_y_continuous(breaks = seq(0, 8000, by = 1000), limits = c(0, 8000))

      layout <- rbind(
        c(1, 1, NA),
        c(1, 1, 2),
        c(1, 1, 2),
        c(1, 1, NA)
      )
      # Arrange the plots
      p <- grid.arrange(p1, p2, layout_matrix = layout, heights = c(.1, .4, .4, .1), widths = c(0.275, 0.275, 0.45))

      # Save the plot
      ggsave(p, file = paste("Outputs/animation/", sprintf("%04d", i), "_animation_plot.png", sep = ""), width = 18, height = 9)
    }
    # record agents at the end of the spin-up phase for debugging purposes
    if (i == steps_1) {
      ag_list <- agents
      sp_par <- species_par
    }
  }
  # randomize again for fragmentation
  if (switch$random_post_frag == 0) {
    set.seed(NULL)
  } else {
    set.seed(seed)
  }

  # cookie-cut the landscape
  fragmanted_space <- cookie_cutting(grid, agents, agents_grid, var_par$hab, var_par$frag)

  grid <- fragmanted_space$grid
  agents <- fragmanted_space$agents
  agents_grid <- fragmanted_space$agents_grid

  if (switch$print_agents == 1) {
    print("FRAGMENTATION")
  }

  # choose habitat cells to sample
  grid_values <- getValues(grid)
  possible_cells <- which(!is.na(grid_values))

  # choose between all cells or number of samples according to sample parameter
  if (switch$sample_all == 1) {
    samples <- possible_cells
  } else if (switch$sample_all == 0) {
    samples <- sample(possible_cells, mod_par$n_samples)
  } else {
    print("check switches please")
  }
  # clumping raster to get the number of patches later
  clumped <- clump(grid, direction = 4, gaps = F)
  # creating a frequency table from the patches raster
  clump_freq <- na.omit(freq(clumped))

  for (j in 1:steps_2) {
    start.time <- Sys.time()
    # an if statement to record data only at last time step for optimization - delete when all data needed

    # if (j == steps_2) { # a loop to increase simulation speed in case results are needed only for the last time step

    # function that returns a vector with species presence data for the landscape
    species_count_vec <- sapply(
      species_sequence, function(sp) {
        sum(agents$species_id == sp)
      }
    )

    # creating a vector with abundance value for each patch
    abundance_patch <- sapply(clump_freq[, 1], function(id) {
      sum(agents$patch_id == id, na.rm = T)
    })

    # creating a vector with number of species for each patch
    n_spec_patch <- sapply(clump_freq[, 1], function(id) {
      length(unique(agents$species_id[agents$patch_id == id]))
    })

    # calculating average diversity across all patches
    # a function to create a diversity vectors with diversity values of all patches
    diversity_vector <- sapply(clump_freq[, 1], function(cl) {
      single_patch <- sapply(species_sequence, function(sp) {
        sum(agents$species_id[agents$patch_id == cl] == sp, na.rm = T)
      })

      vegan::diversity(single_patch)
    })

    # vector of abundance data for all samples

    abundance_sample <- sapply(samples, function(sm) {
      xyloc <- rowColFromCell(grid, sm)
      sum(agents$x_loc == xyloc[1] & agents$y_loc == xyloc[2])
    })

    # vector for richness data for all samples

    richness_sample <- sapply(samples, function(sm) {
      xyloc <- rowColFromCell(grid, sm)
      length(unique(agents$species_id[agents$x_loc == xyloc[1] & agents$y_loc == xyloc[2]]))
    })

    # vector of shannon diversity for each sample

    diversity_vector_sample <- sapply(samples, function(sm) {
      single_sample <- sapply(species_sequence, function(sp) {
        xyloc <- rowColFromCell(grid, sm)
        sum(agents$species_id[agents$x_loc == xyloc[1] & agents$y_loc == xyloc[2]] == sp, na.rm = T)
      })

      vegan::diversity(single_sample)
    })

    # recording data in the results df
    output_all <- tibble::add_row(output_all,
      sim_id = sim_id,
      step = j + steps_1,
      individuals = nrow(agents),
      present_species = length(unique(agents$species_id)),
      shannon = vegan::diversity(species_count_vec, index = "shannon"),
      patch_individuals_mean = mean(abundance_patch),
      patch_species_mean = mean(n_spec_patch),
      patch_shannon_mean = mean(diversity_vector),
      sample_individuals_mean = mean(abundance_sample),
      sample_species_mean = mean(richness_sample),
      sample_shannon_mean = mean(diversity_vector_sample),
      habitat = var_par$hab,
      fragmentation = var_par$frag,
      n_patches = clumped@data@max,
      n_samples = length(samples),
      within_patch_dispersal = wp, # number of successful within patch dispersal events
      between_patch_dispersal = bp # number of successful between patch dispersal events
    )
    # recording data in the dispersal results file
    if (switch$species_specific_disp == 1) {
      # get a vector of unique dispersal distances
      unique_dispersal_distances <- species_par$dispersal_distance[species_par$species_id %in% unique(agents$species_id)]

      for (d in unique_dispersal_distances) {
        output_dispersal <- tibble::add_row(output_dispersal,
          sim_id = sim_id,
          step = j + steps_1,
          species_dispersal_distance = d,
          individuals = nrow(agents[agents$species_id %in% species_par$species_id[species_par$dispersal_distance == d]]),
          present_species = length(unique(agents$species_id[agents$species_id %in% species_par$species_id[species_par$dispersal_distance == d]])),
          species_id = first(species_par$species_id[species_par$dispersal_distance == d]),
          habitat = var_par$hab,
          fragmentation = var_par$frag
        )
      }
    }
    # recording data in the sample output data frame at first time step after fragmentation and at the last time step
    if (j == 1 | j == steps_2) {
      # go through all samples 1 by 1
      for (l in 1:length(samples)) {
        xyloc <- rowColFromCell(grid, samples[l]) # get sample coordinates
        sample_patch_id <- clumped[samples[l]] # get the sample patch id

        # create a presence vector for sample l and for all species
        species_presence_vec <- sapply(
          species_sequence, function(sp) {
            sum(agents$species_id == sp & agents$x_loc == xyloc[1] & agents$y_loc == xyloc[2])
          }
        )
        # add all data that will go into "output_sample" in 1 row as a vector. Reason for vector instead of df row is the changing number of species between simulations
        new_row <- c(
          sim_id,
          clumped[xyloc],
          l,
          j + steps_1,
          xyloc[1],
          xyloc[2],
          clump_freq[sample_patch_id, 2],
          var_par$frag,
          var_par$hab,
          species_presence_vec
        )

        output_sample <- rbind(output_sample, new_row)
      }
    }
    if (nrow(agents) > 0) {
      # run birth function and
      for (b in 1:mod_par$n_birth) {
        step1 <- birth(
          agents = agents,
          agents_grid = agents_grid,
          grid = grid,
          NB = var_par$nb,
          disp = var_par$disp,
          d_dis = var_par$disp_dist
        )
        agents <- step1$agents
        agents_grid <- step1$agents_grid
      }
      wp <- step1$within_patch
      bp <- step1$between_patch

      # run death function and update agents list and grid
      step2 <- death(
        agents = agents,
        agents_grid = agents_grid,
        grid = grid,
        edge_fac = var_par$edge
      )
      agents <- step2$agents
      agents_grid <- step2$agents_grid

      # run immigration function and update agent list and grid
      if (switch$immigration == 1) {
        step3 <- immigration(agents, agents_grid, grid)
        agents <- step3$agents
        agents_grid <- step3$agents_grid
      }

      # run disturbance function and update agent list and grid
      if (switch$disturbance == 1) {
        step4 <- disturbance(agents, grid, var_par$disturbance)
        agents <- step4$agents
      }

      # run environment variation function and update the grid
      if (switch$env_var == 1) {
        agents_grid <- environment_variation(grid)
      }
    } else {
      print("ALL DEAD :(")
    }

    n_agents <- nrow(agents)
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    if (switch$print_agents == 1) {
      print(paste("step ", j + steps_1, " took ", time.taken, " with ", n_agents, " agents", sep = ""))
    }

    if (switch$animation_export == 1) {
      # update abundance and richness data frames
      richness_df <- rbind(richness_df, data.frame(time = j + steps_1, value = length(unique(agents$species_id))))
      abundance_df <- rbind(abundance_df, data.frame(time = j + steps_1, value = nrow(agents)))

      flipped <- t(flip(grid, 1))
      flipped[is.na(flipped)] <- 0

      grid_df <- as.data.frame(rasterToPoints(flipped))

      # Rename the columns to 'x', 'y', and 'value'
      names(grid_df) <- c("x", "y", "value")

      # agents_plot <- agents %>%
      #   group_by(x_loc, y_loc) %>%
      #   summarise(n = n())

      # agents_plot <- agents %>%
      #   group_by(x_loc, y_loc) %>%
      #   summarise(n = n_distinct(species_id))

      agents_plot <- agents %>%
        group_by(x_loc, y_loc, species_id) %>%
        count() %>%
        group_by(x_loc, y_loc) %>%
        slice_max(n, n = 1)


      p1 <- ggplot() +
        geom_raster(data = grid_df, aes(x = x, y = y, fill = value)) +
        geom_point(data = agents_plot, aes(x = x_loc - 0.5, y = y_loc - 0.5, color = species_id), size = 3.5) +
        scale_fill_gradient(low = "white", high = "black") +
        scale_color_viridis(option = "turbo") +
        theme_minimal() +
        ggtitle(paste0("Time ", strrep("•", i), "|", strrep("•", j))) + # replace 'i' with red dots
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(size = 25)) +
        theme(legend.position = "none")

      # Assuming 'richness' and 'abundance' are data frames with columns 'time' and 'value'
      p2 <- ggplot(richness_df, aes(x = time, y = value)) +
        geom_line(size = 3) +
        theme_minimal() +
        ggtitle("Species Richness Over Time") +
        theme(axis.title.y = element_blank(), plot.title = element_text(size = 20)) +
        scale_x_continuous(limits = c(0, steps_1 + steps_2)) +
        scale_y_continuous(breaks = seq(0, mod_par$n_species, by = 10), limits = c(0, mod_par$n_species))

      p3 <- ggplot(abundance_df, aes(x = time, y = value)) +
        geom_line() +
        theme_minimal() +
        ggtitle("Abundance Over Time") +
        theme(axis.title.y = element_blank(), plot.title = element_text(size = 20)) +
        scale_x_continuous(limits = c(0, steps_1 + steps_2)) +
        scale_y_continuous(breaks = seq(0, 8000, by = 1000), limits = c(0, 8000))

      layout <- rbind(
        c(1, 1, NA),
        c(1, 1, 2),
        c(1, 1, 2),
        c(1, 1, NA)
      )
      # Arrange the plots
      p <- grid.arrange(p1, p2, layout_matrix = layout, heights = c(.1, .4, .4, .1), widths = c(0.275, 0.275, 0.45))

      # Save the plot
      ggsave(p, file = paste("Outputs/animation/", sprintf("%04d", j + steps_1), "_animation_plot.png", sep = ""), width = 18, height = 9)
    }
  }

  print(paste(" simulation ", sim_id, " is completed", sep = ""))

  end.time.sim <- Sys.time()
  time.taken.sim <- round(end.time.sim - start.time.sim, 2)
  print(paste(" and it took ", time.taken.sim, sep = ""))

  # prepare grid raster for export

  grid_sam <- grid
  grid_sam[grid_sam] <- NA
  grid_sam[samples] <- 2

  # save raster of simulation space and of samples

  if (switch$export_raster == 1) {
    writeRaster(grid, paste("Outputs/", file_name, "_sim_", sim_id, "_sim_space_raster.grd", sep = ""))
    writeRaster(grid_sam, paste("Outputs/", file_name, "_sim_", sim_id, "_samples_raster.grd", sep = ""))
  } else if (switch$export_raster == 0) {

  } else {
    print("check switches please")
  }


  # Write output ------------------------------------------------------------

  # remove 1st row of samples df
  output_sample <- output_sample[-1, ]

  list_of_outputs <- list(
    output_all = output_all,
    output_sample = output_sample,
    output_dispersal = output_dispersal,
    grid = exp_init_grid,
    agents = ag_list,
    species_par = sp_par
  )

  return(list_of_outputs)
}
