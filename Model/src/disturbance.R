# A function to simulate a disturbance event

disturbance <- function(agents, grid, dis_r) {
    # 25% chance of disturbance
    if (runif(1, 0, 1) > 0.75) {
        # get a list of all habitat cells
        grid_values <- getValues(grid)
        possible_cells <- which(!is.na(grid_values))

        # choose cells to be disturbed
        disturbed_cells <- sample(possible_cells, 0.03 * length(possible_cells))
        newly_burned <- disturbed_cells

        # spread disturbance to neighboring cells

        while (length(newly_burned) > 0) {
            # get neighbors of the first cell in newly_burned
            neighbors <- adjacent(grid, cells = newly_burned[1], directions = 4, pairs = FALSE)

            # add the cells to newly_burned with a 10% chance
            for (i in 1:length(neighbors)) {
                # checking the cell is not already disturbed and is a habitat cell
                if (!neighbors[i] %in% disturbed_cells && neighbors[i] %in% possible_cells && runif(1, 0, 1) < dis_r) {
                    newly_burned <- c(newly_burned, neighbors[i])
                    disturbed_cells <- c(disturbed_cells, neighbors[i])
                }
            }

            newly_burned <- newly_burned[-1]
        }


        # get XY loc of disturbed cells
        agents_cells <- cellFromRowCol(grid, agents$x_loc, agents$y_loc)

        # find all agents where location matches xy
        agents_to_remove <- which(agents_cells %in% disturbed_cells)

        # remove agents from the list
        agents <- agents[-agents_to_remove, ]
    }
    return_list <- list(agents = agents)
    return(return_list)
}


# plotting!!!
# model_start <- initialize(
#     frag = 0.5,
#     hab = 1,
#     ac = 0.001,
#     nb = 0.1
# )

# # extract simulation space, agents grid and agents list from the results list
# grid <- model_start$grid
# agents_grid <- model_start$agents_grid
# agents <- model_start$agents

# fragmanted_space <- cookie_cutting(grid, agents, agents_grid, 0.2, 0.8)

# grid <- fragmanted_space$grid
# agents <- fragmanted_space$agents
# agents_grid <- fragmanted_space$agents_grid


# griddis <- grid
# griddis[griddis] <- NA
# griddis[disturbed_cells] <- 1
# plot(grid)
# plot(griddis, add = TRUE, col = "red")
