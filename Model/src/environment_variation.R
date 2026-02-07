######################## environmnet variation function ##############################

# The function takes the existing simulation grid and alters the environmental values of each cell
# to simulate environmental variation.

environment_variation <- function(grid) {
    # create a new grid to store the altered environmental values
    new_grid <- grid

    # loop through each cell in the grid
    for (i in seq_len(dim(grid)[1])) {
        for (j in seq_len(dim(grid)[2])) {
            # if the cell is a habitat cell, alter the environmental value
            if (!is.na(grid[i, j])) {
                # alter the environmental value by a random amount
                new_grid[i, j] <- grid[i, j] + rnorm(1, mean = 0, sd = 0.01)

                # ensure the environmental value is within the range [0.001, 1]
                if (new_grid[i, j] < 0.001) {
                    new_grid[i, j] <- 0.001
                } else if (new_grid[i, j] > 1) {
                    new_grid[i, j] <- 1
                }
            }
        }
    }

    # return the new grid
    return(new_grid)
}
