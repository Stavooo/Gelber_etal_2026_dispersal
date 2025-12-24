######################## Dispersal function ########################################

# The function takes the current location of an individual and returns a new location
# based on a log-normal dispersal kernel or an exponential dispersal kernel or a fixed distance dispersal kernel

disperse <- function(cur_loc, d_sd, d_mean) {
  dis_sd <- d_sd
  dis_mean <- d_mean

  if (switch$kernel_type == 0) {
    sigma <- sqrt(log(1 + (dis_sd * dis_sd) / (dis_mean * dis_mean)))
    mu <- log(dis_mean) - 0.5 * sigma * sigma

    distance <- rlnorm(1, mu, sigma)
    direction <- runif(1, min = 0, max = 2 * pi)

    new_x <- cur_loc[1] + round(cos(direction) * distance)
    new_y <- cur_loc[2] + round(sin(direction) * distance)
  } else if (switch$kernel_type == 1) {
    distance <- rexp(1, 1 / dis_mean)
    direction <- runif(1, min = 0, max = 2 * pi)

    new_x <- cur_loc[1] + round(cos(direction) * distance)
    new_y <- cur_loc[2] + round(sin(direction) * distance)
  } else if (switch$kernel_type == 2) {
    distance_x <- sample(-(dis_mean):dis_mean, 1, replace = TRUE)
    distance_y <- sample(-(dis_mean):dis_mean, 1, replace = TRUE)
    new_x <- cur_loc[1] + (distance_x)
    new_y <- cur_loc[2] + (distance_y)
  } else {
    print("please check switches")
  }



  if (switch$torus == 0) {
    new_loc <- c(round(new_x), round(new_y))
  } else if (switch$torus == 1) {
    new_loc <- c((round(new_x) + mod_par$grid_size - 1) %% mod_par$grid_size + 1, (round(new_y) + mod_par$grid_size - 1) %% mod_par$grid_size + 1)
  } else {
    print("please check switches (torus)")
  }

  return(new_loc)
}
