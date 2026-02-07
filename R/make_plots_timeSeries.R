# a function that returns the time series plots for a given simulation
library(knitr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(grid)
library(viridis)
library(gridExtra)
library(patchwork)
library(gtools)

# Okabe-Ito palette
okabe_ito_palette <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

make_plots_ts_n <- function(sim_nm, title, loggg) {
    # set path to data
    path <- paste0("data-raw/", sim_nm)

    # Read data
    data_dis <- list.files(path = path, pattern = "output_dispersal", full.names = TRUE)
    data_var <- list.files(path = path, pattern = "varaying_parameters", full.names = TRUE)
    data_all <- list.files(path = path, pattern = "output_general", full.names = TRUE)
    static_params <- read.csv(list.files(path = path, pattern = "static_parameters", full.names = TRUE))

    data_dis <- mixedsort(data_dis)

    # Read varying parameters
    var_param <- read.csv(data_var)

    # Rename sim_ID to sim_id
    var_param <- var_param %>% rename(sim_id = sim_ID)

    if (!"disturbance" %in% colnames(var_param)) {
        var_param$disturbance <- NA
    }

    # Initialize a data frame to store all CWM data
    all_cwm_data <- data.frame()

    # Loop through each file in data_dis and add a unique identifier
    for (i in seq_along(data_dis)) {
        data_dis1 <- read.csv(data_dis[i])
        data_dis1$repetition <- i

        # Calculate community weighted mean and standard deviation
        data_cwm <- data_dis1 %>%
            group_by(step, sim_id, repetition) %>%
            summarise(
                cwm = weighted.mean(species_dispersal_distance, individuals),
                sd_dispersal = sd(species_dispersal_distance),
                .groups = "drop"
            ) %>%
            ungroup()

        # Add columns 'ac' and 'frag' and 'disturbance' from var_param to data_dis1 by matching sim_id
        data_cwm <- data_cwm %>%
            left_join(var_param %>% select(sim_id, ac, frag, hab, disturbance), by = "sim_id")

        # Combine the data
        all_cwm_data <- bind_rows(all_cwm_data, data_cwm)
    }

    # Get the richness values for all simulations
    all_richness_data <- data.frame()

    for (i in seq_along(data_all)) {
        # add unique identifier
        data_all1 <- read.csv(data_all[i])
        data_all1$repetition <- i

        # add columns 'ac' and 'frag' and 'disturbance' from var_param to data_all1 by matching sim_id
        data_all1 <- data_all1 %>%
            left_join(var_param %>% select(sim_id, ac, frag, hab, disturbance), by = "sim_id")
        # combine data
        all_richness_data <- bind_rows(all_richness_data, data_all1)
    }

    # Get the varying parameters for time series plot
    if (n_distinct(var_param$disturbance) > 1) {
        var_param_ts <- "disturbance"
    } else if (n_distinct(var_param$ac) > 1) {
        var_param_ts <- "ac"
    } else if (n_distinct(var_param$hab) > 1) {
        var_param_ts <- "hab"
    } else if (n_distinct(var_param$frag) > 1) {
        var_param_ts <- "frag"
    } else {
        var_param_ts <- NULL
        print("No varying parameter found or something else went wrong")
    }

    # Get average of cwm and sd_dispersal for all reps in each simulation
    all_cwm_data_avg <- all_cwm_data %>%
        group_by(step, across(all_of(var_param_ts))) %>%
        summarise(
            cwm = mean(cwm),
            sd_dispersal = mean(sd_dispersal),
            .groups = "drop"
        )
    # Get average of richness for all reps in each simulation
    all_richness_data_avg <- all_richness_data %>%
        group_by(step, across(all_of(var_param_ts))) %>%
        summarise(richness = mean(present_species), .groups = "drop")

    # remove last time step as it is not complete
    all_cwm_data_avg <- all_cwm_data_avg %>% filter(step != max(step))
    all_richness_data_avg <- all_richness_data_avg %>% filter(step != max(step))

    # legend_title
    if (var_param_ts == "disturbance") {
        legend_title <- "Disturbance"
    } else if (var_param_ts == "ac") {
        legend_title <- "Landscape\nAutocorrelation"
    } else if (var_param_ts == "hab") {
        legend_title <- "Habitat\nAmount"
    } else if (var_param_ts == "frag") {
        legend_title <- "Fragmentation\nPer Se"
    } else {
        legend_title <- NULL
    }
    print(legend_title)

    if (loggg == FALSE) {
        # Time series plot (figure 1)
        time_series_plot <- ggplot(all_cwm_data_avg, aes(x = step, y = cwm, color = factor(.data[[var_param_ts]]))) +
            geom_line(linewidth = 1.2) +
            labs(
                title = title,
                x = "Time Step",
                y = "Community Weighted Mean (CWM)",
                color = legend_title
            ) +
            scale_color_manual(values = okabe_ito_palette) +
            theme_minimal() +
            theme(legend.position = "none")

        # time series of Richness plot
        time_series_plot_richness <- ggplot(all_richness_data_avg, aes(x = step, y = richness, color = factor(.data[[var_param_ts]]))) +
            geom_line(linewidth = 1.2) +
            labs(
                title = NULL,
                x = "Time Step",
                y = "Species Richness",
                color = legend_title
            ) +
            scale_color_manual(values = okabe_ito_palette) +
            theme_minimal() +
            theme(legend.position = "none")

        # time series of standard deviation of dispersal distance
        time_series_plot_sd_dispersal <- ggplot(all_cwm_data_avg, aes(x = step, y = sd_dispersal, color = factor(.data[[var_param_ts]]))) +
            geom_line(linewidth = 1.2) +
            labs(
                title = NULL,
                x = "Time Step",
                y = "Standard Deviation of Dispersal Distance",
                color = legend_title
            ) +
            scale_color_manual(values = okabe_ito_palette) +
            theme_minimal()
    }

    if (loggg == TRUE) {
        # Time series plot (figure 1)
        time_series_plot <- ggplot(all_cwm_data_avg, aes(x = step, y = cwm, color = factor(.data[[var_param_ts]]))) +
            geom_line(linewidth = 1.2) +
            labs(
                title = title,
                x = "Time Step",
                y = "Community Weighted Mean (CWM) (log10)",
                color = legend_title
            ) +
            scale_color_manual(values = okabe_ito_palette) +
            scale_y_log10() + # Log-transform the y-axis
            theme_minimal() +
            theme(legend.position = "none")

        # Time series of Richness plot
        time_series_plot_richness <- ggplot(all_richness_data_avg, aes(x = step, y = richness, color = factor(.data[[var_param_ts]]))) +
            geom_line(linewidth = 1.2) +
            labs(
                title = NULL,
                x = "Time Step",
                y = "Species Richness (log10)",
                color = legend_title
            ) +
            scale_color_manual(values = okabe_ito_palette) +
            scale_y_log10() + # Log-transform the y-axis
            theme_minimal() +
            theme(legend.position = "none")

        # Time series of standard deviation of dispersal distance
        time_series_plot_sd_dispersal <- ggplot(all_cwm_data_avg, aes(x = step, y = sd_dispersal, color = factor(.data[[var_param_ts]]))) +
            geom_line(linewidth = 1.2) +
            labs(
                title = NULL,
                x = "Time Step",
                y = "Standard Deviation of Dispersal Distance (log10)",
                color = legend_title
            ) +
            scale_color_manual(values = okabe_ito_palette) +
            scale_y_log10() + # Log-transform the y-axis
            theme_minimal()
    }

    # return plots
    plots_list <- list(ts = time_series_plot, ts_richness = time_series_plot_richness, ts_sd_dispersal = time_series_plot_sd_dispersal)
    return(plots_list)
}

