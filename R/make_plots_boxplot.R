# Function to create boxplots for the last time step (e.g., step 1000) with data points for all replicates
library(ggplot2)
library(dplyr)
library(gtools)
library(viridis)

make_plots_boxplot_fig2 <- function(sim_nm, title) {
    # set path to data
    path <- paste0("data-raw/", sim_nm)
    
    print(paste("Processing simulation", sim_nm, "for boxplots..."))

    # Read data
    data_dis <- list.files(path = path, pattern = "output_dispersal", full.names = TRUE)
    data_var <- list.files(path = path, pattern = "varaying_parameters", full.names = TRUE)
    data_all <- list.files(path = path, pattern = "output_general", full.names = TRUE)
    static_params <- read.csv(list.files(path = path, pattern = "static_parameters", full.names = TRUE))

    data_dis <- mixedsort(data_dis)
    var_param <- read.csv(data_var)
    var_param <- var_param %>% rename(sim_id = sim_ID)
    if (!"disturbance" %in% colnames(var_param)) {
        var_param$disturbance <- NA
    }

    print(paste("Found", length(data_dis), "data files to process"))

    # Gather CWM and SD dispersal data
    all_cwm_data <- data.frame()
    for (i in seq_along(data_dis)) {
        if (i %% 10 == 0) print(paste("Processing file", i, "of", length(data_dis)))
        data_dis1 <- read.csv(data_dis[i])
        data_dis1$repetition <- i
        data_cwm <- data_dis1 %>%
            group_by(step, sim_id, repetition) %>%
            summarise(
                cwm = weighted.mean(species_dispersal_distance, individuals),
                sd_dispersal = sd(species_dispersal_distance),
                .groups = "drop"
            ) %>%
            ungroup() %>%
            left_join(var_param %>% select(sim_id, ac, frag, hab, disturbance), by = "sim_id")
        all_cwm_data <- bind_rows(all_cwm_data, data_cwm)
    }

    print("Finished processing CWM and SD dispersal data")

    # Gather richness data
    all_richness_data <- data.frame()
    for (i in seq_along(data_all)) {
        if (i %% 10 == 0) print(paste("Processing richness file", i, "of", length(data_all)))
        data_all1 <- read.csv(data_all[i])
        data_all1$repetition <- i
        data_all1 <- data_all1 %>%
            left_join(var_param %>% select(sim_id, ac, frag, hab, disturbance), by = "sim_id")
        all_richness_data <- bind_rows(all_richness_data, data_all1)
    }

    print("Finished processing richness data")

    # Detect varying parameter for x axis
    if (n_distinct(var_param$disturbance) > 1) {
        var_param_x <- "disturbance"
        x_title <- "Disturbance"
    } else if (n_distinct(var_param$ac) > 1) {
        var_param_x <- "ac"
        x_title <- "Landscape Autocorrelation"
    } else if (n_distinct(var_param$hab) > 1) {
        var_param_x <- "hab"
        x_title <- "Habitat Amount"
    } else if (n_distinct(var_param$frag) > 1) {
        var_param_x <- "frag"
        x_title <- "Fragmentation Per Se"
    } else {
        var_param_x <- NULL
        x_title <- NULL
        print("No varying parameter found or something else went wrong")
    }

    # Filter for last time step (e.g., step 1000)
    last_step <- 1000
    cwm_last <- all_cwm_data %>% filter(step == last_step)
    richness_last <- all_richness_data %>% filter(step == last_step)

    # Ensure x variable is factor for plotting
    if (!is.null(var_param_x)) {
        cwm_last[[var_param_x]] <- as.factor(cwm_last[[var_param_x]])
        richness_last[[var_param_x]] <- as.factor(richness_last[[var_param_x]])
    }

    title_prefix <- if (!is.null(title) && nzchar(title)) paste0(title, " - ") else ""

    # Boxplot for CWM
    boxplot_cwm <- ggplot(cwm_last, aes(x = .data[[var_param_x]], y = cwm)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray70") +
        labs(
            title = paste0(title_prefix, "CWM at last time step"),
            x = x_title,
            y = "CWMDD"
        ) +
        theme_bw() +
        theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5)
        )
    
    print("Created CWM boxplot (no points)")

    # Boxplot for Species Richness
    boxplot_richness <- ggplot(richness_last, aes(x = .data[[var_param_x]], y = present_species)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray70") +
        labs(
            title = paste0(title_prefix, "Species richness at last time step"),
            x = x_title,
            y = "Richness"
        ) +
        theme_bw() +
        theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5)
        )
    
    print("Created Richness boxplot (no points)")

    # Boxplot for SD Dispersal
    boxplot_sd_dispersal <- ggplot(cwm_last, aes(x = .data[[var_param_x]], y = sd_dispersal)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray70") +
        labs(
            title = paste0(title_prefix, "SD dispersal at last time step"),
            x = x_title,
            y = "SDDD"
        ) +
        theme_bw() +
        theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5)
        )
    
    print("Created SD dispersal boxplot (no points)")

    # Return plots
    plots_list <- list(
        boxplot_cwm = boxplot_cwm,
        boxplot_richness = boxplot_richness,
        boxplot_sd_dispersal = boxplot_sd_dispersal
    )
    
    print(paste("Completed boxplot generation for simulation", sim_nm))
    return(plots_list)
}

