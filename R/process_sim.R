# Function to read and process data
library(dplyr)

process_simulation_data <- function(sim_nm) {
    path <- paste0("data-raw/", sim_nm)
    data_dis <- list.files(path = path, pattern = "output_dispersal", full.names = TRUE)
    data_var <- list.files(path = path, pattern = "varaying_parameters", full.names = TRUE)
    data_all <- list.files(path = path, pattern = "output_general", full.names = TRUE)
    static_params <- read.csv(list.files(path = path, pattern = "static_parameters", full.names = TRUE))

    data_all1 <- bind_rows(lapply(seq_along(data_all), function(i) {
        df <- read.csv(data_all[i], header = TRUE)
        df$repetition <- i
        return(df)
    }))

    var_param <- read.csv(data_var) %>% rename(sim_id = sim_ID)
    steps <- c(static_params$V1[static_params$X == "steps_pre_frag"], static_params$V1[static_params$X == "steps_post_frag"] + static_params$V1[static_params$X == "steps_pre_frag"])

    all_cwm_data <- bind_rows(lapply(seq_along(data_dis), function(i) {
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
            left_join(var_param %>% select(sim_id, ac, frag), by = "sim_id")
        print(paste0("Processing simulation ", i, " of ", length(data_dis)))
        return(data_cwm)
    }))

    data_all1 <- data_all1 %>%
        left_join(all_cwm_data %>% select(step, sim_id, repetition, cwm, sd_dispersal, ac, frag), by = c("step", "sim_id", "repetition")) %>%
        filter(step == steps[2] - 1) %>%
        select(sim_id, step, present_species, ac, habitat, fragmentation, repetition, cwm, sd_dispersal) %>%
        group_by(repetition, sim_id)


    data_all1 <- data_all1 %>%
        left_join(var_param %>% select(sim_id, disturbance), by = "sim_id")

    return(data_all1)
}

