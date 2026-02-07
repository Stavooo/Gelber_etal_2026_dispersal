# Generate Figures for:
# Gelber, Tietjen & May (2026)
# Disturbance and landscape characteristics interactively drive
# dispersal strategies in continuous and fragmented metacommunities

# This script is used to plot the figure for the dispersal manuscript

# Load the required functions
source("R/make_plots_timeSeries.R")
source("R/make_plots_boxplot.R")
source("R/process_sim.R")

# Load required libraries

library(ggplot2)
library(patchwork)
library(ggdist)


# ===== Figure 2: Boxplots of single-factor effects =====


# Panel 1: varying autocorrelation in continuous landscape
sim_nm <- 96
title <- "Varying autocorrelation in continuous landscape"
plots1 <- make_plots_boxplot_fig2(sim_nm, title)
# Shorten the middle plot title
plots1$boxplot_richness <- plots1$boxplot_richness + labs(title = "Varying autocorrelation in continuous landscape")

# Panel 2: varying disturbance in continuous landscape
sim_nm <- 97
title <- "Varying disturbance in continuous landscape"
plots2 <- make_plots_boxplot_fig2(sim_nm, title)
# Shorten the middle plot title
plots2$boxplot_richness <- plots2$boxplot_richness + labs(title = "Varying disturbance in continuous landscape")

# Panel 3: varying fragmentation level in modified landscape
sim_nm <- 99
title <- "Varying fragmentation in modified landscape"
plots3 <- make_plots_boxplot_fig2(sim_nm, title)
# Shorten the middle plot title
plots3$boxplot_richness <- plots3$boxplot_richness + labs(title = "Varying fragmentation in modified landscape")

# Panel 4: varying habitat amount in modified landscape
sim_nm <- 100
title <- "Varying habitat amount in modified landscape"
plots4 <- make_plots_boxplot_fig2(sim_nm, title)
# Shorten the middle plot title
plots4$boxplot_richness <- plots4$boxplot_richness + labs(title = "Varying habitat amount in modified landscape")

# Combine rows - keep title on middle plot only, remove from first and third
combined_plot_1b <- 
    (plots1$boxplot_cwm + labs(title = NULL)) +
    plots1$boxplot_richness +  # Keep title on middle
    (plots1$boxplot_sd_dispersal + labs(title = NULL)) +
    plot_layout(nrow = 1)

combined_plot_2b <- 
    (plots2$boxplot_cwm + labs(title = NULL)) +
    plots2$boxplot_richness +  # Keep title on middle
    (plots2$boxplot_sd_dispersal + labs(title = NULL)) +
    plot_layout(nrow = 1)

combined_plot_3b <- 
    (plots3$boxplot_cwm + labs(title = NULL)) +
    plots3$boxplot_richness +  # Keep title on middle
    (plots3$boxplot_sd_dispersal + labs(title = NULL)) +
    plot_layout(nrow = 1)

combined_plot_4b <- 
    (plots4$boxplot_cwm + labs(title = NULL)) +
    plots4$boxplot_richness +  # Keep title on middle
    (plots4$boxplot_sd_dispersal + labs(title = NULL)) +
    plot_layout(nrow = 1)

# Merge all panels for final figure with panel tags a) through l)
combined_plot_b <-
    combined_plot_1b /
    combined_plot_2b /
    combined_plot_3b /
    combined_plot_4b +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = list(paste0(letters[1:12], ")")))

# Style tags to avoid collision with titles
combined_plot_b <- combined_plot_b & theme(
    plot.tag = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0.02, 0.88),  # Unified tag position (left, slightly lower)
    plot.margin = margin(t = 5, r = 5, b = 10, l = 5)  # Smaller top margin to preserve panel proportions
)
ggsave("R/figures/fig2.png", plot = combined_plot_b, width = 14, height = 18, bg = "white")

# ===== Figure 3: Autocorrelation x Disturbance (continuous landscape) =====

data_87 <- process_simulation_data(102)

# get the reference level for continuous landscape from sim 78 (autocorrelation = 0.9, disturbance = 0)
ref_lev <- list(
    cwm = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(cwm) %>% mean(),
    richness = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(present_species) %>% mean(),
    sd = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(sd_dispersal) %>% mean()
)

# Left panel: ac on the x-axis and cwm on the y-axis grouped by disturbance
left_panel <- ggplot(data_87, aes(x = as.factor(ac), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() +
    labs(
        x = NULL, # Remove x label
        y = "CWMDD",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(), # Remove x label
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Middle panel: ac on the x-axis and present_species on the y-axis grouped by disturbance
middle_panel <- ggplot(data_87, aes(x = as.factor(ac), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() +
    labs(
        x = "Autocorrelation", # Only middle panel has x label
        y = "Richness",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Right panel: ac on the x-axis and sd_dispersal on the y-axis grouped by disturbance
right_panel <- ggplot(data_87, aes(x = as.factor(ac), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() +
    labs(
        x = NULL, # Remove x label
        y = "SDDD",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(), # Remove x label
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Combine the three panels into a single figure, collect guides and set legend position only once

combined_plot <- left_panel + middle_panel + right_panel +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(
        tag_levels = 'a',
        tag_prefix = "",
        tag_suffix = ")"
    ) &
    theme(
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 14, face = "bold"),
        plot.tag.position = c(0.02, 0.88),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
    )

# Save the combined plot
ggsave("R/figures/fig3.png", plot = combined_plot, width = 15, height = 5, bg = "white")

# Fig 4

# get the reference level for continuous landscape from sim 78 (autocorrelation = 0.9, disturbance = 0)
ref_lev <- list(
    cwm = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(cwm) %>% mean(),
    richness = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(present_species) %>% mean(),
    sd = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(sd_dispersal) %>% mean()
)

data_88 <- process_simulation_data(103)

# Left panel: habitat on the x-axis and cwm on the y-axis grouped by fragmentation
left_panel <- ggplot(data_88, aes(x = as.factor(habitat), y = cwm, fill = as.factor(fragmentation))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev$cwm, linetype = "dashed") +
    labs(
        x = NULL,
        y = "CWMDD",
        fill = "Fragmentation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Middle panel: habitat on the x-axis and present_species on the y-axis grouped by fragmentation
middle_panel <- ggplot(data_88, aes(x = as.factor(habitat), y = present_species, fill = as.factor(fragmentation))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev$richness, linetype = "dashed") +
    labs(
        x = "Habitat Amount",
        y = "Richness",
        fill = "Fragmentation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Right panel: habitat on the x-axis and sd_dispersal on the y-axis grouped by fragmentation
right_panel <- ggplot(data_88, aes(x = as.factor(habitat), y = sd_dispersal, fill = as.factor(fragmentation))) +
    geom_boxplot() +
    geom_hline(
        aes(yintercept = ref_lev$sd, linetype = "Continuous Landscape"),
        color = "black"
    ) +
    labs(
        x = NULL,
        y = "SDDD",
        fill = "Fragmentation",
        linetype = NULL
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous Landscape" = "dashed")) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Combine the three panels into a single figure, collect guides and set legend position only once
combined_plot <- left_panel + middle_panel + right_panel +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(
        tag_levels = 'a',
        tag_prefix = "",
        tag_suffix = ")"
    ) &
    theme(
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 14, face = "bold"),
        plot.tag.position = c(0.07, 0.88),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
    )


        


# Save the combined plot
ggsave("R/figures/fig4.png", plot = combined_plot, width = 15, height = 5, bg = "white")

# Fig 5-7

data_101 <- process_simulation_data(101)

ref_lev_2 <- list(
    cwm1 = data_87 %>% filter(ac == 0.1, disturbance == 0) %>% pull(cwm) %>% mean(),
    cwm2 = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(cwm) %>% mean(),
    cwm3 = data_87 %>% filter(ac == 0.9, disturbance == 0) %>% pull(cwm) %>% mean(),
    richness1 = data_87 %>% filter(ac == 0.1, disturbance == 0) %>% pull(present_species) %>% mean(),
    richness2 = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(present_species) %>% mean(),
    richness3 = data_87 %>% filter(ac == 0.9, disturbance == 0) %>% pull(present_species) %>% mean(),
    sd1 = data_87 %>% filter(ac == 0.1, disturbance == 0) %>% pull(sd_dispersal) %>% mean(),
    sd2 = data_87 %>% filter(ac == 0.5, disturbance == 0) %>% pull(sd_dispersal) %>% mean(),
    sd3 = data_87 %>% filter(ac == 0.9, disturbance == 0) %>% pull(sd_dispersal) %>% mean()
)
# Figure 5a - Remove 'fragmentation' from the plot, use 'disturbance' for fill, and Okabe-Ito palette
plot_3_panels <- ggplot(data_101, aes(x = as.factor(habitat), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() +
    # Add reference lines for each panel using geom_hline and facetting variable, with linetype mapped for legend
    geom_hline(
        data = data.frame(
            ac = c(0.1, 0.5, 0.9),
            ref = c(ref_lev_2$cwm1, ref_lev_2$cwm2, ref_lev_2$cwm3),
            linetype = "Continuous landscape"
        ),
        aes(yintercept = ref, linetype = linetype, group = ac),
        color = "black",
        inherit.aes = FALSE,
        show.legend = TRUE
    ) +
    labs(
        title = "Modified Landscape: Community Weighted Mean",
        x = "Habitat Amount",
        y = "CWMDD",
        fill = "Disturbance",
        linetype = NULL
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous landscape" = "dashed")) +
    theme_bw() +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
    ) +
    facet_grid(
        cols = vars(ac), # Only facet by ac (autocorrelation)
        labeller = labeller(
            ac = c(
                "0.1" = "autocorrelation 0.1",
                "0.5" = "autocorrelation 0.5",
                "0.9" = "autocorrelation 0.9"
            )
        )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

# Save the plot
ggsave("R/figures/fig5.png", plot = plot_3_panels, width = 10, height = 5, bg = "white")

# Figure 5b - ac on the x axis
plot_3_panels_ac_x_habitat_facet <- ggplot(data_101, aes(x = as.factor(ac), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() +
    # Add reference lines for each panel using geom_hline and facetting variable, with linetype mapped for legend
    geom_hline(
        data = data.frame(
            habitat = c(0.1, 0.5, 0.9),
            ref = c(ref_lev_2$cwm1, ref_lev_2$cwm2, ref_lev_2$cwm3),
            linetype = "Continuous landscape"
        ),
        aes(yintercept = ref, linetype = linetype, group = habitat),
        color = "black",
        inherit.aes = FALSE,
        show.legend = TRUE
    ) +
    labs(
        title = "Modified Landscape: Community Weighted Mean",
        x = "Autocorrelation",
        y = "Community Weighted Mean (CWM)",
        fill = "Disturbance",
        linetype = NULL
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous landscape" = "dashed")) +
    theme_bw() +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
    ) +
    facet_grid(
        cols = vars(habitat),
        labeller = labeller(
            habitat = c(
                "0.1" = "habitat 0.1",
                "0.5" = "habitat 0.5",
                "0.9" = "habitat 0.9"
            )
        )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

ggsave("R/figures/fig5b.png", plot = plot_3_panels_ac_x_habitat_facet, width = 10, height = 5, bg = "white")

# Figure 6a - Species Richness (habitat on x-axis, faceted by ac)
plot_3_panels <- ggplot(data_101, aes(x = as.factor(habitat), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() +
    # Add reference lines for each panel using geom_hline and facetting variable, with linetype mapped for legend
    geom_hline(
        data = data.frame(
            ac = c(0.1, 0.5, 0.9),
            ref = c(ref_lev_2$richness1, ref_lev_2$richness2, ref_lev_2$richness3),
            linetype = "Continuous landscape"
        ),
        aes(yintercept = ref, linetype = linetype, group = ac),
        color = "black",
        inherit.aes = FALSE,
        show.legend = TRUE
    ) +
    labs(
        title = "Modified Landscape: Species Richness",
        x = "Habitat Amount",
        y = "Species Richness",
        fill = "Disturbance",
        linetype = NULL
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous landscape" = "dashed")) +
    theme_bw() +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
    ) +
    facet_grid(
        cols = vars(ac), # Only facet by ac (autocorrelation)
        labeller = labeller(
            ac = c(
                "0.1" = "autocorrelation 0.1",
                "0.5" = "autocorrelation 0.5",
                "0.9" = "autocorrelation 0.9"
            )
        )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

# Save the plot
ggsave("R/figures/fig6.png", plot = plot_3_panels, width = 10, height = 5, bg = "white")

# Figure 6b - Species Richness (ac on x-axis, faceted by habitat)
plot_3_panels_ac_x_habitat_facet <- ggplot(data_101, aes(x = as.factor(ac), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() +
    # Add reference lines for each panel using geom_hline and facetting variable, with linetype mapped for legend
    geom_hline(
        data = data.frame(
            habitat = c(0.1, 0.5, 0.9),
            ref = c(ref_lev_2$richness1, ref_lev_2$richness2, ref_lev_2$richness3),
            linetype = "Continuous landscape"
        ),
        aes(yintercept = ref, linetype = linetype, group = habitat),
        color = "black",
        inherit.aes = FALSE,
        show.legend = TRUE
    ) +
    labs(
        title = "Modified Landscape: Species Richness",
        x = "Autocorrelation",
        y = "Species Richness",
        fill = "Disturbance",
        linetype = NULL
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous landscape" = "dashed")) +
    theme_bw() +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
    ) +
    facet_grid(
        cols = vars(habitat),
        labeller = labeller(
            habitat = c(
                "0.1" = "habitat 0.1",
                "0.5" = "habitat 0.5",
                "0.9" = "habitat 0.9"
            )
        )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

# Save the plot
ggsave("R/figures/fig6b.png", plot = plot_3_panels_ac_x_habitat_facet, width = 10, height = 5, bg = "white")

# Figure 7a - Standard Deviation of Dispersal Distance (habitat on x-axis, faceted by ac)
plot_3_panels <- ggplot(data_101, aes(x = as.factor(habitat), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() +
    # Add reference lines for each panel using geom_hline and facetting variable, with linetype mapped for legend
    geom_hline(
        data = data.frame(
            ac = c(0.1, 0.5, 0.9),
            ref = c(ref_lev_2$sd1, ref_lev_2$sd2, ref_lev_2$sd3),
            linetype = "Continuous landscape"
        ),
        aes(yintercept = ref, linetype = linetype, group = ac),
        color = "black",
        inherit.aes = FALSE,
        show.legend = TRUE
    ) +
    labs(
        title = "Modified Landscape: Standard Deviation of Dispersal Distance",
        x = "Habitat Amount",
        y = "SDDD",
        fill = "Disturbance",
        linetype = NULL
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous landscape" = "dashed")) +
    theme_bw() +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
    ) +
    facet_grid(
        cols = vars(ac), # Only facet by ac (autocorrelation)
        labeller = labeller(
            ac = c(
                "0.1" = "autocorrelation 0.1",
                "0.5" = "autocorrelation 0.5",
                "0.9" = "autocorrelation 0.9"
            )
        )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

# Save the plot
ggsave("R/figures/fig7.png", plot = plot_3_panels, width = 10, height = 5, bg = "white")

# Figure 7b - Standard Deviation of Dispersal Distance (ac on x-axis, faceted by habitat)
plot_3_panels_ac_x_habitat_facet <- ggplot(data_101, aes(x = as.factor(ac), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() +
    # Add reference lines for each panel using geom_hline and facetting variable, with linetype mapped for legend
    geom_hline(
        data = data.frame(
            habitat = c(0.1, 0.5, 0.9),
            ref = c(ref_lev_2$sd1, ref_lev_2$sd2, ref_lev_2$sd3),
            linetype = "Continuous landscape"
        ),
        aes(yintercept = ref, linetype = linetype, group = habitat),
        color = "black",
        inherit.aes = FALSE,
        show.legend = TRUE
    ) +
    labs(
        title = "Modified Landscape: Standard Deviation of Dispersal Distance",
        x = "Autocorrelation",
        y = "SDDD",
        fill = "Disturbance",
        linetype = NULL
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_linetype_manual(values = c("Continuous landscape" = "dashed")) +
    theme_bw() +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
    ) +
    facet_grid(
        cols = vars(habitat),
        labeller = labeller(
            habitat = c(
                "0.1" = "habitat 0.1",
                "0.5" = "habitat 0.5",
                "0.9" = "habitat 0.9"
            )
        )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

# Save the plot
ggsave("R/figures/fig7b.png", plot = plot_3_panels_ac_x_habitat_facet, width = 10, height = 5, bg = "white")
# ----- Appendix Figures 1-4: Additional 2-variable combinations -----

# Process simulation 102 data for reference levels (independent from code above)
data_87_appendix <- process_simulation_data(102)

# Get reference level from continuous landscape (sim 102)
ref_lev_app1 <- list(
    cwm = data_87_appendix %>% filter(ac == 0.5, disturbance == 0) %>% pull(cwm) %>% mean(),
    richness = data_87_appendix %>% filter(ac == 0.5, disturbance == 0) %>% pull(present_species) %>% mean(),
    sd = data_87_appendix %>% filter(ac == 0.5, disturbance == 0) %>% pull(sd_dispersal) %>% mean()
)

# Appendix Figure 1: Habitat amount & disturbance (with fixed fragmentation and autocorrelation) - sim 104
data_104 <- process_simulation_data(104)

# Check what values exist in the data
print("Unique values in data_104:")
print(paste("fragmentation values:", paste(unique(data_104$fragmentation), collapse = ", ")))
print(paste("ac values:", paste(unique(data_104$ac), collapse = ", ")))
print(paste("habitat values:", paste(unique(data_104$habitat), collapse = ", ")))
print(paste("disturbance values:", paste(unique(data_104$disturbance), collapse = ", ")))

# Get the middle values for fixed parameters (use actual values from data)
fixed_frag <- unique(data_104$fragmentation)[length(unique(data_104$fragmentation)) %/% 2 + 1]  # Middle value
fixed_ac <- unique(data_104$ac)[length(unique(data_104$ac)) %/% 2 + 1]  # Middle value

print(paste("Using fixed fragmentation =", fixed_frag, "and fixed ac =", fixed_ac))

# Filter for specific fixed values to get 9 parameter combinations
data_104_filtered <- data_104 %>% filter(fragmentation == fixed_frag, ac == fixed_ac)

print(paste("Filtered data_104 has", nrow(data_104_filtered), "rows"))

# Left panel: habitat on x-axis, cwm on y-axis, grouped by disturbance
left_panel_app1 <- ggplot(data_104_filtered, aes(x = as.factor(habitat), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$cwm, linetype = "dashed") +
    labs(
        x = NULL,
        y = "CWMDD",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Middle panel: habitat on x-axis, richness on y-axis, grouped by disturbance
middle_panel_app1 <- ggplot(data_104_filtered, aes(x = as.factor(habitat), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$richness, linetype = "dashed") +
    labs(
        x = "Habitat Amount",
        y = "Richness",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Right panel: habitat on x-axis, sd_dispersal on y-axis, grouped by disturbance
right_panel_app1 <- ggplot(data_104_filtered, aes(x = as.factor(habitat), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$sd, linetype = "dashed") +
    labs(
        x = NULL,
        y = "SDDD",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Combine panels for Appendix Figure 1
appendix_fig1 <- left_panel_app1 + middle_panel_app1 + right_panel_app1 +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(
        tag_levels = 'a',
        tag_prefix = "",
        tag_suffix = ")"
    ) &
    theme(
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 14, face = "bold"),
        plot.tag.position = c(0.07, 0.88),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
    )

# Save Appendix Figure 1
ggsave("R/figures/fig_s1.png", plot = appendix_fig1, width = 15, height = 5, bg = "white")

# Appendix Figure 2: Fragmentation & disturbance (with fixed habitat amount and autocorrelation) - sim 105
data_105 <- process_simulation_data(105)

# Check what values exist in the data
print("Unique values in data_105:")
print(paste("habitat values:", paste(unique(data_105$habitat), collapse = ", ")))
print(paste("ac values:", paste(unique(data_105$ac), collapse = ", ")))
print(paste("fragmentation values:", paste(unique(data_105$fragmentation), collapse = ", ")))
print(paste("disturbance values:", paste(unique(data_105$disturbance), collapse = ", ")))

# Get the middle values for fixed parameters
fixed_habitat <- unique(data_105$habitat)[length(unique(data_105$habitat)) %/% 2 + 1]  # Middle value
fixed_ac_105 <- unique(data_105$ac)[length(unique(data_105$ac)) %/% 2 + 1]  # Middle value

print(paste("Using fixed habitat =", fixed_habitat, "and fixed ac =", fixed_ac_105))

# Filter for specific fixed values to get 9 parameter combinations
data_105_filtered <- data_105 %>% filter(habitat == fixed_habitat, ac == fixed_ac_105)

print(paste("Filtered data_105 has", nrow(data_105_filtered), "rows"))

# Left panel: fragmentation on x-axis, cwm on y-axis, grouped by disturbance
left_panel_app2 <- ggplot(data_105_filtered, aes(x = as.factor(fragmentation), y = cwm, fill = as.factor(disturbance))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$cwm, linetype = "dashed") +
    labs(
        x = NULL,
        y = "CWMDD",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Middle panel: fragmentation on x-axis, richness on y-axis, grouped by disturbance
middle_panel_app2 <- ggplot(data_105_filtered, aes(x = as.factor(fragmentation), y = present_species, fill = as.factor(disturbance))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$richness, linetype = "dashed") +
    labs(
        x = "Fragmentation",
        y = "Richness",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Right panel: fragmentation on x-axis, sd_dispersal on y-axis, grouped by disturbance
right_panel_app2 <- ggplot(data_105_filtered, aes(x = as.factor(fragmentation), y = sd_dispersal, fill = as.factor(disturbance))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$sd, linetype = "dashed") +
    labs(
        x = NULL,
        y = "SDDD",
        fill = "Disturbance"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Combine panels for Appendix Figure 2
appendix_fig2 <- left_panel_app2 + middle_panel_app2 + right_panel_app2 +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(
        tag_levels = 'a',
        tag_prefix = "",
        tag_suffix = ")"
    ) &
    theme(
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 14, face = "bold"),
        plot.tag.position = c(0.07, 0.88),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
    )

# Save Appendix Figure 2
ggsave("R/figures/fig_s2.png", plot = appendix_fig2, width = 15, height = 5, bg = "white")

# Appendix Figure 3: Habitat amount & autocorrelation (without disturbance, fixed fragmentation) - sim 104
# Filter for no disturbance (disturbance = 0) and fixed fragmentation
min_disturbance <- min(data_104$disturbance, na.rm = TRUE)  # Use minimum disturbance value (likely 0)
print(paste("Using minimum disturbance =", min_disturbance, "and fixed fragmentation =", fixed_frag))

data_104_no_dist <- data_104 %>% filter(disturbance == min_disturbance, fragmentation == fixed_frag)

print(paste("Filtered data_104_no_dist has", nrow(data_104_no_dist), "rows"))

# Left panel: habitat on x-axis, cwm on y-axis, grouped by autocorrelation
left_panel_app3 <- ggplot(data_104_no_dist, aes(x = as.factor(habitat), y = cwm, fill = as.factor(ac))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$cwm, linetype = "dashed") +
    labs(
        x = NULL,
        y = "CWMDD",
        fill = "Autocorrelation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Middle panel: habitat on x-axis, richness on y-axis, grouped by autocorrelation
middle_panel_app3 <- ggplot(data_104_no_dist, aes(x = as.factor(habitat), y = present_species, fill = as.factor(ac))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$richness, linetype = "dashed") +
    labs(
        x = "Habitat Amount",
        y = "Richness",
        fill = "Autocorrelation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Right panel: habitat on x-axis, sd_dispersal on y-axis, grouped by autocorrelation
right_panel_app3 <- ggplot(data_104_no_dist, aes(x = as.factor(habitat), y = sd_dispersal, fill = as.factor(ac))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$sd, linetype = "dashed") +
    labs(
        x = NULL,
        y = "SDDD",
        fill = "Autocorrelation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Combine panels for Appendix Figure 3
appendix_fig3 <- left_panel_app3 + middle_panel_app3 + right_panel_app3 +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(
        tag_levels = 'a',
        tag_prefix = "",
        tag_suffix = ")"
    ) &
    theme(
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 14, face = "bold"),
        plot.tag.position = c(0.07, 0.88),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
    )

# Save Appendix Figure 3
ggsave("R/figures/fig_s3.png", plot = appendix_fig3, width = 15, height = 5, bg = "white")

# Appendix Figure 4: Fragmentation & autocorrelation (without disturbance, fixed habitat amount) - sim 105
# Filter for no disturbance and fixed habitat
min_disturbance_105 <- min(data_105$disturbance, na.rm = TRUE)  # Use minimum disturbance value (likely 0)
print(paste("Using minimum disturbance =", min_disturbance_105, "and fixed habitat =", fixed_habitat))

data_105_no_dist <- data_105 %>% filter(disturbance == min_disturbance_105, habitat == fixed_habitat)

print(paste("Filtered data_105_no_dist has", nrow(data_105_no_dist), "rows"))

# Left panel: fragmentation on x-axis, cwm on y-axis, grouped by autocorrelation
left_panel_app4 <- ggplot(data_105_no_dist, aes(x = as.factor(fragmentation), y = cwm, fill = as.factor(ac))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$cwm, linetype = "dashed") +
    labs(
        x = NULL,
        y = "CWMDD",
        fill = "Autocorrelation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Middle panel: fragmentation on x-axis, richness on y-axis, grouped by autocorrelation
middle_panel_app4 <- ggplot(data_105_no_dist, aes(x = as.factor(fragmentation), y = present_species, fill = as.factor(ac))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$richness, linetype = "dashed") +
    labs(
        x = "Fragmentation",
        y = "Richness",
        fill = "Autocorrelation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Right panel: fragmentation on x-axis, sd_dispersal on y-axis, grouped by autocorrelation
right_panel_app4 <- ggplot(data_105_no_dist, aes(x = as.factor(fragmentation), y = sd_dispersal, fill = as.factor(ac))) +
    geom_boxplot() +
    geom_hline(yintercept = ref_lev_app1$sd, linetype = "dashed") +
    labs(
        x = NULL,
        y = "SDDD",
        fill = "Autocorrelation"
    ) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme_bw() +
    theme(
        plot.title = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray85", color = "black"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none"
    )

# Combine panels for Appendix Figure 4
appendix_fig4 <- left_panel_app4 + middle_panel_app4 + right_panel_app4 +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(
        tag_levels = 'a',
        tag_prefix = "",
        tag_suffix = ")"
    ) &
    theme(
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 14, face = "bold"),
        plot.tag.position = c(0.07, 0.88),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
    )

# Save Appendix Figure 4
ggsave("R/figures/fig_s4.png", plot = appendix_fig4, width = 15, height = 5, bg = "white")





