
###########
## PLOTS ##
###########

### (1) DIRECTORY

if (!dir.exists("output/figures")){
  dir.create("output/figures", recursive = TRUE)
}


### (2) DATA

# 1. Final Decisions
fd_plot_data <- read_rds("data/interim/final_decisions_long.rds")

# 2. RCA Recommendations
rca_plot_data <- read_rds("data/interim/observed_recommendations.rds")

# 3. NET Discretion
disc_plot_data <- read_rds("data/interim/net_discretion_long.rds")

# 4. SIGNED Discretion
raw_disc_plot_data <- read_rds("data/interim/signed_discretion_long.rds")

### (3) SETUP

# Pivot RCA Recommendation Data
rca_plot_data <- rca_plot_data |>
  pivot_longer(
    cols = c(obs_rec_release, obs_rec_spdet, obs_rec_detwbond, obs_rec_detnobond),
    names_to = "recommendation",
    values_to = "percentage"
  )

# Setting Standard Theme for All Plots
global_plot_theme <- theme_minimal(base_family = "Georgia") + 
  theme(
    # Title, Subtitle, and Caption Styling
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
    # Axis Label Styling
    axis.title.x = element_text(family = "Georgia", face = "bold", size = 14),
    axis.title.y = element_text(family = "Georgia", face = "bold", size = 14),
    # Axis Text styling
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Georgia", size = 12),
    axis.text.y = element_text(family = "Georgia", size = 12),
    # Grid Line styling
    panel.grid.major = element_line(color = "grey80")
) 

# Dataframe for Period Labels used in All Plots
period_labels <- data.frame(
  x = c(9, 25, 42),
  y_title = 95,
  y_subtitle = 90, 
  label = c("Period 1", "Period 2", "Period 3"),
  dates = c("July 2012 - Dec 2013", "Jan 2014 - Feb 2015", "Mar 2015 - Oct 2016")
)

### (4) PLOT OF FINAL DECISIONS

fd_plot <- ggplot(fd_plot_data, aes(x = monthnumber, y = percentage, color = decision, group = decision)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.3) +
  # Vertical Line to Delineate Updates to Algorithm
  geom_vline(xintercept = c(18.5, 32.5), linetype = "dashed", color = "black") +
  # Period Annotations using dataframe
  geom_text(data = period_labels, aes(x = x, y = y_title, label = label), 
            inherit.aes = FALSE, fontface = "bold", family = "Georgia", size = 5) +
  geom_text(data = period_labels, aes(x = x, y = y_subtitle, label = dates), 
            inherit.aes = FALSE, fontface = "italic", family = "Georgia", size = 4) +
  # Colors, Scales and Labels 
  scale_color_manual(
    values = c(
      "released" = "#33a02c",
      "detained_with_bond" = "#ff7f00",
      "detained_without_bond" = "purple"
    ),
    labels = c(
      "released" = "Released",
      "detained_with_bond" = "Detained with Bond",
      "detained_without_bond" = "Detained without Bond"
    )
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Final Decisions Over Time",
    x = "Month Index",
    y = "Percentage of Cases (%)",
    color = "Final Decision"
  ) +
  global_plot_theme

### (5) PLOT OF RCA RECOMMENDATION

rca_plot <- ggplot(rca_plot_data, aes(x = monthnumber, y = percentage, color = recommendation, group = recommendation)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.3) +
  # Vertical Line to Delineate Updates to Algorithm
  geom_vline(xintercept = c(18.5, 32.5), linetype = "dashed", color = "black") +
  # Period Annotations using dataframe
  geom_text(data = period_labels, aes(x = x, y = y_title, label = label), 
            inherit.aes = FALSE, fontface = "bold", family = "Georgia", size = 5) +
  geom_text(data = period_labels, aes(x = x, y = y_subtitle, label = dates), 
            inherit.aes = FALSE, fontface = "italic", family = "Georgia", size = 4) +
  # Colors, Scales and Labels 
  scale_color_manual(
    values = c(
      "obs_rec_release" = "#33a02c",
      "obs_rec_spdet" = "#1f78b4",
      "obs_rec_detwbond" = "#ff7f00",
      "obs_rec_detnobond" = "purple"
    ),
    labels = c(
      "obs_rec_release" = "Release",
      "obs_rec_spdet" = "Supervisor to Determine",
      "obs_rec_detwbond" = "Detain with Bond",
      "obs_rec_detnobond" = "Detain without Bond"
    )
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "RCA Recommendations Over Time",
    x = "Month Index",
    y = "Percentage of Cases (%)",
    color = "RCA Recommendation"
  ) +
  global_plot_theme

### (6) PLOT OF OFFICER DISCRETION (MAGNITUDE/NET)

disc_plot <- ggplot(disc_plot_data, aes(x = monthnumber, y = percentage, color = discretion_vector, group = discretion_vector)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.3) +
  # Vertical Line to Delineate Updates to Algorithm
  geom_vline(xintercept = c(18.5, 32.5), linetype = "dashed", color = "black") +
  # Period Annotations using dataframe
  geom_text(data = period_labels, aes(x = x, y = y_title, label = label), 
            inherit.aes = FALSE, fontface = "bold", family = "Georgia", size = 5) +
  geom_text(data = period_labels, aes(x = x, y = y_subtitle, label = dates), 
            inherit.aes = FALSE, fontface = "italic", family = "Georgia", size = 4) +
  # Colors, Scales and Labels 
  scale_color_manual(
    values = c(
      "release_disc_net" = "#33a02c",
      "detwbond_disc_net" = "#ff7f00",
      "detnobond_disc_net" = "#1f78b4"
    ),
    labels = c(
      "release_disc_net" = "Release Discretion",
      "detwbond_disc_net" = "Detain with Bond Discretion",
      "detnobond_disc_net" = "Detain without Bond Discretion"
    )
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Discretionary Behavior Over Time",
    x = "Month Index",
    y = "Absolute Discretion (%)",
    color = "Discretion Type"
  ) +
  global_plot_theme

### (7) PLOT OF OFFICER DISCRETION (SIGNED)
signed_disc_plot <- ggplot(raw_disc_plot_data, aes(x = monthnumber, y = percentage, color = discretion_vector, group = discretion_vector)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.3) +
  # Vertical Line to Delineate Updates to Algorithm
  geom_vline(xintercept = c(18.5, 32.5), linetype = "dashed", color = "black") +
  # Period Annotations using dataframe
  geom_text(data = period_labels, aes(x = x, y = y_title, label = label), 
            inherit.aes = FALSE, fontface = "bold", family = "Georgia", size = 5) +
  geom_text(data = period_labels, aes(x = x, y = y_subtitle, label = dates), 
            inherit.aes = FALSE, fontface = "italic", family = "Georgia", size = 4) +
  # Colors, Scales and Labels 
  scale_color_manual(
    values = c(
      "release_disc_raw" = "#33a02c",
      "detwbond_disc_raw" = "#ff7f00",
      "detnobond_disc_raw" = "#1f78b4"
    ),
    labels = c(
      "release_disc_raw" = "Release Discretion",
      "detwbond_disc_raw" = "Detain with Bond Discretion",
      "detnobond_disc_raw" = "Detain without Bond Discretion"
    )
  ) +
  # Update Range to -100 to 100
  coord_cartesian(ylim = c(-100, 100)) +
  labs(
    title = "Signed Discretionary Behavior Over Time",
    x = "Month Index",
    y = "Signed Discretion (%)",
    color = "Discretion Type"
  ) +
  global_plot_theme


### (8) SAVING PLOTS
# Saving Final Decisions Plot
ggsave(
  filename = "output/figures/final_decisions_plot.png",
  plot = fd_plot,
  width = 10, 
  height = 7,
  dpi = 300, 
  bg = "white"
)

# Saving RCA Recommendations Plot
ggsave(
  filename = "output/figures/rca_recommendations_plot.png",
  plot = rca_plot,
  width = 10, 
  height = 7,
  dpi = 300, 
  bg = "white"
)

# Saving Discretion Plot
ggsave(
  filename = "output/figures/discretion_plot.png",
  plot = disc_plot,
  width = 10, 
  height = 7,
  dpi = 300, 
  bg = "white"
)

# Saving Signed Discretion Plot
ggsave(
  filename = "output/figures/signed_discretion_plot.png",
  plot = signed_disc_plot,
  width = 10, 
  height = 7,
  dpi = 300, 
  bg = "white"
)