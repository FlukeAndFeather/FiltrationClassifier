library(tidyverse)
library(lubridate)
# Also depends on RcppRoll (on CRAN) and patchwork (on GitHub)
# devtools::install_github("thomasp85/patchwork")

lungetable <- read_csv("data/bw170813-44/bw170813-44LungeTable.csv")
# Speed is smoothed over 5s and Gyro-Y is smoothed over 1.5s
prh <- read_csv("data/bw170813-44/bw170813-44 10Hzprh.csv") %>%
  mutate(speed = RcppRoll::roll_mean(speed, n = 30, fill = NA),
         Gy = RcppRoll::roll_mean(Gy, n = 15, fill = NA),
         pitch = pitch * 180/pi,
         roll = roll * 180/pi)

# Plot a lunge + 5min
# Returns a ggplot object
plot_lunge <- function(idx) {
  lunge_idx <- lungetable$LungeI[idx]
  lunge_time <- prh$datetime[lunge_idx]
  plot_start <- lunge_time - minutes(1)
  plot_end <- lunge_time + minutes(4)
  plot_data_prh <- prh %>%
    filter(between(datetime, plot_start, plot_end)) %>%
    mutate(secs_since = as.numeric(difftime(datetime, lunge_time, units = "secs")))
  find_nearest0 <- approxfun(plot_data_prh$secs_since - 0.05,
                             plot_data_prh$secs_since - 0.05,
                             method = "constant")
  find_nearest <- function(x) find_nearest0(x) + 0.05
  plot_data_lunge <- with(lungetable[1,], tibble(Lunge = 0,
                                                 Purge1 = purge1,
                                                 Purge2 = purge2, 
                                                 Purge3 = purge3)) %>%
    gather(event, secs_since, Lunge:Purge3) %>%
    mutate(secs_since = find_nearest(secs_since)) %>%
    left_join(plot_data_prh, by = "secs_since")
  
  plot_theme_last <- theme_classic() +
    theme(axis.ticks.x = element_blank())
  
  plot_theme <- plot_theme_last +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.2))
  
  # depth ~ time
  label_data <- plot_data_lunge %>% 
    mutate(secs_since = secs_since + 2,
           p = min(plot_data_prh$p - 8))
  p1 <- ggplot(mapping = aes(secs_since, p)) +
    geom_line(data = plot_data_prh) +
    geom_vline(aes(xintercept = secs_since),
               plot_data_lunge,
               linetype = "dashed") +
    # Label the lunge and purge events
    geom_text(aes(x = secs_since,
                  y = p,
                  label = event),
              label_data, 
              hjust = 0,
              vjust = 1) +
    scale_y_reverse() +
    plot_theme +
    labs(y = "Depth (m)") 
  # speed ~ time
  p2 <- ggplot(mapping = aes(secs_since, speed)) +
    geom_line(data = plot_data_prh) +
    geom_vline(aes(xintercept = secs_since),
               plot_data_lunge,
               linetype = "dashed") +
    plot_theme +
    labs(y = "Speed (m/s)") 
  # pitch, roll ~ time
  # rescale roll limits (-120, 120) to pitch limits (-90, 90)
  pr_data <- plot_data_prh %>% 
    rename(Pitch = pitch, Roll = roll) %>%
    gather(orientation, value, Pitch:Roll)
  roll_axis <- sec_axis(~ . * 120 / 90,
                        name = expression("Roll " ( degree )),
                        breaks = seq(-120, 120, by = 60))
  p3 <- ggplot(mapping = aes(secs_since)) +
    geom_hline(yintercept = 0,
               size = 0.2) +
    # pitch on primary, roll on secondary
    geom_line(aes(y = value, color = orientation),
              pr_data) +
    geom_vline(aes(xintercept = secs_since),
               plot_data_lunge,
               linetype = "dashed") +
    scale_y_continuous(limits = c(-90, 90),
                       breaks = seq(-90, 90, by = 45),
                       sec.axis = roll_axis) +
    scale_color_manual(values = c("red", "blue")) +
    plot_theme +
    theme(axis.line.x = element_blank(),
          # Color-code y-axes
          axis.text.y.left = element_text(color = "red"),
          axis.line.y.left = element_line(color = "red"),
          axis.title.y.left = element_text(color = "red"),
          axis.text.y.right = element_text(color = "blue"),
          axis.line.y.right = element_line(color = "blue"),
          axis.title.y.right = element_text(color = "blue"),
          legend.position = "none") +
    labs(y = expression("Pitch (" ( degree ))) 
  # gyro_y ~ time
  p4 <- ggplot(mapping = aes(secs_since, Gy)) +
    geom_hline(yintercept = 0,
               size = 0.2) +
    geom_line(data = plot_data_prh) +
    geom_vline(aes(xintercept = secs_since),
               plot_data_lunge,
               linetype = "dashed") +
    scale_x_continuous(breaks = seq(-60, 240, by = 60)) +
    plot_theme_last +
    theme(axis.line.x = element_blank()) +
    labs(x = "Seconds since lunge",
         y = "Gyro-Y (rad/s)")
  
  p1 + p2 + p3 + p4 + patchwork::plot_layout(ncol = 1, heights = c(1, 1, 2, 2))
}
