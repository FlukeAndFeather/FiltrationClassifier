library(tidyverse)
library(lubridate)

lungetable <- read_csv("data/bw170813-44/bw170813-44LungeTable.csv")
prh <- read_csv("data/bw170813-44/bw170813-44 10Hzprh.csv") %>%
  mutate(speed0.5s = RcppRoll::roll_mean(speed, n = 5, fill = NA),
         speed1.0s = RcppRoll::roll_mean(speed, n = 10, fill = NA),
         speed1.5s = RcppRoll::roll_mean(speed, n = 15, fill = NA),
         speed2.0s = RcppRoll::roll_mean(speed, n = 20, fill = NA),
         speed2.5s = RcppRoll::roll_mean(speed, n = 25, fill = NA),
         speed3.0s = RcppRoll::roll_mean(speed, n = 30, fill = NA),
         speed3.5s = RcppRoll::roll_mean(speed, n = 15, fill = NA),
         speed4.0s = RcppRoll::roll_mean(speed, n = 20, fill = NA),
         speed4.5s = RcppRoll::roll_mean(speed, n = 25, fill = NA),
         speed5.0s = RcppRoll::roll_mean(speed, n = 30, fill = NA),
         Gy0.5s = RcppRoll::roll_mean(Gy, n = 5, fill = NA),
         Gy1.0s = RcppRoll::roll_mean(Gy, n = 10, fill = NA),
         Gy1.5s = RcppRoll::roll_mean(Gy, n = 15, fill = NA),
         Gy2.0s = RcppRoll::roll_mean(Gy, n = 20, fill = NA),
         Gy2.5s = RcppRoll::roll_mean(Gy, n = 25, fill = NA),
         Gy3.0s = RcppRoll::roll_mean(Gy, n = 30, fill = NA),
         Gy3.5s = RcppRoll::roll_mean(Gy, n = 15, fill = NA),
         Gy4.0s = RcppRoll::roll_mean(Gy, n = 20, fill = NA),
         Gy4.5s = RcppRoll::roll_mean(Gy, n = 25, fill = NA),
         Gy5.0s = RcppRoll::roll_mean(Gy, n = 30, fill = NA),
         pitch = pitch * 180/pi,
         roll = roll * 180/pi)

# Plot a lunge + 5min
# Returns a ggplot object
# Speed is smoothed over 5s and Gyro-Y is smoothed over 1.5s
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
  # depth ~ time
  p1 <- ggplot(mapping = aes(secs_since, p)) +
    geom_line(data = plot_data_prh) +
    geom_point(data = plot_data_lunge,
               size = 3,
               col = 'red') +
    scale_y_reverse() +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.2)) +
    labs(y = "Depth (m)") 
  # speed ~ time
  p2 <- ggplot(mapping = aes(secs_since, speed5.0s)) +
    geom_line(data = plot_data_prh) +
    geom_point(data = plot_data_lunge,
               size = 3,
               col = "red") +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.2)) +
    labs(y = "Speed (m/s)") 
  # pitch ~ time
  p3 <- ggplot(mapping = aes(secs_since, pitch)) +
    geom_hline(yintercept = 0,
               size = 0.2) +
    geom_line(data = plot_data_prh) +
    geom_point(data = plot_data_lunge,
               size = 3,
               col = "red") +
    scale_y_continuous(limits = c(-90, 90),
                       breaks = seq(-90, 90, by = 45)) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.2)) +
    labs(y = "Pitch (deg)") 
  # roll ~ time
  p4 <- ggplot(mapping = aes(secs_since, roll)) +
    geom_hline(yintercept = 0,
               size = 0.2) +
    geom_line(data = plot_data_prh) +
    geom_point(data = plot_data_lunge,
               size = 3,
               col = "red") +
    scale_y_continuous(limits = c(-120, 120),
                       breaks = seq(-120, 120, by = 60)) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line = element_line(size = 0.2)) +
    labs(y = "Roll (deg)") 
  # gyro_y ~ time
  p5 <- ggplot(mapping = aes(secs_since, Gy1.5s)) +
    geom_hline(yintercept = 0,
               size = 0.2) +
    geom_line(data = plot_data_prh) +
    geom_point(data = plot_data_lunge,
               size = 3,
               col = "red") +
    scale_x_continuous(breaks = seq(-60, 240, by = 60)) +
    theme_classic() +
    theme(axis.line.x = element_blank(),
          axis.line = element_line(size = 0.2)) +
    labs(x = "Seconds since lunge",
         y = "Gyro-Y (rad/s)")
  
  p1 + p2 + p3 + p4 + p5 + patchwork::plot_layout(ncol = 1)
}
