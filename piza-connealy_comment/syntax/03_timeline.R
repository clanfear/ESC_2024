library(tidyverse)
library(patchwork)

load("./piza-connealy_comment/data/derived/spd_crime_daily.RData")

timeline <- tribble( ~date, ~event, ~ offset,
                     "25-05-2020", "George Floyd killed", 0,
                     # "29-05-2020", "Protests start",
                     # "30-05-2020", "Downtown riots", 6,
                     "01-06-2020", "Capitol Hill protests begin", 8,
                     # "05-06-2020", "Tear gas ban", 
                     # "06-06-2020", "SPD disperses protesters",
                     # "06-07-2020", "Fernandez shooting",
                     # "06-07-2020", "SPD uses tear gas, flashbangs",
                     "08-06-2020", "Eastern precinct evacuated", 0,
                     "01-07-2020", "SPD returns to eastern precinct", 0,
                     "25-07-2020", "Detention center protest", 0) |> 
  mutate(date = lubridate::dmy(date),
         y = 1 + offset,
         ymax = 0.25 + offset,
         ymin = 0)

date_min <- as.Date("2020-04-01")
date_max <- as.Date("2020-08-01")

crime_plot <- spd_crime_daily %>%
  mutate(zone = str_replace(zone, "\n", " ")) |>
  filter(date >= date_min & date < date_max  & zone %in% c("CHOP", "Other Precincts")) %>%
  ggplot(aes(x     = date, 
             y     = tot, 
             color = zone, 
             group = zone)) + 
  annotate("rect",
           xmin = as.Date("2020-06-08"), 
           xmax = as.Date("2020-07-01"), 
           ymin = -Inf, 
           ymax = Inf, 
           fill  = "black", 
           alpha = 0.3) + 
  geom_hline(yintercept = 0, color = "#333d4d") +
  geom_line()  +
  scale_color_manual(values = c("CHOP" = "#b0514a", "Other Precincts" = "#333d4d")) +
  geom_vline(data = timeline, aes(xintercept = date), color = "black", linetype = "dashed", linewidth = 0.25, color = "#333d4d") +
  facet_grid(zone~., scales = "free_y") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .2))) +
  labs(y     = "Crime counts", 
       x     = NULL, 
       color = NULL) +
  scale_x_date(date_breaks = "months", 
               date_labels = "%b",
               limits = c(date_min, date_max)) + 
  theme_minimal(base_size = 32, base_family = "EB Garamond") + 
  theme(legend.position   = "none", 
        strip.text = element_text(lineheight = 0.25, color = "#333d4d"),
        legend.background = element_rect(fill  = "white", 
                                         color = "white"),
        text              = element_text(family = "EB Garamond", color = "#333d4d"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.15, color = "#d1ccc4"),
        panel.spacing.y   = unit(0, "pt"),
        axis.text.x = element_text(margin = margin(0,0,0,0, unit = "pt"), color = "#333d4d"),
        plot.margin = margin(0, 5, 5, 5, unit = "pt"))

event_plot <- timeline |>
  mutate(event = ifelse(str_detect(event, "protests begin"), event, str_wrap(event, 10))) |>
  ggplot(aes(x = date)) +
  geom_text(aes(y = y, label = event), vjust = 0, family = "EB Garamond", size = 10, lineheight = 0.25, color = "#333d4d") +
  geom_segment(aes(y = ymin, yend = ymax, xend = date), color = "black", linetype = "dashed", linewidth = 0.25, color = "#333d4d") +
  scale_x_date(limits = c(date_min, date_max)) +
  scale_y_continuous(limits = c(0, 11)) +
  theme_void(base_size = 32, base_family = "EB Garamond") +
  theme(text              = element_text(family = "EB Garamond", color = "#333d4d"),
        plot.margin = margin(5, 5, 0, 5, unit = "pt"))

timeline_plot <- event_plot / crime_plot + plot_layout(heights = c(1,3)) & 
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"))
timeline_plot
ggsave("slides/img/timeline_plot.png", plot = timeline_plot, device = ragg::agg_png, width = 16, height = 9, units = "cm")
