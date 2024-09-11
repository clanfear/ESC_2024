library(tidyverse)
library(patchwork)
library(ggrepel)
library(showtext)
library(ggtext)
font_add_google("EB Garamond")
showtext_auto()

load("./piza-connealy_comment/data/derived/spd_crime_daily.RData")

shootings <- tribble(~date, ~type, 
                     "2020-06-07", "S",
                     "2020-06-20", "S/F",
                     "2020-06-21", "S",
                     "2020-06-23", "S",
                     "2020-06-29", "S/F") |>
  mutate(date = as.Date(date))

timeline <- tribble( ~date, ~event, ~position, ~ offset, ~ include,
                     "2020-05-25", "George Floyd killed",             14 , 0, TRUE,
                     "2020-05-29", "Downtown protests start",        0 , 0, FALSE,
                     "2020-05-30", "Downtown riots",                  0 , 0, FALSE,
                     "2020-06-01", "Capitol Hill protests start",     14 , 6, TRUE,
                     "2020-06-05", "Tear gas ban",                    0 , 0, FALSE,
                     "2020-06-06", "SPD disperses protesters",        0 , 0, FALSE,
                     "2020-06-07", "Fernandez shooting",              0 , 0, FALSE,
                     "2020-06-07", "SPD uses tear gas, flashbangs",   0 , 4, FALSE,
                     "2020-06-08", "Eastern precinct evacuated",      14 , 0, TRUE,
                     "2020-07-01", "SPD returns and clears CHOP", 14 , 6,TRUE,
                     "2020-07-25", "Detention center protest",        0 , 0, TRUE) |> 
  mutate(date = as.Date(date),
         y = 1 + offset,
         ymax = 0.25 + offset,
         ymin = 0,
         vjust = as.numeric(position < 0))

background_plot  <-  timeline |> filter(position!=0) |>
  mutate(event = str_wrap(event, 11)) |>
  ggplot(aes(x = date)) +
  annotate("rect",
           xmin = as.Date("2020-06-08"), 
           xmax = as.Date("2020-07-01"), 
           ymin = 0, 
           ymax = 10, 
           fill  = "black", 
           alpha = 0.3) +
  annotate("text", label = "CHOP Period", x = as.Date("2020-06-20"), y = 6, family = "EB Garamond", color = "#b0514a", size = 14, fontface = "bold") +
  geom_segment(aes(y = 0, yend = position-0.25, xend = date), linetype = "dashed", linewidth = 0.25, color = "#333d4d") +
  geom_segment(data = shootings, aes(y = 0, yend = 1.4, x = date, xend = date, color = type)) +
  geom_text(data = shootings, aes(y = 2, x = date, color = type, label = type), family = "EB Garamond", size = 6) +
  geom_label(aes(y = position, label = event), label.size  = NA, fill = "#fff5e5", vjust = 0, family = "EB Garamond", size = 10, lineheight = 0.25, color = "#333d4d") +
  scale_x_date(limits = c(as.Date("2020-05-15"), as.Date("2020-07-15")), breaks = as.Date(c("2020-05-15", "2020-06-01", "2020-06-15", "2020-07-01", "2020-07-15")), date_labels = "%B %e") +
  scale_y_continuous(limits = c(0, 22)) +
  scale_color_manual(values = c("S" = "#326b69", "S/F" = "#b0514a"), 
                     labels = c("<span style='color:#326b69'>Non-fatal shooting</span>", "<span style='color:#b0514a'>Fatal and non-fatal shootings</span>")) +
  geom_hline(yintercept = 0, color = "#333d4d") +
  theme_minimal(base_size = 32, base_family = "EB Garamond") + 
  labs(x = "2020", y = NULL, color = NULL) +
  guides(color = guide_legend(
    title = NULL,
    byrow = TRUE,
    override.aes = aes(label = "")
  )) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.2), 
        legend.key.spacing.y = unit(-10, "pt"),
        legend.key.width = rel(0.5),
        legend.text = element_markdown(size = 16, margin = margin(0,0,0,0, "pt")),
        strip.text = element_text(lineheight = 0.25, color = "#333d4d"),
        legend.background = element_rect(fill  = NA, 
                                         color = NA),
        text              = element_text(family = "EB Garamond", color = "#333d4d"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.15, color = "#d1ccc4"),
        panel.grid.major = element_blank(),
        panel.spacing.y   = unit(0, "pt"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(0,0,0,0, unit = "pt"), color = "#333d4d"),
        plot.margin = margin(0, 5, 5, 5, unit = "pt")
  )
background_plot
ggsave("./piza-connealy_comment/img/background_plot.png", plot = background_plot, device = ragg::agg_png, width = 14, height = 4.75, units = "cm")


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
  geom_vline(data = timeline |> filter(include), aes(xintercept = date), linetype = "dashed", linewidth = 0.25, color = "#333d4d") +
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

event_plot <- timeline |> filter(include) |>
  mutate(event = ifelse(str_detect(event, "protests begin"), event, str_wrap(event, 10))) |>
  ggplot(aes(x = date)) +
  geom_text(aes(y = y, label = event), vjust = 0, family = "EB Garamond", size = 10, lineheight = 0.25, color = "#333d4d") +
  geom_segment(aes(y = ymin, yend = ymax, xend = date), linetype = "dashed", linewidth = 0.25, color = "#333d4d") +
  scale_x_date(limits = c(date_min, date_max)) +
  scale_y_continuous(limits = c(0, 14)) +
  theme_void(base_size = 32, base_family = "EB Garamond") +
  theme(text              = element_text(family = "EB Garamond", color = "#333d4d"),
        plot.margin = margin(5, 5, 0, 5, unit = "pt"))

timeline_plot <- event_plot / crime_plot + plot_layout(heights = c(1.5,3)) & 
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"))
timeline_plot
ggsave("piza-connealy_comment/img/timeline_plot.png", plot = timeline_plot, device = ragg::agg_png, width = 16, height = 9, units = "cm")

