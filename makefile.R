# Load packages -----------------------------------------------------------
library(tidyverse)
library(hrbrthemes)
library(scales)
library(ggrepel)

# Update and load data ----------------------------------------------------

source("data-raw/")

# Policy timeline ---------------------------------------------------------

policy_timeline_plot <-
  policy %>%
  mutate(
    event = fct_reorder(event, start),
    group = factor(
      group,
      levels = c("Indonesia", "Jawa Barat", "Bodebek", "Bandung Raya")
    ),
    group = fct_rev(group),
    label_anchor = start + ((end - start) / 2)
  ) %>%
  ggplot(aes(
    x = start,
    xend = end,
    y = group,
    yend = group
  )) +
  geom_rect(
    data = ~ .x %>%
      group_by(period = periode) %>%
      summarise(start = min(start),
                end = max(end)) %>%
      mutate(end = if_else(period == "1", max(start), end)),
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = period
    ),
    alpha = 0.3,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(colour = event),
    size = 5,
    alpha = 1,
    show.legend = FALSE
  ) +
  geom_text_repel(
    aes(x = start, label = event),
    direction = "y",
    nudge_y = c(-0.3, 0.3),
    min.segment.length = 0,
    show.legend = FALSE,
    family = font_es,
    fontface = "italic",
    segment.colour = "gray40",
    colour = "gray30",
    size = 2.5
  ) +
  geom_point(colour = "gray30") +
  scale_x_date(
    breaks = "21 days",
    labels = label_date(format = "%e %b"),
    expand = c(0.1, 0.1)
  ) +
  scale_colour_grey() +
  scale_fill_grey() +
  coord_cartesian(clip = "off") +
  labs(x = NULL,
       y = NULL) +
  theme_ipsum_es(base_size = 10,
                 grid = "Y",
                 ticks = TRUE)

ggsave(
  "outfile/policy_timeline.png",
  plot = policy_timeline_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)
