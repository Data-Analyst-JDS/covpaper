# Load packages -----------------------------------------------------------

library(tidyverse)
library(slider)
library(gghighlight)
library(hrbrthemes)
library(scales)
library(ggrepel)


# Set up local time language ----------------------------------------------

Sys.setlocale("LC_TIME", "id_ID.UTF-8")

# Update and load data ----------------------------------------------------

source("data-raw/data.R")

# Policy timeline ---------------------------------------------------------

policy_timeline_plot <-
  policy %>%
  filter(start < "2020-10-01") %>% 
  mutate(
    label_anchor = start + ((end - start) / 2)
  ) %>%
  ggplot(aes(
    x = start,
    xend = end,
    y = group,
    yend = group
  )) +
  geom_rect(
    data = policy_period,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = period
    ),
    alpha = 0.15,
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
    aes(x = label_anchor, label = event),
    direction = "y",
    nudge_y = c(-0.35, 0.35),
    min.segment.length = 0,
    show.legend = FALSE,
    family = "Times New Roman",
    fontface = "italic",
    segment.colour = "gray40",
    colour = "gray30",
    size = 3
  ) +
  geom_point(aes(x = label_anchor), colour = "gray30") +
  scale_x_date(
    date_breaks = "1 months",
    date_minor_breaks = "14 days",
    labels = label_date(format = "%b"),
    expand = c(0.1, 0.1)
  ) +
  scale_colour_grey() +
  scale_fill_grey() +
  labs(x = NULL,
       y = NULL) +
  coord_cartesian(
    xlim = c(as.Date("2020-03-01"), as.Date("2020-09-30")),
    clip = "off"
  ) +
  theme_ipsum(
    base_size = 11,
    base_family = "Times New Roman",
    grid = "XY",
    ticks = TRUE
  )

ggsave(
  "outfile/policy_timeline.png",
  plot = policy_timeline_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

# COVID-19 ----------------------------------------------------------------

covid19_plot <- 
  covid19 %>% 
  filter(date < "2020-10-01") %>% 
  mutate(
    rollmean = slide_dbl(newcase, mean, .before = 3, .after = 3)
  ) %>% 
  ggplot(aes(date, newcase)) +
  geom_rect(
    data = policy_period,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = period
    ),
    alpha = 0.15,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_col(fill = "gray60", colour = "gray60") +
  geom_line(aes(y = rollmean), colour = "gray40") +
  scale_x_date(
    date_breaks = "1 months",
    date_minor_breaks = "14 days",
    labels = label_date(format = "%b"),
    expand = c(0.1, 0.1)
  ) +
  scale_fill_grey() +
  labs(
    x = NULL,
    y = "Jumlah kasus harian",
    title = "Perkembangan Harian Kasus COVID-19 di Jawa Barat",
    caption = "Data: Dinas Kesehatan Provinsi Jawa Barat"
  ) +
  theme_ipsum(
    base_size = 11,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  )

ggsave(
  "outfile/covid19.png",
  plot = covid19_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

# Stayput -----------------------------------------------------------------

stayput_plot <-
  stayput %>% 
  filter(date >= "2020-03-01",
         date < "2020-10-01") %>% 
  ggplot(aes(date, stayput)) +
  geom_rect(
    data = policy_period,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = period
    ),
    alpha = 0.15,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_point(colour = "gray60", size = 0.75, alpha = 0.6) +
  geom_line(
    data = ~ stayput %>% 
      group_by(date) %>% 
      summarise(
        stayput = median(stayput)
      ) %>% 
      ungroup() %>% 
      mutate(roll3d = slide_dbl(stayput, mean, .before = 1, .after = 1)),
    aes(y = roll3d), 
    colour = "gray40",
    size = 0.9
  ) +
  scale_x_date(
    date_breaks = "1 months",
    date_minor_breaks = "14 days",
    labels = label_date(format = "%b"),
    expand = c(0.05, 0.05)
  ) +
  scale_y_percent(sec.axis = dup_axis(name = NULL)) +
  scale_fill_grey() +
  labs(
    x = NULL,
    y = "% masyarakat berada di rumah",
    fill = NULL,
    title = "Masyarakat Berada di Rumah Saat Pandemi COVID-19",
    caption = "Data: Facebook Movement Range Maps"
  ) +
  theme_ipsum(
    base_size = 11,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  )

ggsave(
  "outfile/stayput.png",
  plot = stayput_plot,
  width = 8, 
  height = 5, 
  dpi = 300,
  type = "cairo-png"
)

# Mobility ----------------------------------------------------------------

mobility_plot <-
  mobility %>% 
  filter(date >= "2020-03-01",
         date < "2020-10-01") %>% 
  mutate(
    category = str_wrap(category, width = 25)
  ) %>% 
  ggplot() +
  facet_wrap( ~ category, scales = "free_x") +
  geom_rect(
    data = policy_period,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = period
    ),
    alpha = 0.15,
    colour = NA,
    size = 0,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_line(
    aes(
      date, 
      pct_mobility_changes,
      group = category
    ),
    colour = "gray40"
  ) +
  gghighlight(
    unhighlighted_params = list(
      colour = "gray60",
      alpha = 0.6
    ),
    use_direct_label = FALSE
  ) +
  scale_x_date(
    date_breaks = "1 months",
    guide = guide_axis(check.overlap = TRUE, n.dodge = 2),
    date_minor_breaks = "14 days",
    labels = label_date(format = "%b"),
    expand = c(0.05, 0.05)
  ) +
  scale_y_percent(sec.axis = dup_axis(name = NULL)) +
  scale_fill_grey() +
  labs(
    x = NULL,
    y = "% Perubahan mobilitas",
    fill = NULL,
    title = "Mobilitas Masyarakat Saat Pandemi COVID-19",
    caption = "Data: Google COVID-19 Community Mobility Reports"
  ) +
  coord_cartesian(ylim = c(-1, 1), clip = "on") +
  theme_ipsum(
    base_size = 11,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  )

ggsave(
  "outfile/mobility.png",
  plot = mobility_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)
