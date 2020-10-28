# Load packages -----------------------------------------------------------

library(tidyverse)
library(slider)
library(gghighlight)
library(hrbrthemes)
library(scales)
library(ggrepel)

# Set up local time language ----------------------------------------------

# Sys.setlocale("LC_TIME", "id_ID.UTF-8")

# Update and load data ----------------------------------------------------

update <- FALSE

if (isTRUE(update)) {
  source("data-raw/data.R")
} else {
  walk(
    list.files("data", full.names = TRUE),
    ~ load(.x, envir = .GlobalEnv)
  )
}

# Policy timeline ---------------------------------------------------------

policy_timeline_plot <-
  policy %>%
  filter(start < "2020-10-01") %>% 
  filter(event_id != "Kasus Terkonfirmasi COVID19 Pertama di Indonesia, Bodebek dan Jawa Barat") %>% 
  mutate(
    # label_anchor = start + ((end - start) / 2),
    label_anchor = start,
    event = str_wrap(event, 30)
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
    show.legend = TRUE
  ) +
  geom_segment(
    aes(colour = event),
    size = 3.5,
    alpha = 0.85,
    show.legend = FALSE
  ) +
  geom_text_repel(
    aes(x = label_anchor, label = event),
    direction = "y",
    nudge_y = c(-0.4, 0.4),
    min.segment.length = 0,
    show.legend = FALSE,
    family = "Times New Roman",
    fontface = "italic",
    segment.colour = "gray40",
    colour = "gray30",
    size = 2.5
  ) +
  geom_point(
    aes(x = label_anchor), 
    shape = "circle filled",
    fill = "grey30",
    colour = "grey80"
  ) +
  geom_point(
    data = policy %>% 
      filter(event_id == "Kasus Terkonfirmasi COVID19 Pertama di Indonesia, Bodebek dan Jawa Barat"),
    shape = "asterisk",
    colour = "grey20"
  ) +
  annotate(
    geom = "label",
    x = as.Date("2020-07-05"),
    y = 0.75,
    # label = "*kasus terkonfirmasi COVID-19 pertama\n(2 Maret 2020)",
    label = "The (*) symbol marks the first confirmed COVID-19 case\n(2 March 2020)",
    family = "Times New Roman",
    fontface = "italic",
    hjust = "left",
    colour = "gray30",
    size = 2.5
  ) +
  scale_x_date(
    date_breaks = "1 months",
    date_minor_breaks = "14 days",
    labels = label_date(format = "%b"),
    expand = c(0.05, 0.05)
  ) +
  scale_colour_grey(guide = FALSE) +
  scale_fill_manual(
    values = c("grey15", "grey55"),
    guide = guide_legend(
      override.aes = list(colour = "grey20")
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
    ) +
  coord_cartesian(
    xlim = c(as.Date("2020-03-01"), as.Date("2020-09-30")),
    clip = "off"
  ) +
  theme_ipsum(
    base_size = 11,
    axis_title_size = 13,
    base_family = "Times New Roman",
    grid = "XY",
    ticks = TRUE
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    plot.margin = margin(1, 1, 1, 1)
  )

ggsave(
  "outfile/policy_timeline.png",
  plot = policy_timeline_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

ggsave(
  "outfile/policy_timeline.pdf",
  plot = policy_timeline_plot,
  device = cairo_pdf,
  width = 8,
  height = 5,
  dpi = 300
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
    show.legend = TRUE
  ) +
  geom_col(fill = "gray60", colour = "gray60") +
  geom_line(aes(y = rollmean), colour = "gray40") +
  scale_x_date(
    date_breaks = "1 months",
    date_minor_breaks = "14 days",
    labels = label_date(format = "%b"),
    expand = c(0.1, 0.1)
  ) +
  scale_fill_manual(
    values = c("grey15", "grey55"),
    guide = guide_legend(
      override.aes = list(colour = "grey20")
    )
  ) +
  labs(
    x = NULL,
    # y = "Jumlah kasus harian",
    y = "Daily cases",
    fill = NULL
    # title = "Perkembangan Harian Kasus COVID-19 di Jawa Barat",
    # caption = "Data: Dinas Kesehatan Provinsi Jawa Barat"
  ) +
  theme_ipsum(
    base_size = 11,
    axis_title_size = 13,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    plot.margin = margin(1, 1, 1, 1)
  )

ggsave(
  "outfile/covid19.png",
  plot = covid19_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

ggsave(
  "outfile/covid19.pdf",
  plot = covid19_plot,
  device = cairo_pdf,
  width = 8,
  height = 5,
  dpi = 300
)

# Traffic -----------------------------------------------------------------

traffic_plot <- 
  traffic %>% 
  filter(date >= "2020-03-01",
         date < "2020-10-01") %>% 
  ggplot(aes(date, pct_traffic_changes)) +
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
    show.legend = TRUE
  ) +
  geom_point(colour = "gray60", size = 0.75, alpha = 0.6) +
  geom_line(
    data = ~ .x %>% 
      group_by(date) %>% 
      summarise(
        pct_traffic_changes = median(pct_traffic_changes)
      ) %>% 
      ungroup() %>% 
      mutate(roll3d = slide_dbl(pct_traffic_changes, mean, .before = 1, .after = 1)),
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
  scale_fill_manual(
    values = c("grey15", "grey55"),
    guide = guide_legend(
      override.aes = list(colour = "grey20")
    )
  ) +
  labs(
    x = NULL,
    # y = "% perubahan kemacetan",
    y = "% changes in traffic jams",
    fill = NULL
    # title = "Masyarakat Berada di Rumah Saat Pandemi COVID-19",
    # caption = "Data: Facebook Movement Range Maps"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_ipsum(
    base_size = 11,
    axis_title_size = 13,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    plot.margin = margin(1, 1, 1, 1)
  )

ggsave(
  "outfile/traffic.png",
  plot = traffic_plot,
  width = 8, 
  height = 5, 
  dpi = 300,
  type = "cairo-png"
)

ggsave(
  "outfile/traffic.pdf",
  plot = traffic_plot,
  device = cairo_pdf,
  width = 8, 
  height = 5, 
  dpi = 300
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
    show.legend = TRUE
  ) +
  geom_point(colour = "gray60", size = 0.75, alpha = 0.6) +
  geom_line(
    data = ~ stayput %>% 
      filter(date >= "2020-03-01",
             date < "2020-10-01") %>% 
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
  scale_fill_manual(
    values = c("grey15", "grey55"),
    guide = guide_legend(
      override.aes = list(colour = "grey20")
    )
  ) +
  labs(
    x = NULL,
    # y = "% masyarakat berada di rumah",
    y = "% stay-at-home",
    fill = NULL
    # title = "Masyarakat Berada di Rumah Saat Pandemi COVID-19",
    # caption = "Data: Facebook Movement Range Maps"
  ) +
  expand_limits(y = 0) +
  theme_ipsum(
    base_size = 11,
    axis_title_size = 13,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    plot.margin = margin(1, 1, 1, 1)
  )

ggsave(
  "outfile/stayput.png",
  plot = stayput_plot,
  width = 8, 
  height = 5, 
  dpi = 300,
  type = "cairo-png"
)

ggsave(
  "outfile/stayput.pdf",
  plot = stayput_plot,
  device = cairo_pdf,
  width = 8, 
  height = 5, 
  dpi = 300
)

stayput_plot2 <-
  stayput %>% 
  filter(date >= "2020-03-01",
         date < "2020-10-01") %>% 
  ggplot(aes(date, pct_movement_changes)) +
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
    show.legend = TRUE
  ) +
  geom_point(colour = "gray60", size = 0.75, alpha = 0.6) +
  geom_line(
    data = ~ stayput %>% 
      filter(date >= "2020-03-01",
             date < "2020-10-01") %>% 
      group_by(date) %>% 
      summarise(
        pct_movement_changes = median(pct_movement_changes)
      ) %>% 
      ungroup() %>% 
      mutate(roll3d = slide_dbl(pct_movement_changes, mean, .before = 1, .after = 1)),
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
  scale_fill_manual(
    values = c("grey15", "grey55"),
    guide = guide_legend(
      override.aes = list(colour = "grey20")
    )
  ) +
  labs(
    x = NULL,
    y = "% Perubahan pergerakan",
    fill = NULL
    # title = "Masyarakat Berada di Rumah Saat Pandemi COVID-19",
    # caption = "Data: Facebook Movement Range Maps"
  ) +
  expand_limits(y = 0) +
  theme_ipsum(
    base_size = 11,
    axis_title_size = 13,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    plot.margin = margin(1, 1, 1, 1)
  )

ggsave(
  "outfile/stayput2.png",
  plot = stayput_plot2,
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
    category = factor(
      category,
      levels = c(
        "Area permukiman",
        "Toko bahan makanan & apotek",
        "Tempat kerja",
        "Pusat transportasi umum",
        "Retail & rekreasi",
        "Taman"
      ),
      labels = c(
        "Residential", 
        "Grocery And Pharmacy",
        "Workplaces", 
        "Transit Stations", 
        "Retail And Recreation", 
        "Parks"
      )
    )
  ) %>% 
  mutate(
    category = str_wrap(category, width = 25),
    category = fct_reorder(category, pct_mobility_changes, last)
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
    alpha = 0.075,
    colour = NA,
    size = 0,
    inherit.aes = FALSE,
    show.legend = TRUE
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
  scale_fill_manual(
    values = c("grey15", "grey55"),
    guide = guide_legend(
      override.aes = list(colour = "grey20", size = 0.35)
    )
  ) +
  labs(
    x = NULL,
    # y = "% Perubahan mobilitas",
    y = "% changes in mobility",
    fill = NULL
    # title = "Mobilitas Masyarakat Saat Pandemi COVID-19",
    # caption = "Data: Google COVID-19 Community Mobility Reports"
  ) +
  coord_cartesian(ylim = c(-1, 1), clip = "on") +
  theme_ipsum(
    base_size = 11,
    axis_title_size = 13,
    base_family = "Times New Roman",
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    plot.margin = margin(1, 1, 1, 1)
  )

ggsave(
  "outfile/mobility.png",
  plot = mobility_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

ggsave(
  "outfile/mobility.pdf",
  plot = mobility_plot,
  device = cairo_pdf,
  width = 8,
  height = 5,
  dpi = 300
)

mobility %>% 
  filter(date >= "2020-03-01",
         date < "2020-10-01") %>% 
  group_by(
    category
  ) %>% 
  group_walk(
    ~ {
      mobility_subplot <- .x %>% 
        ggplot() +
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
        geom_hline(yintercept = 0, linetype = "longdash", colour = "gray50") +
        geom_line(
          aes(
            date, 
            pct_mobility_changes
          ),
          colour = "gray40"
        ) +
        geom_label(
          data = ~.x %>% 
            filter(date == max(date)) %>% 
            mutate(label = percent(pct_mobility_changes)),
          aes(
            date, 
            pct_mobility_changes,
            label = label,
          ),
          nudge_x = 8,
          family = "Times New Roman",
          size = 3.75,
          colour = "gray30"
        ) +
        scale_x_date(
          date_breaks = "1 months",
          guide = guide_axis(check.overlap = TRUE),
          date_minor_breaks = "14 days",
          labels = label_date(format = "%b"),
          expand = c(0.05, 0.05)
        ) +
        scale_y_percent() +
        scale_fill_grey() +
        labs(
          x = NULL,
          y = "% Perubahan mobilitas",
          fill = NULL,
          title = "Mobilitas Masyarakat Saat Pandemi COVID-19",
          subtitle = .y,
          caption = "Data: Google COVID-19 Community Mobility Reports"
        ) +
        coord_cartesian(ylim = c(-1, 1), clip = "on") +
        theme_ipsum(
          base_size = 11,
          axis_title_size = 13,
          base_family = "Times New Roman",
          grid = "Y",
          ticks = TRUE
        )
      
      ggsave(
        paste0("outfile/mobility-",.y, ".png"),
        plot = mobility_subplot,
        width = 8,
        height = 5,
        dpi = 300,
        type = "cairo-png"
      )
    }
  )
