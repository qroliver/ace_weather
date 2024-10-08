# packages
library(tidyverse)
library(extrafont)
library(htmltools)
library(ggtext)
library(ggrepel)
library(scales)


# Q1: What is the evolution of the annual mean temperature?
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = tmean)) +
  geom_line(linewidth = 1.2, colour = "#1f78b4") +
  geom_point(size = 3, colour = "#1f78b4") +
  geom_label_repel(data = ~.x %>% filter(min(tmean))) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  labs(title = "Annual mean temperature at Lanzarote Airport",
       subtitle = "Data from 1973 to 2023",
       x = "Year",
       y = "Mean temperature (°C)",
       caption = "Data source: Aemet.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(face = "bold", colour = "#003080"),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())



































