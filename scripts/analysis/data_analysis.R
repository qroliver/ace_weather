# packages
library(tidyverse)
library(extrafont)
library(htmltools)
library(ggtext)
library(ggrepel)
library(scales)
library(reactable)
library(reactablefmtr)
library(giscoR)
library(sf)
library(cowplot)


# importing data
ace_data <- read.csv("data/aemet_ace_data_clean.csv")


# geographic location of Lanzarote
europe_africa_map_data <- gisco_get_countries(year = "2024",
                                              resolution = "03",
                                              region = c("Africa", "Europe", "Asia"))



canary_islands_map_data <- gisco_get_nuts(nuts_id = "ES70",
                                          resolution = "03",
                                          year = "2024")


lanzarote_map_data <- gisco_get_nuts(nuts_id = "ES708",
                                     resolution = "03",
                                     year = "2024")


europe_africa_map <- europe_africa_map_data %>%
  ggplot() +
  theme(panel.background = element_rect(fill = "#f7fbff")) +
  geom_sf(aes(geometry = geometry), fill = "grey90", size = 0.3) +
  geom_sf(data = canary_islands_map_data,
          aes(geometry = geometry),
          fill = "#fee0d2") +
  geom_sf(data = lanzarote_map_data, 
          aes(geometry = geometry), 
          fill = "darkred") +
  scale_x_continuous(limits = c(-32.5, 42.5)) +
  scale_y_continuous(limits = c(22.5, 72.5)) +
  coord_sf(expand = FALSE) +
  labs(title = "Geographic location of Lanzarote",
       caption = "Made by Oliver Q.R.") +
  geom_rect(xmin = -20, xmax = -12, ymin = 27, ymax = 30,
            fill = NA, colour = "black", linewidth = 1) +
  theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"))



canary_islands_map <- canary_islands_map_data %>%
  ggplot() +
  theme(panel.background = element_rect(fill = "#f7fbff")) +
  geom_sf(data = europe_africa_map_data,
          aes(geometry = geometry),
          fill = "grey90") +
  geom_sf(fill = "#fee0d2", colour = "black") +
  geom_sf(data = lanzarote_map_data, 
          aes(geometry = geometry), 
          fill = "darkred") +
  geom_label_repel(aes(x = -13.6092, y = 28.9456, label = "ACE"),
                   nudge_y = -0.3, nudge_x = 0.15, fontface = "bold") +
  geom_point(aes(x = -13.6092, y = 28.9456), colour = "#3288bd", size = 2) +
  scale_x_continuous(limits = c(-19, -13)) +
  scale_y_continuous(limits = c(27, 30)) +
  coord_sf(expand = FALSE) +
  theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 2),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(.1, .1, .1, .1), "mm"))


ggdraw(europe_africa_map) +
  draw_plot(canary_islands_map,
            x = 0.15,
            y = 0.11,
            height = 0.25)


ggsave("graphs/Geographic_location_Lanzarote.png",
       plot = last_plot())


# Q1: What is the evolution of the annual mean temperature?
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = tmean)) +
  geom_line(linewidth = 1.2, colour = "#1f78b4") +
  geom_point(size = 3, colour = "#1f78b4") +
  geom_label_repel(data = ~.x %>% filter(tmean == min(tmean)),
                   aes(x = year, y = tmean, label = paste0(round(tmean, 2), "°C")),
                   size = 3.5, nudge_x = 3, colour ="#313695", fontface = "bold") +
  geom_label_repel(data = ~.x %>% filter(tmean == max(tmean)),
                   aes(x = year, y = tmean, label = paste0(round(tmean, 2), "°C")),
                   size = 3.5, nudge_x = -3, colour ="#a50026", fontface = "bold") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  labs(title = "Annual mean temperature at Lanzarote Airport",
       subtitle = "Data from 1973 to 2023",
       x = "Year",
       y = "Mean temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())

ggsave("graphs/Annual temperature.png",
       plot = last_plot())


# yearly temperature anomalies
ref_tmean <- ace_data %>%
  filter(year > 1980 & year < 2011) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  pull()

ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  mutate(ta = tmean - ref_tmean) %>%
  ggplot(aes(x = year, y = ta, fill = ta)) +
  geom_col(colour = "#333333") +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 9),
                     limits = c(-2.25, 2.25)) +
  scale_fill_gradient2(low = "#053061", mid = "#f7f7f7", high = "#67001f",
                       midpoint = 0, limits = c(-2.1, 2.1)) +
  geom_label_repel(data = ~.x %>% filter(ta == max(ta)),
                   aes(x = year, y = ta, label = paste0("+", round(ta, 2), "°C")),
                   size = 3.5, nudge_x = -2.5, nudge_y = 0.1, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  geom_label_repel(data = ~.x %>% 
                     filter(ta != max(ta)) %>%
                     filter(ta == max(ta)),
                   aes(x = year, y = ta, label = paste0("+", round(ta, 2), "°C")),
                   size = 3.5, nudge_x = -2.5, nudge_y = 0.1, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  geom_label_repel(data = ~.x %>% 
                     filter(ta != max(ta)) %>%
                     filter(ta != max(ta)) %>%
                     filter(ta == max(ta)),
                   aes(x = year, y = ta, label = paste0("+", round(ta, 2), "°C")),
                   size = 3.5, nudge_x = -2.5, nudge_y = 0.1, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  labs(title = "Annual mean temperature deviations at Lanzarote Airport",
       subtitle = "Comparison of temperature deviations from the reference mean (period: 1981-2010)",
       x = "Year",
       y = "Temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.",
       fill = "°C") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())

ggsave("graphs/Temp anomalies.png",
       plot = last_plot())


# mean temperature by decade
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  mutate(decade = 10 * floor(year / 10)) %>%
  group_by(decade) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  ggplot(aes(x = decade, y = tmean)) +
  geom_line(linewidth = 1.2, colour = "#1f78b4") +
  geom_point(size = 4, colour = "#1f78b4") +
  scale_x_continuous(labels = c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s")) +
  scale_y_continuous(breaks = pretty_breaks(n = 7)) +
  labs(title = "Mean temperature by decade at Lanzarote Airport",
       subtitle = "Data from 1973 to 2023",
       x = "Decade",
       y = "Temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())

ggsave("graphs/Mean temperature by decade.png",
       plot = last_plot())


# monthly mean temperature by decade
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  mutate(decade = factor(paste0(10 * floor(year / 10), "s"),
                         levels = paste0(seq(1970, 2020, 10), "s"))) %>%
  group_by(decade, month) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = month, y = tmean, group = decade, colour = decade, shape = decade)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  scale_y_continuous(breaks = pretty_breaks(n = 7)) +
  scale_colour_manual(values = c("#313695", "#74add1", "#abd9e9", "#fee090", "#f46d43", "#a50026")) +
  scale_shape_manual(values = 20:15) +
  labs(title = "Monthly mean temperature by decade at Lanzarote Airport",
       subtitle = "Data from 1973 to 2023",
       x = "Month",
       y = "Temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.",
       colour = "Decade",
       shape = "Decade") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Monthly mean temp by decade.png",
       plot = last_plot())



# mean temperature by month and decade
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  mutate(decade = factor(as.character(10 * floor(year / 10)),
                         levels = as.character(seq(1970, 2020, 10)))) %>%
  group_by(decade, month) %>%
  summarise(tmean = round(mean(tmean, na.rm = TRUE), 2), .groups = "drop") %>%
  pivot_wider(names_from = decade, values_from = tmean, names_prefix = "d") %>%
  mutate(month = month.name,
         tdiff = d2020 - d1970) %>%
  reactable(fullWidth = FALSE,
            pagination = FALSE,
            bordered = TRUE,
            sortable = FALSE,
            defaultColDef = colDef(
              headerStyle = list(
                background = "#1f78b4",
                color = "#f0f0f0",
                align = "center",
                fontWeight = "bold",
                fontSize = 18,
                borderBottom = "1px solid #f0f0f0",
                padding = "3px"
              ),
              align = "center",
              style = list(
                fontFamily = "Arial",
                padding = "3px"
              ),
              minWidth = 120
            ),
            columns = list(
              month = colDef(name = "Month"),
              d1970 = colDef(name = "1970s"),
              d1980 = colDef(name = "1980s"),
              d1990 = colDef(name = "1990s"),
              d2000 = colDef(name = "2000s"),
              d2010 = colDef(name = "2010s"),
              d2020 = colDef(name = "2020s"),
              tdiff = colDef(name = "Variation",
                             style = function(value){
                               if(value < 0){
                                 list(background = "#eff3ff",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               } else if(value >= 0 & value < 0.5){
                                 list(background = "white",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               } else if(value >= 0.5 & value < 1){
                                 list(background = "#fee5d9",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               } else if(value >= 1 & value < 1.5){
                                 list(background = "#fcbba1",
                                      fontWeight = "bold",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               } else if(value >= 1.5 & value < 2){
                                 list(background = "#fc9272",
                                      fontWeight = "bold",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               } else if(value >= 2 & value < 2.5){
                                 list(background = "#fb6a4a",
                                      color = "white",
                                      fontWeight = "bold",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               } else if(value >= 2.5 & value < 3){
                                 list(background = "#de2d26",
                                      color = "white",
                                      fontWeight = "bold",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               } else {
                                 list(background = "#a50f15",
                                      color = "white",
                                      fontWeight = "bold",
                                      fontFamily = "Arial",
                                      padding = "3px",
                                      vAlign = "center")
                               }
                             },
                             cell = function(value){
                               paste0("+", value)
                             })
            )) %>%
  add_source(source = html("Data: Aemet.<br>Made by Oliver Q.R."),
             font_size = 14,
             font_style = "italic",
             font_color = "grey50",
             align = "left",
             margin = margin(t = 10))



# annual precipitation
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = prec)) +
  geom_col(fill = "darkblue") +
  geom_label_repel(data = ~.x %>% filter(prec == min(prec)),
                   aes(x = year, y = prec, label = paste(prec, "mm", sep = " ")),
                   size = 3.5, nudge_y = 120, colour ="red", fontface = "bold") +
  geom_label_repel(data = ~.x %>% filter(prec == max(prec)),
                   aes(x = year, y = prec, label = paste(prec, "mm", sep = " ")),
                   size = 3.5, nudge_x = 1, nudge_y = 10, colour ="blue", fontface = "bold") +
  scale_y_continuous(breaks = pretty_breaks(n = 7),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_continuous(breaks = seq(1975, 2020, 5),
                     expand = c(0.01, 0.01)) +
  labs(title = "Annual precipitation at Lanzarote Airport",
       subtitle = "Data from 1973 to 2023",
       x = "Year",
       y = "mm",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Annual precipitation.png",
       plot = last_plot())



# yearly precipitation anomalies (reference period: 1981-2010)
ref_prec <- ace_data %>%
  filter(year > 1980 & year < 2011) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  summarise(prec = mean(prec, na.rm = TRUE)) %>%
  pull()


ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  mutate(prec_an = (100 * prec / ref_prec) - 100) %>%
  ggplot(aes(x = year, y = prec_an, fill = prec_an)) +
  geom_col(colour = "#333333") +
  geom_label_repel(data = ~.x %>% filter(prec_an == min(prec_an)),
                   aes(x = year, y = prec_an, label = paste0(round(prec_an, 2), "%")),
                   size = 3.5, nudge_x = -0.5, nudge_y = -15, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  geom_label_repel(data = ~.x %>% filter(prec_an == max(prec_an)),
                   aes(x = year, y = prec_an, 
                       label = paste0("+", round(prec_an, 2), "%")),
                   size = 3.5, nudge_x = 1, nudge_y = 1, fill = "white",
                   colour = "darkblue", fontface = "bold") +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(limits = c(-200, 200),
                     breaks = pretty_breaks(n = 9)) +
  scale_fill_gradient2(low = "#9e0142", mid = "#f7f7f7", high = "#5e4fa2",
                       midpoint = 0,
                       limits = c(-200, 200)) +
  labs(title = "Annual precipitation anomalies at Lanzarote Airport",
       subtitle = "Deviations from the annual mean precipitation for the period 1981-2010",
       x = "Year",
       y = "Precipitation anomaly (%)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.",
       fill = "mm") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Prec anomalies.png",
       plot = last_plot())


# mean temperature by month and decade
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  mutate(decade = factor(as.character(10 * floor(year / 10)),
                         levels = as.character(seq(1970, 2020, 10)))) %>%
  group_by(decade, month) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = tmean, group = decade, colour = decade, shape = decade)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  scale_y_continuous(breaks = pretty_breaks(n = 7)) +
  scale_colour_manual(values = c("#313695", "#74add1", "#abd9e9", "#fee090", "#f46d43", "#a50026")) +
  scale_shape_manual(values = 20:15) +
  labs(title = "Monthly mean temperature by decade at Lanzarote Airport",
       subtitle = "Data from 1973 to 2023",
       x = "Month",
       y = "Temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.",
       colour = "Decade",
       shape = "Decade") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Monthly mean temp by decade.png",
       plot = last_plot())


# annual precipitation
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = prec)) +
  geom_col(fill = "darkblue") +
  geom_label_repel(data = ~.x %>% filter(prec == min(prec)),
                   aes(x = year, y = prec, label = paste(prec, "mm", sep = " ")),
                   size = 3.5, nudge_y = 120, colour ="red", fontface = "bold") +
  geom_label_repel(data = ~.x %>% filter(prec == max(prec)),
                   aes(x = year, y = prec, label = paste(prec, "mm", sep = " ")),
                   size = 3.5, nudge_x = 1, nudge_y = 10, colour ="blue", fontface = "bold") +
  scale_y_continuous(breaks = pretty_breaks(n = 7),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_continuous(breaks = seq(1975, 2020, 5),
                     expand = c(0.01, 0.01)) +
  labs(title = "Annual precipitation at Lanzarote Airport",
       subtitle = "Data from 1973 to 2023",
       x = "Year",
       y = "mm",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Annual precipitation.png",
       plot = last_plot())


# yearly precipitation anomalies (reference period: 1981-2010)
ref_prec <- ace_data %>%
  filter(year > 1980 & year < 2011) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  summarise(prec = mean(prec, na.rm = TRUE)) %>%
  pull()


ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  mutate(prec_an = (100 * prec / ref_prec) - 100) %>%
  ggplot(aes(x = year, y = prec_an, fill = prec_an)) +
  geom_col(colour = "#333333") +
  geom_label_repel(data = ~.x %>% filter(prec_an == min(prec_an)),
                   aes(x = year, y = prec_an, label = paste0(round(prec_an, 2), "%")),
                   size = 3.5, nudge_x = -0.5, nudge_y = -15, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  geom_label_repel(data = ~.x %>% filter(prec_an == max(prec_an)),
                   aes(x = year, y = prec_an, 
                       label = paste0("+", round(prec_an, 2), "%")),
                   size = 3.5, nudge_x = 1, nudge_y = 1, fill = "white",
                   colour = "darkblue", fontface = "bold") +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(limits = c(-200, 200),
                     breaks = pretty_breaks(n = 9)) +
  scale_fill_gradient2(low = "#9e0142", mid = "#f7f7f7", high = "#5e4fa2",
                       midpoint = 0,
                       limits = c(-200, 200)) +
  labs(title = "Annual precipitation anomalies at Lanzarote Airport",
       subtitle = "Deviations from the annual mean precipitation for the period 1973-2000",
       x = "Year",
       y = "Precipitation anomaly (%)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.",
       fill = "mm") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Prec anomalies.png",
       plot = last_plot())



# year classification according to mean temperature and precipitation anomalies
ace_data %>%
  filter(year > 1972 & year < 2024) %>%
  group_by(year) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  mutate(tmean_an = tmean - ref_tmean) %>%
  left_join(ace_data %>%
              filter(year > 1972 & year < 2024) %>%
              group_by(year) %>%
              summarise(prec = sum(prec, na.rm = TRUE)) %>%
              mutate(prec_an = (100 * prec / ref_prec) - 100),
            by = "year") %>%
  mutate(decade = paste0(as.character(10 * floor(year / 10)), "s")) %>%
  ggplot(aes(x = prec_an, y = tmean_an, shape = decade)) +
  geom_rect(aes(xmin = -120, xmax = 0, ymin = -2, ymax = 0),
            fill = "#a6cee3", alpha = 0.05) +
  geom_rect(aes(xmin = 0, xmax = 200, ymin = -2, ymax = 0),
            fill = "#1f78b4", alpha = 0.05) +
  geom_rect(aes(xmin = 0, xmax = 200, ymin = 0, ymax = 2.5),
            fill = "#d73027", alpha = 0.05) +
  geom_rect(aes(xmin = -120, xmax = 0, ymin = 0, ymax = 2.5),
            fill = "#fdae61", alpha = 0.05) +
  annotate(geom = "text", label = "Colder\nDrier", x = -115, y = -1.75, colour = "white",
           hjust = "left", fontface = "bold") +
  annotate(geom = "text", label = "Colder\nWetter", x = 195, y = -1.75, colour = "white",
           hjust = "right", fontface = "bold") +
  annotate(geom = "text", label = "Warmer\nWetter", x = 195, y = 2.25, colour = "white",
           hjust = "right", fontface = "bold") +
  annotate(geom = "text", label = "Warmer\nDrier", x = -115, y = 2.25, colour = "white",
           hjust = "left", fontface = "bold") +
  geom_label_repel(data = ~.x %>% filter(year == 2023),
                   aes(label = year), fontface = "bold") +
  geom_point(size = 3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_shape_manual(values = 20:15) +
  labs(title = "Temperature and precipitation anomalies at Lanzarote airport",
       subtitle = "Data from 1973-2023. Reference period: 1981-2010",
       x = "Precipitation anomaly (%)",
       y = "Temperature anomaly (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.",
       shape = "Decade") +
  theme_bw(base_family = "sans") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave("graphs/Prec temp anomalies.png",
       plot = last_plot())


# year 2023 daily mean temperature
ace_data %>%
  filter(year > 1980 & year < 2011) %>%
  group_by(month, day) %>%
  summarise(tmean_ref = mean(tmean, na.rm = TRUE)) %>%
  right_join(ace_data %>%
               filter(year == 2023) %>%
               select(month, day, tmean),
             by = c("month", "day")) %>%
  mutate(tmean_diff = tmean - tmean_ref,
         red_max = if_else(tmean_diff > 0,
                           tmean,
                           NA),
         red_min = if_else(tmean_diff > 0,
                           tmean_ref,
                           NA),
         blue_max = if_else(tmean_diff < 0,
                            tmean_ref,
                            NA),
         blue_min = if_else(tmean_diff < 0,
                            tmean,
                            NA),
         day_month = make_date(month = month, day = day)) %>%
  ggplot(aes(x = day_month)) +
  geom_ribbon(aes(ymin = blue_min, ymax = blue_max),
              fill = "blue", alpha = 0.25) +
  geom_ribbon(aes(ymin = red_min, ymax = red_max),
              fill = "red", alpha = 0.25) +
  geom_line(aes(y = tmean_ref), colour = "#313695", linewidth = 1.2) +
  geom_line(aes(y = tmean), colour = "#a50026", linewidth = 1.2) +
  scale_x_continuous(breaks = c(1, yday("1970-02-01"), yday("1970-03-01"),
                                yday("1970-04-01"), yday("1970-05-01"), yday("1970-06-01"),
                                yday("1970-07-01"), yday("1970-08-01"), yday("1970-09-01"),
                                yday("1970-10-01"), yday("1970-11-01"), yday("1970-12-01"),
                                yday("1970-12-31")),
                     labels = c(paste0("1-", month.abb), "31-Dec")) +
  labs(title = "Daily mean temperature deviations in 2023 at Lanzarote Airport",
       subtitle = "Comparison of <span style=color:#a50026>**2023**</span> daily temperatures 
       with the <span style=color:#313695>**1981-2010 reference period**</span>",
       x = "Day",
       y = "Mean temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_markdown(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Temp2023_day_by_day.png",
       plot = last_plot())


# monthly precipitation
ace_data %>%
  filter(year == 2023) %>%
  group_by(month) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  left_join(ace_data %>%
              filter(year > 1980 & year < 2011) %>%
              group_by(month, year) %>%
              summarise(prec_ref = sum(prec, na.rm = TRUE)) %>%
              group_by(month) %>%
              summarise(prec_ref = mean(prec_ref, na.rm = TRUE)),
            by = "month") %>%
  pivot_longer(cols = starts_with("prec"),
               names_to = "period",
               values_to = "prec_mm") %>%
  ggplot(aes(x = month, y = prec_mm, fill = period)) +
  geom_col(position = "dodge",
           colour = "#333333") +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb,
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4"),
                    labels = c("2023", "1981-2010"),
                    name = "Period") +
  labs(title = "Monthly precipitation in 2023 vs. the reference period",
       subtitle = "Reference period: 1981-2010",
       x = "Month",
       y = "Precipitation (mm)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Monthly prec 2023.png",
       plot = last_plot())



# 2024 annual mean temperature (first 9 months)
ace_data %>%
  filter(year > 1980 & year < 2011,
         month <= 9) %>%
  group_by(year) %>%
  summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
  summarise(tmean_ref = mean(tmean, na.rm = TRUE)) %>%
  bind_cols(ace_data %>%
              filter(year == 2023,
                     month <= 9) %>%
              summarise(tmean23 = mean(tmean, na.rm = TRUE)),
            ace_data %>%
              filter(year == 2024,
                     month <= 9) %>%
              summarise(tmean24 = mean(tmean, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "period", values_to = "tmean") %>%
  ggplot(aes(x = period, y = tmean, fill = period)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = tmean, label = paste0(round(tmean, 2), "°C")), 
            vjust = 2, fontface = "bold", colour = "white") +
  scale_y_continuous(breaks = pretty_breaks(n = 8),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = c("reference", "2023", "2024")) +
  scale_fill_manual(values = c("#1f78b4", "#a50026", "#f46d43")) +
  labs(title = "Annual mean temperature at Lanzarote Airport (Jan to Sept)",
       subtitle = "Reference period: 1981-2010",
       x = "Year",
       y = "Mean temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


ggsave("graphs/Mean temp 2024.png",
       plot = last_plot())



# Warmest years (first 9 months)
ace_data %>%
  filter(year > 1972, month <= 9) %>%
  group_by(year) %>%
  summarise(tmean = round(mean(tmean, na.rm = TRUE), 2)) %>%
  arrange(desc(tmean)) %>%
  slice_head(n = 10) %>%
  reactable(fullWidth = FALSE,
            pagination = FALSE,
            bordered = TRUE,
            sortable = FALSE,
            defaultColDef = colDef(
              headerStyle = list(
                background = "#ffcc99",
                color = "black",
                align = "center",
                fontWeight = "bold",
                fontSize = 18,
                borderbottom = "1px solid #f0f0f0",
                padding = "6px"
              ),
              align = "center",
              headerVAlign = "center",
              style = list(
                fontFamily = "Arial",
                padding = "6px"
              )
            ),
            columns = list(
              year = colDef(name = "Year"),
              tmean = colDef(name = "Tmean (°C)")
            ),
            rowStyle = function(index){
              if(.$year[index] == 2024){
                list(background = "#ffffcc", fontWeight = "bold")
              }
            }) %>%
  add_source(source = html("Data: Aemet.<br>Made by Oliver Q.R."),
             font_size = 14,
             font_style = "italic",
             font_color = "grey50",
             align = "left",
             margin = margin(t = 10))



# 2024 daily mean temperature (first 9 months)
ace_data %>%
  filter(year > 1980 & year < 2011) %>%
  group_by(month, day) %>%
  summarise(tmean_ref = mean(tmean, na.rm = TRUE)) %>%
  left_join(ace_data %>%
              filter(year == 2024,
                     month <= 9) %>%
              group_by(month, day) %>%
              summarise(tmean24 = mean(tmean, na.rm = TRUE)),
            by = c("month", "day")) %>%
  mutate(month_day = make_date(month = month, day = day),
         tmean_diff = tmean24 - tmean_ref,
         red_max = if_else(tmean_diff > 0,
                           tmean24,
                           NA),
         red_min = if_else(tmean_diff > 0,
                           tmean_ref,
                           NA),
         blue_max = if_else(tmean_diff < 0,
                            tmean_ref,
                            NA),
         blue_min = if_else(tmean_diff < 0,
                            tmean24,
                            NA)) %>%
  ggplot(aes(x = month_day)) +
  geom_ribbon(aes(ymin = blue_min, ymax = blue_max),
              fill = "blue", alpha = 0.25) +
  geom_ribbon(aes(ymin = red_min, ymax = red_max),
              fill = "red", alpha = 0.25) +
  geom_line(aes(y = tmean_ref),
            colour = "#313695", linewidth = 1.2) +
  geom_line(aes(y = tmean24),
            colour = "#a50026", linewidth = 1.2) +
  scale_x_continuous(breaks = c(1, yday("1970-02-01"), yday("1970-03-01"),
                                yday("1970-04-01"), yday("1970-05-01"), yday("1970-06-01"),
                                yday("1970-07-01"), yday("1970-08-01"), yday("1970-09-01"),
                                yday("1970-10-01"), yday("1970-11-01"), yday("1970-12-01"),
                                yday("1970-12-31")),
                     labels = c(paste0("1-", month.abb), "31-Dec")) +
  labs(title = "Daily mean temperature deviations in 2024 at Lanzarote Airport",
       subtitle = "Comparison of <span style=color:#a50026>**2024**</span> (until 30-Sept) daily temperatures 
       with the <span style=color:#313695>**1981-2010 reference period**</span>",
       x = "Day",
       y = "Mean temperature (°C)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_markdown(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())

ggsave("graphs/Daily mean temp 2024.png",
       plot = last_plot())



# Precipitation in the first 9 months of the year
ace_data %>%
  filter(year > 1980 & year < 2011,
         month <= 9) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  summarise(prec_ref = mean(prec, na.rm = TRUE)) %>%
  bind_cols(ace_data %>%
              filter(year == 2023,
                     month <= 9) %>%
              summarise(prec_23 = sum(prec, na.rm = TRUE)),
            ace_data %>%
              filter(year == 2024,
                     month <= 9) %>%
              summarise(prec_24 = sum(prec, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "period", values_to = "precipitation") %>%
  mutate(period = factor(period, levels = c("prec_ref", "prec_23", "prec_24"))) %>%
  ggplot(aes(x = period, y = precipitation, fill = period)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(precipitation, 2)),
            vjust = 2, fontface = "bold", colour = "white") +
  scale_y_continuous(breaks = pretty_breaks(n = 8),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = c("reference", "2023", "2024")) +
  scale_fill_manual(values = c("#4575b4", "#f46d43", "#a50026")) +
  labs(title = "Total rain in the first 9 months of the year at Lanzarote Airport",
       subtitle = "Reference period: 1981-2010",
       x = "Year",
       y = "Precipitation (mm)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


ggsave("graphs/Total precipitation 2024.png",
       plot = last_plot())



# monthly precipitation in 2024 vs. the reference period
ace_data %>%
  filter(year == 2024,
         month <= 9) %>%
  group_by(month) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  left_join(ace_data %>%
              filter(year > 1980 & year < 2011,
                     month <= 9) %>%
              group_by(month, year) %>%
              summarise(prec_ref = sum(prec, na.rm = TRUE)) %>%
              group_by(month) %>%
              summarise(prec_ref = mean(prec_ref, na.rm = TRUE)),
            by = "month") %>%
  pivot_longer(cols = starts_with("prec"),
               names_to = "period",
               values_to = "prec_mm") %>%
  ggplot(aes(x = month, y = prec_mm, fill = period)) +
  geom_col(position = "dodge",
           colour = "#333333") +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb,
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4"),
                    labels = c("2024", "1981-2010"),
                    name = "Period") +
  labs(title = "Monthly precipitation in 2024 vs. the reference period",
       subtitle = "Reference period: 1981-2010",
       x = "Month",
       y = "Precipitation (mm)",
       caption = "Data: Aemet.\nMade by Oliver Q.R.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


ggsave("graphs/Montly precipitation 2024.png",
       plot = last_plot())



# driest years from January to September
ace_data %>%
  filter(year > 1972, month <= 9) %>%
  group_by(year) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  arrange(prec) %>%
  slice_head(n = 10) %>%
  reactable(fullWidth = FALSE,
            pagination = FALSE,
            bordered = TRUE,
            sortable = FALSE,
            defaultColDef = colDef(
              headerStyle = list(
                background = "#ffcc99",
                color = "black",
                align = "center",
                fontWeight = "bold",
                fontSize = 18,
                borderbottom = "1px solid #f0f0f0",
                padding = "6px"
              ),
              align = "center",
              headerVAlign = "center",
              style = list(
                fontFamily = "Arial",
                padding = "6px"
              )
            ),
            columns = list(
              year = colDef(name = "Year"),
              prec = colDef(name = "Prec. (mm)")
            ),
            rowStyle = function(index){
              if(.$year[index] == 2024){
                list(background = "#ffffcc", fontWeight = "bold")
              }
            }) %>%
  add_source(source = html("Data: Aemet.<br>Made by Oliver Q.R."),
             font_size = 14,
             font_style = "italic",
             font_color = "grey50",
             align = "left",
             margin = margin(t = 10))




