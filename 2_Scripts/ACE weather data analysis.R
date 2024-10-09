# packages
library(tidyverse)
library(extrafont)
library(htmltools)
library(ggtext)
library(ggrepel)
library(scales)
library(reactable)
library(reactablefmtr)


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
       caption = "Data: Aemet.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(face = "bold", colour = "#003080"),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())



# yearly temperature anomalies (reference period 1973-2000: https://aemetblog.es/2024/07/24/preguntas-y-respuestas-sobre-las-olas-de-calor/)
ref_tmean <- ace_data %>%
  filter(year > 1972 & year < 2001) %>%
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
                   aes(x = year, y = ta, label = paste0(round(ta, 2), "°C")),
                   size = 3.5, nudge_x = -2.5, nudge_y = 0.1, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  geom_label_repel(data = ~.x %>% 
                     filter(ta != max(ta)) %>%
                     filter(ta == max(ta)),
                   aes(x = year, y = ta, label = paste0(round(ta, 2), "°C")),
                   size = 3.5, nudge_x = -2.5, nudge_y = 0.1, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  geom_label_repel(data = ~.x %>% 
                     filter(ta != max(ta)) %>%
                     filter(ta != max(ta)) %>%
                     filter(ta == max(ta)),
                   aes(x = year, y = ta, label = paste0(round(ta, 2), "°C")),
                   size = 3.5, nudge_x = -2.5, nudge_y = 0.1, fill = "white",
                   colour = "#67001f", fontface = "bold") +
  labs(title = "Annual mean temperature deviations at Lanzarote Airport",
       subtitle = "Comparison of temperature deviations from the reference mean (period: 1973-2020)",
       x = "Year",
       y = "Temperature (°C)",
       caption = "Data: Aemet.",
       fill = "°C") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_text(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.title = element_text(face = "bold", colour = "#003080"),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())


# year 2023
ace_data %>%
  filter(year > 1972 & year < 2001) %>%
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
       with the <span style=color:#313695>**1973-2000 reference period**</span>",
       x = "Day",
       y = "Mean temperature (°C)",
       caption = "Data: Aemet.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_markdown(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())



# maximum temperature calendar
ace_data %>%
  filter(year == 2023) %>%
  mutate(weekday = wday(date, label = TRUE, week_start = 1),
         month = month(date, label = TRUE, abbr = FALSE),
         week = isoweek(date)) %>%
  mutate(week = case_when(month == "December" & week == 1 ~ 53,
                          month == "January" & week %in% 52:53 ~ 0,
                          .default = week),
         tmax_cat = cut(tmax, c(0, 30, 34, 37, 40, max(tmax)))) %>%
  ggplot(aes(x = weekday, y = -week, fill = tmax_cat)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = day), size = 2.5) +
  scale_fill_manual(values = c("white", "#ffffcc", "yellow", "orange", "red"),
                    labels = c("≤30°C", "30-34°C", "34-37°C", "37-40°C", ">40°C"),
                    guide = guide_legend(title.position = "top",
                                         override.aes = list(colour = "black"))) +
  labs(title = "Daily maximum temperatures throughout the year 2023",
       subtitle = "Lanzarote Airport data",
       caption = "Data: Aemet.",
       fill = "Tmax") +
  theme_minimal(base_family = "Arial") +
  theme(aspect.ratio = 1/2,
        plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_markdown(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50", vjust = -1),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        legend.position = "right",
        legend.text = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold", size = 9, hjust = 0.5),
        panel.border = element_rect(colour = "grey", fill = NA, linewidth = 1)) +
  facet_wrap(vars(month), nrow = 4, ncol = 3, scales = "free")


ace_data %>%
  filter(year == 2023) %>%
  summarise(t30_34 = sum(tmax > 30 & tmax <= 34),
            t34_37 = sum(tmax > 34 & tmax <= 37),
            t37_40 = sum(tmax > 37 & tmax <= 40),
            t40plus = sum(tmax > 40)) %>%
  pivot_longer(cols = everything(), 
               names_to = "tmax", 
               values_to = "y2023") %>%
  left_join(ace_data %>%
              filter(year > 1972 & year < 2001) %>%
              group_by(year) %>%
              summarise(t30_34 = sum(tmax > 30 & tmax <= 34),
                        t34_37 = sum(tmax > 34 & tmax <= 37),
                        t37_40 = sum(tmax > 37 & tmax <= 40),
                        t40plus = sum(tmax > 40)) %>%
              summarise(across(starts_with("t"), \(x) round(mean(x, na.rm = TRUE), 2))) %>%
              pivot_longer(cols = everything(),
                           names_to = "tmax",
                           values_to = "ref_period"),
            by = "tmax") %>%
  reactable(fullWidth = FALSE,
            pagination = FALSE,
            defaultColDef = colDef(
              headerStyle = list(
                background = "#67000d",
                color = "#f0f0f0",
                align = "center",
                fontWeight = "bold",
                borderBottom = "1px solid #f0f0f0"
              ),
              align = "center",
              style = list(
                fontFamily = "Arial"
              )),
            columns = list(
              tmax = colDef(
                name = "Temperature",
                minWidth = 120,
                align = "left",
                cell = function(value){
                  if(value == "t30_34"){
                    value <- ">30-34°C"
                  } else if(value == "t34_37"){
                    value <- ">34-37°C"
                  } else if(value == "t37_40"){
                    value <- ">37-40°C"
                  } else{
                    value <- ">40°C"
                  }
                }
              ),
              y2023 = colDef(
                name = "2023",
                cell = data_bars(
                  .,
                  fill_color = "darkred",
                  max_value = 365,
                  text_position = "outside-end"
                )),
              ref_period = colDef(
                name = "1973-2000",
                cell = data_bars(
                  .,
                  fill_color = "darkred",
                  max_value = 365,
                  text_position = "outside-end"
                ))
            ),
            theme = reactableTheme(
              borderColor = "#e0e0e0",
              cellPadding = "8px"
            )) %>%
  add_source(source = html("<strong>Table 1.</strong> Number of days with high temperatures.<br>
                           Data: Aemet."),
             font_size = 14,
             font_style = "italic",
             font_color = "grey50",
             align = "left",
             margin = margin(t = 10))


# Daily minimum temperatures throughout 2023
ace_data %>%
  filter(year == 2023) %>%
  mutate(weekday = wday(date, label = TRUE, week_start = 1),
         month = month(date, label = TRUE, abbr = FALSE),
         week = isoweek(date)) %>%
  mutate(week = case_when(month == "December" & week == 1 ~ 53,
                          month == "January" & week %in% 52:53 ~ 0,
                          .default = week),
         tmin_cat = cut(tmin, c(0, 20, 25, 30, max(tmin)))) %>%
  ggplot(aes(x = weekday, y = -week, fill = tmin_cat)) +
  geom_tile(colour = "white", size = 0.4) +
  geom_text(aes(label = day), size = 2.5) +
  scale_fill_manual(values = c("white", "yellow", "orange", "red"),
                    labels = c("≤20°C", "20-25°C", "25-30°C", ">30°C"),
                    guide = guide_legend(title.position = "top",
                                         override.aes = list(colour = "black"))) +
  labs(title = "Daily minimum temperatures throughout the year 2023",
       subtitle = "Lanzarote Airport data",
       caption = "Data: Aemet.",
       fill = "Tmin") +
  theme_minimal(base_family = "Arial") +
  theme(aspect.ratio = 1/2,
        plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_markdown(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50", vjust = -1),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        legend.position = "right",
        legend.text = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold", size = 9, hjust = 0.5),
        panel.border = element_rect(colour = "grey", fill = NA, linewidth = 1)) +
  facet_wrap(vars(month), nrow = 4, ncol = 3, scales = "free")


ace_data %>%
  filter(year == 2023) %>%
  summarise(t20minus = sum(tmin <= 20),
            t20_25 = sum(tmin > 20 & tmin <= 25),
            t25_30 = sum(tmin > 25 & tmin <= 30),
            t30plus = sum(tmin > 30)) %>%
  pivot_longer(cols = everything(), 
               names_to = "tmin", 
               values_to = "y2023") %>%
  left_join(ace_data %>%
              filter(year > 1972 & year < 2001) %>%
              group_by(year) %>%
              summarise(t20minus = sum(tmin <= 20),
                        t20_25 = sum(tmin > 20 & tmin <= 25),
                        t25_30 = sum(tmin > 25 & tmin <= 30),
                        t30plus = sum(tmin > 30)) %>%
              summarise(across(starts_with("t"), \(x) round(mean(x, na.rm = TRUE), 2))) %>%
              pivot_longer(cols = everything(),
                           names_to = "tmin",
                           values_to = "ref_period"),
            by = "tmin") %>%
  reactable(fullWidth = FALSE,
            pagination = FALSE,
            defaultColDef = colDef(
              headerStyle = list(
                background = "#67000d",
                color = "#f0f0f0",
                align = "center",
                fontWeight = "bold",
                borderBottom = "1px solid #f0f0f0"
              ),
              align = "center",
              style = list(
                fontFamily = "Arial"
              )),
            columns = list(
              tmin = colDef(
                name = "Temperature",
                minWidth = 120,
                align = "left",
                cell = function(value){
                  if(value == "t20minus"){
                    value <- "≤20°C"
                  } else if(value == "t20_25"){
                    value <- ">20-25°C"
                  } else if(value == "t25_30"){
                    value <- ">25-30°C"
                  } else{
                    value <- ">30°C"
                  }
                }
              ),
              y2023 = colDef(
                name = "2023",
                cell = data_bars(
                  .,
                  fill_color = "darkred",
                  max_value = 365,
                  text_position = "outside-end"
                )),
              ref_period = colDef(
                name = "1973-2000",
                cell = data_bars(
                  .,
                  fill_color = "darkred",
                  max_value = 365,
                  text_position = "outside-end"
                ))
            ),
            theme = reactableTheme(
              borderColor = "#e0e0e0",
              cellPadding = "8px"
            )) %>%
  add_source(source = html("<strong>Table 2.</strong> Number of days according to the min. temperature.<br>
                           Data: Aemet."),
             font_size = 14,
             font_style = "italic",
             font_color = "grey50",
             align = "left",
             margin = margin(t = 10))




# monthly deviations for the year 2023
ace_data %>%
  filter(year > 1972 & year < 2001) %>%
  group_by(month) %>%
  summarise(tmean_ref = mean(tmean, na.rm = TRUE)) %>%
  right_join(ace_data %>%
               filter(year == 2023) %>%
               group_by(month) %>%
               summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
               select(month, tmean),
             by = "month") %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = tmean_ref), colour = "#313695", linewidth = 1.5) +
  geom_point(aes(y = tmean_ref), colour = "#313695", size = 4, shape = 15) +
  geom_line(aes(y = tmean), colour = "#a50026", linewidth = 1.5) +
  geom_point(aes(y = tmean), colour = "#a50026", size = 4, shape = 16) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  scale_y_continuous(breaks = pretty_breaks(n = 11)) +
  labs(title = "Monthly average temperatures at Lanzarote Airport",
       subtitle = "Comparison between <span style=color:#a50026>**2023**</span> and 
       the <span style=color:#313695>**1973-2000 reference period**</span>",
       x = "Month",
       y = "Mean temperature (°C)",
       caption = "Data: Aemet.") +
  theme_bw(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 20, colour = "#003080"),
        plot.subtitle = element_markdown(size = 14, colour = "#5a5a5a"),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey50"),
        axis.title = element_text(face = "bold", colour = "#003080"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank())



# monthly temperature deviations
ace_data %>%
  filter(year > 1972 & year < 2001) %>%
  group_by(month) %>%
  summarise(tmean_ref = mean(tmean, na.rm = TRUE)) %>%
  right_join(ace_data %>%
               filter(year == 2023) %>%
               group_by(month) %>%
               summarise(tmean = mean(tmean, na.rm = TRUE)) %>%
               select(month, tmean),
             by = "month") %>%
  mutate(tmean_diff = tmean - tmean_ref) %>%
  mutate(across(starts_with("tmean"), ~round(., 2))) %>%
  relocate(tmean, .before = tmean_ref) %>%
  reactable(fullWidth = FALSE,
            pagination = FALSE,
            bordered = FALSE,
            defaultColDef = colDef(
              headerStyle = list(
                background = "#67000d",
                color = "#f0f0f0",
                align = "center",
                fontWeight = "bold",
                borderBottom = "1px solid #f0f0f0"
              ),
              align = "center",
              style = list(
                fontFamily = "Arial"
              )
            ),
            columns = list(
              month = colDef(name = "Month",
                             cell = function(value, index){
                               value <- month.name[index]
                             }),
              tmean = colDef(name = "2023"),
              tmean_ref = colDef(name = "1973-2000"),
              tmean_diff = colDef(name = "difference",
                                  style = function(value){
                                    if(value < 0){
                                      list(background = "#eff3ff")
                                    } else if(value >= 0 & value < 0.5){
                                      list(background = "white")
                                    } else if(value >= 0.5 & value < 1){
                                      list(background = "#fee5d9")
                                    } else if(value >= 1 & value < 1.5){
                                      list(background = "#fcbba1",
                                           fontWeight = "bold")
                                    } else if(value >= 1.5 & value < 2){
                                      list(background = "#fc9272",
                                           fontWeight = "bold")
                                    } else if(value >= 2 & value < 2.5){
                                      list(background = "#fb6a4a",
                                           color = "white",
                                           fontWeight = "bold")
                                    } else if(value >= 2.5 & value < 3){
                                      list(background = "#de2d26",
                                           color = "white",
                                           fontWeight = "bold")
                                    } else {
                                      list(background = "#a50f15",
                                           color = "white",
                                           fontWeight = "bold")
                                    }
                                  })
            )) %>%
  add_source(source = html("<strong>Table 3.</strong> Monthly average temperature differences.<br>
                           Data: Aemet."),
             font_size = 14,
             font_style = "italic",
             font_color = "grey50",
             align = "left",
             margin = margin(t = 10))


# precipitation

























