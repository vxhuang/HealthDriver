pop_all_2017_for_y0 <- read.csv("~/HIAData/population_all_2017_y0_age_groups.csv", stringsAsFactors = F)
pop_ssp22050_for_y0 <- read.csv("~/HIAData/population_ssp2_2050_y0.csv", stringsAsFactors = F)
pop_ssp32050_for_y0 <- read.csv("~/HIAData/population_ssp3_2050_y0.csv", stringsAsFactors = F)
pop_ssp52050_for_y0 <- read.csv("~/HIAData/population_ssp5_2050_y0.csv", stringsAsFactors = F)

pop_all_2017_for_y0 <- pop_all_2017_for_y0 %>% 
  mutate(STATE = sprintf("%02d", STATE), COUNTY = sprintf("%03d", COUNTY))
pop_all_2017_for_y0 <- pop_all_2017_for_y0 %>% mutate(Other = AA + IA)
names(pop_all_2017_for_y0)[7:9] <- c("WNH", "Hispanic", "BNH")
pop_ssp22050_for_y0 <- pop_ssp22050_for_y0 %>% 
  mutate(STATE = sprintf("%02d", STATE), COUNTY = sprintf("%03d", COUNTY), GEOID10 = sprintf("%05d", GEOID10))
pop_ssp32050_for_y0 <- pop_ssp32050_for_y0 %>% 
  mutate(STATE = sprintf("%02d", STATE), COUNTY = sprintf("%03d", COUNTY), GEOID10 = sprintf("%05d", GEOID10))
pop_ssp52050_for_y0 <- pop_ssp52050_for_y0 %>% 
  mutate(STATE = sprintf("%02d", STATE), COUNTY = sprintf("%03d", COUNTY), GEOID10 = sprintf("%05d", GEOID10))

# Notes: 
# Air quality models: 1. resultsInMAP; 2. jie_wrf; 3. resultsISRM
# Scenarios: nz, race2
# SSPs: ssp2, ssp3, ssp5

# uniform y0 uniform RR
resultsInMAP_nz_HIA <- resultsInMAP_nz_cty %>% left_join(pop_ssp22050_for_y0, 
                                                         by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
resultsInMAP_nz_HIA <- resultsInMAP_nz_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                                         by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
resultsInMAP_nz_HIA <- resultsInMAP_nz_HIA %>% mutate(deaths_WNH = Value * (1 - 1 / exp(beta * TotalPM25)) * WNH, 
                                                      deaths_Hispanic = Value * (1 - 1 / exp(beta * TotalPM25)) * Hispanic, 
                                                      deaths_BNH = Value * (1 - 1 / exp(beta * TotalPM25)) * BNH, 
                                                      deaths_Other = Value * (1 - 1 / exp(beta * TotalPM25)) * Other)
resultsInMAP_nz_HIA %>% 
  mutate(pop_white = WNH, pop_nonwhite = BNH, 
         deaths_white = deaths_WNH, deaths_nonwhite = deaths_BNH) %>%
  select(STATEFP, COUNTYFP, fips, TotalPM25, pop_white, pop_nonwhite, year, deaths_white, deaths_nonwhite) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(pop_white = sum(pop_white), pop_nonwhite = sum(pop_nonwhite), 
            deaths_white = sum(deaths_white), deaths_nonwhite = sum(deaths_nonwhite), .groups = "drop") %>%
  select(TotalPM25, pop_white, pop_nonwhite, deaths_white, deaths_nonwhite) %>% drop_na() %>%
  summarise(pm_white = weighted.mean(TotalPM25, pop_white, na.rm = T), pm_nonwhite = weighted.mean(TotalPM25, pop_nonwhite, na.rm = T), 
            pop_white = sum(pop_white, na.rm = T), pop_nonwhite = sum(pop_nonwhite, na.rm = T), 
            deaths_white = sum(deaths_white, na.rm = T), deaths_nonwhite = sum(deaths_nonwhite, na.rm = T)) %>% 
  mutate(dr_white = deaths_white / pop_white, dr_nonwhite = deaths_nonwhite / pop_nonwhite) %>% drop_na()

# uniform y0 different RR

resultsInMAP_nz_di <- resultsInMAP_nz_cty %>% left_join(pop_ssp22050_for_y0,
                                                        by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
resultsInMAP_nz_di <- resultsInMAP_nz_di %>% left_join(y0 %>% filter(year == 2050),
                                                       by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
resultsInMAP_nz_di <- resultsInMAP_nz_di %>% mutate(deaths_WNH = Value * (1 - 1 / exp(beta_di_nhwa * TotalPM25)) * WNH,
                                                    deaths_Hispanic = Value * (1 - 1 / exp(beta_di_hispanic * TotalPM25)) * Hispanic,
                                                    deaths_BNH = Value * (1 - 1 / exp(beta_di_black * TotalPM25)) * BNH,
                                                    deaths_Other = Value * (1 - 1 / exp(beta_di_asian * TotalPM25)) * Other)
resultsInMAP_nz_di %>%
  mutate(pop_white = WNH, pop_nonwhite = BNH,
         deaths_white = deaths_WNH, deaths_nonwhite = deaths_BNH) %>%
  select(STATEFP, COUNTYFP, fips, TotalPM25, pop_white, pop_nonwhite, year, deaths_white, deaths_nonwhite) %>%
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>%
  summarise(pop_white = sum(pop_white), pop_nonwhite = sum(pop_nonwhite),
            deaths_white = sum(deaths_white), deaths_nonwhite = sum(deaths_nonwhite), .groups = "drop") %>%
  select(TotalPM25, pop_white, pop_nonwhite, deaths_white, deaths_nonwhite) %>% drop_na() %>%
  summarise(pm_white = weighted.mean(TotalPM25, pop_white, na.rm = T), pm_nonwhite = weighted.mean(TotalPM25, pop_nonwhite, na.rm = T),
            pop_white = sum(pop_white, na.rm = T), pop_nonwhite = sum(pop_nonwhite, na.rm = T),
            deaths_white = sum(deaths_white, na.rm = T), deaths_nonwhite = sum(deaths_nonwhite, na.rm = T)) %>%
  mutate(dr_white = deaths_white / pop_white, dr_nonwhite = deaths_nonwhite / pop_nonwhite) %>% drop_na()

# different y0 uniform RR
resultsInMAP_nz_HIA <- resultsInMAP_nz_cty %>% left_join(pop_ssp22050_for_y0, 
                                                         by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
resultsInMAP_nz_HIA <- resultsInMAP_nz_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                                         by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
resultsInMAP_nz_HIA <- resultsInMAP_nz_HIA %>% left_join(y0_race, by = c("AGEGRP")) %>%
  mutate(deaths_WNH = Value * (1 - 1 / exp(beta * TotalPM25)) * WNH * wa, 
         deaths_Hispanic = Value * (1 - 1 / exp(beta * TotalPM25)) * Hispanic * his, 
         deaths_BNH = Value * (1 - 1 / exp(beta * TotalPM25)) * BNH * ba, 
         deaths_Other = Value * (1 - 1 / exp(beta * TotalPM25)) * Other * other)
resultsInMAP_nz_HIA %>% 
  mutate(pop_white = WNH, pop_nonwhite = BNH, 
         deaths_white = deaths_WNH, deaths_nonwhite = deaths_BNH) %>%
  select(STATEFP, COUNTYFP, fips, TotalPM25, pop_white, pop_nonwhite, year, deaths_white, deaths_nonwhite) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(pop_white = sum(pop_white), pop_nonwhite = sum(pop_nonwhite), 
            deaths_white = sum(deaths_white), deaths_nonwhite = sum(deaths_nonwhite), .groups = "drop") %>%
  select(TotalPM25, pop_white, pop_nonwhite, deaths_white, deaths_nonwhite) %>% drop_na() %>%
  summarise(pm_white = weighted.mean(TotalPM25, pop_white, na.rm = T), pm_nonwhite = weighted.mean(TotalPM25, pop_nonwhite, na.rm = T), 
            pop_white = sum(pop_white, na.rm = T), pop_nonwhite = sum(pop_nonwhite, na.rm = T), 
            deaths_white = sum(deaths_white, na.rm = T), deaths_nonwhite = sum(deaths_nonwhite, na.rm = T)) %>% 
  mutate(dr_white = deaths_white / pop_white, dr_nonwhite = deaths_nonwhite / pop_nonwhite) %>% drop_na()

# different y0 different RRR

resultsInMAP_nz_di <- resultsInMAP_nz_cty %>% left_join(pop_ssp22050_for_y0, 
                                                        by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
resultsInMAP_nz_di <- resultsInMAP_nz_di %>% left_join(y0 %>% filter(year == 2050), 
                                                       by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
resultsInMAP_nz_di <- resultsInMAP_nz_di %>% left_join(y0_race, by = c("AGEGRP")) %>%
  mutate(deaths_WNH = Value * (1 - 1 / exp(beta_di_nhwa * TotalPM25)) * WNH * wa, 
         deaths_Hispanic = Value * (1 - 1 / exp(beta_di_hispanic * TotalPM25)) * Hispanic * his, 
         deaths_BNH = Value * (1 - 1 / exp(beta_di_black * TotalPM25)) * BNH * ba, 
         deaths_Other = Value * (1 - 1 / exp(beta_di_asian * TotalPM25)) * Other * other)
resultsInMAP_nz_di %>% 
  mutate(pop_white = WNH, pop_nonwhite = BNH, 
         deaths_white = deaths_WNH, deaths_nonwhite = deaths_BNH) %>%
  select(STATEFP, COUNTYFP, fips, TotalPM25, pop_white, pop_nonwhite, year, deaths_white, deaths_nonwhite) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(pop_white = sum(pop_white), pop_nonwhite = sum(pop_nonwhite), 
            deaths_white = sum(deaths_white), deaths_nonwhite = sum(deaths_nonwhite), .groups = "drop") %>%
  select(TotalPM25, pop_white, pop_nonwhite, deaths_white, deaths_nonwhite) %>% drop_na() %>%
  summarise(pm_white = weighted.mean(TotalPM25, pop_white, na.rm = T), pm_nonwhite = weighted.mean(TotalPM25, pop_nonwhite, na.rm = T), 
            pop_white = sum(pop_white, na.rm = T), pop_nonwhite = sum(pop_nonwhite, na.rm = T), 
            deaths_white = sum(deaths_white, na.rm = T), deaths_nonwhite = sum(deaths_nonwhite, na.rm = T)) %>% 
  mutate(dr_white = deaths_white / pop_white, dr_nonwhite = deaths_nonwhite / pop_nonwhite) %>% drop_na()

#########
# 2017 
resultsInMAP_ref_HIA <- resultsInMAP_ref_cty %>% left_join(pop_all_2017_for_y0, by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
resultsInMAP_ref_HIA <- resultsInMAP_ref_HIA %>% left_join(y0 %>% filter(year == 2015), 
                                                           by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
resultsInMAP_ref_HIA <- resultsInMAP_ref_HIA %>% mutate(deaths_WNH = Value * (1 - 1 / exp(beta * TotalPM25)) * WNH, 
                                                        deaths_Hispanic = Value * (1 - 1 / exp(beta * TotalPM25)) * Hispanic, 
                                                        deaths_BNH = Value * (1 - 1 / exp(beta * TotalPM25)) * BNH, 
                                                        deaths_Other = Value * (1 - 1 / exp(beta * TotalPM25)) * Other)
resultsInMAP_ref_HIA %>% 
  mutate(pop_white = WNH, pop_nonwhite = BNH, 
         deaths_white = deaths_WNH, deaths_nonwhite = deaths_BNH) %>%
  select(STATEFP, COUNTYFP, fips, TotalPM25, pop_white, pop_nonwhite, year, deaths_white, deaths_nonwhite) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(pop_white = sum(pop_white), pop_nonwhite = sum(pop_nonwhite), 
            deaths_white = sum(deaths_white), deaths_nonwhite = sum(deaths_nonwhite), .groups = "drop") %>%
  select(TotalPM25, pop_white, pop_nonwhite, deaths_white, deaths_nonwhite) %>% drop_na() %>%
  summarise(pm_white = weighted.mean(TotalPM25, pop_white, na.rm = T), pm_nonwhite = weighted.mean(TotalPM25, pop_nonwhite, na.rm = T), 
            pop_white = sum(pop_white, na.rm = T), pop_nonwhite = sum(pop_nonwhite, na.rm = T), 
            deaths_white = sum(deaths_white, na.rm = T), deaths_nonwhite = sum(deaths_nonwhite, na.rm = T)) %>% 
  mutate(dr_white = deaths_white / pop_white, dr_nonwhite = deaths_nonwhite / pop_nonwhite) %>% drop_na()
# 2050 ref
resultsInMAP_ref50_HIA <- resultsInMAP_ref50_cty %>% left_join(pop_ssp22050_for_y0, 
                                             by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
resultsInMAP_ref50_HIA <- resultsInMAP_ref50_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                             by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
resultsInMAP_ref50_HIA <- resultsInMAP_ref50_HIA %>% mutate(deaths_WNH = Value * (1 - 1 / exp(beta * TotalPM25)) * WNH, 
                                          deaths_Hispanic = Value * (1 - 1 / exp(beta * TotalPM25)) * Hispanic, 
                                          deaths_BNH = Value * (1 - 1 / exp(beta * TotalPM25)) * BNH, 
                                          deaths_Other = Value * (1 - 1 / exp(beta * TotalPM25)) * Other)
resultsInMAP_ref50_HIA %>% 
  mutate(pop_white = WNH, pop_nonwhite = BNH, 
         deaths_white = deaths_WNH, deaths_nonwhite = deaths_BNH) %>%
  select(STATEFP, COUNTYFP, fips, TotalPM25, pop_white, pop_nonwhite, year, deaths_white, deaths_nonwhite) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(pop_white = sum(pop_white), pop_nonwhite = sum(pop_nonwhite), 
            deaths_white = sum(deaths_white), deaths_nonwhite = sum(deaths_nonwhite), .groups = "drop") %>%
  select(TotalPM25, pop_white, pop_nonwhite, deaths_white, deaths_nonwhite) %>% drop_na() %>%
  summarise(pm_white = weighted.mean(TotalPM25, pop_white, na.rm = T), pm_nonwhite = weighted.mean(TotalPM25, pop_nonwhite, na.rm = T), 
            pop_white = sum(pop_white, na.rm = T), pop_nonwhite = sum(pop_nonwhite, na.rm = T), 
            deaths_white = sum(deaths_white, na.rm = T), deaths_nonwhite = sum(deaths_nonwhite, na.rm = T)) %>% 
  mutate(dr_white = deaths_white / pop_white, dr_nonwhite = deaths_nonwhite / pop_nonwhite) %>% drop_na()

########


draw_key_multi_points <- function(data, params, size) {
  col <- if (!is.null(data$colour)) data$colour[1] else "black"
  grid::pointsGrob(
    x    = grid::unit(c(0.25, 0.50, 0.75), "npc"),
    y    = grid::unit(rep(0.50, 3), "npc"),
    pch  = c(1, 2, 19),                    # circle, triangle, square
    size = grid::unit(1.8, "mm"),
    gp   = grid::gpar(col = col)
  )
}


ribbon_exposure <- data.frame(key = -2:3, 
                              max = c(7.84, 5.07, 4.52, 4.53, 3.56, 6.59), # for Black
                              min = c(6.57, 4.38, 3.93, 3.94, 3.38, 6.22)) # for White

fig_2b <-
ggplot() +
  annotate(geom = "rect", xmin = -0.4, ymin = 0, xmax = 3.5, ymax = 10, fill = "grey", alpha = .2) +
  geom_ribbon(data = ribbon_exposure, mapping = aes(x = key, ymax = max, ymin = min), 
              fill = "linen") +
  geom_point(data = ribbon_exposure[3:6, ], mapping = aes(x = key, y = max, color = "ba", shape = "nz"), size = 3, key_glyph = draw_key_multi_points) +
  geom_point(data = ribbon_exposure[3:6, ], mapping = aes(x = key, y = min, color = "wa", shape = "nz"), size = 3, key_glyph = draw_key_multi_points) +
  geom_line(data = ribbon_exposure[3:6, ], mapping = aes(x = key, y = max, color = "ba"), size = 1, show.legend = F) +
  geom_line(data = ribbon_exposure[3:6, ], mapping = aes(x = key, y = min, color = "wa"), size = 1, show.legend = F) +
  labs(x = "", y = "&mu;g/m<sup>3</sup>", 
       title = "<b>b) National average pollution disparity: PM<sub>2.5</sub> exposure for <br>the Black and the White populations</b>",
       color = "2050 Net-Zero scenarios: ", shape = "", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        plot.title = element_markdown(), axis.text.x = element_markdown(size = 13),
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_blank(), 
        legend.key = element_rect(fill = "transparent", color = NA), 
        legend.box.background = element_rect(fill = "transparent", color = "black"), legend.background = element_rect(fill = "transparent"),
        legend.key.width = grid::unit(2.2, "lines"),
        legend.position = c(.2, .15), plot.margin = margin(5.5, 5.5, 55, 5.5)
  ) +
  coord_cartesian(clip = "off") + 
  scale_x_continuous(limits = c(-2.5, 3.5), expand = c(0, 0), breaks = -2:3, 
                     labels = c("", "", "Energy<br>decarbonization", "Socioeconomic<br>factors", 
                                "Emission<br>downscaling", "Atmospheric<br>processses")) + 
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_color_manual(labels = c("Exposure for the Black population", 
                                "Exposure for the White population"), 
                     values = c("grey30", "brown4")) +
  # Black points
  geom_point(aes(x = -2, y = 7.84, shape = "ref"), color = "grey30", size = 3, stroke = 1) +
  annotate("segment", x = -2, y = 7.84, xend = -1, yend = 5.07, color = "grey30", linetype = "dashed") +
  geom_point(aes(x = -1, y = 5.07, shape = "ref50"), color = "grey30", size = 3, stroke = 1) +
  annotate("segment", x = -1, y = 5.07, xend = 0, yend = 4.52, color = "grey30", linetype = "dashed") +
  annotate("point", x = 0, y = 4.52, color = "grey30", size = 3) +
  # White points
  geom_point(aes(x = -2, y = 6.57, shape = "ref"), color = "brown4", size = 3, stroke = 1) +
  annotate("segment", x = -2, y = 6.57, xend = -1, yend = 4.38, color = "brown4", linetype = "dashed") +
  geom_point(aes(x = -1, y = 4.38, shape = "ref50"), color = "brown4", size = 3, stroke = 1) +
  annotate("segment", x = -1, y = 4.38, xend = 0, yend = 3.93, color = "brown4", linetype = "dashed") +
  annotate("point", x = 0, y = 3.93, color = "brown4", size = 3) + 
  scale_shape_manual(values = c("ref" = 1, "ref50" = 2, "nz" = 19), 
                     labels = c("ref" = "2017", "ref50" = "2050 Reference", "nz" = "2050 Net-Zero"), 
                     limits = c("ref", "ref50", "nz"), guide = "none") +
  # guides(shape = guide_legend(order = 1, override.aes = list(color = "black"))) + 
  geom_richtext(label = "<b>2017</b>", aes(x = -2, y = 9), label.color = NA, size = 5) + 
  geom_richtext(label = "<b>2050 Reference</b>", aes(x = -1, y = 9), label.color = NA, size = 5) + 
  geom_richtext(label = "<b>2050 Net-Zero</b>", aes(x = 1.5, y = 9), label.color = NA, fill = NA, size = 5) + 
  # annotate("text", x = -2, y = 8.4, label = "All benchmark") + 
  annotate("text", x = -2, y = 5.9, label = "All benchmark") + 
  # annotate("text", x = -1, y = 6.5, label = "All benchmark") + 
  annotate("text", x = -1, y = 3.5, label = "All benchmark") + 
  # annotate("text", x = 0, y = 5.5, label = "All benchmark") + 
  annotate("text", x = 0, y = 3.5, label = "All benchmark") + 
  annotate("text", x = 1, y = 5.1, label = "SSP3") +
  # annotate("text", x = 1, y = 3.4, label = "SSP5") + 
  # annotate("text", x = 2, y = 5.6, label = "Benchmark") + 
  annotate("text", x = 2, y = 2.7, label = "Equity-oriented") + 
  annotate("text", x = 3, y = 7.3, label = "WRF-Chem") +
  # annotate("text", x = 3, y = 2.6, label = "Benchmark") +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(4/12 + .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(4/12 + .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(4/12 + .01, "npc"), y0 = unit(-0.15, "npc"),  
      x1 = unit(8/12 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(8/12 - .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(8/12 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(8/12 + .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(8/12 + .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(8/12 + .01, "npc"), y0 = unit(-0.15, "npc"),  
      x1 = unit(12/12 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(12/12 - .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(12/12 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = textGrob(
      x = unit(6/12, "npc"), y = unit(-0.225, "npc"),  
      gp = gpar(lineheight = 0.8, fontsize = 13), 
      label = "Macro-scale\nfactors"
    )
  ) +
  annotation_custom(
    grob = textGrob(
      x = unit(10/12, "npc"), y = unit(-0.225, "npc"),  
      gp = gpar(lineheight = 0.8, fontsize = 13), 
      label = "Micro-scale\nfactors"
    )
  ) +
  annotate("segment", x = 0.5, xend = 0.7, y = 4.25, yend = 2.5, color = "chocolate4") + 
  annotate("text", x = 0.7, y = 2, label = "Disparity", size = 6, color = "chocolate4") 

#### Variant for dr

ribbon_dr <- data.frame(key = -2:5, 
                              max = c(291, 298, 267, 275, 223, 409, 440, 1360), # for Black
                              min = c(395, 276, 249, 258, 227, 413, 434, 454)) # for White

fig_2c <-
ggplot() +
  annotate(geom = "rect", xmin = -0.4, ymin = 0, xmax = 5.5, ymax = 1500, fill = "grey", alpha = .2) +
  geom_ribbon(data = ribbon_dr, mapping = aes(x = key, ymax = max, ymin = min), 
              fill = "linen") +
  geom_point(data = ribbon_dr[3:8, ], mapping = aes(x = key, y = max, color = "ba", shape = "nz"), size = 2, key_glyph = draw_key_multi_points) +
  geom_point(data = ribbon_dr[3:8, ], mapping = aes(x = key, y = min, color = "wa", shape = "nz"), size = 2, key_glyph = draw_key_multi_points) +
  geom_line(data = ribbon_dr[3:8, ], mapping = aes(x = key, y = max, color = "ba"), size = .5, show.legend = F) +
  geom_line(data = ribbon_dr[3:8, ], mapping = aes(x = key, y = min, color = "wa"), size = .5, show.legend = F) +
  labs(x = "", y = "Annual deaths per million", 
       title = "<b>c) National average health disparity: PM<sub>2.5</sub>-attributable death rates for <br>the Black and the White populations</b>",
       color = "2050 Net-Zero scenarios: ", shape = "", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        plot.title = element_markdown(), axis.text.x = element_markdown(size = 13),
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_blank(), 
        legend.key = element_rect(fill = "transparent", color = NA), 
        legend.box.background = element_rect(fill = "transparent", color = "black"), legend.background = element_rect(fill = "transparent"),
        legend.key.width = grid::unit(2.2, "lines"),
        legend.position = c(.2, .65), plot.margin = margin(5.5, 5.5, 55, 5.5)
  ) +
  coord_cartesian(clip = "off") + 
  scale_x_continuous(limits = c(-2.5, 5.5), expand = c(0, 0), breaks = -2:5, 
                     labels = c("", "", "Energy<br>decarbonization", "Socioeconomic<br>factors", 
                                "Emission<br>downscaling", "Atmospheric<br>processses", 
                                "Baseline<br>mortality rates", "Relative<br>risks")) + 
  scale_y_continuous(limits = c(0, 1500), expand = c(0, 0)) +
  scale_color_manual(labels = c("Death rates for the Black population", 
                                "Death rates for the White population"), 
                     values = c("grey30", "brown4")) +
  # Black points
  geom_point(aes(x = -2, y = 291, shape = "ref"), color = "grey30", size = 3, stroke = 1) +
  annotate("segment", x = -2, y = 291, xend = -1, yend = 298, color = "grey30", linetype = "dashed") +
  geom_point(aes(x = -1, y = 298, shape = "ref50"), color = "grey30", size = 3, stroke = 1) +
  annotate("segment", x = -1, y = 298, xend = 0, yend = 267, color = "grey30", linetype = "dashed") +
  annotate("point", x = 0, y = 267, color = "grey30", size = 3) +
  # White points
  geom_point(aes(x = -2, y = 395, shape = "ref"), color = "brown4", size = 3, stroke = 1) +
  annotate("segment", x = -2, y = 395, xend = -1, yend = 276, color = "brown4", linetype = "dashed") +
  geom_point(aes(x = -1, y = 276, shape = "ref50"), color = "brown4", size = 3, stroke = 1) +
  annotate("segment", x = -1, y = 276, xend = 0, yend = 249, color = "brown4", linetype = "dashed") +
  annotate("point", x = 0, y = 249, color = "brown4", size = 3) + 
  scale_shape_manual(values = c("ref" = 1, "ref50" = 2, "nz" = 19), 
                     labels = c("ref" = "2017", "ref50" = "2050 Reference", "nz" = "2050 Net-Zero"), 
                     limits = c("ref", "ref50", "nz"), guide = "none") +
  # guides(shape = guide_legend(order = 1, override.aes = list(color = "black"))) + 
  geom_richtext(label = "<b>2017</b>", aes(x = -2, y = 1350), label.color = NA, size = 5) + 
  geom_richtext(label = "<b>2050 Reference</b>", aes(x = -1, y = 1350), label.color = NA, size = 5) + 
  geom_richtext(label = "<b>2050 Net-Zero</b>", aes(x = 2.5, y = 1350), label.color = NA, fill = NA, size = 5) + 
  # annotate("text", x = -2, y = 500, label = "All benchmark") + 
  annotate("text", x = -2, y = 225, label = "All benchmark") + 
  # annotate("text", x = -1, y = 450, label = "All benchmark") + 
  annotate("text", x = -1, y = 175, label = "All benchmark") +   
  # annotate("text", x = 0, y = 425, label = "All benchmark") + 
  annotate("text", x = 0, y = 150, label = "All benchmark") + 
  annotate("text", x = 1, y = 400, label = "SSP5") + 
  # annotate("text", x = 1, y = 150, label = "SSP3") + 
  # annotate("text", x = 2, y = 400, label = "Benchmark") + 
  annotate("text", x = 2, y = 100, label = "Equity-oriented") + 
  annotate("text", x = 2.9, y = 550, label = "WRF-Chem") + 
  # annotate("text", x = 3, y = 100, label = "Benchmark") +
  annotate("text", x = 3.7, y = 650, label = "Race-specific") + 
  # annotate("text", x = 4, y = 100, label = "Benchmark") +
  annotate("text", x = 4.5, y = 1400, label = "Race-specific") + 
  # annotate("text", x = 5, y = 100, label = "Benchmark") +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(4/16 + .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(4/16 + .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(4/16 + .01, "npc"), y0 = unit(-0.15, "npc"),  
      x1 = unit(8/16 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(8/16 - .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(8/16 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(8/16 + .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(8/16 + .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(8/16 + .01, "npc"), y0 = unit(-0.15, "npc"),  
      x1 = unit(16/16 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(16/16 - .01, "npc"), y0 = unit(-0.125, "npc"),  
      x1 = unit(16/16 - .01, "npc"), y1 = unit(-0.15, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = textGrob(
      x = unit(6/16, "npc"), y = unit(-0.225, "npc"),  
      gp = gpar(lineheight = 0.8, fontsize = 13), 
      label = "Macro-scale\nfactors"
    )
  ) +
  annotation_custom(
    grob = textGrob(
      x = unit(12/16, "npc"), y = unit(-0.225, "npc"),  
      gp = gpar(lineheight = 0.8, fontsize = 13), 
      label = "Micro-scale\nfactors"
    )
  ) +
  annotate("text", x = 4.6, y = 600, label = "Disparity", size = 6, color = "chocolate4") 

fig2 <- ggarrange(p2, fig_2b, fig_2c, NULL,
                  nrow = 4, widths = 11, heights = c(4.5, 5, 5, .1), align = "v")
ggsave(filename = "~/Documents/PSU/2023 Spring/ModelsComparison/Viz_Maintext_v1/Fig2.png", width = 11, height = 14.6, plot = fig2)
