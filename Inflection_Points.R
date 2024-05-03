library(tidyverse)
library(RSQLite)
library(ggrepel)
library(ggpubr)
library(mgcv)

# theme_set(tRead::theme_tread_bw())

# Loading in the data and then cleaning it
myDB <- "C:/Users/nicol/OneDrive/Documents/Tread/Statcast Database/database.sqlite" # Absolute file location
myConn <- dbConnect(drv = SQLite(), dbname = myDB)
dbListTables(myConn)
MLB_result <- dbGetQuery(myConn, "SELECT * FROM statcast_2023 WHERE game_year >= 2022")
dbDisconnect(myConn)

# MLB_result <- load_seasons(2023) |> 
  #filter(game_type == "R")

MLB_result$pitch_name[MLB_result$pitch_name == "Split-Finger" |
                        MLB_result$pitch_name == "Changeup"] <- "Changeup-Splitter"

MLB_result$pitch_name[MLB_result$pitch_name == "Knuckle Curve"] <- "Curveball"
MLB_result$pitch_name[MLB_result$pitch_name == "2-Seam Fastball"] <- "Sinker"


pitch_types_allowed <- c("4-Seam Fastball", "Sinker", "Cutter", "Curveball", "Slider", "Changeup-Splitter")
#pitch_types_allowed <- c("Slider")

whiff_descriptions <- c("swinging_strike", "swinging_strike_blocked", 
                        "foul_tip", "missed_bunt", "bunt_foul_tip")


result <- MLB_result |>
  mutate(swing = ifelse(type %in% c("S", "B"), 1, 0),
         whiff = ifelse(grepl("Swinging", description), 1, 0),
         pfx_x_pv_adj = ifelse(p_throws == "R", pfx_x, -pfx_x)) |>
  as_tibble() |> 
  filter(pitch_name %in% pitch_types_allowed,
         release_speed >= 60) |>
  #mutate(pitch_name = case_when(pfx_x_pv_adj < -8 ~ "Sweeper",
                                #pfx_x_pv_adj > -3 ~ "Gyro")) %>%
  drop_na(pitch_name)

mean_df <- result |> 
  group_by(pitch_name) |> 
  summarise(mean_woba = mean(woba_value, na.rm = TRUE),
            mean_whiff = mean(whiff[swing == 1], na.rm = TRUE))

overall_df_VAA <- merge(result, mean_df) |> 
  mutate(woba_AA = woba_value - mean_woba,
         whiff_AA = if_else(swing == 1, whiff - mean_whiff, NA_real_))

overall_df <- overall_df_VAA %>% 
  filter(
    pfx_z <= 30 & pfx_z >= -30,
    pfx_x_pv_adj <= 3,
    pfx_x_pv_adj >= -25,
    release_speed >= 70,
    pitch_name != "Sinker" | pfx_x_pv_adj >= 0
  ) %>%
  ungroup()

#pitch_types_allowed <- c("Sweeper", "Gyro")
for(i in pitch_types_allowed){
  pitch_df <- overall_df %>% 
    group_by(pitcher) %>%
    filter(n() >= 25,
           pitch_name == i) %>% 
    ungroup() %>% 
    mutate(Velo = round(release_speed,1),
           Vert = round(pfx_z,1),
           Horz = round(pfx_x_pv_adj,1),
           # VAA = round(VAA_pred, 1),
           Rel = round(release_pos_z, 1),
           Ext = round(release_extension, 1),
    )
  
  pitch_Velo <- pitch_df %>% 
    group_by(Velo) %>% 
    summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
              whiff_AA = mean(whiff_AA, na.rm = TRUE),
              n = n()
    )
  
  pitch_Vert <- pitch_df %>% 
    group_by(Vert) %>% 
    summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
              whiff_AA = mean(whiff_AA, na.rm = TRUE),
              n = n()
    )
  
  pitch_Horz <- pitch_df %>% 
    group_by(Horz) %>% 
    summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
              whiff_AA = mean(whiff_AA, na.rm = TRUE),
              n = n()
    )
  
  # pitch_VAA <- pitch_df %>% 
  #   group_by(VAA) %>% 
  #   summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
  #             whiff_AA = mean(whiff_AA, na.rm = TRUE),
  #             n = n()
  #   )
  
  pitch_ext <- pitch_df %>%
    group_by(Ext) %>%
    summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
              whiff_AA = mean(whiff_AA, na.rm = TRUE),
              n = n()
    )

  Velo <- ggplot(pitch_Velo, aes(x = Velo)) +
    geom_hline(yintercept = 0) +
    #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
    scale_y_continuous(limits = c(-0.25, 0.25)) +
    geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
    geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
    labs(
      x = "Velocity",
      y = "Difference Against Average",
      title = "Velo"
    ) +
    guides(color = guide_legend(title = "Legend"))
  
  Vert <- ggplot(pitch_Vert, aes(x = Vert)) +
    geom_hline(yintercept = 0) +
    #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
    scale_y_continuous(limits = c(-0.25, 0.25)) +
    geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
    geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
    labs(
      x = "Vertical Break",
      y = "Difference Against Average",
      title = "Vertical Break"
    ) +
    guides(color = guide_legend(title = "Legend"))
  
  Horz <- ggplot(pitch_Horz, aes(x = Horz)) +
    geom_hline(yintercept = 0) +
    #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
    scale_y_continuous(limits = c(-0.25, 0.25)) +
    scale_color_gradient(low="blue", high="red") +
    geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
    geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
    labs(
      x = "Horizontal Break",
      y = "Difference Against Average",
      title = "Horizontal Break",
      caption = "Positive horizontal indicates ASR"
    ) +
    guides(color = guide_legend(title = "Legend"))
  
  
  # VAA <- ggplot(pitch_VAA, aes(x = VAA)) +
  #   geom_hline(yintercept = 0) +
  #   #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
  #   scale_y_continuous(limits = c(-0.25, 0.25)) +
  #   scale_color_gradient(low="blue", high="red") +
  #   geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") + 
  #   geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
  #   labs(
  #     x = "Vertical Approach Angle",
  #     y = "Difference Against Average",
  #     title = "Vertical Approach Angle"
  #   )
  
  Ext <- ggplot(pitch_ext, aes(x = Ext)) +
    geom_hline(yintercept = 0) +
    #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
    scale_y_continuous(limits = c(-0.25, 0.25)) +
    scale_color_gradient(low="blue", high="red") +
    geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") + 
    geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
    labs(
      x = "Release Extension (ft)",
      y = "Difference Against Average",
      title = "Extension"
    ) +
    guides(color = guide_legend(title = "Legend"))
# 
#   Rel <- ggplot(pitch_rel_ht, aes(x = Rel)) +
#     geom_hline(yintercept = 0) +
#     #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#     scale_y_continuous(limits = c(-0.25, 0.25)) +
#     scale_color_gradient(low="blue", high="red") +
#     geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
#     geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#     labs(
#       x = "Release Height (ft)",
#       y = "Difference Against Average",
#       title = "Release Height"
#     )
  # 
  # Vert_Rel <- ggplot(pitch_vert_rel, aes(x = Vert_Rel)) +
  #   geom_hline(yintercept = 0) +
  #   #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
  #   scale_y_continuous(limits = c(-0.25, 0.25)) +
  #   scale_color_gradient(low="blue", high="red") +
  #   geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") + 
  #   geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
  #   labs(
  #     x = "Vert/Rel Ht",
  #     y = "Difference Against Average",
  #     title = "Vert/Release Height"
  #   )
  # 
  # Vert_Ext <- ggplot(pitch_vert_ext, aes(x = Vert_Ext)) +
  #   geom_hline(yintercept = 0) +
  #   #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
  #   scale_y_continuous(limits = c(-0.25, 0.25)) +
  #   scale_color_gradient(low="blue", high="red") +
  #   geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") + 
  #   geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
  #   labs(
  #     x = "Vert/Ext Ht",
  #     y = "Difference Against Average",
  #     title = "Vert/Extension"
  #   )
  
  all_plots <- ggarrange(Velo, Vert, Horz, nrow = 1)
  
  figure <- annotate_figure(all_plots, top = text_grob(paste(paste0("2023 MLB Inflection Points - ", i), "\nBlue : wOBA   Red : Whiff Rate", sep = ""), face = "bold", size = 16))
  
  ggsave(paste0("C:/Users/nicol/Downloads/Tread/2023 Inflection Points/", i,".png"), height = 8, width = 12, dpi = "retina")
}


# 
# for (pitcher_hand in c("L", "R")){
#   for (batter_stand in c("L", "R")){
#     for(i in pitch_types_allowed){
#       pitch_df <- overall_df %>% 
#         filter(pitch_name == i, p_throws == pitcher_hand, stand == batter_stand) %>% 
#         group_by(pitcher) %>%
#         filter(n() >= 25) %>% 
#         ungroup() %>% 
#         mutate(Velo = round(release_speed,1),
#                Vert = round(pfx_z,1),
#                Horz = round(pfx_x_pv_adj,1),
#                VAA = round(VAA_pred, 1))
#       
#       pitch_Velo <- pitch_df %>% 
#         group_by(Velo) %>% 
#         summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                   whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                   n = n()
#         )
#       
#       pitch_Vert <- pitch_df %>% 
#         group_by(Vert) %>% 
#         summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                   whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                   n = n()
#         )
#       
#       pitch_Horz <- pitch_df %>% 
#         group_by(Horz) %>% 
#         summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                   whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                   n = n()
#         )
#       
#       pitch_VAA <- pitch_df %>% 
#         group_by(VAA) %>% 
#         summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                   whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                   n = n()
#         )
#       
#       
#       Velo <- ggplot(pitch_Velo, aes(x = Velo)) +
#         geom_hline(yintercept = 0) +
#         #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#         scale_y_continuous(limits = c(-0.25, 0.25)) +
#         geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
#         geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#         labs(
#           x = "Velocity",
#           y = "Difference Against Average",
#           title = "Velo"
#         )
#       
#       Vert <- ggplot(pitch_Vert, aes(x = Vert)) +
#         geom_hline(yintercept = 0) +
#         #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#         scale_y_continuous(limits = c(-0.25, 0.25)) +
#         geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
#         geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#         labs(
#           x = "Vertical Break",
#           y = "Difference Against Average",
#           title = "Vertical Break"
#         )
#       
#       Horz <- ggplot(pitch_Horz, aes(x = Horz)) +
#         geom_hline(yintercept = 0) +
#         #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#         scale_y_continuous(limits = c(-0.25, 0.25)) +
#         scale_color_gradient(low="blue", high="red") +
#         geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
#         geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#         labs(
#           x = "Horizontal Break",
#           y = "Difference Against Average",
#           title = "Horizontal Break",
#           caption = "Positive horizontal indicates ASR"
#         )
#       
#       
#       VAA <- ggplot(pitch_VAA, aes(x = VAA)) +
#         geom_hline(yintercept = 0) +
#         #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#         scale_y_continuous(limits = c(-0.25, 0.25)) +
#         scale_color_gradient(low="blue", high="red") +
#         geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") + 
#         geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#         labs(
#           x = "Vertical Approach Angle",
#           y = "Difference Against Average",
#           title = "Vertical Approach Angle"
#         )
#       
#       
#       all_plots <- ggarrange(Velo, Vert, Horz, VAA, nrow = 1)
#       
#       figure <- annotate_figure(all_plots, top = text_grob(paste(paste0("2021 MLB Inflection Points - ",pitcher_hand, "HP vs ", batter_stand, "HH ", i), "\nBlue : wOBA   Red : Whiff Rate", sep = ""), face = "bold", size = 16))
#       
#       ggsave(paste0("~/Desktop/2021 PH Inflection Points/", pitcher_hand, "HP vs ", batter_stand, "HH ", i,".png"), height = 8, width = 20, dpi = "retina")
#     }
#     
#   }
# }
# 
# for (pitcher_hand in c("L", "R")){
#   for(i in pitch_types_allowed){
#     pitch_df <- overall_df %>% 
#       filter(pitch_name == i, p_throws == pitcher_hand) %>% 
#       group_by(pitcher) %>%
#       filter(n() >= 50) %>% 
#       ungroup() %>% 
#       mutate(Velo = round(release_speed,1),
#              Vert = round(pfx_z,1),
#              Horz = round(pfx_x_pv_adj,1),
#              VAA = round(VAA_pred, 1))
#     
#     pitch_Velo <- pitch_df %>% 
#       group_by(Velo) %>% 
#       summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                 whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                 n = n()
#       )
#     
#     pitch_Vert <- pitch_df %>% 
#       group_by(Vert) %>% 
#       summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                 whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                 n = n()
#       )
#     
#     pitch_Horz <- pitch_df %>% 
#       group_by(Horz) %>% 
#       summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                 whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                 n = n()
#       )
#     
#     pitch_VAA <- pitch_df %>% 
#       group_by(VAA) %>% 
#       summarise(wOBA_AA = mean(woba_AA, na.rm = TRUE),
#                 whiff_AA = mean(whiff_AA, na.rm = TRUE),
#                 n = n()
#       )
#     
#     
#     Velo <- ggplot(pitch_Velo, aes(x = Velo)) +
#       geom_hline(yintercept = 0) +
#       #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#       scale_y_continuous(limits = c(-0.25, 0.25)) +
#       geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
#       geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#       labs(
#         x = "Velocity",
#         y = "Difference Against Average",
#         title = "Velo"
#       )
#     
#     Vert <- ggplot(pitch_Vert, aes(x = Vert)) +
#       geom_hline(yintercept = 0) +
#       #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#       scale_y_continuous(limits = c(-0.25, 0.25)) +
#       geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
#       geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#       labs(
#         x = "Vertical Break",
#         y = "Difference Against Average",
#         title = "Vertical Break"
#       )
#     
#     Horz <- ggplot(pitch_Horz, aes(x = Horz)) +
#       geom_hline(yintercept = 0) +
#       #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#       scale_y_continuous(limits = c(-0.25, 0.25)) +
#       scale_color_gradient(low="blue", high="red") +
#       geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") +
#       geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#       labs(
#         x = "Horizontal Break",
#         y = "Difference Against Average",
#         title = "Horizontal Break",
#         caption = "Positive horizontal indicates ASR"
#       )
#     
#     
#     VAA <- ggplot(pitch_VAA, aes(x = VAA)) +
#       geom_hline(yintercept = 0) +
#       #geom_point(aes(size = n), alpha = 0.75, show.legend = FALSE) +
#       scale_y_continuous(limits = c(-0.25, 0.25)) +
#       scale_color_gradient(low="blue", high="red") +
#       geom_smooth(aes(y = wOBA_AA), method = "loess", color = "blue") + 
#       geom_smooth(aes(y = whiff_AA), method = "loess", color = "red") +
#       labs(
#         x = "Vertical Approach Angle",
#         y = "Difference Against Average",
#         title = "Vertical Approach Angle"
#       )
#     
#     
#     all_plots <- ggarrange(Velo, Vert, Horz, VAA, nrow = 1)
#     
#     figure <- annotate_figure(all_plots, top = text_grob(paste(paste0("2021 MLB Inflection Points - ",pitcher_hand, "HP ", i), "\nBlue : wOBA   Red : Whiff Rate", sep = ""), face = "bold", size = 16))
#     
#     ggsave(paste0("~/Desktop/2021 P Inflection Points/", pitcher_hand, "HP ", i,".png"), height = 8, width = 20, dpi = "retina")
#   }
# }
# 
# 