source('aci_functions.R')
library(tidyverse)
library(data.table)
library(osmdata)
library(ggmap)
library(ggthemes)
library(cowplot)
library(ggh4x)


bb_paris <- getbb("Paris")
bb_paris[1] = 2.241749
bb_paris[3] = 2.426782
paris_tiles <- get_map(bb_paris,source = "stamen")

utrecht_dir = "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht"
path_to_buildings <- file.path(utrecht_dir,"R5/building_as_origins_reduced.csv")


setwd("D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris")

inperiph = fread("D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/R5/building_as_origins_periph.csv")
#  
inperiph = as.character(inperiph$id)
#  
buildings_together <- fread(path_to_buildings) %>% left_join(fread("codeact_per_bat_2.csv"),
                                                               by = c("id" = "from_id")) %>%
    filter(id %in% inperiph)
#  
#  
# filtered_out_amenities <- c("AB102","AC102","AF102","AA102","AF104","AB106",
#                              "AB103","AB104","AB105","AF103","AB101","af104",
#                              "AF104","aa101","AA101","Af104","Af102","af102",
#                              "SB301","CI201","SA506","AD107", "AD105")
#  
filtered_out_amenities <- c("AB102","AC102","AF102","AA102","AF104","AB106",
                              "AB103","AB104","AB105","AF103","AB101","af104",
                              "AF104","aa101","AA101","Af104","Af102","af102",
                              "SB301", "")
# # #Exclude ambulances and infirmières because there is no local consumption
# # 
voitures_a_aggreger = c("CG102","CG103","CG104","CG201","CG202")
# # 
motos_a_aggreger = c("CG106", "CG204")
# # 
# # ########## TO DO : TO Upper ce403 , cb111 and aggregate
# # ########## Figure out if agg is better or presence is better
# # ########## Which are best to aggregate with CA115? All organic foods
# # 
# tobio <- c("CA202","CH303","CC202","CA202","CC201","CA301","CA103","CA108","CA302", "CC202")
# # 
# # 
# 
# 
buildings_together <- buildings_together %>% filter(!CODE_ACTIVITE %in% filtered_out_amenities)
# 
# 
buildings_together[buildings_together$CODE_ACTIVITE %in% voitures_a_aggreger,]$CODE_ACTIVITE = "Car related"
buildings_together[buildings_together$CODE_ACTIVITE %in% motos_a_aggreger,]$CODE_ACTIVITE = "Motorbike related"
# 
buildings_together$CODE_ACTIVITE = toupper(buildings_together$CODE_ACTIVITE)
# 
buildings_together <- buildings_together %>%
   group_by(id, CODE_ACTIVITE, lon, lat) %>%
   summarise(Year14 = sum(Year14, na.rm = T),
             Year17 = sum(Year17, na.rm = T),
             Year20 = sum(Year20, na.rm = T),
             Year23 = sum(Year23, na.rm = T))
# 
gc()

#fwrite(buildings_together, "codeact_per_bat_cleaned.csv")

#Filter out drive piéton (only 2020, 5 of them total..) and CBD (Only 2023 (no)) and Pop-up Store (no)??
#Pole emploi ofc SA506, hotel de préfecture too
filter_again = c("","CA305","SA506","CI201")
#All medical amenities missing in 2023
# -> Ignore medical


########################### START HERE #####################################


# bad_ids <- as.data.frame(as.character(unique(ACI_all_rolling[ACI_all_rolling$diversity < 20,]$id)))
# names(bad_ids)[1] = "id"
# fwrite(bad_ids,"bad_ids.csv")

bad_ids <- fread("bad_ids.csv")

buildings_together <- fread("codeact_per_bat_cleaned.csv") %>% 
  filter(!(CODE_ACTIVITE %in% filter_again) & !(substr(CODE_ACTIVITE,1,1)=="A") & !(id %in% bad_ids$id))

names <- fread("D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/Translated_amenity_types_2020.csv")


gc()




fixed_lemonpie <- function(aci_df, origins, ranks = T, n = 2, evol_ranks = F, legend = T,
                           base_year = "2014", end_year = "2023"){
  #aci_df$Year = paste0("20",substr(aci_df$id,1,2))
  #aci_df <- ACI_all_rolling
  
  unique_years = length(unique(aci_df$Year))
  aci_df <- aci_df %>%
    group_by(id) %>% filter(length(unique(Year)) == unique_years) %>% ungroup()
  
  if(isTRUE(ranks)){
    MOR_together_wide = pivot_wider(aci_df, id_cols = c("id"),
                                    names_from = "Year",values_from = "rank",
                                    names_prefix = "rank")
    
    MOR_together_wide$rankdiff = MOR_together_wide[,paste0("rank",base_year)][[1]] -  MOR_together_wide[,paste0("rank",end_year)][[1]]
    #MOR_together_wide$rankdiff = (MOR_together_wide[,paste0("rank",base_year)][[1]] - MOR_together_wide[,paste0("rank",end_year)][[1]])/MOR_together_wide[,paste0("rank",base_year)][[1]]
    
    MOR_together_wide <- left_join(MOR_together_wide,
                                   origins %>% mutate(id = as.character(id)))
    if(isTRUE(evol_ranks)){
      
      if(n == "quantiles"){
        
        MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                          breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                            probs = 0:3/3), 
                                                          labels = 3:1,include.lowest = TRUE),
                                             "_evolutionQ",cut(MOR_together_wide$rankdiff, 
                                                               breaks = quantile(MOR_together_wide$rankdiff, 
                                                                                 probs = 0:4/4),
                                                               labels = 1:4,include.lowest = TRUE))
        
      }else{
        
        MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                          breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                            probs = 0:3/3), 
                                                          labels = 3:1,include.lowest = TRUE),
                                             "_evolutionQ",cut(MOR_together_wide$rankdiff, 
                                                               breaks = c(min(MOR_together_wide$rankdiff)-1,
                                                                          -(sd(MOR_together_wide$rankdiff)/n),
                                                                          (sd(MOR_together_wide$rankdiff)/n),
                                                                          sd(MOR_together_wide$rankdiff)/(n/2),
                                                                          max(MOR_together_wide$rankdiff)+1),
                                                               labels = 1:4,include.lowest = TRUE))
        
      }
      
      
      
      
      
      
    }else{
      
      
      MOR_together_widecomp = pivot_wider(aci_df, id_cols = c("id"),
                                      names_from = "Year",values_from = "Complexity",
                                      names_prefix = "Complexity")
      
      MOR_together_wide$Diff = MOR_together_widecomp[,paste0("Complexity",end_year)][[1]] - MOR_together_widecomp[,paste0("Complexity",base_year)][[1]]
      
      if(n == "quantiles"){
        MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                          breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                            probs = 0:3/3), 
                                                          labels = 3:1,include.lowest = TRUE),
                                             "_evolutionQ",cut(MOR_together_wide$Diff, 
                                                               breaks = quantile(MOR_together_wide$Diff, 
                                                                                 probs = 0:4/4),
                                                               labels = 1:4,include.lowest = TRUE))
        
      }else{MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                              breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                                probs = 0:3/3), 
                                                              labels = 3:1,include.lowest = TRUE),
                                                 "_evolutionQ",cut(MOR_together_wide$Diff, 
                                                                   breaks = c(min(MOR_together_wide$Diff)-1,
                                                                              -(sd(MOR_together_wide$Diff)/n),
                                                                              (sd(MOR_together_wide$Diff)/n),
                                                                              sd(MOR_together_wide$Diff)/(n/2),
                                                                              max(MOR_together_wide$Diff)+1),
                                                                   labels = 1:4,include.lowest = TRUE))}
    }
  }else{
    
    MOR_together_wide = pivot_wider(aci_df, id_cols = c("id"),
                                    names_from = "Year",values_from = "Complexity",
                                    names_prefix = "Year")
    
    MOR_together_wide$Diff_20_14 = MOR_together_wide[,paste0("Complexity",end_year)] -  MOR_together_wide[,paste0("Complexity",base_year)]
    
    MOR_together_wide <- left_join(MOR_together_wide,
                                   origins %>% mutate(id = as.character(id)))
    
    if(n == "quantiles"){
      MOR_together_wide$category <- paste0("levelQ", cut(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                         breaks = quantile(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                                           probs = 0:3/3), 
                                                         labels = 1:3,include.lowest = TRUE),
                                           "_evolutionQ", cut(MOR_together_wide$Diff_20_14, 
                                                              breaks = c(min(MOR_together_wide$Diff_20_14)-1,
                                                                         -(sd(MOR_together_wide$Diff_20_14)/2),
                                                                         (sd(MOR_together_wide$Diff_20_14)/2),
                                                                         sd(MOR_together_wide$Diff_20_14),
                                                                         max(MOR_together_wide$Diff_20_14)+1),
                                                              labels = 1:4,include.lowest = TRUE))
    }else{
      MOR_together_wide$category <- paste0("levelQ", cut(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                         breaks = quantile(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                                           probs = 0:3/3), 
                                                         labels = 1:3,include.lowest = TRUE),
                                           "_evolutionQ", cut(MOR_together_wide$Diff_20_14, 
                                                              breaks = quantile(MOR_together_wide$Diff, 
                                                                                probs = 0:4/4),
                                                              labels = 1:4,include.lowest = TRUE))
    }
    
  }
  
  # convert category to factor
  MOR_together_wide$category <- factor(MOR_together_wide$category)
  
  MOR_together_wide$category <- factor(MOR_together_wide$category, 
                                       levels = c("levelQ1_evolutionQ1", "levelQ2_evolutionQ1", "levelQ3_evolutionQ1",
                                                  "levelQ1_evolutionQ2", "levelQ2_evolutionQ2", "levelQ3_evolutionQ2",
                                                  "levelQ1_evolutionQ3", "levelQ2_evolutionQ3", "levelQ3_evolutionQ3",
                                                  "levelQ1_evolutionQ4", "levelQ2_evolutionQ4", "levelQ3_evolutionQ4"))
  
  palette <- c()
  
  # Define the hue values for each level
  hues <- c(312, 275, 231)
  
  # Define the luminance values for each evolution
  lums <- c(25, 50,75, 90)
  
  # Loop over the levels and evolutions and assign colors to the palette
  for (level in 1:3) {
    for (evolution in 1:4) {
      # Generate a color name based on the level and evolution
      name <- paste0("levelQ", level, "_evolutionQ", evolution)
      # Generate a color value based on the hue, chroma and luminance
      value <- hcl(h = hues[level], c = 50, l = lums[evolution])
      # Assign the color value to the palette with the name as the index
      palette[name] <- value
    }
  }
  
  bb_paris <- getbb("Paris")
  bb_paris[1] = 2.241749
  bb_paris[3] = 2.426782
  #paris_tiles <- get_map(bb_paris,source = "stamen",force = F)
  paris_tiles <- get_map(bb_paris,source = "stadia",force = F, maptype = "alidade_smooth")
  #plot(paris_tiles)
  lemon_pie <- ggmap(paris_tiles, darken = c(0.6, "black"))+
    geom_point(data = MOR_together_wide,
               mapping = aes(x = lon, y = lat, color = as.factor(category)), alpha = 0.5, size = 1)+
    scale_color_manual(values = palette) +
    #geom_path(spdf, mapping=aes(x=long, y = lat, group = group), color = "black")+
    
    #geom_path(spdf_arrond, mapping=aes(x=long, y = lat, group = group), color = "black",linewidth=1)+
    
    theme_void()+
    #guides(color = guide_legend(nrow = 3, override.aes = list(shape = 15,size = 4)))+
    theme(legend.position = "bottom",legend.key = element_blank())
  # create a plot without legend
  p <- lemon_pie + theme(legend.position = "none") + theme(plot.margin = ggplot2::margin(0,0,-2,-2))
  
  legend_df <- data.frame(category = levels(MOR_together_wide$category)) %>% 
    left_join(.,as.data.frame(palette) %>% mutate(category = rownames(.)))
  
  # separate category into levelQ and evolutionQ
  legend_df <- legend_df %>%
    separate(category, c("levelQ", "evolutionQ"), sep = "_")
  
  # create a plot for the legend
  plot_legend <- ggplot(legend_df, aes(x =levelQ , y = evolutionQ, fill = palette)) +
    geom_tile() +
    scale_fill_identity() +
    labs(x = "", y = NULL) +
    theme_minimal()+
    theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"),linewidth = 1.3),
          axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"),linewidth = 1.3),
          axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank(),
          axis.title.x = element_text(vjust = 0, angle = 0,hjust=0.5,face="bold"),
          axis.title.y = element_text(vjust = 1, angle = 90,hjust=0.5,face="bold"),plot.margin = ggplot2::margin(0,0,0.5,0))+
    labs(x = paste0(base_year," ACI rank"), y = paste0(end_year,"-",base_year,"\nACI Difference"))+
    theme(panel.background = element_rect(fill = 'white', color = NA),
          plot.background = element_rect(fill = 'white', color = NA))
  plot_legend
  
  library(cowplot)
  library(grid)
  
  #xmin and xmax as a quarter of the length of the bbox either way
  #bbox found using attr(paris_tiles,"bb")
  with_legend <- p+
    ggmap::inset(ggplotGrob(plot_legend), xmin = 2.24288, xmax = 2.28007, 
                 ymin = 48.88052, ymax = 48.90216)
  
  if(isTRUE(legend)){
    return(with_legend)
  }else{return(p)}
}

fixed_lemonpie <- function(aci_df, origins, ranks = T, n = 2, evol_ranks = F, legend = T,
                           base_year = "2014", end_year = "2023"){
  #aci_df$Year = paste0("20",substr(aci_df$id,1,2))
  #aci_df <- ACI_all_rolling
  
  unique_years = length(unique(aci_df$Year))
  aci_df <- aci_df %>%
    group_by(id) %>% filter(length(unique(Year)) == unique_years) %>% ungroup()
  
  if(isTRUE(ranks)){
    MOR_together_wide = pivot_wider(aci_df, id_cols = c("id"),
                                    names_from = "Year",values_from = "rank",
                                    names_prefix = "rank")
    
    MOR_together_wide$rankdiff = MOR_together_wide[,paste0("rank",base_year)][[1]] -  MOR_together_wide[,paste0("rank",end_year)][[1]]
    #MOR_together_wide$rankdiff = (MOR_together_wide[,paste0("rank",base_year)][[1]] - MOR_together_wide[,paste0("rank",end_year)][[1]])/MOR_together_wide[,paste0("rank",base_year)][[1]]
    
    MOR_together_wide <- left_join(MOR_together_wide,
                                   origins %>% mutate(id = as.character(id)))
    if(isTRUE(evol_ranks)){
      
      if(n == "quantiles"){
        
        MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                          breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                            probs = 0:4/4), 
                                                          labels = 4:1,include.lowest = TRUE),
                                             "_evolutionQ",cut(MOR_together_wide$rankdiff, 
                                                               breaks = quantile(MOR_together_wide$rankdiff, 
                                                                                 probs = 0:4/4),
                                                               labels = 1:4,include.lowest = TRUE))
        
      }else{
        
        MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                          breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                            probs = 0:4/4), 
                                                          labels = 4:1,include.lowest = TRUE),
                                             "_evolutionQ",cut(MOR_together_wide$rankdiff, 
                                                               breaks = c(min(MOR_together_wide$rankdiff)-1,
                                                                          -(sd(MOR_together_wide$rankdiff)/n),
                                                                          (sd(MOR_together_wide$rankdiff)/n),
                                                                          sd(MOR_together_wide$rankdiff)/(n/2),
                                                                          max(MOR_together_wide$rankdiff)+1),
                                                               labels = 1:4,include.lowest = TRUE))
        
      }
      
      
      
      
      
      
    }else{
      
      
      MOR_together_widecomp = pivot_wider(aci_df, id_cols = c("id"),
                                          names_from = "Year",values_from = "Complexity",
                                          names_prefix = "Complexity")
      
      MOR_together_wide$Diff = MOR_together_widecomp[,paste0("Complexity",end_year)][[1]] - MOR_together_widecomp[,paste0("Complexity",base_year)][[1]]
      
      if(n == "quantiles"){
        MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                          breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                            probs = 0:4/4), 
                                                          labels = 4:1,include.lowest = TRUE),
                                             "_evolutionQ",cut(MOR_together_wide$Diff, 
                                                               breaks = quantile(MOR_together_wide$Diff, 
                                                                                 probs = 0:4/4),
                                                               labels = 1:4,include.lowest = TRUE))
        
      }else{MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                              breaks = quantile(MOR_together_wide[,paste0("rank",base_year)][[1]], 
                                                                                probs = 0:4/4), 
                                                              labels = 4:1,include.lowest = TRUE),
                                                 "_evolutionQ",cut(MOR_together_wide$Diff, 
                                                                   breaks = c(min(MOR_together_wide$Diff)-1,
                                                                              -(sd(MOR_together_wide$Diff)/n),
                                                                              (sd(MOR_together_wide$Diff)/n),
                                                                              sd(MOR_together_wide$Diff)/(n/2),
                                                                              max(MOR_together_wide$Diff)+1),
                                                                   labels = 1:4,include.lowest = TRUE))}
    }
  }else{
    
    MOR_together_wide = pivot_wider(aci_df, id_cols = c("id"),
                                    names_from = "Year",values_from = "Complexity",
                                    names_prefix = "Year")
    
    MOR_together_wide$Diff_20_14 = MOR_together_wide[,paste0("Complexity",end_year)] -  MOR_together_wide[,paste0("Complexity",base_year)]
    
    MOR_together_wide <- left_join(MOR_together_wide,
                                   origins %>% mutate(id = as.character(id)))
    
    if(n == "quantiles"){
      MOR_together_wide$category <- paste0("levelQ", cut(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                         breaks = quantile(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                                           probs = 0:4/4), 
                                                         labels = 1:4,include.lowest = TRUE),
                                           "_evolutionQ", cut(MOR_together_wide$Diff_20_14, 
                                                              breaks = c(min(MOR_together_wide$Diff_20_14)-1,
                                                                         -(sd(MOR_together_wide$Diff_20_14)/2),
                                                                         (sd(MOR_together_wide$Diff_20_14)/2),
                                                                         sd(MOR_together_wide$Diff_20_14),
                                                                         max(MOR_together_wide$Diff_20_14)+1),
                                                              labels = 1:4,include.lowest = TRUE))
    }else{
      MOR_together_wide$category <- paste0("levelQ", cut(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                         breaks = quantile(MOR_together_wide[,paste0("Complexity",base_year)], 
                                                                           probs = 0:4/4), 
                                                         labels = 1:4,include.lowest = TRUE),
                                           "_evolutionQ", cut(MOR_together_wide$Diff_20_14, 
                                                              breaks = quantile(MOR_together_wide$Diff, 
                                                                                probs = 0:4/4),
                                                              labels = 1:4,include.lowest = TRUE))
    }
    
  }
  
  # convert category to factor
  MOR_together_wide$category <- factor(MOR_together_wide$category)
  
  MOR_together_wide$category <- factor(MOR_together_wide$category, 
                                       levels = c("levelQ1_evolutionQ1", "levelQ2_evolutionQ1", "levelQ3_evolutionQ1", "levelQ4_evolutionQ1",
                                                  "levelQ1_evolutionQ2", "levelQ2_evolutionQ2", "levelQ3_evolutionQ2", "levelQ4_evolutionQ2",
                                                  "levelQ1_evolutionQ3", "levelQ2_evolutionQ3", "levelQ3_evolutionQ3", "levelQ4_evolutionQ3",
                                                  "levelQ1_evolutionQ4", "levelQ2_evolutionQ4", "levelQ3_evolutionQ4", "levelQ4_evolutionQ4"))
  
  palette <- c()
  
  # Define the hue values for each level
  hues <- c(12, 312, 275, 231)
  
  # Define the luminance values for each evolution
  lums <- c(25, 50,75, 90)
  
  # Loop over the levels and evolutions and assign colors to the palette
  for (level in 1:4) {
    for (evolution in 1:4) {
      # Generate a color name based on the level and evolution
      name <- paste0("levelQ", level, "_evolutionQ", evolution)
      # Generate a color value based on the hue, chroma and luminance
      value <- hcl(h = hues[level], c = 50, l = lums[evolution])
      # Assign the color value to the palette with the name as the index
      palette[name] <- value
    }
  }
  
  #"#994242"
  
  
  bb_paris <- getbb("Paris")
  bb_paris[1] = 2.241749
  bb_paris[3] = 2.426782
  #paris_tiles <- get_map(bb_paris,source = "stamen",force = F)
  paris_tiles <- get_map(bb_paris,source = "stadia",force = F, maptype = "alidade_smooth")
  #plot(paris_tiles)
  
  
  lemon_pie <- ggmap(paris_tiles, darken = c(0.3, "black"))+
    geom_point(data = MOR_together_wide,
               mapping = aes(x = lon, y = lat, color = as.factor(category)), alpha = 0.5, size = 1.1)+
    scale_color_manual(values = palette) +
    #geom_path(spdf, mapping=aes(x=long, y = lat, group = group), color = "black")+
    
    #geom_path(spdf_arrond, mapping=aes(x=long, y = lat, group = group), color = "black",linewidth=1)+
    
    theme_void()+
    #guides(color = guide_legend(nrow = 3, override.aes = list(shape = 15,size = 4)))+
    theme(legend.position = "bottom",legend.key = element_blank())
  
  # create a plot without legend
  p <- lemon_pie + theme(legend.position = "none") + theme(plot.margin = ggplot2::margin(0,0,-2,-2))
  
  
  legend_df <- data.frame(category = levels(MOR_together_wide$category)) %>% 
    left_join(.,as.data.frame(palette) %>% mutate(category = rownames(.)))
  
  # separate category into levelQ and evolutionQ
  legend_df <- legend_df %>%
    separate(category, c("levelQ", "evolutionQ"), sep = "_")
  
  # create a plot for the legend
  plot_legend <- ggplot(legend_df, aes(x =levelQ , y = evolutionQ, fill = palette)) +
    geom_tile() +
    scale_fill_identity() +
    labs(x = "", y = NULL) +
    theme_minimal()+
    theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"),linewidth = 1.3),
          axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"),linewidth = 1.3),
          axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank(),
          axis.title.x = element_text(vjust = 0, angle = 0,hjust=0.5,face="bold"),
          axis.title.y = element_text(vjust = 1, angle = 90,hjust=0.5,face="bold"),plot.margin = ggplot2::margin(0,0,0.5,0))+
    labs(x = paste0(base_year," ACI rank"), y = paste0(end_year,"-",base_year,"\nACI Difference"))+
    theme(panel.background = element_rect(fill = 'white', color = NA),
          plot.background = element_rect(fill = 'white', color = NA))
  plot_legend
  
  library(cowplot)
  library(grid)
  
  #xmin and xmax as a quarter of the length of the bbox either way
  #bbox found using attr(paris_tiles,"bb")
  with_legend <- p+
    ggmap::inset(ggplotGrob(plot_legend), xmin = 2.24288, xmax = 2.28007, 
                 ymin = 48.88052, ymax = 48.90216)
  
  if(isTRUE(legend)){
    return(with_legend)
  }else{return(p)}
}



view_staticdynamic <- function(aci_df, origins,ranks = T, end_year = "2023", base_year = "2014"){
  require(patchwork)
  bb_paris <- getbb("Paris")
  bb_paris[1] = 2.241749
  bb_paris[3] = 2.426782
  paris_tiles <- get_map(bb_paris,source = "stadia",force = F, maptype = "alidade_smooth")
  
  #aci_df$Year = paste0("20",aci_df$Year)
  #aci_df$id = substr(aci_df$id,4,nchar(aci_df$id))
  aci_df <- left_join(aci_df,origins %>% mutate(id = as.character(id)))
  
  MOR_together_wide = pivot_wider(aci_df %>% select(id, Year, Complexity, lon, lat) , 
                                  id_cols = c("id","lon","lat"),
                                  names_from = "Year",values_from = "Complexity",
                                  names_prefix = "Year")
  
  MOR_together_wide$Diff = aci_df[aci_df$Year == end_year,]$Complexity - aci_df[aci_df$Year == base_year,]$Complexity
  
  high_lim = quantile(MOR_together_wide$Diff,0.99) 
  low_lim = quantile(MOR_together_wide$Diff,0.01) 
  
  paris_comp_14_20 <- ggmap(paris_tiles)+
    geom_point(data = MOR_together_wide,
               mapping = aes(x = lon, y = lat, color = Diff), alpha = 0.6, size = 0.6)+
    scale_color_viridis_c(direction = 1,option = "viridis", 
                          limits = c(low_lim, high_lim), oob = scales::squish)+
    theme_map()+
    labs(color = paste("ACI change between",base_year,"and",end_year,sep = " "))+
    guides(colour = guide_colorbar(title.position = "bottom"))+
    
    #      title = paste("Amenity complexity index in Paris, 2020-2017"))+
    theme(legend.position = "bottom",legend.direction = "horizontal",
          legend.key.width = unit(1.5,"cm"), legend.key.height = unit(0.3,"cm"),
          legend.title = element_text(vjust = 0, hjust = 0.5),legend.justification = "center")
  
  if(isTRUE(ranks)){
    # MOR_together_rank = pivot_wider(aci_df %>% select(id, Year, yearly_rank, lon, lat), 
    #                                 id_cols = c("id","lon","lat"),
    #                                 names_from = "Year",values_from = "yearly_rank",
    #                                 names_prefix = "Year")%>%
    #   group_by(id) %>%
    #   mutate(meanACI = (Year14+Year17+Year20)/3) %>% ungroup()
    
    MOR_together_rank = aci_df %>% select(id, Year, yearly_rank, lon, lat) %>%
      filter(as.integer(Year) %in% seq.int(as.integer(base_year)-1,as.integer(end_year)+1)) %>%
      group_by(id,lon,lat) %>%
      summarise(meanACI = mean(yearly_rank))%>% ungroup() %>% distinct()
    
  }else{
    # MOR_together_rank = pivot_wider(aci_df %>% select(id, Year, Complexity, lon, lat) , 
    #                                 id_cols = c("id","lon","lat"),
    #                                 names_from = "Year",values_from = "Complexity",
    #                                 names_prefix = "Year")%>%
    #   group_by(id) %>%
    #   mutate(meanACI = (Year14+Year17+Year20)/3) %>% ungroup()
    
    MOR_together_rank = aci_df %>% select(id, Year, Complexity, lon, lat) %>%
      filter(as.integer(Year) %in% seq.int(as.integer(base_year)-1,as.integer(end_year)+1)) %>%
      group_by(id,lon,lat) %>%
      summarise(meanACI = mean(Complexity)) %>% ungroup() %>% distinct()
  }
  
  
  paris_comp_mean <- ggmap(paris_tiles)+
    geom_point(data = MOR_together_rank,
               mapping = aes(x = lon, y = lat, color = meanACI),
               alpha = 0.6, size = 0.6)+
    scale_color_viridis_c(direction = -1,option = "viridis")+
    theme_map()+
    labs(color = paste("Mean ACI rank between",base_year,"and",end_year,sep = " "))+
    guides(colour = guide_colorbar(title.position = "bottom"))+
    
    #      title = paste("Amenity complexity index in Paris, 2020-2017"))+
    theme(legend.position = "bottom",legend.direction = "horizontal",
          legend.key.width = unit(1.5,"cm"), legend.key.height = unit(0.3,"cm"),
          legend.title = element_text(vjust = 0, hjust = 0.5),legend.justification = "center")
  
  plot <- plot_grid(paris_comp_mean,paris_comp_14_20,
                    nrow = 1)
  return(plot)
  
}

view_density <- function(aci_df, years = T, boxplots = F, bounded = T, 
                         tech = F, need_minus = F, rca = NULL, histo = F){
  
  if(isTRUE(tech)){
    
    rca <- rca[rowSums(rca) > 0, ]
    rca <- rca[, colSums(rca) > 0]
    
    yci <- eigen((t(rca) / colSums(rca)) %*% (rca / rowSums(rca)))
    yci <- Re(yci$vectors[, 2])
    
    yci = setNames(
      (yci - mean(yci, na.rm = T)) / sd(yci, na.rm = T),
      colnames(mat))
    
    if(need_minus == T){
      yci <- (-1) * yci
    }
    
    ubiquity = colSums(rca)
    
    df = as.data.frame(yci)
    names(df)[1] = "Complexity"
    df$CODE_ACTIVITE = colnames(rca)
    df$Ubiquity = ubiquity
    
    input = df$Complexity
    output = df$Ubiquity
    name    = rep("Name", length(output))
    data    = data.frame(input, output, name)
    data$name = as.factor(data$name)
    
    # create a new variable that calculates the mean value of input for each bin
    data <- data %>%
      mutate(bin = cut(input, breaks = seq(min(input), max(input), by = 0.05))) %>%
      group_by(bin) %>%
      mutate(mean_output = mean(output,na.rm = T)) %>%
      ungroup()
    
    # plot the histogram of output filled by mean_input
    density_comp <- ggplot(data, aes(x = input, fill = mean_output, group = bin)) +
      geom_histogram(binwidth = 0.05) +
      scale_fill_viridis_c()+
      theme_bw()+
      theme(legend.position = "bottom", legend.key.height = unit(0.2,"cm"),
            legend.key.width = unit(1.5,"cm"),axis.ticks.y = element_blank(), axis.text.y = element_blank())+
      labs(fill = "Ubiquity", x = "TCI",y="")
    
    # if(isTRUE(histo)){
    #   density_comp <- density_comp+ 
    #     geom_histogram(aes(y = ..density..),
    #                    colour = 1, fill = "white")
    # }
    
    return(density_comp)
    
    
  }else{
    if(isTRUE(boxplots)){
      # boxplots <- ggplot(aci_df %>% mutate(Year = paste0("20",Year)), 
      #                    aes(x=Year,y = rank,colour = Year))+
      #   geom_boxplot(width=0.1,show.legend = FALSE)+
      #   stat_summary(fun=mean, geom="point", 
      #                shape=16, size=3, show.legend=FALSE,aes(colour = Year))+
      #   scale_color_discrete(name = "Year")+
      #   ylim(c(max(aci_df$rank),1))+
      #   theme_bw()+
      #   theme(plot.margin = margin(t=0.2,r=0.3,b=0,l=0.9, unit="cm"),
      #         axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      #         legend.position ="none")+
      #   labs(y="Rank amongst",x="")
      # 
      # aci_df <- aci_df %>% group_by(Year) %>% mutate(rank=ifelse(is.na(rank),max(rank),rank),
      #                                                mean = mean(rank)) %>% ungroup()
      # boxplots <- ggplot(aci_df %>% mutate(Year = paste0("20",Year)), 
      #                   aes(x=Year,y = rank,fill = Year, group = Year))+
      #   geom_violin(trim = F,adjust = 2, alpha = 0.4)+
      #   geom_boxplot(fill = "white",width = 0.2, alpha = 0.4)+
      #   scale_x_discrete(breaks = c("2014", "2017", "2020"))+
      #   scale_y_reverse(lim = c(max(aci_df$rank),1), breaks = c(1,150000),
      #                   labels = c("Most Complex","Least Complex"))+
      #   scale_fill_discrete(name = "Year")+
      #   theme_bw()+
      #   labs(fill = 'mean rank',
      #        y = "ACI rank")+
      #   theme()
      #boxplots
      
      if(isTRUE(bounded)){
        # aci_df <- aci_df %>% 
        #   mutate(Complexity = (Complexity - mean(Complexity, na.rm = T)) / sd(Complexity, na.rm = T))
        
        rescale <- function(x) {
          ((x) - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                          min(x, na.rm = TRUE)) * 100
        }
        aci_df <-aci_df %>% group_by(Year) %>%
          mutate(Complexity = rescale(Complexity))
        
      }
      
      
      library(ggridges)
      
      ridges <- ggplot(aci_df, 
                       aes(x=Complexity, y=Year, fill = factor(after_stat(quantile)))) +
        stat_density_ridges(
          geom = "density_ridges_gradient", calc_ecdf = TRUE,
          quantiles = 10, quantile_lines = TRUE
        ) +
        scale_fill_viridis_d(name = "Quartiles")+
        scale_y_discrete(limits  = c("2023", "2020","2017","2014"))+
        #guides(fill = guide_coloursteps(breaks = 2))+
        labs(x = "ACI")+
        theme_bw()+theme(legend.position= "none")
      #ridges
      return(ridges)
    }else{
      if(isTRUE(bounded)){
        # aci_df <- aci_df %>% 
        #   mutate(Complexity = (Complexity - mean(Complexity, na.rm = T)) / sd(Complexity, na.rm = T))
        
        rescale <- function(x) {
          ((x) - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                          min(x, na.rm = TRUE)) * 100
        }
        aci_df$Complexity = rescale(aci_df$Complexity)
        
      }
      density_comp <- ggplot(aci_df %>% mutate(Year = paste0("20",Year)), 
                             aes(x = Complexity, fill = Year,colour = Year))+
        geom_density(aes(y=after_stat(density)),
                     position="identity", alpha = 0.1,linewidth=1)+
        theme_bw()+
        theme(plot.margin = margin(t=0.2,r=0.3,b=-0.4,l=0, unit="cm"))+
        theme(legend.position = "none", legend.title.align = 0.5, legend.direction = "horizontal")+
        labs(y="density",x="ACI")
      return(density_comp)
      
    }
  }
}


origins <- buildings_together %>% select(id, lon, lat) %>% distinct()

TCI_all <- TCI(buildings_together %>% filter(!CODE_ACTIVITE == "CC203"), RCA = F)
TCI_all_RCA <- TCI(buildings_together %>% filter(!CODE_ACTIVITE == "CC203"), RCA = T)

TCI_all_rolling <- TCI(buildings_together,
                       RCA = F, rolling = T)
TCI_all_rolling <- left_join(TCI_all_rolling, names, by = c("id" = "CODE_ACTIVITE"))


TCI_table <- TCI_all_rolling %>% 
  group_by(id, AMENITY_NAME, LIBELLE_ACTIVITE) %>%
  summarise(medianrank = median(rank),
            Year14 = rank[Year == 2014],
            Year17 = rank[Year == 2017],
            Year20 = rank[Year == 2020],
            Year23 = rank[Year == 2023]) %>%
  arrange(medianrank)

#fwrite(TCI_all_rolling, "TCI_all_rolling.csv")

# sim = TCI(buildings_together,
#           RCA = F, rolling = T, return_similarity = T)
# 
# simdt = as.data.table(sim) %>% mutate(CODE_ACTIVITE = rownames(sim), 
#                                            .before = CA101) %>% 
#                left_join(., names) %>% 
#                relocate(c("LIBELLE_ACTIVITE","AMENITY_NAME"), .after = CODE_ACTIVITE)
# 
# fwrite(simdt, "similarity_mat_all_rolling.csv")



ACI_all_rolling = ACI_fixed(buildings_together, tci_data = TCI_all_rolling, RCA = F, 
          years = c("All"), full_sum = F, rolling = T)

#fwrite(ACI_all_rolling, "ACI_all_rolling.csv")

fig6_all_rolling = fixed_lemonpie(ACI_all_rolling, origins = origins, ranks = T,
                                  legend = T, evol_ranks = F, n = "quantiles",
                          end_year = "2023", base_year = "2014")

fig6_all_rolling


TCI_all_rolling_RCA <- TCI(buildings_together,
                       RCA = T, rolling = T, years = c("All"))
TCI_all_rolling_RCA <- left_join(TCI_all_rolling_RCA, names, by = c("id" = "CODE_ACTIVITE"))



TCI_tog <- left_join(TCI_all_rolling, TCI_all_rolling_RCA %>% select(id, Year, rank, Complexity) %>%
                       rename(rankRCA = rank, CompRCA = Complexity))

ACI_all_rolling_RCA = ACI_fixed(buildings_together, tci_data = TCI_all_rolling_RCA, RCA = T, 
                            years = c("All"), full_sum = F, rolling = T)

#fwrite(ACI_all_rolling_RCA, "ACI_all_rolling_RCA.csv")

fig6_all_rolling_RCA = fixed_lemonpie(ACI_all_rolling_RCA, origins = origins, ranks = T,
                                  legend = T, evol_ranks = F,
                                  end_year = "2023", base_year = "2014")

ggsave("Figures/RCA_nofullsum_rolling/lemon_pie_1423.png", fig6_all_rolling_RCA,
       width = 1600, height = 1200, dpi = 200, units = "px")

for(low_year in c("2014","2017","2020")){
  for(high_year in c("2017","2020","2023")){
    if(high_year > low_year){
      
      lemon_pie = fixed_lemonpie(ACI_all_rolling_RCA, origins = origins, ranks = T,
                                            legend = T, evol_ranks = F,
                                            end_year = high_year, base_year = low_year)
      
      ggsave(paste0("Figures/RCA_nofullsum_rolling/lemon_pie_",
                    substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
             lemon_pie,
             width = 1600, height = 1200, dpi = 200, units = "px")
    }
  }
}

for(low_year in c("2014","2017","2020")){
  for(high_year in c("2017","2020","2023")){
    if(high_year > low_year){
      
      
      density <- view_staticdynamic(ACI_all_rolling_RCA, origins = origins,
                                    ranks = T, base_year = low_year, end_year = high_year)
      
      ggsave(paste0("Figures/RCA_nofullsum_rolling/staticdynamic_",
                    substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
             density,
             width = 1600, height = 800, dpi = 200, units = "px")
    }
  }
}

density <- view_density(ACI_all_rolling, boxplots = T, bounded = F)
density
ggsave(paste0("Figures/RCA_nofullsum_rolling/density_ridges_14_23",
              substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
       density,
       width = 1600, height = 800, dpi = 200, units = "px")


################ NO RCA

TCI_all_rolling <- TCI(buildings_together,
                       RCA = F, rolling = T)
TCI_all_rolling <- left_join(TCI_all_rolling, names, by = c("id" = "CODE_ACTIVITE"))

ACI_all_rolling = ACI_fixed(buildings_together, tci_data = TCI_all_rolling, RCA = F, 
                            years = c("All"), full_sum = F, rolling = T)

test <- ACI_all_rolling %>% group_by(id) %>%
  summarise(Diff = Complexity[Year == "2023"] - Complexity[Year == "2014"])


plot1 <- fixed_lemonpie(ACI_all_rolling, origins = origins, ranks = T,
                        legend = T, evol_ranks = F,
                        end_year = "2017", base_year = "2014")
plot1
plot2 <- fixed_lemonpie(ACI_all_rolling, origins = origins, ranks = T,
                        legend = T, evol_ranks = F,
                        end_year = "2020", base_year = "2017")
plot3 <- fixed_lemonpie(ACI_all_rolling, origins = origins, ranks = T,
                        legend = F, evol_ranks = F,
                        end_year = "2023", base_year = "2020")

arranged <- ggpubr::ggarrange(plot1,plot2, nrow = 1)

arranged
for(low_year in c("2014","2017","2020")){
  for(high_year in c("2017","2020","2023")){
    if(high_year > low_year){
      
      lemon_pie = fixed_lemonpie(ACI_all_rolling, origins = origins, ranks = T,
                                 legend = T, evol_ranks = F,
                                 end_year = high_year, base_year = low_year, n = "quantiles")
      
      ggsave(paste0("Figures/nofullsum_rolling/lemon_pie_",
                    substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
             lemon_pie,
             width = 1600, height = 1200, dpi = 200, units = "px")
    }
  }
}

for(low_year in c("2014","2017","2020")){
  for(high_year in c("2017","2020","2023")){
    if(high_year > low_year){
      
      
      density <- view_staticdynamic(ACI_all_rolling, origins = origins,
                                    ranks = T, base_year = low_year, end_year = high_year)
      
      ggsave(paste0("Figures/nofullsum_rolling/staticdynamic_",
                    substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
             density,
             width = 1600, height = 800, dpi = 200, units = "px")
    }
  }
}

density <- view_density(ACI_all_rolling, boxplots = T, bounded = F)

density

ggsave(paste0("Figures/nofullsum_rolling/density_ridges_14_23",
              substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
       density,
       width = 1600, height = 800, dpi = 200, units = "px")


################ FULLSUM

TCI_all_rolling <- TCI(buildings_together,
                       RCA = F, rolling = T)
TCI_all_rolling <- left_join(TCI_all_rolling, names, by = c("id" = "CODE_ACTIVITE"))

ACI_all_rolling = ACI_fixed(buildings_together, tci_data = TCI_all_rolling, RCA = F, 
                            years = c("All"), full_sum = T, rolling = T)

library(ggpubr)



for(low_year in c("2014","2017","2020")){
  for(high_year in c("2017","2020","2023")){
    if(high_year > low_year){
      
      lemon_pie = fixed_lemonpie(ACI_all_rolling, origins = origins, ranks = T,
                                 legend = T, evol_ranks = F,
                                 end_year = high_year, base_year = low_year)
      
      ggsave(paste0("Figures/fullsum_rolling/lemon_pie_",
                    substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
             lemon_pie,
             width = 1600, height = 1200, dpi = 200, units = "px")
    }
  }
}

for(low_year in c("2014","2017","2020")){
  for(high_year in c("2017","2020","2023")){
    if(high_year > low_year){
      
      
      density <- view_staticdynamic(ACI_all_rolling, origins = origins,
                                    ranks = T, base_year = low_year, end_year = high_year)
      
      ggsave(paste0("Figures/fullsum_rolling/staticdynamic_",
                    substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
             density,
             width = 1600, height = 800, dpi = 200, units = "px")
    }
  }
}

density <- view_density(ACI_all_rolling, boxplots = T)

density

ggsave(paste0("Figures/fullsum_rolling/density_ridges_14_23",
              substr(low_year,3,4),"_",substr(high_year,3,4),".png"), 
       density,
       width = 1600, height = 800, dpi = 200, units = "px")


statdyn_all_rolling_RCA <- view_staticdynamic(ACI_all_rolling_RCA, origins = origins,
                                              ranks = T)
statdyn_all_rolling_RCA


distribution_all_rolling_RCA <- view_density(ACI_all_rolling_RCA, boxplots = T)

distribution_all_rolling_RCA


table(ACI_all_rolling_RCA$Year)
summary(ACI_all_rolling_RCA$Complexity[ACI_all_rolling_RCA$Year == "2023"])
length(unique(ACI_all_rolling_RCA$Complexity[ACI_all_rolling_RCA$Year == "2023"]))


for (year_level in unique(aci_df$Year)) {
  # Subset data for the current year
  current_complexity_data <- aci_df$Complexity[aci_df$Year == year_level]
  
  # Calculate the 11 quantile values (for 10 bins)
  # probs = seq(0, 1, length.out = number_of_bins + 1)
  quantile_values <- stats::quantile(current_complexity_data,
                                     probs = seq(0, 1, length.out = 10 + 1),
                                     na.rm = TRUE,
                                     names = FALSE, # Keep it simple
                                     type = 7)      # type=7 is default in R and ggridges
  
  unique_quantile_values <- unique(quantile_values)
  num_unique_q_values <- length(unique_quantile_values)
  
  cat("Year:", year_level, "\n")
  cat("  Number of unique Complexity values for 11 quantile points:", num_unique_q_values, "\n")
  # cat("  Unique quantile values:", paste(round(unique_quantile_values, 3), collapse=", "), "\n") # Optional: view values
  
  if (num_unique_q_values == 5) {
    cat("  >>> This year might be related to the 'size 5' in the error! <<<\n")
  }
}



TCI_all <- left_join(TCI_all, names, by = c("id" = "CODE_ACTIVITE"))

TCI_all_RCA <- left_join(TCI_all_RCA, names, by = c("id" = "CODE_ACTIVITE"))


TCI_no23 <- TCI(buildings_together, RCA = F, years = c("2014","2017","2020"))
TCI_no23_RCA <- TCI(buildings_together, RCA = T, years = c("2014","2017","2020"))

TCI_no23 <- left_join(TCI_no23, names, by = c("id" = "CODE_ACTIVITE"))
TCI_no23_RCA <- left_join(TCI_no23_RCA, names, by = c("id" = "CODE_ACTIVITE"))

ACI_no23 <- ACI_fixed(buildings_together, tci_data=TCI_no23, RCA = F, 
                      years = c("2014","2017","2020"), full_sum = T)
ACI_no23_RCA <- ACI_fixed(buildings_together, tci_data=TCI_no23, 
                          RCA = T, years = c("2014","2017","2020"), full_sum = T)



fig6_no23_RCA = fixed_lemonpie(ACI_no23_RCA, origins = origins, ranks = T, n = 2, legend = F,
                               end_year = "20")

fig6_no23_RCA


fig6_no23 = fixed_lemonpie(ACI_no23, origins = origins, ranks = T,legend = F, evol_ranks = F,
                               end_year = "20")

fig6_no23


ACI_all <- ACI_fixed(buildings_together, tci_data=TCI_all, RCA = F, 
                      years = c("All"), full_sum = T)

ACI_all_RCA <- ACI_fixed(buildings_together, tci_data=TCI_all_RCA, 
                          RCA = T, years = c("All"), full_sum = T)

origins <- buildings_together %>% select(id, lon, lat) %>% distinct()

fig6_all_RCA = fixed_lemonpie(ACI_all_RCA, origins = origins, ranks = T, n = 2, legend = F,
                               end_year = "23")

fig6_all_RCA

fig6_all = fixed_lemonpie(ACI_all, origins = origins, ranks = T,legend = F, evol_ranks = F,
                           end_year = "20", base_year = "14")

fig6_all



TCI_23 <- RQCI(buildings_23, rca = F)
TCI_23RCA <- RQCI(buildings_23, rca = T)



buildings_23 <- buildings_together %>% filter(substr(building_id,nchar(building_id)-1, nchar(building_id)) == "23")


TCI_all <- RQCI(buildings_all, RCA = F)

TCI_all_RCA <- RQCI(buildings_all, RCA = T)

setdiff(TCI_all, TCI_all_RCA)

names <- fread("D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/Translated_amenity_types_2020.csv")

TCI <- left_join(TCI, names, by = c("id" = "CODE_ACTIVITE"))

TCI_all_RCA = left_join(TCI_all_RCA, names, by = c("id" = "CODE_ACTIVITE"))

ACI_all <- ACI(buildings_data = buildings_all, tci_data = TCI_all, full_sum = T)

ACI_all_RCA <- ACI(buildings_data = buildings_all, tci_data = TCI_all_RCA, full_sum = T, rca = T)


bad_routing <- aci_df[aci_df$Complexity > 1 & aci_df$diversity < 10,]$id


fig6 <- view_lemonpie(ACI_all, ranks = T, evol_ranks = F, legend = T,n=2)

fig6_rca <- view_lemonpie(ACI_all_RCA, ranks = T, evol_ranks = F, legend = T,n=2)

fig6

fig6_rca

view_lemonpie <- function(aci_df, ranks = T, n = 2, evol_ranks = F, legend = T){
  #aci_df$Year = paste0("20",substr(aci_df$id,1,2))
  #aci_df <- ACI_all
  unique_years = length(unique(aci_df$Year))
  aci_df$id = substr(aci_df$id,1,nchar(aci_df$id)-3)
  aci_df <- aci_df %>%
    group_by(id) %>% filter(length(unique(Year)) == unique_years) %>% ungroup()
  
  if(isTRUE(ranks)){
    MOR_together_wide = pivot_wider(aci_df, id_cols = c("id"),
                                    names_from = "Year",values_from = "rank",
                                    names_prefix = "rank")
    
    MOR_together_wide$rankdiff = MOR_together_wide$rank14 - MOR_together_wide$rank20
    MOR_together_wide$rankdiff = (MOR_together_wide$rank14 - MOR_together_wide$rank20)/MOR_together_wide$rank14
    
    MOR_together_wide <- left_join(MOR_together_wide,
                                   fread("R5/building_as_origins.csv") %>% mutate(id = as.character(id)))
    if(isTRUE(evol_ranks)){
      MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide$rank14, 
                                                        breaks = quantile(MOR_together_wide$rank14, 
                                                                          probs = 0:3/3), 
                                                        labels = 3:1,include.lowest = TRUE),
                                           "_evolutionQ",cut(MOR_together_wide$rankdiff, 
                                                             breaks = c(min(MOR_together_wide$rankdiff)-1,
                                                                        -(sd(MOR_together_wide$rankdiff)/n),
                                                                        (sd(MOR_together_wide$rankdiff)/n),
                                                                        sd(MOR_together_wide$rankdiff)/(n/2),
                                                                        max(MOR_together_wide$rankdiff)+1),
                                                             labels = 1:4,include.lowest = TRUE))
    }else{
      
      MOR_together_wide$Diff = aci_df[aci_df$Year == "20",]$Complexity - aci_df[aci_df$Year == "14",]$Complexity
      MOR_together_wide$category <- paste0("levelQ",cut(MOR_together_wide$rank14, 
                                                        breaks = quantile(MOR_together_wide$rank14, 
                                                                          probs = 0:3/3), 
                                                        labels = 3:1,include.lowest = TRUE),
                                           "_evolutionQ",cut(MOR_together_wide$Diff, 
                                                             breaks = c(min(MOR_together_wide$Diff)-1,
                                                                        -(sd(MOR_together_wide$Diff)/n),
                                                                        (sd(MOR_together_wide$Diff)/n),
                                                                        sd(MOR_together_wide$Diff)/(n/2),
                                                                        max(MOR_together_wide$Diff)+1),
                                                             labels = 1:4,include.lowest = TRUE))
    }
  }else{
    
    MOR_together_wide = pivot_wider(aci_df, id_cols = c("id"),
                                    names_from = "Year",values_from = "Complexity",
                                    names_prefix = "Year")
    
    MOR_together_wide$Diff_20_14 = MOR_together_wide$Year20 - MOR_together_wide$Year14
    
    MOR_together_wide <- left_join(MOR_together_wide,
                                   fread("R5/building_as_origins.csv") %>% mutate(id = as.character(id)))
    
    MOR_together_wide$category <- paste0("levelQ", cut(MOR_together_wide$Year14, 
                                                       breaks = quantile(MOR_together_wide$Year14, 
                                                                         probs = 0:3/3), 
                                                       labels = 1:3,include.lowest = TRUE),
                                         "_evolutionQ", cut(MOR_together_wide$Diff_20_14, 
                                                            breaks = c(min(MOR_together_wide$Diff_20_14)-1,
                                                                       -(sd(MOR_together_wide$Diff_20_14)/2),
                                                                       (sd(MOR_together_wide$Diff_20_14)/2),
                                                                       sd(MOR_together_wide$Diff_20_14),
                                                                       max(MOR_together_wide$Diff_20_14)+1),
                                                            labels = 1:4,include.lowest = TRUE))
  }
  
  # convert category to factor
  MOR_together_wide$category <- factor(MOR_together_wide$category)
  
  MOR_together_wide$category <- factor(MOR_together_wide$category, 
                                       levels = c("levelQ1_evolutionQ1", "levelQ2_evolutionQ1", "levelQ3_evolutionQ1",
                                                  "levelQ1_evolutionQ2", "levelQ2_evolutionQ2", "levelQ3_evolutionQ2",
                                                  "levelQ1_evolutionQ3", "levelQ2_evolutionQ3", "levelQ3_evolutionQ3",
                                                  "levelQ1_evolutionQ4", "levelQ2_evolutionQ4", "levelQ3_evolutionQ4"))
  
  palette <- c()
  
  # Define the hue values for each level
  hues <- c(312, 275, 231)
  
  # Define the luminance values for each evolution
  lums <- c(25, 50,75, 90)
  
  # Loop over the levels and evolutions and assign colors to the palette
  for (level in 1:3) {
    for (evolution in 1:4) {
      # Generate a color name based on the level and evolution
      name <- paste0("levelQ", level, "_evolutionQ", evolution)
      # Generate a color value based on the hue, chroma and luminance
      value <- hcl(h = hues[level], c = 50, l = lums[evolution])
      # Assign the color value to the palette with the name as the index
      palette[name] <- value
    }
  }
  
  bb_paris <- getbb("Paris")
  bb_paris[1] = 2.241749
  bb_paris[3] = 2.426782
  #paris_tiles <- get_map(bb_paris,source = "stamen",force = F)
  paris_tiles <- get_map(bb_paris,source = "stadia",force = F, maptype = "alidade_smooth")
  #plot(paris_tiles)
  lemon_pie <- ggmap(paris_tiles)+
    geom_point(data = MOR_together_wide,
               mapping = aes(x = lon, y = lat, color = as.factor(category)), alpha = 0.3)+
    scale_color_manual(values = palette) +
    #geom_path(spdf, mapping=aes(x=long, y = lat, group = group), color = "black")+
    
    #geom_path(spdf_arrond, mapping=aes(x=long, y = lat, group = group), color = "black",linewidth=1)+
    
    theme_void()+
    #guides(color = guide_legend(nrow = 3, override.aes = list(shape = 15,size = 4)))+
    theme(legend.position = "bottom",legend.key = element_blank())
  # create a plot without legend
  p <- lemon_pie + theme(legend.position = "none") + theme(plot.margin = margin(0,0,-2,-2))
  
  legend_df <- data.frame(category = levels(MOR_together_wide$category)) %>% 
    left_join(.,as.data.frame(palette) %>% mutate(category = rownames(.)))
  
  # separate category into levelQ and evolutionQ
  legend_df <- legend_df %>%
    separate(category, c("levelQ", "evolutionQ"), sep = "_")
  
  # create a plot for the legend
  plot_legend <- ggplot(legend_df, aes(x =levelQ , y = evolutionQ, fill = palette)) +
    geom_tile() +
    scale_fill_identity() +
    labs(x = "", y = NULL) +
    theme_minimal()+
    theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"),linewidth = 1.3),
          axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"),linewidth = 1.3),
          axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank(),
          axis.title.x = element_text(vjust = 0, angle = 0,hjust=0.5,face="bold"),
          axis.title.y = element_text(vjust = 1, angle = 90,hjust=0.5,face="bold"),plot.margin = margin(0,0,0.5,0))+
    labs(x = "2014 ACI rank", y = "2020-2014\nACI Difference")+
    theme(panel.background = element_rect(fill = 'white', color = NA),
          plot.background = element_rect(fill = 'white', color = NA))
  plot_legend
  
  library(cowplot)
  library(grid)
  
  #xmin and xmax as a quarter of the length of the bbox either way
  #bbox found using attr(paris_tiles,"bb")
  with_legend <- p+
    ggmap::inset(ggplotGrob(plot_legend), xmin = 2.24288, xmax = 2.28007, 
                 ymin = 48.88052, ymax = 48.90216)
  
  if(isTRUE(legend)){
    return(with_legend)
  }else{return(p)}
}


