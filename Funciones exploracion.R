  #a plot to see accuracy by feature
  
  pplot <- function(feat) {
    feat <- substitute(feat)
    ggplot(data = fill_target, aes_q(x = feat)) +
      geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
      scale_fill_brewer(palette = "Set1", direction = -1) +
      ggtitle(paste("accuracy by", feat))
    
  }
  
  # a plot to see position by feature
  courtplot <- function(feat) {
    feat <- substitute(feat)
    fill_target %>% 
      ggplot(aes(x = lon, y = lat)) +
      geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
      ylim(c(33.7, 34.0883)) +
      scale_color_brewer(palette = "Set1") +
      theme_void() +
      ggtitle(paste(feat))
  }
  
# Tiros sin "jump shots"
  
ggplot() +
    geom_point(data = filter(fill_target, combined_shot_type == "Jump Shot"),
    aes(x = lon, y = lat), color = "grey", alpha = 0.3, size = 2) +
    geom_point(data = filter(fill_target, combined_shot_type != "Jump Shot"),
    aes(x = lon, y = lat, 
    color = combined_shot_type), alpha = 0.7, size = 3) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    ggtitle("Shot Types")
	
	
# cambio de campos
train$shot_made_flag <- factor(train$shot_made_flag)

train$period <- factor(train$period)

train$playoffs <- factor(train$playoffs)

#reducir datos

season04.06 <- train[train$season==c('2004-05','2005-06'),]

season04.06$local <- ifelse(trimws(substr(season04.06$matchup,5,6))=="@","N","S")

season04.06$local <- factor(season04.06$local)  

lvl <- levels(season04.06$action_type)

lvl <- ifelse(!(lvl %in% c("Driving Layup Shot","Fadeaway Jump Shot","Jump Bank Shot","Jump Shot","Layup Shot","Reverse Layup Shot","Running Jump Shot","Slam Dunk Shot","Turnaround Jump Shot ")),"other",lvl)

levels(season04.06$action_type) <- lvl

library(arules)

season04.06$shot_dis_nominal <- discretize(season04.06$shot_distance,method = "frequency")

