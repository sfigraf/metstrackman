library(tidyverse)
library(lubridate)

trackman <- read_csv("20190924-ChaseField-1.csv")
trackman$spin_rate <- as.numeric(trackman$spin_rate) #change column to numeric
trackman$exit_speed <- as.numeric(trackman$exit_speed) #change column to numeric


#what is the average exit speed of each pitch hit by paul goldmschidt?

goldschmidt <- trackman %>%
  filter(batter == "Goldschmidt, Paul",
         exit_speed != "NULL") %>%
  group_by(pitch_type_auto) %>%
  summarize(avg.exit.velo = mean(exit_speed)) %>%
  ggplot(aes(x = pitch_type_auto, y = avg.exit.velo)) +
  geom_bar(stat = "identity")


View(goldschmidt)
goldschmidt$exit_speed <- as.numeric(goldschmidt$exit_speed) #change column to numeric

goldschmidt %>%
  group_by(pitch_type_auto, play_result) %>%
  summarize(avg.exit.velo = mean(exit_speed)) %>%
  ggplot(aes(x = pitch_type_auto, y = avg.exit.velo)) +
  geom_bar(stat = "identity", 
           aes(fill = play_result))

#can do spin rate stuff? how much one changes from game to game?
#CAN SEE HOW how consistent putchers are by looking at their standarddeviations of pitches?
#which variables are correlated to break, both horizontal and non horizontal?
#which pitches are 

#can begin to cluster based on variables
x <- trackman %>%
  group_by(pitcher,pitch_type_auto) %>%
  summarize(avg_vertbreak = mean(vert_break),
            avg_horbreak = mean(horz_break),
            avg_velo = mean(zone_speed),
            avg_spin = mean(spin_rate))
#gcan also begin to boxplot
#fastball velo
x %>%
  filter(pitch_type_auto == "Fastball") %>%
  ggplot(aes(y = avg_velo)) +
  geom_boxplot() 
?labels

#look into ggmarginal in the ggExtra Package
?

#plate locations
library(lattice)
pitchername = "Flaherty, Jack"

pitches <- trackman %>%
  filter(pitcher == pitchername)

#defining the legend
myKey <- list(space="right",  
              border=TRUE,  
              cex.title=.8,  
              title="pitch type",  
              padding.text=4) 

#defining variables to use the k zone to plot on the graph
topKzone <- 3.5  #average value for top of the zone
botKzone <- 1.6  #average value for bottom of zone
inKzone <- -.95  
outKzone <- 0.95 


# xyplot(plate_loc_z ~ plate_loc_x | batside, data=pitches, groups=pitch_type_auto,  auto.key=TRUE,
#        aspect = "iso",
#        xlim=c(-2.2, 2.2),  ylim=c(0, 5),  xlab="horizontal location\n(ft. from middle of plate)",  ylab="vertical location\n(ft. from ground)",
#        panel=function(...){
#       panel.xyplot(...)
#       panel.rect(inKzone, botKzone, outKzone, topKzone,  border="black", lty=3)  } 
#        ) 
#for ggplot need to make kzone into a dataframe
kZone <- data.frame(x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
                    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)) 


pitches %>%
  ggplot(aes(x = plate_loc_x, y = plate_loc_z)) +
  geom_path(aes(x, y), data = kZone, lwd = 2, col = "black", alpha = .4) +
  geom_point(aes(color = pitch_type_auto, shape = pitch_call)) +
  facet_wrap(~ batside) + 
  coord_equal() +
  theme(panel.background = element_blank()) +
  ggtitle(paste0(pitchername," Pitches"))

# axis.title.x = "Horizontal location <br> (ft from middle of the plate)",
# axis.text.y = "Vertical Location (ft from")
exitspeeds <- trackman$exit_speed
bins <- seq(min(exitspeeds, na.rm = TRUE), max(exitspeeds, na.rm = TRUE), length.out = 4) 
?seq
trackman %>%
  select(exit_speed) %>%
  mutate(exitvelobins = seq(min(exit_speed, na.rm = TRUE), max(exit_speed, na.rm = TRUE), length.out = 4))


pitches[,2] <- as.character(pitches[,2])
pitches %>%
  ggplot(aes(x = pitchid, y = zone_speed, label = pitches$date)) +
  facet_wrap(~pitch_type_auto) +
  geom_point(aes(color = exit_speed, shape = pitch_call, size = 2)) +
  geom_smooth(col = "black") +
  theme_classic() +
  ggtitle(paste0(pitchername," Pitch Speeds")) 

#data prep
ranks <- trackman %>%
  group_by(pitcher,pitch_type_auto) %>%
  summarize(avg_vertbreak = mean(vert_break),
            avg_horbreak = mean(horz_break),
            avg_velo = mean(zone_speed),
            avg_spin = mean(spin_rate))

ranks$avg_velo_z <- round((ranks$avg_velo - mean(ranks$avg_velo))/sd(ranks$avg_velo), 2)  # compute normalized avg_velo
ranks$avg_velo_type <- ifelse(ranks$avg_velo_z < 0, "below", "above")  # above / below avg flag
ranks <- ranks[order(ranks$avg_velo_z), ]  # sort
ranks <- ranks %>%
  filter(pitch_type_auto == "Fastball")
ranks$pitcher <- factor(ranks$pitcher, levels = ranks$pitcher)


  
##
#View(mtcars)
ranks %>%
  arrange(avg_velo_z) %>%
  ggplot(aes(x=pitcher, y=avg_velo_z, label=avg_velo)) + 
  geom_point(stat='identity', aes(col=avg_velo_type), size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="black", size=2) +
  labs(title="Pitcher Fastball Velos", 
       subtitle="Normalized Fastball Velos from trackman data") + 
  ylim(-2.5, 2.5) +
  coord_flip()

#dumbbell plot
#devtools::install_github("hrbrmstr/ggalt")
library(ggalt)
x$pitcher <- factor(x$pitcher, levels = x$pitcher)

####
ranks2 <- ranks %>%
                    filter(pitch_type_auto == "Fastball") %>%
                    mutate(avg_velo_z = round((avg_velo - mean(avg_velo))/sd(avg_velo), 2)) %>%
                    mutate(avg_velo_type <- ifelse(ranks$avg_velo_z < 0, "below", "above")) %>%
                    arrange(avg_velo_z) %>%
                    mutate(pitcher = factor(pitcher, levels = pitcher))


