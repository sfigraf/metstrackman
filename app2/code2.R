library(tidyverse)
library(dendextend)
library(randomForest)
library(plotly)

###changing columns to numeric
trackman$spin_rate <- as.numeric(trackman$spin_rate) #change column to numeric
trackman$horz_break <- as.numeric(trackman$horz_break) #change column to numeric
trackman$vert_break <- as.numeric(trackman$vert_break) #change column to numeric
trackman$tilt <- as.numeric(trackman$tilt) #change column to numeric
trackman$extension <- as.numeric(trackman$extension) #change column to numeric
trackman$rel_speed <- as.numeric(trackman$rel_speed) #change column to numeric
trackman$spin_axis <- as.numeric(trackman$spin_axis) #change column to numeric


##comparisons data
ranks <- trackman %>%
  group_by(pitcher,pitch_type_auto) %>%
  summarize(avg_vertbreak = mean(vert_break),
            avg_horbreak = mean(horz_break),
            avg_velo = mean(zone_speed),
            avg_spin = mean(spin_rate))



#defining the kzone for later graphic

topKzone <- 3.5  #average value for top of the zone
botKzone <- 1.6  #average value for bottom of zone
inKzone <- -.95  
outKzone <- 0.95 
kZone <- data.frame(x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
                    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)) 