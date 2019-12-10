library(tidyverse)
library(dendextend)
library(randomForest)
library(plotly)

###changing columns to numeric
trackman$Spin.Rate <- as.numeric(trackman$spin_rate) #change column to numeric
trackman$Horz.Break <- as.numeric(trackman$horz_break) #change column to numeric
trackman$Vert.Break <- as.numeric(trackman$vert_break) #change column to numeric
trackman$Extension <- as.numeric(trackman$extension) #change column to numeric
trackman$Spin.Axis <- as.numeric(trackman$spin_axis) #change column to numeric
trackman$Velocity <- as.numeric(trackman$rel_speed) #change column to numeric

trackman.subset <- data.frame(
  ##this is potentially the porblem here
  trackman %>% 
    group_by(pitcher,pitch_type_auto) %>%
    summarize(Spin.Rate = mean(Spin.Rate),
              Horz.Break = mean(Horz.Break),
              Vert.Break = mean(Vert.Break),
              Extension = mean(Extension),
              Spin.Axis = mean(Spin.Axis),
              Velocity = mean(Velocity))
)
#key is the column name for all the columns about to be put into one column
#value is the data in the table that you will put there
#-country is the column you wont gather but will still keep in the new dataframe

# trackman.gathered <- trackman.subset %>%
#   gather(key = "Parameter", 
#          value = "Value",
#          -c(pitcher, pitch_type_auto))

##comparisons data
ranks <- trackman %>%
  group_by(pitcher,pitch_type_auto) %>%
  summarize(avg_vertbreak = mean(vert_break),
            avg_horbreak = mean(horz_break),
            avg_velo = mean(zone_speed),
            avg_spin = mean(spin_rate))



#defining the kzone for later graphic

# topKzone <- 3.5  #average value for top of the zone
# botKzone <- 1.6  #average value for bottom of zone
# inKzone <- -.95  
# outKzone <- 0.95 
# kZone <- data.frame(x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
#                     y=c(botKzone, topKzone, topKzone, botKzone, botKzone)) 

###function to get list of similar pitchers based off the cluster
similarpitchersfunction <- function(pitchfiltered, pitchername){
  pitcherclust <- pitchfiltered[pitchfiltered$pitcher == paste(as.character(pitchername)), c("cluster")]
  similar.pitchers <- NULL
  
  for (i in 1:nrow(pitchfiltered)) {
    if (pitchfiltered$cluster[i] == pitcherclust) {
      
      similar.pitchers[i] <- paste0(pitchfiltered$pitcher[i],";")

    }
  }
  
  similar.pitchers <- na.omit(similar.pitchers)
  #print(as.character(similar.pitchers))
}
