library(reshape2)
library(ggplot2)

kobe$shot_made_flag <- factor(kobe$shot_made_flag)

kobe$playoffs <- factor(kobe$playoffs)

kobe$local <- factor(ifelse(trimws(substr(kobe$matchup,5,6))=="@","N","S"))

train <- kobe[!is.na(kobe$shot_made_flag),]

test <- kobe[is.na(kobe$shot_made_flag),]


dat.m <- melt(kobe,id.vars='shot_made_flag', measure.vars=c("lat","lon","loc_x","loc_y","minutes_remaining","seconds_remaining","shot_distance"))

p <- ggplot(dat.m) + geom_boxplot(aes(x=shot_made_flag, y=value, color=variable))

p+facet_wrap( ~ variable, scales="free")

library(lattice)
library(GGally)

ggpairs(train,mapping = ggplot2::aes(color=shot_made_flag),columns = c("lat","lon","loc_x","loc_y","minutes_remaining","seconds_remaining","shot_distance"))
