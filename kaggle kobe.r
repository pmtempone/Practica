# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

system("ls ../input")

kobe <- read.csv("../input/data.csv", stringsAsFactors = TRUE)

kobe$game_id <- NULL

kobe$team_id <- NULL

kobe$team_name <- NULL

kobe$game_event_id <- NULL

kobe$shot_made_flag <- factor(kobe$shot_made_flag)

kobe$playoffs <- factor(kobe$playoffs)

kobe$period <- factor(kobe$period)


kobe$local <- factor(ifelse(trimws(substr(kobe$matchup,5,6))=="@","N","S"))

summary(kobe)

library(RWeka)

library(reshape2)

dat.m <- melt(kobe,id.vars='shot_made_flag', measure.vars=c("lat","lon","loc_x","loc_y","minutes_remaining","seconds_remaining","shot_distance"))

p <- ggplot(dat.m) + geom_boxplot(aes(x=shot_made_flag, y=value, color=variable))

p+facet_wrap( ~ variable, scales="free")

#lvl <- levels(kobe$action_type)

#lvl <- ifelse(!(lvl %in% c("Driving Layup Shot","Fadeaway Jump Shot","Jump Bank Shot","Jump Shot","Layup Shot","Reverse Layup Shot","Running Jump Shot","Slam Dunk Shot","Turnaround Jump Shot ")),"other",lvl)

#levels(kobe$action_type) <- lvl

#levels(kobe$action_type)

#ggplot(kobe,aes(shot_made_flag,seconds_remaining))+geom_boxplot()

#train <- kobe[!is.na(kobe$shot_made_flag),]

#test <- kobe[is.na(kobe$shot_made_flag),]

train <- kobe[!is.na(kobe$shot_made_flag),]

test <- kobe[is.na(kobe$shot_made_flag),]



#first model using J48

fit.1 <- J48(shot_made_flag~action_type+combined_shot_type+period+shot_distance+shot_type+shot_zone_area+local,data = train,control=Weka_control(C=0.25,M=2))
library(GGally)

ggpairs(train,mapping = ggplot2::aes(color=shot_made_flag),columns = c("lat","lon","loc_x","loc_y","minutes_remaining","seconds_remaining","shot_distance"))


library(plyr)

t1 <- count(train,vars = c("shot_made_flag","action_type"))
 
p1 <- ggplot(t1,aes(x=action_type,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t2 <- count(train,vars = c("shot_made_flag","combined_shot_type"))

p2 <- ggplot(t2,aes(x=combined_shot_type,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t3 <- count(train,vars = c("shot_made_flag","period"))

p3 <- ggplot(t3,aes(x=period,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t4 <- count(train,vars = c("shot_made_flag","playoffs"))

p4 <- ggplot(t4,aes(x=playoffs,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t5 <- count(train,vars = c("shot_made_flag","season"))

p5 <- ggplot(t5,aes(x=season,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t6 <- count(train,vars = c("shot_made_flag","shot_type"))

p6 <- ggplot(t6,aes(x=shot_type,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t7 <- count(train,vars = c("shot_made_flag","shot_zone_area"))

p7 <- ggplot(t7,aes(x=shot_zone_area,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t8 <- count(train,vars = c("shot_made_flag","shot_zone_basic"))

p8 <- ggplot(t8,aes(x=shot_zone_basic,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t9 <- count(train,vars = c("shot_made_flag","shot_zone_range"))

p9 <- ggplot(t9,aes(x=shot_zone_range,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

t10 <- count(train,vars = c("shot_made_flag","local"))

p10 <- ggplot(t10,aes(x=local,y=freq,fill=shot_made_flag))+geom_bar(stat="identity",position="dodge")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p2,p3,p4,p5,p6,p7,p8,p9,p10,cols=2)


kobe$lat <- NULL

kobe$lon <- NULL

library(outliers)

outlier(train$loc_x)

outlier(train$loc_x,opposite=TRUE)

outlier(train$loc_y)

outlier(train$loc_y,opposite=TRUE)

outlier(train$minutes_remaining)

outlier(train$minutes_remaining,opposite=TRUE)

outlier(train$seconds_remaining)

outlier(train$seconds_remaining,opposite=TRUE)

# Loc_x, and loc_y data binning

kobe$loc_x_bins <- cut(kobe$loc_x,10) #use stem.leaf to see how many bins


kobe$loc_y_bins <- cut(kobe$loc_y,12)

#count action_type
at <- arrange(count(kobe$action_type),freq)

at$porc <- at$freq/30697

#see most action_types with more than 1% of frequency

at[at$porc>0.01,]

#replace with 'Other'

levels(kobe$action_type)[at$x[at$porc<=0.01]] <- 'Other'
library(mlbench)
library(caret)
library("randomForest")
library("ipred")
library("gbm")

set.seed(123)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe.train <- rfe(kobe[!is.na(kobe$shot_made_flag),c(1,2,7,8,9,10,11,12,14,15,16,17,24,25,26)], kobe[!is.na(kobe$shot_made_flag),13], sizes=1:12, rfeControl=control)
rfe.train

#Recursive feature selection

#Outer resampling method: Cross-Validated (10 fold) 

'Resampling performance over subset size:

 Variables Accuracy  Kappa AccuracySD KappaSD Selected
         1   0.6787 0.3285   0.007111 0.01489         
         2   0.6785 0.3277   0.007201 0.01531         
         3   0.6788 0.3278   0.007029 0.01469         
         4   0.6790 0.3264   0.008027 0.01744         
         5   0.6795 0.3267   0.006594 0.01432         
         6   0.6788 0.3244   0.005768 0.01226         
         7   0.6793 0.3254   0.006571 0.01394         
         8   0.6796 0.3261   0.006090 0.01328        *
         9   0.6684 0.3076   0.007875 0.01572         
        10   0.6703 0.3111   0.009983 0.01824         
        11   0.6714 0.3132   0.008935 0.01743         
        12   0.6739 0.3193   0.007008 0.01402         
        15   0.6770 0.3264   0.009394 0.01918         

The top 5 variables (out of 8):
   action_type, shot_distance, loc_y_bins, combined_shot_type, local'
   
 train <- kobe[!is.na(kobe$shot_made_flag),]
 
 test <- kobe[is.na(kobe$shot_made_flag),]
  
 fit.rf <- randomForest(shot_made_flag ~ action_type+shot_distance+loc_y_bins+ combined_shot_type+local,data=train,ntree=100)
   
 test.rf <- predict(fit.rf,test)
 
set.seed(123)

boruta.train <- Boruta(shot_made_flag ~ action_type+shot_distance+loc_y_bins+ combined_shot_type+local+loc_x_bins+shot_zone_basic+season,data=train, doTrace = 2)


J48_ML <- function(x) {cf_v<<-0.025
for (i in 1:20) {
  e <- assign(paste("fit",i,sep = "."),
              J48(shot_made_flag~shot_distance+combined_shot_type+local+shot_zone_basic+playoffs+period+minutes_remaining+shot_zone_area,
                  data= x ,control = Weka_control(C=cf_v,M=2)),envir = .GlobalEnv)
  cf_v=cf_v+0.025
  assign(paste("eval",i,sep="."),evaluate_Weka_classifier(e,
                                                          cost = matrix(c(0,2,1,0), ncol = 2), complexity = TRUE,seed = 123, class = TRUE),envir = .GlobalEnv)}
}

#Particion test y train
library(caret)
set.seed(123)
intrain <- createDataPartition(y=season04.06$shot_made_flag,p=0.80,list = FALSE)
season_train <- season04.06[intrain,]
season_test <- season04.06[-intrain,]

write.csv(season_train, file = "train_kobe.csv")
write.csv(season_test, file = "test_kobe.csv")

txt_pcorrect <- cbind.data.frame(eval.1$details,eval.2$details,eval.3$details,eval.4$details,eval.5$details,eval.6$details,eval.7$details,eval.8$details,eval.9$details,eval.10$details,eval.11$details,eval.12$details,eval.13$details,eval.14$details,eval.15$details,eval.16$details,eval.17$details,eval.18$details,eval.19$details,eval.20$details)

