
library(caret)

f <- as.data.frame(table(season04.06$action_type))
f$porc  <- f$Freq/3051
b <- ifelse(f$porc<0.01,'Other',levels(f$Var1))

levels(season04.06$action_type) <- b

#Information gain

library(FSelector)
ig <- information.gain(shot_made_flag~action_type+shot_distance+combined_shot_type+local+shot_zone_basic+period+shot_zone_area+minutes_remaining,season04.06)

#punto 1 tp ML

set.seed(123)

intrain <- createDataPartition(y=season04.06$shot_made_flag,p=0.80,list = FALSE)
season_train <- season04.06[intrain,]
season_test <- season04.06[-intrain,]

write.csv(season04.06,file = "season0406_v2.csv")

write.csv(season_train, file = "train_kobe.csv")
write.csv(season_test, file = "test_kobe.csv")

library(RWeka)
modelo <- J48_ML(season_train,season_test)

library(ggplot2)

g1 <- ggplot(data = modelo,mapping = aes(x=ConfidenceF))+geom_line(aes(y=pCorrect,color="Train"))+geom_line(aes(y=pTest,color="Test"))
g1+scale_colour_discrete(name  ="Dataset")+ggtitle("Accuracy vs CF")+ylab("Accuracy")

g2 <- ggplot(data = modelo,mapping = aes(x=ConfidenceF))+geom_line(aes(y=Leaves,color="Hojas"))+geom_line(aes(y=Size,color="Nodos"))+theme(axis.title.y = element_blank()) 


# Punto 2

addNA <- function(x, noise = 0.1, seed = 432429) {
       set.seed(seed)
       idx = sample(1:length(x), size = length(x)*noise)
       x = replace(x = x, list = idx, NA)  
       return(x)
}

Moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Genero los NA y les asigno la moda

NA_Moda <- function(dataset) {
  for (porc in seq(0.025,0.85,0.025)) {
    NA1 <- addNA(dataset$action_type,porc)
    moda1 <- Moda(season04.06$action_type)
    dataset$action_type <- replace(dataset$action_type,which(is.na(NA1)),moda1)
    assign(paste("season_moda",porc,sep = "."),dataset,envir = .GlobalEnv)
    }
}

NA_Moda(season04.06)

#funcion para correr j48 para cada dataset con la moda

J48_ML_MODA <- function(dataset) {library(caret)
  library(RWeka)
  
  set.seed(123)
  
  intrain <- createDataPartition(y=dataset$shot_made_flag,p=0.80,list = FALSE)
  train <- dataset[intrain,]
  test <- dataset[-intrain,]
  
  ff <- formula(shot_made_flag~action_type+shot_distance+combined_shot_type+local+shot_zone_basic+period+shot_zone_area+minutes_remaining) #seconds_total
  modelos <- data.frame()
  
  for (cf_v in seq(0.025,0.5,0.025)) {
    
    a = J48(ff, data = train ,control = Weka_control(C=cf_v,M=2))
    pCorrect  = summary(a)$details[1]
    Leaves = as.numeric(strsplit(grep(pattern = 'Number of Leaves', capture.output(a), value = T), split = '\t')[[1]][2])
    Size = as.numeric(strsplit(grep(pattern = 'Size of the tree', capture.output(a), value = T), split = '\t')[[1]][2])
    Test.frame = transform(test, pred = predict(a, newdata = test))
    pTest = with(Test.frame, (table(shot_made_flag, pred)[1] + table(shot_made_flag, pred)[4])/nrow(Test.frame))*100
    modelos = rbind(modelos, data.frame(ConfidenceF = cf_v, pCorrect = pCorrect, Leaves = Leaves, Size = Size, pTest = pTest, row.names = NULL))
    
  }
  return(modelos)
}



NA1 <- addNA(season04.06$action_type,0.025)
moda1 <- Moda(season04.06$action_type)
season_moda_01 <- season04.06
season_moda_01$action_type <- replace(season_moda_01$action_type,which(is.na(NA1)),moda1)
