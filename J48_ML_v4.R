J48_ML <- function(train, test) {
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