NH_train <- function(a){
  Leaves <- strsplit(grep(pattern = 'Number of Leaves', capture.output(a), value = T), split = '\t')[[1]][2]
  Size <- strsplit(grep(pattern = 'Size of the tree', capture.output(a), value = T), split = '\t')[[1]][2]
  print(cbind.data.frame(x,y))
    }