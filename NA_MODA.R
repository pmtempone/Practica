NA_Moda <- function(dataset) {
  for (porc in seq(0.025,0.85,0.025)) {
    NA1 <- addNA(dataset$action_type,porc)
    moda1 <- Moda(season04.06$action_type)
    dataset$action_type <- replace(dataset$action_type,which(is.na(NA1)),moda1)
    assign(paste("season_moda",porc,sep = "."),dataset,envir = .GlobalEnv)
    }
}