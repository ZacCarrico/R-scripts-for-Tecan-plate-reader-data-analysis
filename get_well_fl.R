# gets fluorescence from tecan m1000 pro multiple reads per well plate reader data assuming that the first fluorescent measurements are red fluorescence and the second set are green fluorescence
get_well_fl<-function (file.csv, startwell, endwell, bg.start.well, bg.end.well) 
{
  df.csv <- read.csv(file.csv, header = F) # creates a dataframe from the csv file
  well <- df.csv[,1]
  Mean <- df.csv[,2]
  options(stringsAsFactors = F)
  df <- data.frame(well, Mean)
  df$Mean <- as.numeric(df$Mean)
  
  #"which" function returns the row number for the given well name. If you acquired both red and green fluorescence then you will have the same well name twice in the excel. If you acquired the red first then green index 1 from "which" will be red and index 2 will be green.
  redStart <- which(df$well == startwell)[1] #gets the first occurence of a given well name and for my protocols the red fluorescence is measured before the green fluorescence so the first occurence is the red fluorescence and the second occurence is the green fluorescence
  greenStart <- which(df$well == startwell)[2]
  red.bg.start <- which(df$well == bg.start.well)[1]
  green.bg.start <- which(df$well == bg.start.well)[2]
  
  redEnd <- which(df$well == endwell)[1]
  greenEnd <- which(df$well == endwell)[2]
  red.bg.end <- which(df$well == bg.end.well)[1]
  green.bg.end <- which(df$well == bg.end.well)[2]
  
  well.names<-df[redStart:redEnd,1]
  
  R.fl <- df[redStart:redEnd,2]
  G.fl <- df[greenStart:greenEnd,2]
  R.bg.fl <- df[red.bg.start:red.bg.end, 2]
  G.bg.fl <- df[green.bg.start:green.bg.end, 2]
  
  return(list(well.names, R.fl, G.fl, R.bg.fl, G.bg.fl))
}
