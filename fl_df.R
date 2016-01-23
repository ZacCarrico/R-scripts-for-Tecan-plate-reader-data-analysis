# automates fluorescence dataframe (thus "fl_df") creation from Tecan m1000 multi-read excel data output and assumes that there are fluorescence measurements taken in the red and green fluorescence channels
# the arguments are: files in csv format, the date the culture or samples were made, the first well, the last well, the condition for that well, the background fluorescence startwell and endwells (the background well's fluorescence are subtracted from the fluorescence of the measured wells)
# setwd to the folder with your tecan plate reader acquisition files then assign the files to the files list

fldf_row_specific_v2<-function(files.csv, culture_date, startWell, endWell,condition, bg.startWell, bg.endWell)
{
  acq_date <- c() # acquisition date
  r.fl <- c() # red fluorescence
  g.fl <- c() # green fluorescence
  well <- c()
  row <- c()
  col <- c()
  comments <- c()
  r.bg.fl <- c() # background fluorescence for subtracting from the red fluorescence wells
  g.bg.fl <- c() # background fluorescence for subtracting from the green fluorescence wells
  r.minus.bg <- c() # background subtracted red fluorescence
  g.minus.bg <- c() # background subtracted green fluorescence
  r.pc.minus.bg <- c() # percent change in background subtracted red fluorescence
  g.pc.minus.bg <- c() # percent change in background subtracted green fluorescence
  list.csv <- list()
  
  for(i in files.csv){  # takes the well names (eg "B2") measurements and combines them into a single vector
    pr.data <- get_well_fl(i, startWell, endWell, bg.startWell, bg.endWell)
    well.i <- pr.data[[1]]
    r.fl.i <- pr.data[[2]]
    g.fl.i <- pr.data[[3]]
    r.bg.fl.i <- mean(pr.data[[4]])
    g.bg.fl.i <- mean(pr.data[[5]])
    
    well <- append(well, well.i)
    r.fl <- append(r.fl, r.fl.i)
    g.fl <- append(g.fl, g.fl.i)
    r.bg.fl <- append(r.bg.fl, rep(r.bg.fl.i, length(condition)))
    g.bg.fl <- append(g.bg.fl, rep(g.bg.fl.i, length(condition)))
    r.minus.bg <- append(r.minus.bg, r.fl.i - r.bg.fl.i)
    g.minus.bg <- append(g.minus.bg, g.fl.i - g.bg.fl.i)
  }
  well<-factor(well,levels=well)

  acq_dates<-c() # will contain a single date for each plate reader measurment day
  for (i in 1:length(files.csv)){  #produces a vector with all the dates
    acq_dates[i]<-(paste("20",substring(files.csv[i],9,10),"-",substring(files.csv[i],11,12),"-",substring(files.csv[i],13,14),sep=""))
  }
  
  acq_date<-c() # this is a vector with each day repeated for as many rows as wells were measured on that day
  for (i in 1:length(acq_dates)){acq_date<-append(acq_date,rep(acq_dates[i],length(well)/length(acq_dates)))} #this fills the empty acq_date vector with as many repetitions as there are individual wells measured that day by taking the vector of well for the df, which has repetitions of the rows for each days acqusition, and dividing by the number of days acquired for. This results in the correct number of repeptions of the date for the number of wells measured from

  DIV <- c()
  for (i in 1:length(acq_date)){DIV[i] <- as.Date(acq_date[i]) - as.Date(culture_date)} # this provide the number of days after the reagents were added
  
  for(i in 1:length(well)){row[i]<-substr(well[i],1,1)} # gets the letter from each well
  
  for(i in 1:length(well)) { # gets the column number of the well
    col[i]<-substr(well[i],2,nchar(as.character(well[i])))
  }
  
  # percent change (pc) in fluorescence
  r.pc <- NA
  if(length(files.csv) > 1){
    for(i in (length(condition)+1):length(r.fl)){
      r.pc[i] <- (((r.fl[i]-r.fl[i-length(condition)])/r.fl[i-length(condition)])*100)
    }}
  
  g.pc <- NA
  if(length(files.csv) > 1){
    for(i in (length(condition)+1):length(g.fl)){g.pc[i] <- (((g.fl[i]-g.fl[i-length(condition)])/g.fl[i-length(condition)])*100)}}
  
  sum.r.pc <- 0 
  if(length(files.csv) > 1){ #Here I will sum the PC changes from all the previous days
    for(j in 1:length(condition)){
      sum.r.pc[j] <- 0
    }
    for(i in (length(condition)+1):length(r.fl)){
      sum.r.pc[i] <- r.pc[i] + sum.r.pc[i-length(condition)]
    }
  }
  
  fldf <- data.frame(acq_date, DIV, well, condition, r.fl, g.fl, r.bg.fl, g.bg.fl, r.minus.bg, g.minus.bg, r.pc, g.pc, row, col)

  return(fldf)
}
