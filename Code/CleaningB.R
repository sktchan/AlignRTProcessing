library(ggplot2)
library(plotly)
library(gridExtra)
library(data.table)

setwd("/Users/Serena/Desktop/JCC/Git/RawData/CaseB")

############################## CHECKING BEAM OFF VALUES ##############################
# this is done to overlap different dates on the same x-y axis


file_names <- c("231115.csv","231116.csv","231120.csv","231121.csv",
                "231122.csv","231123.csv","231124.csv","231127.csv",
                "231128.csv","231130.csv","231201.csv","231204.csv",
                "231205.csv","231207.csv","231211.csv")

func <- function(x) {
  df <- read.csv(x)
  df <- na.omit(df)
  df$Elapsed.Time..sec. <- as.numeric(df$Elapsed.Time..sec.)
  df <- df[!(df$D.VRT..cm.==999),]
  return(df)
}

data_list <- lapply(file_names, func)

data_list <- lapply(seq_along(data_list), function(i){
  df <- data_list[[i]]
  df$TreatmentDate <- NA
  df$TreatmentDate <- i
  df
})

finalTableBeamOff <- do.call(rbind, data_list)
finalTableBeamOff$TreatmentDate <- factor(finalTableBeamOff$TreatmentDate,  
                                          levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                                          labels = c("Nov 15", "Nov 16", "Nov 20",
                                                     "Nov 21", "Nov 22", "Nov 23",
                                                     "Nov 24", "Nov 27", "Nov 28",
                                                     "Nov 30", "Dec 1", "Dec 4",
                                                     "Dec 5", "Dec 7", "Dec 11"))

############################## factoring treatment dates ##############################
# this is done to overlap different dates on the same x-y axis

file_names <- c("231115.csv","231116.csv","231120.csv","231121.csv",
                "231122.csv","231123.csv","231124.csv","231127.csv",
                "231128.csv","231130.csv","231201.csv","231204.csv",
                "231205.csv","231207.csv","231211.csv")

func <- function(x) {
  df <- read.csv(x)
  df <- na.omit(df)
  df <- df[!(df$XRayState==0),]
  df$Elapsed.Time..sec. <- as.numeric(df$Elapsed.Time..sec.)
  df <- df[!(df$D.VRT..cm.==999),]
  return(df)
}

data_list <- lapply(file_names, func)

data_list <- lapply(seq_along(data_list), function(i){
  df <- data_list[[i]]
  df$TreatmentDate <- NA
  df$TreatmentDate <- i
  df
})

finalTable <- do.call(rbind, data_list)
finalTable$TreatmentDate <- factor(finalTable$TreatmentDate,  
                                   levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                                   labels = c("Nov 15", "Nov 16", "Nov 20",
                                              "Nov 21", "Nov 22", "Nov 23",
                                              "Nov 24", "Nov 27", "Nov 28",
                                              "Nov 30", "Dec 1", "Dec 4",
                                              "Dec 5", "Dec 7", "Dec 11"))

dates = c("Nov 15", "Nov 16", "Nov 20",
          "Nov 21", "Nov 22", "Nov 23",
          "Nov 24", "Nov 27", "Nov 28",
          "Nov 30", "Dec 1", "Dec 4",
          "Dec 5", "Dec 7", "Dec 11")

############################## adding a column of gantry angles ##################################

factor_counts <- table(finalTable$TreatmentDate)
result_variable <- as.data.frame(factor_counts)
treatmentFreq <- result_variable$Freq
treatmentFreq

ctrlpts <- read.csv("JQ_CtrlPts.csv")
cp_angles <- ctrlpts$Gantry.Rtn..deg.
cp_time <- ctrlpts$Elapsed.Time..s.

angle_func <- function(f) {
  num_rows <- f
  new_time <- seq(0, max(cp_time), length.out = num_rows)
  new_angles <- approx(x = cp_time, y = cp_angles, xout = new_time)$y
  return(new_angles)
}

angle_list <- lapply(treatmentFreq, angle_func)
combined_list <- unlist(angle_list, recursive = FALSE)

finalTable$Gantry.Angle..deg. <- combined_list

finalTable90270 <- finalTable[!(finalTable$Gantry.Angle..deg.<90),]
finalTable90270 <- finalTable90270[!(finalTable90270$Gantry.Angle..deg.>270),]


############################## creating clusters ##################################
# looking at distr between 180-270, 90-80-90, and 270-180
# slight issue -- when transitioning b/n cluster 2/3, it shows to be + incorrectly

# look at sign changes to distinguish between 180-270 and 270-180
finalTable90270$SignChange <- sign(c(0, diff(finalTable90270$Gantry.Angle..deg.)))

# adding columns for cluster factors
finalTable90270$Angle.Grouping <- ifelse(180 < finalTable90270$Gantry.Angle..deg. & finalTable90270$Gantry.Angle..deg. < 270 & finalTable90270$SignChange == "1", "180-270",
                                         ifelse(finalTable90270$SignChange == 0, "180-270",
                                                ifelse(90 < finalTable90270$Gantry.Angle..deg. & finalTable90270$Gantry.Angle..deg. < 180, "90-180-90",
                                                       ifelse(180 < finalTable90270$Gantry.Angle..deg. & finalTable90270$Gantry.Angle..deg. < 270 & finalTable90270$SignChange == "-1", "270-90", NA))))

finalTable90270$Angle.Grouping <- factor(finalTable90270$Angle.Grouping, levels = c("180-270", "90-180-90", "270-90"), labels = c("180-270", "90-180-90", "270-90"))

# ordering clusters by time for further graphing
finalTable90270$Angle.Grouping <- factor(finalTable90270$Angle.Grouping,
                                         levels = c("180-270", "90-180-90", "270-90"), ordered = TRUE)

############################## saving cleaned data ##################################

write.csv(finalTable90270, "/Users/Serena/Desktop/JCC/Git/CleanData/CleanB.csv", row.names=FALSE)


