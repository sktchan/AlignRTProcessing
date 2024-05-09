library(ggplot2)
library(plotly)
library(gridExtra)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

# import clean, beam on datasets
setwd("/Users/Serena/Desktop/JCC/Git/CleanData")

A_on <- read.csv("CleanA.csv")
B_on <- read.csv("CleanB.csv")
C_on <- read.csv("CleanC.csv")
D_on <- read.csv("CleanD.csv")

# converting cm to mm
columns_to_multiply <- c("D.VRT..cm.", "D.LNG..cm.", "D.LAT..cm.")
A_on <- A_on %>%
  mutate_at(vars(columns_to_multiply), ~ . * 10)

columns_to_multiply <- c("D.VRT..cm.", "D.LNG..cm.", "D.LAT..cm.")
B_on <- B_on %>%
  mutate_at(vars(columns_to_multiply), ~ . * 10)

columns_to_multiply <- c("D.VRT..cm.", "D.LNG..cm.", "D.LAT..cm.")
C_on <- C_on %>%
  mutate_at(vars(columns_to_multiply), ~ . * 10)

columns_to_multiply <- c("D.VRT..cm.", "D.LNG..cm.", "D.LAT..cm.")
D_on <- D_on %>%
  mutate_at(vars(columns_to_multiply), ~ . * 10)


# take mean of first 10 points as the baseline for movement off of
A_baseline <- A_on %>%
  group_by(TreatmentDate) %>%
  select(-Trans.Threshold..cm., -Rot.Threshold..deg., -Gating.Active., 
         -Elapsed.Time..sec., -Translation..cm., -Angle.Grouping) %>%
  mutate_all(as.numeric) %>%
  slice_head(n = 10) %>%
  mutate(TreatmentDate = factor(TreatmentDate, 
                                levels = c("Oct 30", "Oct 31", "Nov 1", "Nov 2", 
                                           "Nov 7", "Nov 8", "Nov 10", "Nov 15", 
                                           "Nov 16", "Nov 17"))) %>%
  arrange(TreatmentDate) %>%
  summarize(across(everything(), mean, na.rm = TRUE))


B_baseline <- B_on %>%
  group_by(TreatmentDate) %>%
  select(-Trans.Threshold..cm., -Rot.Threshold..deg., -Gating.Active., 
         -Elapsed.Time..sec., -Translation..cm., -Angle.Grouping) %>%
  mutate_all(as.numeric) %>%
  slice_head(n = 10) %>%
  mutate(TreatmentDate = factor(TreatmentDate, 
                                levels = c("Nov 15", "Nov 16", "Nov 20",
                                           "Nov 21", "Nov 22", "Nov 23",
                                           "Nov 24", "Nov 27", "Nov 28",
                                           "Nov 30", "Dec 1", "Dec 4",
                                           "Dec 5", "Dec 7", "Dec 11"))) %>%
  arrange(TreatmentDate) %>%
  summarize(across(everything(), mean, na.rm = TRUE))


C_baseline <- C_on %>%
  group_by(TreatmentDate) %>%
  select(-Trans.Threshold..cm., -Rot.Threshold..deg., -Gating.Active., 
         -Elapsed.Time..sec., -Translation..cm., -Angle.Grouping) %>%
  mutate_all(as.numeric) %>%
  slice_head(n = 10) %>%
  mutate(TreatmentDate = factor(TreatmentDate, 
                                levels = c("Nov 15", "Nov 16", "Nov 20",
                                           "Nov 21", "Nov 23", "Nov 28",
                                           "Nov 29", "Dec 4", "Dec 7",
                                           "Dec 8", "Dec 12"))) %>%
  arrange(TreatmentDate) %>%
  summarize(across(everything(), mean, na.rm = TRUE))


D_baseline <- D_on %>%
  group_by(TreatmentDate) %>%
  select(-Trans.Threshold..cm., -Rot.Threshold..deg., -Gating.Active., 
         -Elapsed.Time..sec., -Translation..cm., -Angle.Grouping) %>%
  mutate_all(as.numeric) %>%
  slice_head(n = 10) %>%
  mutate(TreatmentDate = factor(TreatmentDate, 
                                levels = c("Feb 29", "Mar 5", "Mar 7", "Mar 11", "Mar 12",
                                           "Mar 15", "Mar 19", "Mar 25", "Mar 26",
                                           "Mar 27", "Mar 28"))) %>%
  arrange(TreatmentDate) %>%
  summarize(across(everything(), mean, na.rm = TRUE))











# function to calculate absolute vrt differences greater than 3mm
check_vrt_3mm <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.VRT..cm.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.VRT..cm.[i] - baseline_mean)
    if (abs_diff > 3) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

# for greater than 5mm
check_vrt_5mm <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.VRT..cm.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.VRT..cm.[i] - baseline_mean)
    if (abs_diff > 5) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}


A_on$greater_vrt_3mm <- check_vrt_3mm(A_on, A_baseline)
print(mean(A_on$greater_vrt_3mm) * 100)
A_on$greater_vrt_5mm <- check_vrt_5mm(A_on, A_baseline)
print(mean(A_on$greater_vrt_5mm) * 100)

C_on$greater_vrt_3mm <- check_vrt_3mm(C_on, C_baseline)
print(mean(C_on$greater_vrt_3mm) * 100)
C_on$greater_vrt_5mm <- check_vrt_5mm(C_on, C_baseline)
print(mean(C_on$greater_vrt_5mm) * 100)

B_on$greater_vrt_3mm <- check_vrt_3mm(B_on, B_baseline)
print(mean(B_on$greater_vrt_3mm) * 100)
B_on$greater_vrt_5mm <- check_vrt_5mm(B_on, B_baseline)
print(mean(B_on$greater_vrt_5mm) * 100)

D_on$greater_vrt_3mm <- check_vrt_3mm(D_on, D_baseline)
print(mean(D_on$greater_vrt_3mm) * 100)
D_on$greater_vrt_5mm <- check_vrt_5mm(D_on, D_baseline)
print(mean(D_on$greater_vrt_5mm) * 100)










# function to calculate absolute lng differences greater than 3mm
check_lng_3mm <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.LNG..cm.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.LNG..cm.[i] - baseline_mean)
    if (abs_diff > 3) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

# for greater than 5mm
check_lng_5mm <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.LNG..cm.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.LNG..cm.[i] - baseline_mean)
    if (abs_diff > 5) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

A_on$greater_lng_3mm <- check_lng_3mm(A_on, A_baseline)
print(mean(A_on$greater_lng_3mm) * 100)
A_on$greater_lng_5mm <- check_lng_5mm(A_on, A_baseline)
print(mean(A_on$greater_lng_5mm) * 100)

C_on$greater_lng_3mm <- check_lng_3mm(C_on, C_baseline)
print(mean(C_on$greater_lng_3mm) * 100)
C_on$greater_lng_5mm <- check_lng_5mm(C_on, C_baseline)
print(mean(C_on$greater_lng_5mm) * 100)

B_on$greater_lng_3mm <- check_lng_3mm(B_on, B_baseline)
print(mean(B_on$greater_lng_3mm) * 100)
B_on$greater_lng_5mm <- check_lng_5mm(B_on, B_baseline)
print(mean(B_on$greater_lng_5mm) * 100)

D_on$greater_lng_3mm <- check_lng_3mm(D_on, D_baseline)
print(mean(D_on$greater_lng_3mm) * 100)
D_on$greater_lng_5mm <- check_lng_5mm(D_on, D_baseline)
print(mean(D_on$greater_lng_5mm) * 100)














# function to calculate absolute lat differences greater than 3mm
check_lat_3mm <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.LAT..cm.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.LAT..cm.[i] - baseline_mean)
    if (abs_diff > 3) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

# for greater than 5mm
check_lat_5mm <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.LAT..cm.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.LAT..cm.[i] - baseline_mean)
    if (abs_diff > 5) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}


A_on$greater_lat_3mm <- check_lat_3mm(A_on, A_baseline)
print(mean(A_on$greater_lat_3mm) * 100)
A_on$greater_lat_5mm <- check_lat_5mm(A_on, A_baseline)
print(mean(A_on$greater_lat_5mm) * 100)

C_on$greater_lat_3mm <- check_lat_3mm(C_on, C_baseline)
print(mean(C_on$greater_lat_3mm) * 100)
C_on$greater_lat_5mm <- check_lat_5mm(C_on, C_baseline)
print(mean(C_on$greater_lat_5mm) * 100)

B_on$greater_lat_3mm <- check_lat_3mm(B_on, B_baseline)
print(mean(B_on$greater_lat_3mm) * 100)
B_on$greater_lat_5mm <- check_lat_5mm(B_on, B_baseline)
print(mean(B_on$greater_lat_5mm) * 100)

D_on$greater_lat_3mm <- check_lat_3mm(D_on, D_baseline)
print(mean(D_on$greater_lat_3mm) * 100)
D_on$greater_lat_5mm <- check_lat_5mm(D_on, D_baseline)
print(mean(D_on$greater_lat_5mm) * 100)










# function to calculate absolute pit differences greater than 1deg
check_pit_1deg <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.Pitch..deg.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.Pitch..deg.[i] - baseline_mean)
    if (abs_diff > 1) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

# greater than 3deg
check_pit_3deg <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.Pitch..deg.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.Pitch..deg.[i] - baseline_mean)
    if (abs_diff > 3) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

A_on$greater_pit_1deg <- check_pit_1deg(A_on, A_baseline)
print(mean(A_on$greater_pit_1deg) * 100)
A_on$greater_pit_3deg <- check_pit_3deg(A_on, A_baseline)
print(mean(A_on$greater_pit_3deg) * 100)

C_on$greater_pit_1deg <- check_pit_1deg(C_on, C_baseline)
print(mean(C_on$greater_pit_1deg) * 100)
C_on$greater_pit_3deg <- check_pit_3deg(C_on, C_baseline)
print(mean(C_on$greater_pit_3deg) * 100)

B_on$greater_pit_1deg <- check_pit_1deg(B_on, B_baseline)
print(mean(B_on$greater_pit_1deg) * 100)
B_on$greater_pit_3deg <- check_pit_3deg(B_on, B_baseline)
print(mean(B_on$greater_pit_3deg) * 100)

D_on$greater_pit_1deg <- check_pit_1deg(D_on, D_baseline)
print(mean(D_on$greater_pit_1deg) * 100)
D_on$greater_pit_3deg <- check_pit_3deg(D_on, D_baseline)
print(mean(D_on$greater_pit_3deg) * 100)






# function to calculate absolute roll differences greater than 1deg
check_rol_1deg <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.Roll..deg.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.Roll..deg.[i] - baseline_mean)
    if (abs_diff > 1) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

# greater than 3deg
check_rol_3deg <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.Roll..deg.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.Roll..deg.[i] - baseline_mean)
    if (abs_diff > 3) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

A_on$greater_rol_1deg <- check_rol_1deg(A_on, A_baseline)
print(mean(A_on$greater_rol_1deg) * 100)
A_on$greater_rol_3deg <- check_rol_3deg(A_on, A_baseline)
print(mean(A_on$greater_rol_3deg) * 100)

C_on$greater_rol_1deg <- check_rol_1deg(C_on, C_baseline)
print(mean(C_on$greater_rol_1deg) * 100)
C_on$greater_rol_3deg <- check_rol_3deg(C_on, C_baseline)
print(mean(C_on$greater_rol_3deg) * 100)

B_on$greater_rol_1deg <- check_rol_1deg(B_on, B_baseline)
print(mean(B_on$greater_rol_1deg) * 100)
B_on$greater_rol_3deg <- check_rol_3deg(B_on, B_baseline)
print(mean(B_on$greater_rol_3deg) * 100)

D_on$greater_rol_1deg <- check_rol_1deg(D_on, D_baseline)
print(mean(D_on$greater_rol_1deg) * 100)
D_on$greater_rol_3deg <- check_rol_3deg(D_on, D_baseline)
print(mean(D_on$greater_rol_3deg) * 100)










# function to calculate absolute yaw differences greater than 1deg
check_yaw_1deg <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.Rtn..deg.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.Rtn..deg.[i] - baseline_mean)
    if (abs_diff > 1) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

# greater than 3deg
check_yaw_3deg <- function(df, baseline_df) {
  result <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    treatment_date <- df$TreatmentDate[i]
    baseline_mean <- baseline_df$D.Rtn..deg.[baseline_df$TreatmentDate == treatment_date]
    abs_diff <- abs(df$D.Rtn..deg.[i] - baseline_mean)
    if (abs_diff > 3) {
      result[[i]] <- TRUE
    } else {
      result[[i]] <- FALSE
    }
  }
  return(unlist(result))
}

A_on$greater_yaw_1deg <- check_yaw_1deg(A_on, A_baseline)
print(mean(A_on$greater_yaw_1deg) * 100)
A_on$greater_yaw_3deg <- check_yaw_3deg(A_on, A_baseline)
print(mean(A_on$greater_yaw_3deg) * 100)

C_on$greater_yaw_1deg <- check_yaw_1deg(C_on, C_baseline)
print(mean(C_on$greater_yaw_1deg) * 100)
C_on$greater_yaw_3deg <- check_yaw_3deg(C_on, C_baseline)
print(mean(C_on$greater_yaw_3deg) * 100)

B_on$greater_yaw_1deg <- check_yaw_1deg(B_on, B_baseline)
print(mean(B_on$greater_yaw_1deg) * 100)
B_on$greater_yaw_3deg <- check_yaw_3deg(B_on, B_baseline)
print(mean(B_on$greater_yaw_3deg) * 100)

D_on$greater_yaw_1deg <- check_yaw_1deg(D_on, D_baseline)
print(mean(D_on$greater_yaw_1deg) * 100)
D_on$greater_yaw_3deg <- check_yaw_3deg(D_on, D_baseline)
print(mean(D_on$greater_yaw_3deg) * 100)





