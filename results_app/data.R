library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyTree)
library(RCurl)

import_data_dropbox <- function(global_data){
  dropbox_links <- c("https://www.dropbox.com/s/amagzojd4xmj66l/metaphewas_model1a_extracted.RDS?dl=1",
                     "https://www.dropbox.com/s/3ey11hxpqlo6i2h/metaphewas_model1b_extracted.RDS?dl=1"
  )
  all_res <- lapply(dropbox_links,function(x) readRDS(url(x)))
  # a bit of tidying will happen (stick it all together, make lists of unique exposure and outcome classes, make a tibble of unique combinations, put everything in a list and name the objects)
  all_res <- bind_rows(all_res)
  global_data$exp_classes <- unique(all_res$exposure_class)
  global_data$out_classes <- unique(all_res$outcome_class)
  all_res_tib <- as_tibble(unique(all_res[,c("exposure_class", "exposure_subclass", "person_exposed", "exposure_time", "exposure_type", "exposure_source", "exposure_dose", "model")]))
  imported_data <- list(all_res,global_data$exp_classes,global_data$out_classes,all_res_tib)
  names(imported_data) <- c("all_res","exp_classes","out_classes","all_res_tib")

  return(imported_data)
}

import_data_github <- function(global_data){
  github_links <- c("https://raw.github.com/EdHone/RSE-EPoCH-data/main/RDS/metaphewas_model1a_extracted.RDS",
                    "https://raw.github.com/EdHone/RSE-EPoCH-data/main/RDS/metaphewas_model1b_extracted.RDS"
  )

  x <- lapply(github_links,function(x) getURL(x, .opts=curlOptions(followlocation = TRUE)))
  #print(x)
  #y <- lapply(x, function(y) read.csv(text = x)


  #y <- getURL("https://raw.github.com/EdHone/RSE-EPoCH-data/main/csv/metaphewas_model1a_extracted.csv")
  #z <- read.csv(text = y)
  #copied_files <- lapply(github_links,function(x) getURL(x))
  #print(copied_files)
  #z <- read.csv(text = copied_files)

  #print(copied_files)
  #all_res <- lapply(github_links,function(x) read.csv(getURL(x)))
  # a bit of tidying will happen (stick it all together, make lists of unique exposure and outcome classes, make a tibble of unique combinations, put everything in a list and name the objects)
  #all_res <- bind_rows(all_res)
  #global_data$exp_classes <- unique(all_res$exposure_class)
  #global_data$out_classes <- unique(all_res$outcome_class)
  #all_res_tib <- as_tibble(unique(all_res[,c("exposure_class", "exposure_subclass", "person_exposed", "exposure_time", "exposure_type", "exposure_source", "exposure_dose", "model")]))
  #imported_data <- list(all_res,global_data$exp_classes,global_data$out_classes,all_res_tib)
  #names(imported_data) <- c("all_res","exp_classes","out_classes","all_res_tib")

  #return(imported_data)
}

create_exposure_manhattan_dfs <- function(exposureclass,dat){
  df <- dat[dat$exposure_class==exposureclass&dat$person_exposed!="child",]
  df$exposure_dose_ordered <- factor(df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  df <- df[order(df$exposure_subclass,df$exposure_time,df$exposure_dose_ordered),]
  df$exposure_subclass_time_dose <- paste(df$exposure_subclass,df$exposure_time,df$exposure_dose)
  df$exposure_subclass_time_dose<-factor(df$exposure_subclass_time_dose,ordered=T,levels=unique(df$exposure_subclass_time_dose))
  df
}

## For X axis = exposures
create_exposure_manhattan_dfs <- function(exposureclass,dat){
  df <- dat[dat$exposure_class==exposureclass&dat$person_exposed!="child",]
  df$exposure_dose_ordered <- factor(df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  df <- df[order(df$exposure_subclass,df$exposure_time,df$exposure_dose_ordered),]
  df$exposure_subclass_time_dose <- paste(df$exposure_subclass,df$exposure_time,df$exposure_dose)
  df$exposure_subclass_time_dose<-factor(df$exposure_subclass_time_dose,ordered=T,levels=unique(df$exposure_subclass_time_dose))
  df
}

## For X axis = exposures
filter_exposure_dfs <- function(exposureclass,dat){
  filtered_df <- dat[dat$exposure_class==exposureclass&dat$person_exposed!="child",]
  filtered_df$exposure_dose_ordered <- factor(filtered_df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  filtered_df <- filtered_df[order(filtered_df$exposure_subclass,filtered_df$exposure_time,filtered_df$exposure_dose_ordered),]
  filtered_df$exposure_subclass_time_dose <- paste(filtered_df$exposure_subclass,filtered_df$exposure_time,filtered_df$exposure_dose)
  filtered_df$exposure_subclass_time_dose<-factor(filtered_df$exposure_subclass_time_dose,ordered=T,levels=unique(filtered_df$exposure_subclass_time_dose))
  return(filtered_df)
}

## For X axis = outcomes
create_outcome_manhattan_dfs <- function(outcomeclass,dat){
  df <- dat[dat$outcome_class==outcomeclass&dat$person_exposed!="child",]
  df$outcome_time_ordered <- factor(df$outcome_time,ordered=T,levels=c("pregnancy","delivery","first year", "age 1-2","age 3-4","age 5-7","age 8-11","anytime in childhood"))
  df <- df[order(df$outcome_subclass1,df$outcome_subclass2,df$outcome_time_ordered),]
  df$outcome_subclass_time <- paste(df$outcome_subclass1,df$outcome_subclass2,df$outcome_time)
  df$outcome_subclass_time<-factor(df$outcome_subclass_time,ordered=T,levels=unique(df$outcome_subclass_time))
  df
}

## For X axis = outcomes
filter_outcome_dfs <- function(outcomeclass,dat){
  filtered_df <- dat[dat$outcome_class==outcomeclass&dat$person_exposed!="child",]
  filtered_df$outcome_time_ordered <- factor(filtered_df$outcome_time,ordered=T,levels=c("pregnancy","delivery","first year", "age 1-2","age 3-4","age 5-7","age 8-11","anytime in childhood"))
  filtered_df <- filtered_df[order(filtered_df$outcome_subclass1,filtered_df$outcome_subclass2,filtered_df$outcome_time_ordered),]
  filtered_df$outcome_subclass_time <- paste(filtered_df$outcome_subclass1,filtered_df$outcome_subclass2,filtered_df$outcome_time)
  filtered_df$outcome_subclass_time<-factor(filtered_df$outcome_subclass_time,ordered=T,levels=unique(filtered_df$outcome_subclass_time))
  return(filtered_df)
}


# by exposure
create_exposure_dfs <- function(exposureclass,dat){
  df <- dat[dat$exposure_class==exposureclass&dat$person_exposed!="child",]
  df$exposure_dose_ordered <- factor(df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  df <- df[order(df$exposure_subclass,df$exposure_time,df$exposure_dose_ordered),]
  df$exposure_subclass_time_dose <- paste(df$exposure_subclass,df$exposure_time,df$exposure_dose)
  df$exposure_subclass_time_dose<-factor(df$exposure_subclass_time_dose,ordered=T,levels=unique(df$exposure_subclass_time_dose))
  # to convert ln(OR) to cohen's d (SDM), multiply ln(OR) by sqrt(3)/pi (which is 0.5513)
  # to convert SDM to ln(OR), multiply SDM by pi/sqrt(3) (which is 1.814)
  # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  # https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20001130)19:22%3C3127::AID-SIM784%3E3.0.CO;2-M
  df$est_SDM <- df$est
  df$est_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$est[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$se_SDM <- df$se
  df$se_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$se[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df
}


# by exposure
create_exposure_volcano_dfs <- function(exposureclass,dat){
  df <- dat[dat$exposure_class==exposureclass&dat$person_exposed!="child",]
  df$exposure_dose_ordered <- factor(df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  df <- df[order(df$exposure_subclass,df$exposure_time,df$exposure_dose_ordered),]
  df$exposure_subclass_time_dose <- paste(df$exposure_subclass,df$exposure_time,df$exposure_dose)
  df$exposure_subclass_time_dose<-factor(df$exposure_subclass_time_dose,ordered=T,levels=unique(df$exposure_subclass_time_dose))
  # to convert ln(OR) to cohen's d (SDM), multiply ln(OR) by sqrt(3)/pi (which is 0.5513)
  # to convert SDM to ln(OR), multiply SDM by pi/sqrt(3) (which is 1.814)
  # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  # https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20001130)19:22%3C3127::AID-SIM784%3E3.0.CO;2-M
  df$est_SDM <- df$est
  df$est_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$est[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$se_SDM <- df$se
  df$se_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$se[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df
}

create_outcome_dfs <- function(outcomeclass,dat){
  df <- dat[dat$outcome_class==outcomeclass&dat$person_exposed!="child",]
  df$outcome_time_ordered <- factor(df$outcome_time,ordered=T,levels=c("pregnancy","delivery","first year", "age 1-2","age 3-4","age 5-7","age 8-11","anytime in childhood"))
  df <- df[order(df$outcome_subclass1,df$outcome_subclass2,df$outcome_time_ordered),]
  df$outcome_subclass_time <- paste(df$outcome_subclass1,df$outcome_subclass2,df$outcome_time)
  df$outcome_subclass_time<-factor(df$outcome_subclass_time,ordered=T,levels=unique(df$outcome_subclass_time))
  # to convert ln(OR) to cohen's d (SDM), multiply ln(OR) by sqrt(3)/pi (which is 0.5513)
  # to convert SDM to ln(OR), multiply SDM by pi/sqrt(3) (which is 1.814)
  # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  # https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20001130)19:22%3C3127::AID-SIM784%3E3.0.CO;2-M
  df$est_SDM <- df$est
  df$est_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$est[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$se_SDM <- df$se
  df$se_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$se[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df
}
