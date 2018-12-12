# This script appends and cleans Greyhound betfair data (csv files) downloaded/scraped from the following website
# https://promo.betfair.com/betfairsp/prices/index.php

# Clear Environment first
rm(list=ls())

# Clear console -- CTRL + L 

library(tidyverse)
library(lubridate)
library(stringr)
library(XML)
library(data.table)

tidyverse_update()

# Get csv files directly from Betfair website and append into one data frame/tibble
# Thanks to this post https://stackoverflow.com/questions/15954463/read-list-of-file-names-from-web-into-r 
# Had to change from https to http to make htmlParse() work

betfair_url <- "http://promo.betfair.com/betfairsp/prices"
short_url <- "https://promo.betfair.com"
list_of_files <- htmlParse(betfair_url, encoding = "UTF-8")
file_links <- xpathSApply(list_of_files, "//a/@href")
free(list_of_files)

# Select Win markets, note this script is designed for Australian dog races only, there are lots of races with missing data eg BSP 

win_only <- file_links[grepl("dwbfgreyhoundwin*", file_links)]
files_to_get <- paste(short_url,win_only, sep = "")


### Read 500 most recent csv files directly into memory, this takes a few minutes as there are many files
files_to_get <- files_to_get[1:500]

# Restrcited to 500 as beyond before 2018 many of the files have lots of missing data
all_files <- lapply(seq_along(files_to_get),
                    function(x) read.csv(files_to_get[x], header = TRUE, sep = ",", as.is = TRUE))

# Append together list of data frames into one
greyhound_df <- rbindlist(all_files)

# Sort by date and unique race number
grey_sorted_df <- greyhound_df[order(greyhound_df$EVENT_DT),]

# Delete all rows were BSP are NA or missing prices
grey_sorted_df <- grey_sorted_df[!is.na(grey_sorted_df$BSP),]

# Delete rows that have been entered incorrectly, that appeared once sorted
grey_sorted_df <- grey_sorted_df[!grepl("Trap",grey_sorted_df$WIN_LOSE),]

# Sort so that races are bundled together
grey_sorted_df <- grey_sorted_df[order(grey_sorted_df$EVENT_ID),]

# Delete unwanted columns that are mostly empty or are irrelvant
grey_sorted_df <- subset(grey_sorted_df, select = -c(PPWAP,PPMAX,PPMIN,IPMAX,IPMIN,PPTRADEDVOL,IPTRADEDVOL))

# Format WIN_LOSE column as numeric
grey_sorted_df$WIN_LOSE <- as.numeric(grey_sorted_df$WIN_LOSE)

# Find dead heat races eg =2 and NA and delete these races, 
grey_sorted_df <- grey_sorted_df %>%
  group_by(EVENT_ID) %>%
  filter(!any(WIN_LOSE == 2))

# Strip race no, distance and race_type, will throw an ERROR as for non_oz races race_type is missing, hence NA

grey_sorted_df <- separate(grey_sorted_df,EVENT_NAME,c('Race_No','Distance','Race_type'), ' ' )

# Strip R off Race_No and m from Distance, then change to numeric
grey_sorted_df <- separate(grey_sorted_df,Race_No, c('Other','Race_no'),1)
grey_sorted_df <- select(grey_sorted_df, -'Other')
grey_sorted_df <- separate(grey_sorted_df,Distance, c('Distance','Other'),-1)
grey_sorted_df <- select(grey_sorted_df, -'Other')
grey_sorted_df$Race_no <- as.numeric(grey_sorted_df$Race_no)
grey_sorted_df$Distance <- as.numeric(grey_sorted_df$Distance)
grey_sorted_df$Race_type <- as.factor(grey_sorted_df$Race_type)

# Split Dog number and name

grey_sorted_df <- separate(grey_sorted_df,SELECTION_NAME, c('TAB_no','Dog_name'),'[\\.]')
grey_sorted_df$TAB_no <- as.integer(grey_sorted_df$TAB_no)
grey_sorted_df$Dog_name <- as.character(grey_sorted_df$Dog_name)

# First filter for all Australian races, two dataframes Oz and Other countries 

grey_oz <-  grey_sorted_df[grepl("AUS",grey_sorted_df$MENU_HINT),]

grey_not_oz <- grey_sorted_df[!grepl("AUS",grey_sorted_df$MENU_HINT),]

#############################################################################################
####### The remainder of this script specifically deals with AUSTRALIAN races only ##########
#############################################################################################

# Strip out Track names and join on track name with a table that has full track name plus state (as commission is different)
#Different for Oz and not_oz
grey_oz <- separate(grey_oz, MENU_HINT, c('Other','Track_name'),6)
grey_oz <- separate(grey_oz,Track_name, c('Track_name','Other'),4)
grey_oz$Track_name <- trim(grey_oz$Track_name, recode_factor=FALSE)
# delete the field with unwanted data ie 'Other'
grey_oz <- select(grey_oz, -'Other')
# Change abbreviated name for Angle Park as there are two
grey_oz$Track_name <- gsub("Anpk", "AnPk",grey_oz$Track_name)

# Import EXCEL spreadsheet that has full track names and BETFAIR commission rates since 2015
library(xlsx)
library(readxl)
library(httr)

# File share links on Google Drive are not direct file links hence need to be converted https://sites.google.com/site/gdocs2direct/ 

url <- "https://drive.google.com/uc?export=download&id=1rgpCFxU9PNKYNkXySSK9CJKYp-nl4YyU"
temp <- tempfile( fileext = ".zip")
download.file(url,temp)
# Ignore error 1 in extracting from zip file
out <- unzip(temp, exdir =tempdir())
grey_state_comm <- read.csv(temp, sep = ",")

# Delete unwanted columns/rows
grey_state_comm <- grey_state_comm[1:48,1:5]

# JOIN full track name, State and commission to existing dataset i.e. grey_oz
grey_oz <- merge(grey_state_comm, grey_oz, by.x = "CODE_betfair", by.y= "Track_name")
grey_oz <- select(grey_oz, -'NA.')
grey_oz <- grey_oz[order(grey_oz$EVENT_DT),]

# Replace missing values with commission from preceeding column
grey_oz <- grey_oz %>% mutate(Comm...after.1.7.2018 = coalesce(Commission...2015.30th.June.2018))

# Create TRUE ODDS If races are before 1/7/2018 apply old commission, else apply new rates
grey_oz <- grey_oz %>% 
  mutate( true_odds = if_else(EVENT_DT < "01-07-2018", 
                              (1 - Commission...2015.30th.June.2018) * BSP, 
                              (1 - Comm...after.1.7.2018) * BSP))
# Create Probabilities
grey_oz$Prob_BSP <- (1/grey_oz$true_odds)

# Make sure there are no duplicated races which will lead to errors when summing probabilities

grey_oz <- grey_oz[!duplicated(grey_oz),]

# Sum probabilities for each race to calculate vigorish/OR
grey_oz <- grey_oz %>% 
  group_by(EVENT_ID) %>%
  mutate(Sum_Prob = sum(Prob_BSP))

# Check for races where sum of probabilities is ominous 
min(grey_oz$Sum_Prob)
max(grey_oz$Sum_Prob)

# Obviously there are some issues, so set thresholds LOWER limit 1.02, UPPER limit 1.15 and delete races outside this range
dodgy_races <- subset(grey_oz, Sum_Prob < 1.02|Sum_Prob > 1.15)
grey_oz <- subset(grey_oz, !(Sum_Prob < 1.02|Sum_Prob > 1.15))

# Count the number of runners in each race, max 8 

# List of runners in each race
#Method 1 
runners_table <- grey_oz %>% group_by(EVENT_ID) %>% tally()
#Method 2
runners_table <- grey_oz %>% 
  group_by(EVENT_ID) %>%
  summarise(runners = n())

# Runners in each race assigned to each row in entire dataset
grey_oz <- grey_oz %>% 
  group_by(EVENT_ID) %>%
  mutate(runners = n())


### CREATE derivative variables for specific analysis
# 1) Re-ranked horse number to account for scratching, there is something odd here as max = 11. Need to delete all races where>8 before normalising

grey_oz <- grey_oz %>% 
  group_by(EVENT_ID) %>%
  mutate(TAB_no_ranked=min_rank(TAB_no))

# Normalise the reranked TAB no. to take account of scratching and output a scaled/relative value for position
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

grey_oz <- grey_oz %>%
  group_by(EVENT_ID) %>%
  mutate(TAB_norm = normalize(TAB_no_ranked))


# Create rank for odds, i.e. 1= fav, 8=outsider
grey_oz <- grey_oz %>% 
  group_by(EVENT_ID) %>%
  mutate(odds_rank = min_rank(true_odds))

# Natural log of true odds probabilities
grey_oz$log_prob <- log(grey_oz$Prob_BSP)

# 5) Normalise odds probabilities (true odds ie BSP adjusted for commission)
grey_oz$odds_norm <- (grey_oz$Prob_BSP/grey_oz$Sum_Prob)


write.csv(grey_oz, "Greyhound_betfair_2018.csv", row.names=TRUE)

################ DESCRIPTIVE STATS ##################

# Number of races
length(unique(grey_oz$EVENT_ID))

# Average number of runners
mean(grey_oz$runners)

# Subjective probs 
sub_probs_table <- grey_oz %>%
  group_by(TAB_no)%>%
  dplyr::summarise(Sub_prob = mean(odds_norm))

# Objective probs
# = no. of wins / no. of runs tibble
SR <- grey_oz %>% 
  group_by(TAB_no) %>%
  summarise(obj_prob= sum(WIN_LOSE)/n())

#Table with both objective and subjective probabilities 

Sub_vs_obj_probs <- merge(sub_probs_table, SR, by = "TAB_no")

