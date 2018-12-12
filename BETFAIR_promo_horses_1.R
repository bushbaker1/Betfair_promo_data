# This script is designed to clean up betfair data extracted from the following source
# https://promo.betfair.com/betfairsp/prices/index.php

#Clear Environment first
rm(list=ls())

# Clear console -- CTRL + L 

# Load libraries, tidyverse contains dyplr, stringr, ggplot2, tidyr, etc
# tibble replaces data frames with tibbles
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(stringr)
library(dplyr, warn.conflicts = FALSE)
library(XML)

tidyverse_update()

# Get csv files directly from Betfair website and append into one data frame/tibble
## Thanks to this post https://stackoverflow.com/questions/15954463/read-list-of-file-names-from-web-into-r 
# Had to change from https to http to make htmlParse() work

betfair_url <- "http://promo.betfair.com/betfairsp/prices"
short_url <- "https://promo.betfair.com"
list_of_files <- htmlParse(betfair_url, encoding = "UTF-8")
file_links <- xpathSApply(list_of_files, "//a/@href")
free(list_of_files)

# Select Win markets in Australia only, note this script is designed for Australian horse races only

oz_only <- file_links[grepl("dwbfpricesauswin*", file_links)]
files_to_get <- paste(short_url,oz_only, sep = "")

### Read csv files directly into memory, this takes a few minutes as there are 2000+ files
all_files <- lapply(seq_along(files_to_get),
                 function(x) read.csv(files_to_get[x], header = TRUE, sep = ",", as.is = TRUE))

#Append together list of data frames into one
betfair_data <- bind_rows(lapply(all_files, data.frame))

# Change to tibble eg join vectors tibble(variable1, variable2, etc)
betfair_data <- as_tibble(betfair_data)

#Remove duplicate records that may have accidentally been imported, takes some time for large dataset
betfair_data <- betfair_data[!duplicated(betfair_data),]

#Change EVENT_DT from character to date using lubridate (package) function
betfair_data$EVENT_DT <- dmy_hm(betfair_data$EVENT_DT)

#Sort by EVENT_DT i.e. date of race
betfair_data <- betfair_data[order(betfair_data$EVENT_DT),]

#All fields are loaded as characters, change formatting
#Check fomratting of all fields
glimpse(betfair_data)

#Remove duplicate headers 
betfair_data <- betfair_data %>% filter(!WIN_LOSE %in% c("WIN_LOSE"))

#Remove PLACE markets, just in case files are incorrect
betfair_data <- betfair_data %>% filter(!EVENT_NAME %in% c("TO BE PLACED"))

#View the dataset
View(betfair_data)

#Change variabbles from characters to numeric that don't require splitting 
betfair_data$EVENT_ID <- as.numeric(betfair_data$EVENT_ID)
betfair_data$SELECTION_ID <- as.numeric(betfair_data$SELECTION_ID)
betfair_data$WIN_LOSE <- as.numeric(betfair_data$WIN_LOSE)
betfair_data$BSP <- as.numeric(as.character(betfair_data$BSP))
betfair_data$PPWAP <- as.numeric(betfair_data$PPWAP)
betfair_data$MORNINGWAP <- as.numeric(betfair_data$MORNINGWAP)
betfair_data$PPMAX <- as.numeric(betfair_data$PPMAX)
betfair_data$PPMIN <- as.numeric(betfair_data$PPMIN)
betfair_data$IPMAX <- as.numeric(betfair_data$IPMAX)
betfair_data$IPMIN <- as.numeric(betfair_data$IPMIN)
betfair_data$MORNINGTRADEDVOL <- as.numeric(betfair_data$MORNINGTRADEDVOL)
betfair_data$PPTRADEDVOL <- as.numeric(betfair_data$PPTRADEDVOL)
betfair_data$IPTRADEDVOL <- as.numeric(betfair_data$IPTRADEDVOL)


# Split or separate the EVENT_NAME field into 4 columns 

###ERRORS#### are normal as for thoroughbreds there only 3 columns eg race_no, distance, race_type
betfair_data <- separate(betfair_data,EVENT_NAME,c('Race_No','Distance','Race_type','Other'), ' ' )

# Delete fourth column/field containing M and S, i.e. 'Other'.... try rm(colname)
betfair_data <- select(betfair_data, -'Other')

# Strip R off Race_No and m from Distance, then change to numeric
betfair_data <- separate(betfair_data,Race_No, c('Other','Race_no'),1)
betfair_data <- select(betfair_data, -'Other')
betfair_data <- separate(betfair_data,Distance, c('Distance','Other'),-1)
betfair_data <- select(betfair_data, -'Other')
betfair_data$Race_no <- as.numeric(betfair_data$Race_no)
betfair_data$Distance <- as.numeric(betfair_data$Distance)

# Remove/delete NA races in Race_no and Distance and BSP columns
# Not sure if NA are ignored when running clogit etc 
betfair_data <- dplyr::filter(betfair_data,  !is.na(Race_no))
betfair_data <- dplyr::filter(betfair_data,  !is.na(Distance))
betfair_data <- dplyr::filter(betfair_data,  !is.na(BSP))


# Separate out abbreviated track name from the MENU_HINT, rename field Track_name
betfair_data <- separate(betfair_data,MENU_HINT, c('Other','Track_name'),6)
betfair_data <- select(betfair_data, -'Other')
betfair_data <- separate(betfair_data,Track_name, c('Track_name','Other'),4)
betfair_data <- select(betfair_data, -'Other')

# Split horse/cloth/TAB no. from horse name and divide SELECTION_NAME into two columns TAB_no & Horse_name
betfair_data <- separate(betfair_data,SELECTION_NAME, c('TAB_no','Horse_name'),'[\\.]')
betfair_data$TAB_no <- as.integer(betfair_data$TAB_no)
betfair_data$Horse_name <- as.character(betfair_data$Horse_name)

# Delete duplicates again just in case
betfair_data <- betfair_data[!duplicated(betfair_data[,c('TAB_no','Horse_name','EVENT_DT')]),]

# Delete races with dead-heats i.e. where WIN_LOSE = 2 

betfair_data <- betfair_data %>% 
  group_by(EVENT_ID) %>%
  filter(!any(WIN_LOSE > 1))

# Clean up Race types, duplicates and input errors, etc

betfair_data$Race_type <- gsub("Trot", "TROT",betfair_data$Race_type)
betfair_data$Race_type <- gsub("trot", "TROT",betfair_data$Race_type)
betfair_data$Race_type <- gsub("PAce", "PACE",betfair_data$Race_type)
betfair_data$Race_type <- gsub("Pace", "PACE",betfair_data$Race_type)

betfair_data$Race_type <- gsub("3YO", "3yo",betfair_data$Race_type)
betfair_data$Race_type <- gsub("Hcap", "Hcp",betfair_data$Race_type)
betfair_data$Race_type <- gsub("HCap", "Hcp",betfair_data$Race_type)
betfair_data$Race_type <- gsub("hcap", "Hcp",betfair_data$Race_type)
betfair_data$Race_type <- gsub("Cl1", "CL1",betfair_data$Race_type)
betfair_data$Race_type <- gsub("Cl2", "CL2",betfair_data$Race_type)
betfair_data$Race_type <- gsub("Cl3", "CL3",betfair_data$Race_type)
betfair_data$Race_type <- gsub("LIsted", "Listed",betfair_data$Race_type)

### CREATE derivative fields/columns 
###Some examples included below

# Re-ranked horse number to account for scratching

betfair_data <- betfair_data %>% 
  group_by(EVENT_ID) %>%
  mutate(TAB_no_ranked=min_rank(TAB_no))

# Normalise the TAB no. to take account of scratching and output a scaled/relative value

normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

betfair_data <- betfair_data %>%
  group_by(EVENT_ID) %>%
  mutate(TAB_normalized = normalize(TAB_no_ranked))

# Delete abnormalities i.e. delete races where normalized value >1
betfair_data <- betfair_data %>% 
  group_by(EVENT_ID) %>%
  filter(!any(TAB_normalized > 1))
  
#### PROXY METHOD only ########
# Commission rates vary, see below in thoroughbred section for link to rates between 2015 and early Dec 2018
# Probabilities based on BSP, take account of changing commission rates,

betfair_data$BSP_prob_0.07 <- (1/(betfair_data$BSP*0.93))

# Delete dodgy values eg >0.97, less than $1.05
betfair_data <- betfair_data %>% 
  group_by(EVENT_ID) %>%
  filter(!any(BSP_prob_0.07 > 0.97))

# Sum the probabilities conditional to EVENT_ID - Over-round/Vigorish
betfair_data <- betfair_data %>% 
  group_by(EVENT_ID) %>%
  mutate(BSP_Overound=sum(BSP_prob_0.07))

# Delete suspect races, that is OR less than 1.02 and above 1.20, choose what you think is appropriate
betfair_data <- subset(betfair_data, !(BSP_Overound < 1.02|BSP_Overound  > 1.20))

# Normalise odds probabilities (raw BSP odds), this is a proxy method also and different to normalisation method applied above
# Pr. Stephen Clarke and co. working on academic paper using power law adjustment which is more accurate
betfair_data$BSP_normalised <- (betfair_data$BSP_prob_0.07/betfair_data$BSP_Overound)

# Number of runners
betfair_data <- betfair_data %>% 
  group_by(EVENT_ID) %>%
  mutate(Runners=n())

# Easy way create a variable rank for odds, then run ifese to find match
betfair_data <- betfair_data %>% 
  group_by(EVENT_ID) %>%
  mutate(odds_rank=min_rank(BSP))

#Create natural log of odds probabilities, this is more predictive
betfair_data$logodds <- log(betfair_data$BSP_prob_0.07)

#Dummy for favourite i.e. market leader
betfair_data <- betfair_data %>% 
  mutate(fav = ifelse(odds_rank == 1, 1, 0))

#Create a backup of the original imported dataset before cleaning, just in case
betfair_data_backup <- betfair_data

#####################################################
########Split into HARNESS and THOROUGHBREDS###########
#####################################################

# Create tots and thoroughbred races only tibble
betfair_trots <- filter(betfair_data, Race_type=='TROT'| Race_type=='PACE')
betfair_trots <- as.tibble(betfair_trots)

betfair_thoro <- filter(betfair_data, !(Race_type == 'TROT'| Race_type == 'PACE'))
betfair_thoro <- as.tibble(betfair_thoro)

# Create a vector of track codes to index/match in MS Excel with a list of full track names from RISA, etc
# This is useful if you want to join (merge) data from other sources to enhance your model
thoro_track_codes <- unique(betfair_thoro$Track_name)
write.csv(thoro_track_codes, file= 'Betfair_thoro_track_codes.csv', row.names=FALSE)

###########################
##### THOROUGHBREDS #######
###########################

# To accurately account for commission for races between 2015 and Dec. 2018 download this file below, also has full track name, state and meeting type
# Note: code below does not incorporate this detail, you need to modify script to join data, etc 
# File share links on Google Drive are not direct file links hence need to be converted https://sites.google.com/site/gdocs2direct/ 

url <- "https://drive.google.com/uc?export=download&id=1T5dPA8ALGgx12XsrpsKaNgGFvhgEDEch"
temp <- tempfile( fileext = ".zip")
download.file(url,temp)
# Ignore error 1 in extracting from zip file
out <- unzip(temp, exdir =tempdir())
horse_state_comm <- read.csv(temp, sep = ",")

View(horse_state_comm)

# Subjective probabilities for each TAB no, note this uses normalised odds so commission is accounted for
sub_probs_table <- betfair_thoro %>%
  group_by(TAB_no)%>%
  dplyr::summarise(Sub_prob = mean(BSP_normalised))

# Objective probilities, otherwise referred to as win strike rate, etc
# = no. of wins / no. of runs tibble
SR <- betfair_thoro %>% 
  group_by(TAB_no) %>%
  summarise(obj_prob= sum(WIN_LOSE)/n())

#Table with both objective and subjective probabilities

Sub_vs_obj_probs <- merge(sub_probs_table, SR, by = "TAB_no")

# See how efficient odds are according to TAB no. order, shows just how accurate odds are overall. 
View(Sub_vs_obj_probs)

# Create a subset of data to test different types of races
# Examples

# 2YO races only
betfair_thoro_2YOs <- betfair_thoro %>% 
  group_by(EVENT_ID) %>%
  filter(!any(Race_type != "2yo"))

write.csv(betfair_thoro_2YOs, file= 'Betfair_thoro_2yos.csv', row.names=FALSE)

# Handicap races only

betfair_thoro_handicaps <- betfair_thoro %>% 
  group_by(EVENT_ID) %>%
  filter(any(Race_type == "Hcp"))

write.csv(betfair_thoro_handicaps, file= 'Betfair_thoro_hcps.csv', row.names=FALSE)

# Non-handicap races

betfair_thoro_non_handicaps <- betfair_thoro %>% 
  group_by(EVENT_ID) %>%
  filter(!any(Race_type == "Hcp"))

write.csv(betfair_thoro_non_handicaps, file= 'Betfair_thoro_non_hcps.csv', row.names=FALSE)

#### TROTS ######
#################
# Subjective probs - Method 2 - dplyr
sub_probs_table_trots <- betfair_trots %>%
  group_by(TAB_no)%>%
  dplyr::summarise(Sub_prob = mean(BSP_normalised))

# Objective probs
# = no. of wins / no. of runs tibble
SR_trots <- betfair_trots %>% 
  group_by(TAB_no) %>%
  summarise(obj_prob= sum(WIN_LOSE)/n())

#Table with both objective and subjective probabilities 

Sub_vs_obj_probs_trots <- merge(sub_probs_table_trots, SR_trots, by = "TAB_no")

###################
###################

# Save all records as csv
write.csv(betfair_thoro, file= 'Betfair_thoro_promo_data_2012_18.csv', row.names=FALSE)
write.csv(betfair_trots, file= 'Betfair_trots_promo_data.csv_2012_18.csv', row.names=FALSE)

## Create a benchmark model ##

#################################################
#### Run Conditional logit/logistic analysis#####

library(mclogit)
library(survival)
library(Epi)


##########################
#####THOROUGHBREDS########
##########################

# Split data into training 70% and test 30% sets
set.seed(101)
sample <- sample.int(n = nrow(betfair_thoro), size = floor(.70*nrow(betfair_thoro)), replace = F)
train_data_thoro <- betfair_thoro[sample, ]
test_data_thoro <- betfair_thoro[-sample, ]


# Save as csv files for analysis in STATA, etc to verify results
write.csv(train_data_thoro, "Training_betfair_thoro_2012_2018.csv", row.names=TRUE)
write.csv(test_data, "Test_betfair_thoro_2012_2018.csv", row.names=TRUE)

# Random effects included - mclogit
mclogit(data =train_data, cbind(WIN_LOSE,EVENT_ID)~log_prob, random =~1|runners)

# Fixed effects
Training_resuts_1 <- clogistic(WIN_LOSE ~ logodds, strata = EVENT_ID, data = train_data_thoro)



       

