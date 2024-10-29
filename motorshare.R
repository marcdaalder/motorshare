#load packages
library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)

#load data

#df_mshare <- read_tsv("MShareSel (1).csv", locale = locale(encoding = "UTF-16"))
df_reg <- read_tsv('Reg.csv', locale = locale(encoding = 'UTF-16'))

df_streamreg <- df_reg %>%
  mutate(Date = paste0(substr(`Month of Registration date`,1,3),'-',substr(`Year of Registration date`,3,4))) %>% #create appropriately formatted date column
  filter(!is.na(`Variable selected`) & `Variable selected` != '') %>% #remove Total engine type count
  mutate(`Variable selected` = ifelse(`Variable selected` %in% c('Hybrid Petrol','Hybrid Diesel'),'Petrol/diesel hybrid',`Variable selected`)) %>% #replacing individuals hybrids with combined cat
  mutate(`Variable selected` = ifelse(`Variable selected` %in% c('PHEV Petrol','PHEV Diesel'),'Plug-in hybrid',`Variable selected`)) %>% #replacing individuals PHEVs with combined cat
  mutate(`Variable selected` = ifelse(`Variable selected` == 'Battery Electric','Battery electric',`Variable selected`)) %>% #Renaming BEVs
  group_by(`Date`,`Variable selected`) %>% #removing superfluous columns
  summarise(`Number of vehicles` = sum(`Number of vehicles`, na.rm=TRUE), .groups = 'drop') %>% #summing by number of vehicles
  filter(`Variable selected` == 'Plug-in hybrid' | `Variable selected` == 'Petrol' | `Variable selected` == 'Diesel'| `Variable selected` == 'Battery electric'| `Variable selected` == 'Petrol/diesel hybrid' ) %>% #remove excess vehicle types
  pivot_wider(names_from=`Variable selected`,values_from = `Number of vehicles`) %>%
  mutate(Date = dmy(paste0('01-', Date))) %>% #reformat date column
  arrange(Date) %>% #order by date
  relocate(Date,`Plug-in hybrid`,`Battery electric`,`Petrol/diesel hybrid`,Diesel,Petrol) #relocate columns
  
df_streamreg$Date <- as.Date(df_streamreg$Date, format="$b-%y")

write_csv(df_streamreg,'/Users/marcdaalder/RStudio/motorshare/streamgraph.csv', append=FALSE) #write csv

current_year <- max(df_reg$`Year of Registration date`)
last_year <- current_year-1

df_changereg <- df_reg %>%
  filter(!is.na(`Variable selected`) & `Variable selected` != '') %>% #remove Total engine type count
  mutate(`Variable selected` = ifelse(`Variable selected` %in% c('Hybrid Petrol','Hybrid Diesel'),'Petrol/diesel hybrid',`Variable selected`)) %>% #replacing individuals hybrids with combined cat
  mutate(`Variable selected` = ifelse(`Variable selected` %in% c('PHEV Petrol','PHEV Diesel'),'Plug-in hybrid',`Variable selected`)) %>% #replacing individuals PHEVs with combined cat
  mutate(`Variable selected` = ifelse(`Variable selected` == 'Battery Electric','Battery electric',`Variable selected`)) %>% #Renaming BEVs
  filter(`Variable selected` == 'Plug-in hybrid' | `Variable selected` == 'Petrol' | `Variable selected` == 'Diesel'| `Variable selected` == 'Battery electric'| `Variable selected` == 'Petrol/diesel hybrid' ) %>% #remove excess vehicle types
  filter(`Year of Registration date` == current_year | `Year of Registration date` == last_year) %>% #remove all but last two years
  group_by(`Year of Registration date`,`Month of Registration date`,`Variable selected`) %>% #reduce excess entries
  summarise(`Number of vehicles` = sum(`Number of vehicles`, na.rm=TRUE)) %>% #summing by number of vehicles
  pivot_wider(names_from = `Year of Registration date`, values_from=`Number of vehicles`) %>% #break up year column into two columns
  mutate(Change = ((`2024`/`2023`)-1)) %>% #create new change column with value
  mutate(Change = percent(Change,accuracy=1)) %>% #make percentage
  mutate(Date = paste0(substr(`Month of Registration date`,1,3),'-',substr(current_year,3,4))) %>% #create appropriately formatted date column
  group_by(`Date`, `Variable selected`) %>% #group??
  mutate(`Month of Registration date` = NULL) %>% #remove superfluous column
  mutate(`2023` = NULL) %>% #ibid
  mutate(`2024` = NULL) %>% #ibid
  pivot_wider(names_from=`Variable selected`,values_from = `Change`) %>% #un-tidy the data
  mutate(Date = dmy(paste0('01-', Date))) %>% #reformat date column
  arrange(Date) %>% #order by date
  relocate(Date,`Plug-in hybrid`,`Battery electric`,`Petrol/diesel hybrid`,Diesel,Petrol) %>% #relocate columns
  filter(!is.na(Petrol) & Petrol != '') #remove future months

write_csv(df_changereg,'/Users/marcdaalder/RStudio/motorshare/changereg.csv', append=FALSE) #write csv

df_mshare <- df_reg %>%
  mutate(Date = paste0(substr(`Month of Registration date`,1,3),'-',substr(`Year of Registration date`,3,4))) %>% #create appropriately formatted date column
  mutate(`Variable selected` = ifelse(`Variable selected` %in% c('Hybrid Petrol','Hybrid Diesel'),'Petrol/diesel hybrid',`Variable selected`)) %>% #replacing individuals hybrids with combined cat
  mutate(`Variable selected` = ifelse(`Variable selected` %in% c('PHEV Petrol','PHEV Diesel'),'Plug-in hybrid',`Variable selected`)) %>% #replacing individuals PHEVs with combined cat
  mutate(`Variable selected` = ifelse(`Variable selected` == 'Battery Electric','Battery electric',`Variable selected`)) %>% #Renaming BEVs
  group_by(`Date`,`Variable selected`) %>% #removing superfluous columns
  summarise(`Number of vehicles` = sum(`Number of vehicles`, na.rm=TRUE), .groups = 'drop') %>% #summing by number of vehicles
  pivot_wider(names_from=`Variable selected`, values_from = `Number of vehicles`) %>% #pivot table
  rename('Total' = `NA`) %>% #rename Total column
  mutate(`Battery electric` = percent(`Battery electric`/Total, accuracy=.1),
         `Diesel` = percent(`Diesel`/Total, accuracy=.1),
         `Petrol` = percent(`Petrol`/Total, accuracy=.1),
         `Petrol/diesel hybrid` = percent(`Petrol/diesel hybrid`/Total, accuracy=.1),
         `Plug-in hybrid` = percent(`Plug-in hybrid`/Total, accuracy=.1)
         ) %>% #apply calculation
  mutate(`Fuel-cell (H2)` = NULL,
         `LPG/Other` = NULL,
         Total = NULL,
         `Fuel-cell (other)` = NULL) %>% #remove excess columns
  mutate(Date = dmy(paste0('01-', Date))) %>% #reformat date column
  arrange(Date) %>% #order by date
  relocate(Date,`Plug-in hybrid`,`Battery electric`,`Petrol/diesel hybrid`,Diesel,Petrol) #relocate columns

write_csv(df_mshare,'/Users/marcdaalder/RStudio/motorshare/mshare.csv', append=FALSE) #write csv

is.date <- function(x) inherits (x, 'Date')

sapply(list(as.Date('2000-01-01'),123,'ABC',df_streamreg$Date),is.date)



  

