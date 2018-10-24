library(dplyr)
library(ggplot2)

setwd("C:\\Users\\brian\\OneDrive\\Documents\\GitHub\\nflscrapR-data")

game_rushing <- read.csv("legacy_data\\game_team_stats\\game_rushing_df.csv", header=T)
summary(game_rushing)
head(game_rushing)


#load play by play data

filenames <- list.files("play_by_play_data\\regular_season", pattern="*.csv", full.names=TRUE)
rm(pbp)
for (file in filenames){
  season <- paste("20",regmatches(file,regexec("20(.*?)\\.csv",file))[[1]][2],sep="")
  # if the merged dataset doesn't exist, create it
  if (!exists("pbp")){
        pbp <- cbind(season,read.csv(file, header=TRUE))
    print( season)
  }
  
  # if the merged dataset does exist, append to it
  else {
    temp_dataset <-cbind(season,read.csv(file, header=TRUE))
    pbp<-rbind(pbp, temp_dataset)
    rm(temp_dataset)
    print( season)
  }
}

# test # of rows per xls
#2009 44596 rows
pbp %>% group_by(season) %>% summarize(n=n())
##  What is the bills play selection on three and outs

bills_3_out <- pbp %>% filter(posteam == "BUF")
#look at a sample drive
View(bills_3_out[bills_3_out$game_id == 2009091400 & bills_3_out$drive==2,]) #3& out w/ penalty
View(bills_3_out[bills_3_out$game_id == 2009091400 & bills_3_out$drive==4,]) #TD
View(bills_3_out[bills_3_out$game_id == 2009091400 & bills_3_out$drive==6,]) # 6 plays 1 first down and pump

# select drives where there was a punt
bills_punts <- bills_3_out %>% filter(play_type == "punt") %>% select(game_id,drive)

#get all data points for drives with a punt
# filter out penalties

bills_punt_drives <- bills_3_out %>% inner_join(bills_punts, by = c("game_id","drive")) %>%
  filter(play_type != "no_play")

# how many drives lasted 4,5,6,... plays.

num_drives_by_num_plays <- bills_punt_drives %>% group_by(game_id,drive) %>%
  summarize(num_plays = n()) %>%
  group_by(num_plays) %>%
  summarize(num_drives = n()) %>%
  mutate(freq = round(num_drives/ sum(num_drives)*100,1))
num_drives_by_num_plays

# plot the # of drives by drive length
 ggplot(num_drives_by_num_plays, aes(x=num_plays,y=freq))   + 
   geom_bar(stat="identity") +
   geom_text(aes(label=freq), vjust=1, col="white") 
 
# why is one drive only 3 plays
bills_punt_drives %>% group_by(game_id,drive) %>%
  summarize(num_plays = n()) %>%
  filter(num_plays == 3)

# the following drive is missing 3rd down
View(bills_punt_drives %>% filter(game_id == 2015101804,drive==7))

# add in season as a group by
num_drives_by_num_plays_season <- bills_punt_drives %>% group_by(season,game_id,drive) %>%
  summarize(num_plays = n()) %>%
  group_by(season, num_plays) %>%
  summarize(num_drives = n()) %>%
  mutate(freq = round(num_drives/ sum(num_drives)*100,1))
num_drives_by_num_plays_season

ggplot(num_drives_by_num_plays_season, aes(num_plays,freq))   + 
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), vjust=1, col="white")  +
  facet_grid(rows=vars(season))

#Plot 3,4 and 5 and outs across time
bills_3_4_5_out <- num_drives_by_num_plays_season %>% filter(num_plays >= 3 , num_plays <= 6)
ggplot(bills_3_4_5_out,aes(x=season,y=freq,group=num_plays)) +
  geom_line()+
  aes(color=factor(num_plays))+
  scale_color_brewer(palette="Dark2")

# what are the averages for all of the NFL

# select drives where there was a punt
nfl_punts <- pbp %>% filter(play_type == "punt") %>% select(game_id,drive)

#get all data points for drives with a punt
# filter out penalties

nfl_punt_drives <- pbp %>% inner_join(nfl_punts, by = c("game_id","drive")) %>%
  filter(play_type != "no_play")

# how many drives lasted 4,5,6,... plays.

nfl_num_drives_by_num_plays <- nfl_punt_drives %>% group_by(game_id,drive) %>%
  summarize(num_plays = n()) %>%
  group_by(num_plays) %>%
  summarize(num_drives = n()) %>%
  mutate(freq = round(num_drives/ sum(num_drives)*100,1))
nfl_num_drives_by_num_plays

# plot the # of drives by drive length
ggplot(nfl_num_drives_by_num_plays, aes(x=num_plays,y=freq))   + 
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), vjust=1, col="white") 


# add in season as a group by
nfl_num_drives_by_num_plays_season <- nfl_punt_drives %>% group_by(season,game_id,drive) %>%
  summarize(num_plays = n()) %>%
  group_by(season, num_plays) %>%
  summarize(num_drives = n()) %>%
  mutate(freq = round(num_drives/ sum(num_drives)*100,1))
nfl_num_drives_by_num_plays_season

ggplot(nfl_num_drives_by_num_plays_season, aes(num_plays,freq))   + 
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), vjust=1, col="white")  +
  facet_grid(rows=vars(season))


##