library(dplyr)
library(ggplot2)
#import all data with the import dataset button


all_files <- list.files("LG CSV files Looped")
first_file <- TRUE

#Adds ID code onto each trial, binds all individual csvs into one large dataframe
for(i in 1:length(all_files)){
  if(grepl("bk",all_files[i])){
    print(all_files[i])
    if(first_file){
      all_data_df <- read.csv(paste0("LG CSV files Looped/",all_files[i]))
      all_data_df <- all_data_df %>% mutate(ID = substr(all_files[i],1,6))
      first_file <- FALSE
    }
    else{
      new_data_df <- read.csv(paste0("LG CSV files Looped/",all_files[i]))
      new_data_df <- new_data_df %>% mutate(ID = substr(all_files[i],1,6))
      all_data_df <- rbind(all_data_df,new_data_df)
    }
  }
}

#filter out the NAs
all_data_df <- all_data_df %>% filter(!is.na(correct))

all_data_df <- all_data_df %>% mutate(group = ifelse(substr(ID,4,4)=="1","adult","child"))
unique(all_data_df$group)



#CALCULATE CHOICE BIAS
data_2_sounds <- all_data_df %>% group_by(ID,sound,group) %>%
  summarise(bias = sum(correct)/n())

#calculating means and SE to plot means graph 
data_means <- data_2_sounds %>% 
  group_by(sound, group) %>% # Group the data by sound
  summarize(mean_bias=mean(bias), 
            sd_bias=sd(bias), # Create variable with sd of cty per group
            N_bias=n(), # Create new variable N of cty per group
            se=sd_bias/sqrt(N_bias), 
            upper_limit=mean_bias+se, # Upper limit
            lower_limit=mean_bias-se) # Lower limit) # Create variable with se of cty per group) # Create new variable which is the mean of bias

ggplot(data_means, aes(x=sound, y=mean_bias)) + 
  geom_bar(stat="identity", width=0.5) + 
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit)) +
  facet_wrap(~ group) +
  #geom_point(data=mtcars, aes(y=disp, x=cyl, fill=cyl)) +
  theme_light()+
  ylim(0,1)

data_4_sounds <- all_data_df %>% group_by(ID,sound,stimulus,group) %>%
  summarise(bias = sum(correct)/n())

#PLOT IT box plots
library(ggplot2)

#bias graphs for 2 sounds (round or spiky)
ggplot(data_2_sounds, aes(x = sound, y = bias, fill = sound)) +
  geom_boxplot() +
  facet_wrap(~ group) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylim(0,1) +
  theme_light()

#bias for 4 sounds (baba, gaga, kiki, tete)
ggplot(data_4_sounds, aes(x = stimulus, y = bias, fill = sound)) +
  geom_boxplot() +
  facet_wrap(~ group) +
  ylim(0,1) +
  theme_light()
