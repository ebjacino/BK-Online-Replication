library(dplyr)
#import all data with the import dataset button
#df <- read.csv("CSV files/bko001.csv")

data <- bind_rows(bko001, bko002, bko004, bko005, bko006, bko007, bko009, bko010, bko011, bko012, bko013, 
                 bko014, bko015, bko016, bko018, bko019, bko020, bko021, bko022, bko024, bko025, bko026,
                 bko027, bko028)

all_files <- list.files("CSV files")
first_file <- TRUE

for(i in 1:length(all_files)){
  if(first_file){
    all_data_df <- read.csv(paste0("CSV files/",all_files[i]))
    all_data_df <- all_data_df %>% mutate(ID = substr(all_files[i],1,6))
    first_file <- FALSE
  }
  else{
    new_data_df <- read.csv(paste0("CSV files/",all_files[i]))
    new_data_df <- new_data_df %>% mutate(ID = substr(all_files[i],1,6))
    all_data_df <- rbind(all_data_df,new_data_df)
  }
}

all_data_df <- all_data_df %>% filter(!is.na(correct))

#CALCULATE CHOICE BIAS
data_2_sounds <- all_data_df %>% group_by(ID,sound) %>%
  summarise(bias = sum(correct)/n())

data_4_sounds <- all_data_df %>% group_by(ID,sound,stimulus) %>%
  summarise(bias = sum(correct)/n())

library(ggplot2)

ggplot(data_2_sounds, aes(x = sound, y = bias, fill = sound)) +
  geom_boxplot() +
  theme_light()

ggplot(data_4_sounds, aes(x = stimulus, y = bias, fill = sound)) +
  geom_boxplot() +
  theme_light()
