all_files <- list.files("bko2_CSVs")
first_file <- TRUE

#Adds ID code onto each trial, binds all individual csvs into one large dataframe
for(i in 1:length(all_files)){
  if(grepl("b",all_files[i])){
    print(all_files[i])
    if(first_file){
      all_data_df <- read.csv(paste0("bko2_CSVs/",all_files[i]))
      all_data_df <- all_data_df %>% mutate(ID = substr(all_files[i],1,6))
      first_file <- FALSE
    }
    else{
      new_data_df <- read.csv(paste0("bko2_CSVs/",all_files[i]))
      new_data_df <- new_data_df %>% mutate(ID = substr(all_files[i],1,6))
      all_data_df <- rbind(all_data_df,new_data_df)
    }
  }
}

#filter out the NAs
all_data_df <- all_data_df %>% filter(!is.na(correct))

#CALCULATE CHOICE BIAS
#group_by(ID,sound,group) group for when i have both adults and children
data_2_sounds <- all_data_df %>% group_by(ID,sound) %>%
  summarise(bias = sum(correct)/n())

data_all_sounds <- all_data_df %>% group_by(ID,sound,stimulus) %>%
  summarise(bias = sum(correct)/n())

data_vowel_type <- all_data_df %>% group_by(vowel_type, ID) %>%
  summarise(bias = sum(correct)/n())

data_congruency = all_data_df %>% group_by(congruent, ID) %>%
  summarise(bias = sum(correct)/n())

data_voicing = all_data_df %>% group_by(voicing, ID) %>%
  summarise(bias = sum(correct)/n())

#PIVOT TIME
pivot_data_2_sounds <- data_2_sounds %>% pivot_wider(names_from = sound, values_from = bias, values_fill = NA)

pivot_data_all_sounds <- data_all_sounds %>% ungroup() %>% dplyr::select(-c(sound)) %>%
  pivot_wider(names_from = stimulus, values_from = bias, values_fill = NA) 

full_bias_df <- full_join(pivot_data_2_sounds, pivot_data_all_sounds, by="ID")
