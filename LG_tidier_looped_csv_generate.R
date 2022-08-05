library(dplyr)
library(stringr)
library(tidyverse)

correct_stim_list = c("sound/baba_rep.wav" = "bouba",
                      "sound/gaga_rep.wav" = "bouba",
                      "sound/keekee.wav" = "kiki",
                      "sound/teetee.wav" = "kiki")

left_key = "z"
right_key = "m"

which_is_correct <- function(stimuli,lefts,rights) {
  
  #Make a new vector the same length as the number of rows
  correct_vec <- rep(NA,length(stimuli))
  
  #For each row in the dataset....
  for(i in seq(1:length(stimuli))){
    
    #Get the correct type ("Bouba" or "Kiki") by checking in the stim list
    correct_type <- correct_stim_list[stimuli[i]]
    
    #If the correct type is in the left one the correct key was the left key
    if(grepl(correct_type, lefts[i], fixed = TRUE)){
      correct_key <- left_key
    }
    #If the correct type is in the right one the correct key was the right key
    else if(grepl(correct_type, rights[i], fixed = TRUE)){
      correct_key <- right_key
    }
    #If it isn't in either something has gone wrong so you'll get an NA
    else{
      correct_key <- NA
    }
    
    #Put that correct key in the output vector
    correct_vec[i] <- correct_key
  }
  
  #Return the final filled in vector
  return(correct_vec)
}

#look at the results files
rds_files <- list.files("LG_double_image_RDS",
                        pattern = "\\.rds$",
                        full.names = TRUE)

rds_files
#change [#] to pick which one to read

for(file in rds_files){
  print(file)
  file_id <- str_extract(file,"b[kl]g\\d\\d\\d")
  
  data <- readRDS(file)
  
  #assign ID
  participant_ID <- data$results$`sp_id`[[1]]
  main_data <- data$results$`Main page`[[1]]
  
  
  df <- read.table(text = main_data, sep =",", header = TRUE, stringsAsFactors = FALSE)
  #df is the raw data, tells you stimulus and responses
  
  
  ##Ben Code Attempt***creating tidy data and choice bias score***
  
  #First remove unneeded columns
  data_cols <- df %>% select(c(trial_type,stimulus,response,left_image,right_image,correct_response,correct)) 
  
  #Then Filter practice and experimental trials
  data_prac <- data_cols %>% filter(grepl("Prac", stimulus, fixed = TRUE))
  data_exp <- data_cols %>% filter(!grepl("Prac", stimulus, fixed = TRUE)) %>%
    filter(trial_type  %in% c("bouba-kiki","image-keyboard-response")) %>%
    filter(stimulus != "img/fixation.png")
  
  #filter out BK data 
  data_exp_bk <-data_exp %>% filter(trial_type == "bouba-kiki")
  #filter out the LG data
  data_exp_lg <-data_exp %>% filter(trial_type =="image-keyboard-response") %>%
    select(c(trial_type, stimulus, response))
  
  #Now write a function to find if the left or the right image is "correct"
  
  data_exp_bk <- data_exp_bk %>% mutate(correct_response = which_is_correct(stimulus,left_image,right_image))
  data_exp_bk <- data_exp_bk %>% mutate(correct = ifelse(correct_response == response,
                                                         1,
                                                         ifelse(response == left_key | response == right_key,
                                                                0,
                                                                NA)))
  
  data_exp_bk <- data_exp_bk %>% mutate(sound = ifelse(stimulus %in% c("sound/baba_rep.wav","sound/gaga_rep.wav"),
                                                       "round",
                                                       ifelse(stimulus %in% c("sound/teetee.wav","sound/keekee.wav"),
                                                              "spiky",
                                                              NA))) %>%
    filter(!is.na(correct))
  
#adding columns to LG files to do analysis 
  data_exp_lg <- data_exp_lg %>% separate(stimulus, c("type","number"), sep = "/") 
  
  data_exp_lg <- data_exp_lg %>% group_by(type) %>% 
                                mutate(trial_num = row_number()) %>% 
                                ungroup() %>%
                                pivot_wider(names_from = type, values_from = c(number,response)) %>%
                                select(!response_targets)

  
  
  
  
  file_path_bk <- paste0("LG CSV files Looped/",file_id,"_bk.csv")
  file_path_lg <- paste0("LG tidy csv/",file_id,"_lg.csv")
  
  file_path_prac <- paste0("LG practice trials/",file_id,".csv")
  #write.csv(data_prac, file_path_prac)
  
  #write.csv(data_exp_bk, file_path_bk) 
  write.csv(data_exp_lg, file_path_lg)
  
}

#######
data_exp_lg_p <- data_exp_lg %>% separate(stimulus, c("type","number"), sep = "/") 

data_exp_lg_p_tester <- data_exp_lg_p %>% group_by(type) %>% 
                                          mutate(trial_num = row_number()) %>% 
                                          ungroup() %>%
                                          pivot_wider(names_from = type, values_from = c(number,response)) %>%
                                          select(!response_targets)




#write.csv(data_exp_bk, "LG CSV files/blg007_bk.csv") 
#write.csv(data_exp_lg, "LG CSV files/blg007_lg.csv") 


#CALCULATE CHOICE BIAS
data_2_sounds <- data_exp_bk %>% group_by(sound) %>%
  summarise(round_bias = sum(correct)/n())

data_4_sounds <- data_exp_bk %>% group_by(stimulus) %>%
  summarise(round_bias = sum(correct)/n())

