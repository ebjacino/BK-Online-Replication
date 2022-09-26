#analysis for the confound trials in LG

library(dplyr)
library(readxl)
library("writexl")
library(ggplot2)
library(ggpubr)
round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


#look at the results files
rds_files_c <- list.files("LG_double_image_RDS/confound",
                        pattern = "\\.rds$",
                        full.names = TRUE)

rds_files_c
#change [#] to pick which one to read

for(file in rds_files_c){
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
  
  #adding columns to LG files to do analysis ############
  data_exp_lg <- data_exp_lg %>% separate(stimulus, c("type","image_ID"), sep = "/") 
  
  data_exp_lg <- data_exp_lg %>% group_by(type) %>% 
    mutate(trial_num = row_number()) %>% 
    ungroup() %>%
    pivot_wider(names_from = type, values_from = c(image_ID,response)) %>%
    select(!c(trial_type,response_targets))
  
  
  
  
  
  #file_path_bk <- paste0("LG CSV files Looped/",file_id,"_bk.csv")
  file_path_lg <- paste0("LG tidy csv/confound/",file_id,"_lg.csv")
  
  #file_path_prac <- paste0("LG practice trials/",file_id,".csv")
  #write.csv(data_prac, file_path_prac)
  
  #write.csv(data_exp_bk, file_path_bk) 
  write.csv(data_exp_lg, file_path_lg)
  
}
#################

all_files_c <- list.files("LG tidy csv/confound")
first_file <- TRUE

#Adds ID code onto each trial, binds all individual csvs into one large dataframe
for(i in 1:length(all_files_c)){
  if(grepl("b",all_files[i])){
    print(all_files_c[i])
    if(first_file){
      LG_data_df_c <- read.csv(paste0("LG tidy csv/confound/",all_files_c[i]))
      LG_data_df_c <- LG_data_df_c %>% mutate(ID = substr(all_files_c[i],1,6))
      first_file <- FALSE
    }
    else{
      LGnew_data_df_c <- read.csv(paste0("LG tidy csv/confound/",all_files_c[i]))
      LGnew_data_df_c <- LGnew_data_df_c %>% mutate(ID = substr(all_files_c[i],1,6))
      LG_data_df_c <- rbind(LG_data_df_c,LGnew_data_df_c)
    }
  }
}
#the participants with confound trials
target_table <- read_excel("LG_target_table.xlsx")
pairs_table <- read_excel("LG_pair_table.xlsx")
#full_join to combine
LG_data_df_combined_c <- full_join(LG_data_df_c,target_table, by="image_ID_targets")
LG_data_df_combined_c <- full_join(LG_data_df_combined_c,pairs_table, by="image_ID_pairs")

#if the the global choice is on the left and they make the "z" response, then put G in the choice column, etc
LG_data_df_combined_bias_c <- LG_data_df_combined_c %>% mutate(choice = ifelse(response_pairs == left_key &
                                                                             target_global == pair_left_global,
                                                                           "global", 
                                                                           ifelse(response_pairs == left_key &
                                                                                    target_local == pair_left_local,
                                                                                  "local",
                                                                                  ifelse(response_pairs == right_key &
                                                                                           target_global == pair_right_global,
                                                                                         "global",
                                                                                         ifelse(response_pairs == right_key &
                                                                                                  target_local == pair_right_local,
                                                                                                "local",
                                                                                                NA)))))

#calculate global bias: n(global)/n() ################
#for null responses, remove from total n(trials)
LG_data_df_combined_bias_c <- LG_data_df_combined_bias_c %>% mutate(choice_number = ifelse(choice == "global", 1, 0)) %>%
  filter(!is.na(choice))

LG_bias_ind_c <- LG_data_df_combined_bias_c %>% group_by(ID) %>% summarise(global_bias = sum(choice_number)/n())





#LG_confound_trials <- LG_data_df_combined_bias %>% mutate(confound_participant = ifelse(ID %in% c("blg166", "blg167",
                                                                                                  #"blg168", "blg169",
                                                                                                  #"blg170"), 1, 0))



#calculate global bias for confound trials and normal trials 2nd try
LG_confound_trials_c <- LG_data_df_combined_bias_c %>% 
  group_by(ID, target_confound) %>% #mutate(trials = n()) %>%
  summarise(global_bias = sum(choice_number)/n(), trials = n(), global_choices = sum(choice_number))

#write_xlsx(LG_confound_trials_c,"LG_confound_trials_c.xlsx")




