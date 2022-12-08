#analysis for the confound trials in LG
#%>% filter(!ID  %in% c("blg040","blg041"))

library(dplyr)
library(readxl)
library("writexl")
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(stringr)

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
  
  file_path_prac <- paste0("LG practice trials/",file_id,".csv")
  #write.csv(data_prac, file_path_prac)
  
  #write.csv(data_exp_bk, file_path_bk) 
  write.csv(data_exp_lg, file_path_lg)
  
}
#################

all_files_c <- list.files("LG tidy csv/confound")
first_file <- TRUE

#Adds ID code onto each trial, binds all individual csvs into one large dataframe
for(i in 1:length(all_files_c)){
  if(grepl("b",all_files_c[i])){
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
#age table
LG_c_ages <- read_excel("lg_confound_ages.xlsx")

#full_join to combine
LG_data_df_combined_c <- full_join(LG_data_df_c,target_table, by="image_ID_targets")
LG_data_df_combined_c <- full_join(LG_data_df_combined_c,pairs_table, by="image_ID_pairs")
LG_data_df_combined_c <- full_join(LG_data_df_combined_c,LG_c_ages, by="ID")



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
#looking at  individual bias values
LG_bias_ind_c <- LG_data_df_combined_bias_c %>% group_by(ID, Range) %>% summarise(global_bias = sum(choice_number)/n())

#calculate global bias for confound trials and normal trials 2nd try
LG_confound_trials_c <- LG_data_df_combined_bias_c %>% filter(!ID %in% "blg069") %>% 
  group_by(ID, target_confound, Range) %>% 
  summarise(global_bias = sum(choice_number)/n(), trials = n(), global_choices = sum(choice_number))

#write_xlsx(LG_confound_trials_c,"LG_confound_trials_c.xlsx")



##########t.testing analysis 

#what is the difference between confound trials or non confound trials participants? within
# independent sample t-test (2 groups, within participants)
res1 <- t.test(global_bias ~ target_confound, data = LG_confound_trials_c)
# Printing the results
res1 #results are nonsignificant, p = 0.809


#is there a significant difference between within participants(within t test), and also between the non-confound
#trials in people who did and did not see the confound (between subjects t test)
#get a measure for each within, then compare that across participants as well 

LG_bias_confound <- LG_confound_trials_c %>% filter(!ID  %in% "blg069") %>%
  group_by(target_confound, Range) %>%
  summarise(mean_bias = mean(global_bias),
            sd_bias = sd(global_bias),
            se_bias=sd_bias/sqrt(n()))

#compare mean bias between regular participants and those who did the confound. just adults here
#and filter adults only from LG_data_df_combined_bias
#full join LG_data_df_combined_bias with LG_data_df_combined_bias_c  

adults_no_confound <- LG_data_df_combined_bias %>% filter(Range == "Adult") %>%
  select(-c(Age, Range)) %>% mutate(participant_c = 0)

#ALL PARTICIPANTS NOT JUST ADULTS (only adults and 6 to 8 year olds)
all_no_confound <- LG_data_df_combined_bias%>%
  select(-c(Age, Range)) %>% mutate(participant_c = 0)

all_no_confound <- LG_data_df_combined_bias %>% filter(!Range  %in% c("5","9 to 11")) %>%
                    mutate(participant_c = 0)

LG_data_df_combined_bias_c <- LG_data_df_combined_bias_c  %>% mutate(participant_c = 1)

#below dataset is making a df of participants who did see the confounded trials, but deleting the confound trials themselves
LG_confounded_noConfoundTrials <- LG_data_df_combined_bias_c  %>% 
                                  mutate(participant_c = 1) %>%
                                  filter(!target_confound == 1)


#bind the 2 data frames together
#adults_confound_comparison <- rbind(adults_no_confound,LG_confounded_noConfoundTrials)
all_confound_comparison <- rbind(all_no_confound,LG_confounded_noConfoundTrials)

#compare those who got the confound trials to those who did not
confound_comparison <-all_confound_comparison %>% filter(!ID  %in% "blg069") %>%
  group_by(participant_c, ID, Range) %>%
  summarise(global_bias = sum(choice_number)/n(), global_choices = sum(choice_number)) %>% ungroup() %>%
  group_by(participant_c, Range) %>%
  summarise(mean_bias = mean(global_bias),
            sd_bias = sd(global_bias),
            se_bias=sd_bias/sqrt(n()))

#t.testing analysis 
#compare those who got the confound trials to those who did not
confound_comparison_analysis <-all_confound_comparison %>% group_by(participant_c, ID, Range) %>%
  summarise(global_bias = sum(choice_number)/n())

#what is the difference between confound trials or non confound trials participants?
# independent sample t-test (2 groups)
res <- t.test(global_bias ~ participant_c, data = confound_comparison_analysis)
# Printing the results
res 

#not significant, p = .3147

#different for the different age groups?
lgm_mult_cc <- glm(global_bias ~ Range*participant_c, data = confound_comparison_analysis)
lgm_add_cc <- glm(global_bias ~ Range+participant_c, data = confound_comparison_analysis)
ICtab(lgm_mult_cc, lgm_add_cc)

Anova(lgm_add_cc)

#no signifcant effect of Range on the model, and no effect of seeing the confounds

#### graphing
#difference between global bias of confound trials and nonconfounded trials (LG_confound_trials_c)
LG_confound_avg <- confound_comparison_analysis %>% filter(!ID  %in% "blg069") %>%
  group_by(Range, participant_c) %>%
  summarise(avg_bias = mean(global_bias),
            sd_bias = sd(global_bias, na.rm = TRUE),
            se_bias=sd_bias/sqrt(n()), 
            N = n()) %>% ungroup()

LG_confound_plot <-ggplot() +
  geom_col(LG_confound_avg, mapping = aes(x = factor(participant_c), y = avg_bias), color = "black", fill = NA) +
  geom_col(LG_confound_avg, mapping = aes(x = factor(participant_c), y = avg_bias, fill = factor(participant_c)), alpha = 0.5) +
  geom_errorbar(LG_confound_avg, mapping = aes(x = factor(participant_c), y = avg_bias, ymin = avg_bias-se_bias,
                                               ymax = avg_bias+se_bias), alpha = 1, width = .75) +
  #geom_jitter(BK_LG_scores, mapping = aes(x = participant_c, y = avg_bias)) +
  theme_light() +
  facet_wrap("Range") +
  ylab("Global Bias") +
  xlab("trial type") 

LG_confound_plot

####difference between those who do and don't see the confound trials?
LG_confound_plot_bt <-ggplot() +
  geom_col(LG_bias_confound, mapping = aes(x = factor(target_confound), y = mean_bias), color = "black", fill = NA) +
  geom_col(LG_bias_confound, mapping = aes(x = factor(target_confound), y = mean_bias, fill = factor(target_confound)), alpha = 0.5) +
  geom_errorbar(LG_bias_confound, mapping = aes(x = factor(target_confound), y = mean_bias, ymin = mean_bias-se_bias,
                                               ymax = mean_bias+se_bias), alpha = 1, width = .75) +
  theme_light() +
  facet_wrap("Range") +
  ylab("Global Bias") +
  xlab("trial type") 

LG_confound_plot_bt


##########################################################################
#POIREL COUNTING###
LG_count_c <-LG_data_df_combined_bias_c %>% group_by(ID,target_confound) %>% count(choice)
LG_count_p_c <- LG_count_c %>% pivot_wider(names_from = choice, values_from = n) %>% mutate_all(~replace(., is.na(.), 0))
#new column with local-global
LG_count_p_c <- LG_count_p_c %>% mutate(GminusL = global-local) %>% mutate(LG_group = ifelse(GminusL >0, "global", "local"))

##
LG_poirel_all_c <- full_join(LG_data_df_combined_bias_c, LG_count_p_c, by ="ID")

#is there a difference between gminusl in confounded trials vs non-confounded trials? This needs 
#to be a proportional value, though??





