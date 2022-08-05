library(dplyr)
library(stringr)
library(tidyverse)

correct_stim_list = c("sound/DADA.wav" = "bouba",
                      "sound/MAMA.wav" = "bouba",
                      "sound/SASA.wav" = "bouba",
                      "sound/didi.wav" = "bouba",
                      "sound/sisi.wav" = "bouba",
                      "sound/mimi.wav" = "bouba",
                      "sound/FAFA.wav" = "bouba",
                      "sound/fifi.wav" = "bouba",
                      "sound/zizi.wav" = "kiki",
                      "sound/titi.wav" = "kiki",
                      "sound/TATA.wav" = "kiki",
                      "sound/KAKA.wav" = "kiki",
                      "sound/kiki.wav" = "kiki",
                      "sound/LALA.wav" = "bouba",
                      "sound/lili.wav" = "bouba",
                      "sound/ZAZA.wav" = "kiki")


	
	
left_key = "z"
right_key = "m"

which_is_correct <- function(stimuli,lefts,rights) {
  
  #Make a new vector the same length as the number of rows
  correct_vec <- rep(NA,length(stimuli))
  
  #For each row in the dataset....
  for(i in seq(1:length(stimuli))){
    
    #Get the correct type ("curved" or "Kiki") by checking in the stim list
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
rds_files <- list.files("bko2_RDS",
                        pattern = "\\.rds$",
                        full.names = TRUE)

rds_files
#change [#] to pick which one to read

for(file in rds_files){
  print(file)
  file_id <- str_extract(file,"b[kl][pg]\\d\\d\\d")
  
  data <- readRDS(file)
  
  #assign ID
  participant_ID <- data$results$`sp_id`[[1]]
  main_data <- data$results$`Main page`[[1]]
  
  
  df <- read.table(text = main_data, sep =",", header = TRUE, stringsAsFactors = FALSE)
  #df is the raw data, tells you stimulus and responses
  
  
  ##Ben Code Attempt***creating tidy data and choice bias score***
  
  #First remove unneeded columns
  data_cols <- df %>% dplyr::select(c(trial_type,stimulus,response,left_image,right_image,correct_response,correct)) 
  
  #Then Filter practice and experimental trials
  data_prac <- data_cols %>% filter(grepl("Prac", stimulus, fixed = TRUE))
  data_exp <- data_cols %>% filter(!grepl("Prac", stimulus, fixed = TRUE)) %>%
    filter(trial_type  %in% c("bouba-kiki","image-keyboard-response")) %>%
    filter(stimulus != "img/fixation.png")
  
  #filter out BK data 
  data_exp_bk <-data_exp %>% filter(trial_type == "bouba-kiki")
  #filter out the LG data
  data_exp_lg <-data_exp %>% filter(trial_type =="image-keyboard-response") %>%
    dplyr::select(c(trial_type, stimulus, response))
  
  #Now write a function to find if the left or the right image is "correct"
  
  data_exp_bk <- data_exp_bk %>% mutate(correct_response = which_is_correct(stimulus,left_image,right_image))
  data_exp_bk <- data_exp_bk %>% mutate(correct = ifelse(correct_response == response,
                                                         1,
                                                         ifelse(response == left_key | response == right_key,
                                                                0,
                                                                NA)))
  
  data_exp_bk <- data_exp_bk %>% mutate(sound = ifelse(stimulus %in% c("sound/DADA.wav","sound/MAMA.wav",
                                                                 "sound/SASA.wav","sound/didi.wav",
                                                                 "sound/sisi.wav","sound/mimi.wav",
                                                                 "sound/FAFA.wav","sound/fifi.wav","sound/LALA.wav",
                                                                 "sound/lili.wav"),"curved",
                                                 ifelse(stimulus %in% c("sound/zizi.wav","sound/titi.wav",
                                                                        "sound/TATA.wav","sound/KAKA.wav",
                                                                        "sound/kiki.wav","sound/ZAZA.wav"),"angular",
                                                        NA))) %>% filter(!is.na(correct))
  
  data_exp_bk <- data_exp_bk %>% mutate(vowel_type = ifelse(stimulus %in% c("sound/DADA.wav","sound/MAMA.wav",
                                                                       "sound/SASA.wav","sound/TATA.wav",
                                                                       "sound/KAKA.wav","sound/ZAZA.wav",
                                                                       "sound/FAFA.wav","sound/LALA.wav"),"/a/",
                                                       
                                                       ifelse(stimulus %in% c("sound/zizi.wav","sound/titi.wav",
                                                                              "sound/didi.wav","sound/sisi.wav",
                                                                              "sound/kiki.wav","sound/fifi.wav",
                                                                              "sound/lili.wav","sound/mimi.wav"),"/i/",
                                                              NA))) %>% filter(!is.na(correct))
  
  data_exp_bk <- data_exp_bk %>% mutate(congruent = ifelse(stimulus %in% c("sound/DADA.wav","sound/MAMA.wav",
                                                                            "sound/SASA.wav","sound/zizi.wav",
                                                                            "sound/kiki.wav","sound/LALA.wav",
                                                                            "sound/FAFA.wav","sound/titi.wav"),"congruent",
                                                            
                                                            ifelse(stimulus %in% c("sound/TATA.wav","sound/lili.wav",
                                                                                   "sound/didi.wav","sound/sisi.wav",
                                                                                   "sound/KAKA.wav","sound/fifi.wav",
                                                                                   "sound/ZAZA.wav","sound/mimi.wav"),"incongruent",
                                                                   NA))) %>% filter(!is.na(correct))
  
  data_exp_bk <- data_exp_bk %>% mutate(voicing = ifelse(stimulus %in% c("sound/DADA.wav","sound/MAMA.wav",
                                                                           "sound/LALA.wav","sound/zizi.wav",
                                                                           "sound/didi.wav","sound/lili.wav",
                                                                           "sound/ZAZA.wav","sound/mimi.wav"),"voiced",
                                                           
                                                           ifelse(stimulus %in% c("sound/TATA.wav","sound/SASA.wav",
                                                                                  "sound/kiki.wav","sound/sisi.wav",
                                                                                  "sound/KAKA.wav","sound/fifi.wav",
                                                                                  "sound/FAFA.wav","sound/titi.wav"),"unvoiced",
                                                                  NA))) %>% filter(!is.na(correct))
  
  data_exp_bk <- data_exp_bk %>% mutate(consonant_type = ifelse(stimulus %in% c("sound/DADA.wav","sound/didi.wav",
                                                                         "sound/TATA.wav","sound/titi.wav",
                                                                         "sound/kiki.wav","sound/KAKA.wav"), "stop",
                                                   ifelse(stimulus %in% c("sound/MAMA.wav","sound/LALA.wav",
                                                                        "sound/lili.wav","sound/mimi.wav"),"sonorant",
                                                         
                                                         ifelse(stimulus %in% c("sound/SASA.wav","sound/ZAZA.wav",
                                                                                "sound/sisi.wav","sound/zizi.wav",
                                                                                "sound/fifi.wav", "sound/FAFA.wav"),"fricative",
                                                                NA)))) %>% filter(!is.na(correct))
  
  
  
  file_path_bk <- paste0("bko2_CSVs/",file_id,".csv")

  write.csv(data_exp_bk, file_path_bk) 

}



#write.csv(data_exp_bk, "LG CSV files/blg007_bk.csv") 
#write.csv(data_exp_lg, "LG CSV files/blg007_lg.csv") 


#CALCULATE CHOICE BIAS
data_2_sounds <- data_exp_bk %>% group_by(sound) %>%
  summarise(round_bias = sum(correct)/n())

data_4_sounds <- data_exp_bk %>% group_by(stimulus) %>%
  summarise(round_bias = sum(correct)/n())

