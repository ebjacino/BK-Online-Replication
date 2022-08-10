library(dplyr)
library(readxl)
library("writexl")

all_files <- list.files("LG tidy csv")
first_file <- TRUE

#Adds ID code onto each trial, binds all individual csvs into one large dataframe
for(i in 1:length(all_files)){
  if(grepl("b",all_files[i])){
    print(all_files[i])
    if(first_file){
      LG_data_df <- read.csv(paste0("LG tidy csv/",all_files[i]))
      LG_data_df <- LG_data_df %>% mutate(ID = substr(all_files[i],1,6))
      first_file <- FALSE
    }
    else{
      LGnew_data_df <- read.csv(paste0("LG tidy csv/",all_files[i]))
      LGnew_data_df <- LGnew_data_df %>% mutate(ID = substr(all_files[i],1,6))
      LG_data_df <- rbind(LG_data_df,LGnew_data_df)
    }
  }
}

#load in the tables of targets and pairs descriptions
target_table <- read_excel("LG_target_table.xlsx")
pairs_table <- read_excel("LG_pair_table.xlsx")

#full_join to combine
LG_data_df_combined <- full_join(LG_data_df,target_table, by="image_ID_targets")
LG_data_df_combined <- full_join(LG_data_df_combined,pairs_table, by="image_ID_pairs")

#if the the global choice is on the left and they make the "z" response, then put G in the choice column, etc
LG_data_df_combined_bias <- LG_data_df_combined %>% mutate(choice = ifelse(response_pairs == left_key &
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

write_xlsx(LG_data_df_combined_bias,"LG_data_df_combined_bias.xlsx")

                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                         
