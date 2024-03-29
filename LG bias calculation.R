library(dplyr)
library(readxl)
library("writexl")
library(ggplot2)
library(ggpubr)
round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
left_key = "z"
right_key = "m"

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
age_table <- read_excel("LG_ages.xlsx")
age_table <- age_table %>% mutate(ID = tolower(ID)) #%>% select(ID,Age,Range)

#full_join to combine
LG_data_df_combined <- full_join(LG_data_df,target_table, by="image_ID_targets")
#LG_data_df has no confound image IDs so they are populating as NAs
LG_data_df_combined <- full_join(LG_data_df_combined,pairs_table, by="image_ID_pairs")%>%
  filter(!is.na(ID))
LG_data_df_combined <- full_join(LG_data_df_combined,age_table, by="ID")

unique(LG_data_df_combined$ID) #=79

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

#print out excel sheet of the local and global choices
#write_xlsx(LG_data_df_combined_bias,"LG_data_df_combined_bias.xlsx")

#calculate global bias: n(global)/n() ################
#for null responses, remove from total n(trials)
LG_data_df_combined_bias <- LG_data_df_combined_bias %>% mutate(choice_number = ifelse(choice == "global", 1, 0)) %>%
                            filter(!is.na(choice))

unique(LG_data_df_combined_bias$ID) #= 79

LG_bias_ind <- LG_data_df_combined_bias %>% group_by(ID) %>% summarise(global_bias = sum(choice_number)/n())

unique(LG_bias_ind$ID) #= 79

LG_bias_data <- LG_data_df_combined_bias %>% group_by(ID,Range) %>% 
                                    summarise(global_bias = sum(choice_number)/n()) %>% 
                                    group_by(Range) %>%
                                    summarize(mean_bias = mean(global_bias),
                                    sd_bias = sd(global_bias),
                                    se_bias=sd_bias/sqrt(n()),
                                    N=n()) %>% ungroup()
#^this one has NAs

LG_bias_data_points <- LG_data_df_combined_bias  %>% group_by(ID,Range) %>% 
                       summarise(global_bias = sum(choice_number)/n()) %>% 
                        ungroup() %>%
                        mutate(bias_round = round_any(global_bias,0.25)) %>%
                        group_by(Range,bias_round) %>%
                        summarise(Count = n())

#GRAPH TIME
LG_bias_plot <-ggplot() +
  geom_col(LG_bias_data, mapping = aes(x = Range, y = mean_bias), color = "black", fill = NA) +
  geom_point(LG_bias_data_points, mapping = aes(x = Range, y = bias_round, size = Count, color = Range)) +
  geom_col(LG_bias_data, mapping = aes(x = Range, y = mean_bias, fill = Range), alpha = 0.5) +
  geom_point(LG_bias_data, mapping = aes(x = Range, y = mean_bias)) +
  geom_errorbar(LG_bias_data, mapping = aes(x = Range, y = mean_bias, ymin = mean_bias-se_bias, ymax = mean_bias+se_bias), alpha = 1) +
  theme_classic() +
  #facet_wrap("Range") +
  #ylim(-0.5,0.5) +
  ylab("Global Bias") +
  #scale_color_manual(values = rev(cols)) +
  #scale_fill_manual(values = rev(cols)) +
  xlab("Age")

LG_bias_plot

                                

                                              
                                                                           
                                                                           
                                                                           
                                                                           
                                                                         
