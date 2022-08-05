library(dplyr)

#look at the results files
rds_files <- list.files("LG_RDS",
                        pattern = "\\.rds$",
                        full.names = TRUE)

rds_files
#change [#] to pick which one to read
data <- readRDS(rds_files[3])

#assign ID
participant_ID <- data$results$`sp_id`[[1]]
main_data <- data$results$`Main page`[[1]]


df <- read.table(text = main_data, sep =",", header = TRUE, stringsAsFactors = FALSE)
#df is the raw data, tells you stimulus and responses


##Ben Code Attempt***creating tidy data and choice bias score***

#First remove unneeded columns
#data_cols <- df %>% select(c(trial_type,stimulus,response,left_image,right_image,correct_response,correct)) 

#Then Filter practice and experimental trials
#data_prac <- data_cols %>% filter(grepl("Prac", stimulus, fixed = TRUE))
# data_exp <- data_cols %>% filter(!grepl("Prac", stimulus, fixed = TRUE)) %>%
#   filter(trial_type  %in% c("bouba-kiki","image-keyboard-response")) %>%
#   filter(stimulus != "img/fixation.png")

#filter out BK data 
#data_exp_bk <-data_exp %>% filter(trial_type == "bouba-kiki")
#filter out the LG data
data_exp_lg <-df %>% filter(trial_type =="image-keyboard-response") %>%
  select(c(trial_type, stimulus, response, image)) %>% 
  filter(stimulus != "img/fixation.png")



#write.csv(data_exp_bk, "LG CSV files/blg120_bk.csv") 
write.csv(data_exp_lg, "LG CSV files/blg121_lg.csv") 




