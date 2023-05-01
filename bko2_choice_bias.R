library(dplyr)


all_files <- list.files("bko2 full paradigm CSVs")
first_file <- TRUE

#Adds ID code onto each trial, binds all individual csvs into one large dataframe
for(i in 1:length(all_files)){
  if(grepl("b",all_files[i])){
    print(all_files[i])
    if(first_file){
      all_data_df <- read.csv(paste0("bko2 full paradigm CSVs/",all_files[i]))
      all_data_df <- all_data_df %>% mutate(ID = substr(all_files[i],1,6))
      first_file <- FALSE
    }
    else{
      new_data_df <- read.csv(paste0("bko2 full paradigm CSVs/",all_files[i]))
      new_data_df <- new_data_df %>% mutate(ID = substr(all_files[i],1,6))
      all_data_df <- rbind(all_data_df,new_data_df)
    }
  }
}

#filter out the NAs
all_data_df <- all_data_df %>% filter(!is.na(correct))

unique(all_data_df$ID)

#write.csv(all_data_df, "bko2_all_data_df.csv")

#filter out lala and lili stimuli
#all_data_df <- all_data_df %>% filter(!stimulus %in% c("sound/LALA.wav", "sound/lili.wav"))


#CALCULATE CHOICE BIAS
#group_by(ID,sound,group) group for when i have both adults and children
data_orth <- all_data_df %>% group_by(ID,sound) %>%
  summarise(bias = sum(correct)/n())

data_all_sounds <- all_data_df %>% group_by(ID,sound,stimulus) %>%
  summarise(bias = sum(correct)/n())

data_vowel_type <- all_data_df %>% group_by(vowel_type, ID) %>%
  summarise(bias = sum(correct)/n())

data_congruency = all_data_df %>% group_by(congruent, ID) %>%
  summarise(bias = sum(correct)/n())

data_voicing = all_data_df %>% group_by(voicing, ID) %>%
  summarise(bias = sum(correct)/n())

data_consonant = all_data_df %>% group_by(consonant_type, ID) %>%
  summarise(bias = sum(correct)/n())



#calculating means and SE
data_means_orth <- data_orth %>% 
  group_by(sound) %>% # Group the data by sound
  summarize(mean_bias=mean(bias), 
            sd_bias=sd(bias), # Create variable with sd of cty per group
            N_bias=n(), # Create new variable N of cty per group
            se=sd_bias/sqrt(N_bias), 
            upper_limit=mean_bias+se, # Upper limit
            lower_limit=mean_bias-se) # Lower limit) # Create variable with se of cty per group) # Create new variable which is the mean of bias

data_means_all <- data_all_sounds %>% 
  group_by(sound, stimulus) %>% # Group the data by sound
  summarize(mean_bias=mean(bias), 
            sd_bias=sd(bias), # Create variable with sd of cty per group
            N_bias=n(), # Create new variable N of cty per group
            se=sd_bias/sqrt(N_bias), 
            upper_limit=mean_bias+se, # Upper limit
            lower_limit=mean_bias-se) # Lower limit) # Create variable with se of cty per group) # Create new variable which is the mean of bias



#PLOT IT BOX AND WHISKER PLOTS HERE
library(ggplot2)

#bias graphs for 2 sounds (round or spiky)
ggplot(data_orth, aes(x = sound, y = bias, fill = sound)) +
  geom_boxplot() +
#  facet_wrap(~ group) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylim(0,1) +
  theme_light()

#bias for all 16 sounds 
ggplot(data_all_sounds, aes(x = stimulus, y = bias, fill = sound)) +
  geom_boxplot() +
#  facet_wrap(~ group) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylim(0,1) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45))

#bias for vowel type
vowel_comparisions <- list( c("/a/", "/i/"))

ggplot(data_vowel_type, aes(x = vowel_type, y = bias)) +
  geom_boxplot() +
  #  facet_wrap(~ group) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylim(0,1.2) +
  theme_light()+
  stat_compare_means(comparisons = vowel_comparisions, label.y = c(1.1),
                     label = "p.signif", hide.ns = TRUE)

#bias for congruency
congruency_comparisions <- list( c("congruent", "incongruent"))

ggplot(data_congruency, aes(x = congruent, y = bias)) +
  geom_boxplot() +
  #  facet_wrap(~ group) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylim(0,1.25) +
  theme_light() +
  stat_compare_means(comparisons = congruency_comparisions, label.y = c(1.1),
                     label = "p.signif", hide.ns = TRUE)


#bias for voicing
voicing_comparisions <- list( c("unvoiced", "voiced"))

ggplot(data_voicing, aes(x = voicing, y = bias)) +
  geom_boxplot() +
  #  facet_wrap(~ group) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylim(0,1.2) +
  theme_light()+
  stat_compare_means(comparisons = voicing_comparisions, label.y = c(1.1),
                     label = "p.signif", hide.ns = TRUE)




#bias for consonant type
#consonant_comparisons <- list( c("stop", "fricative", "sonorant"))
consonant_comparisons <- list( c("stop", "fricative"),
                        c("fricative", "sonorant"),
                        c("stop", "sonorant"))
                        

ggplot(data_consonant, aes(x = consonant_type, y = bias)) +
  geom_boxplot() +
  #  facet_wrap(~ group) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylim(0,2) +
  theme_light()+
  stat_compare_means(comparisons = consonant_comparisons, label.y = c(1.1, 1.3, 1.5),
                     label = "p.signif", hide.ns = TRUE)

#Do i want this to be proportion spiky:round?? instead of total bias strength? YES






#PLOTS OF THE MEAN BIAS AND SE
p1 <- ggplot() + 
  geom_bar(data = data_means_orth,
           aes(x=sound, y=mean_bias, fill = sound), stat="identity", width=0.5) + 
  geom_errorbar(data = data_means_orth, aes(x=sound, ymin=lower_limit, ymax=upper_limit), width=0.40) + 
  geom_count(data = data_orth, aes(x=sound, y=bias)) +
  scale_size_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14)) +
  theme_light()+
  #facet_wrap(~ group) +
  ylim(0,1) +
  ylab("choice bias")

p1

p2 <- ggplot() +
  geom_bar(data = data_means_all,
           aes(x=stimulus, y=mean_bias, fill = sound), stat="identity", width=0.5) + 
  geom_errorbar(data = data_means_all, aes(x=stimulus, ymin=lower_limit, ymax=upper_limit), width=0.40) + 
  geom_count(data = data_all_sounds, aes(x=stimulus, y=bias)) +
  scale_size_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14)) +
  theme_light()+
  #facet_wrap(~ group) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ylim(0,1) +
  ylab("choice bias")

p2

#look at first trials only 
first_trial <- all_data_df %>% group_by(ID) %>% 
  filter(row_number()==1) 

count_first_trial <- first_trial %>% group_by(sound) %>% summarise(n = n())

#see differences based on vowel type
p3 <- ggplot() + 
  geom_bar(data = data_means_orth,aes(x=sound, y=mean_bias, fill = sound), stat="identity", width=0.5) + 
  geom_errorbar(data = data_means_orth, aes(x=sound, ymin=lower_limit, ymax=upper_limit), width=0.40) + 
  geom_count(data = data_orth, aes(x=sound, y=bias)) +
  scale_size_continuous(breaks = c(1, 2, 4, 6, 8, 10, 12, 14)) +
  theme_light()+
  ylim(0,1) +
  ylab("choice bias")

p3


### BEn is doing stats here!

library(bbmle)
library(car)
library(ggpubr)
library(emmeans)

voiced_color = "#2f6eba"
unvoiced_color = "#c45Baa"

data_all_biases = all_data_df %>% group_by(voicing, congruent, vowel_type, ID, sound) %>%
  summarise(bias = sum(correct)/n())

#model interactive, voicing, congruent, vowel type
#model with the lowest AIC is the best one

m_v <- glm(bias ~ voicing, data = data_all_biases)
m_c <- glm(bias ~ congruent, data = data_all_biases)
m_vt <- glm(bias ~ vowel_type, data = data_all_biases)
mi_v_c <- glm(bias ~ voicing * congruent, data = data_all_biases)
mi_v_vt <- glm(bias ~ voicing * vowel_type, data = data_all_biases)
mi_c_vt <- glm(bias ~ congruent * vowel_type, data = data_all_biases)
mi_v_c_vt <- glm(bias ~ voicing * congruent * vowel_type, data = data_all_biases)
mi_v_c_vt_add <- glm(bias ~ voicing + congruent + vowel_type, data = data_all_biases)
mi_c_v_o <- glm(bias ~ voicing * congruent * sound, data = data_all_biases)


ICtab(m_v,m_c,m_vt,mi_v_c,mi_v_vt,mi_c_vt,mi_v_c_vt, mi_v_c_vt_add, mi_c_v_o)

Anova(mi_c_vt)
Anova(mi_v_c_vt)

em <- emmeans(mi_c_vt, ~  congruent * vowel_type, method = "pairwise", type = "response", adjsut = "mvt")
pairs(em)
emmeans_model <- as.tibble(pairs(em)) 
#write.csv(emmeans_model, "significance tables/emmeans model")


ma_v_c_vt <- glm(bias ~ 0 + voicing + congruent + vowel_type, data = data_all_biases)
summary(ma_v_c_vt)

model_comparisons <- list( c("/a/, congruent", "/i/, incongruent"),
                        c("/a/, incongruent", "/i/, congruent"))


#plotting
model_plot <- ggplot(data_all_biases, aes(x = interaction(vowel_type,congruent,sep=", "), y = bias, 
                            fill = interaction(vowel_type,congruent,sep=", ")))+
  geom_boxplot()+
  #geom_boxplot(aes(x = congruent, y = predict(mi_v_c_vt), color = voicing))+
  ylim(0,1.25) +
  #facet_wrap(~ vowel_type) +
  scale_fill_manual(values = c(voiced_color,unvoiced_color,
                               "pink","red")) +
  theme_light() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  xlab("vowel") +
  ylab("Bias") +
  stat_compare_means(comparisons = model_comparisons, label.y = c(1.2,1.1,1.2,1.1),
                     label = "p.signif", hide.ns = TRUE)

model_plot
#ggsave("graphs/presentation graphs/interaction model graph.png")

  

# BEN IS DOING STATS ALERT A L E R T
#could multiply the p values by 16
#BONFERRONI CORRECTION OF STIMULI SIGINIFICANT DIFFERENCE FROM CHANCE

p.adjust(p, method = "bonferroni", n = 16)

ben_m <- glm(correct ~ 0 + stimulus, data = all_data_df, family = binomial)#, p.adjust.method="bonferroni")
summary(ben_m)#$coefficients[,4]



ben_tib <- tibble(categories = row.names(summary(ben_m)$coefficients),
                  old_p = summary(ben_m)$coefficients[,4],
                  p_vals = p.adjust(summary(ben_m)$coefficients[,4], method = "bonferroni", n = 16),
                  p_val_print = sprintf("%.10f", p_vals),
                  sig = ifelse(p_vals<0.05, "significant", "nonsignificant"))

write.csv(ben_tib, file = "significance tables/bko2 stimuli significance testing corrected")

coef(ben_m)
plogis(coef(ben_m))

0.05/16

(1 - 0.05)^16


b_tib <- tibble(ID = rep(1:100, each = 10),
       data = rnorm(1000, mean = 0, sd = 1))

ben_m_demo <- glm(data ~ 0 + as.factor(ID), data = b_tib)
summary(ben_m_demo)

0.05/100



  