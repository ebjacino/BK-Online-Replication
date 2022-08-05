ggplot(data_2_sounds, aes(x = sound, y = bias, fill = sound)) +
  geom_boxplot() +
  facet_wrap(~ group) +
  ylim(0,1) +
  theme_light()


p <- ggplot(fake_data, aes(x = WJ_score, y = strength_of_choice_bias)) +
  #geom_point() +
  theme_light() +
  geom_smooth(method='lm') +
  ylab("choice bias") +
  xlab("Woodcock Johnson Score")

ggsave("p.png", width = 6, height = 4)
