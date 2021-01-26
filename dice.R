# Boot dice-------------
throws <- data.frame(dice=sample(1:6, 1000, replace=TRUE, prob=c(1,1,1,1,1,1)/6))
throws$sample<-seq(1:nrow(throws))
df_ani <- throws  %>% 
  split(.$sample) %>% 
  accumulate(~ bind_rows(.x,.y)) %>% 
  bind_rows(.id = "frame") %>% 
  mutate(frame = as.integer(frame))


p_anim <- ggplot(data = df_ani, aes(x = dice)) +
  geom_histogram(fill = "grey", col ="black")+
  scale_x_continuous(breaks=(seq(1,6,1)))+
  labs(title = "samples = 1000", x = "Face distribution")+
  geom_hline(yintercept = 1000/6, col = "red", lty = "dashed")+
  theme_bw(base_size = 18)

anim <- p_anim + transition_manual(frame) +
  ease_aes("linear") +
  enter_fade() 
# +exit_fade()
anim
anim_save("dice.gif", anim)
