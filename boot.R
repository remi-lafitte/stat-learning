x <- c(8.1, 8.4, 8.8, 8.7, 9, 9.1, 9.2, 9.3, 9.4, 9.6, 9.9, 10, 10, 10.5, 10.6, 10.6, 11.2, 11.8, 12.6)
y <- c(21, 19, 18, 16, 15, 17, 17, 17, 19, 14, 14, 15, 11, 12, 12, 13, 10, 8, 9)

df <- tbl_df(data.frame(x,y))
model<-t.test(df$x ,df$y)
model$statistic
colnames(df)<-c("a", "b")
t_res <- function(df) {
  df2<-df %>% gather("vi", "vd")
  model <- t.test(formula = vd ~ vi, data =df2 %>% sample_frac(replace=T))
  return (round(model$statistic,1))
}
df_boot <- replicate(10000, t_res(df))
df_boot <- tbl_df(data.frame(t(df_boot)))
tb<-df_boot %>% gather("boot", "t")

library(gganimate)

#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
# example data
df <- data.frame(sample = 1:nrow(tb), t = tb$t)

df_ani <- df %>% 
  split(.$sample) %>% 
  accumulate(~ bind_rows(.x, .y)) %>% 
  bind_rows(.id = "frame") %>% 
  mutate(frame = as.integer(frame))


p_anim <- ggplot(data = df_ani, aes(x = t)) +
  geom_histogram(fill = "grey", col ="black")+
  scale_x_continuous(breaks=(seq(-12,12,1)))

anim <- p_anim + transition_manual(frame) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()
anim
anim_save("t.gif", anim)
