d<-read.csv(here::here("td1.csv"), sep = ";")
x<-d[d$genre == "homme","tr"]
y<-d[d$genre == "femme","tr"]
df <- tbl_df(data.frame(x,y))
model<-t.test(df$x ,df$y)
model$statistic
colnames(df)<-c("a", "b")
t_res <- function(df) {
  df2<-df %>% gather("vi", "vd")
  model <- t.test(formula = vd ~ vi, data =df2 %>% sample_frac(replace=T))
  return (round(model$statistic,1))
}
df_boot <- replicate(1000, t_res(df))
df_boot <- tbl_df(data.frame(t(df_boot)))
tb<-df_boot %>% gather("boot", "t")

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(tb$t)
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
  scale_x_continuous(breaks=(seq(-15,15,1)))+
  labs(title = "n = 24", x = "T distribution")

anim <- p_anim + transition_manual(frame) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()
anim
anim_save("t.gif", anim)
