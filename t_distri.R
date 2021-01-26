#t-------------
library(tidyverse)
library(ggplot2)
library(gganimate)
x<-as_tibble(rnorm(mean = 520, sd = 20, n = 10000))
y<-as_tibble(rnorm(mean = 520, sd = 20, n = 10000))

#function------------
t_res <- function(n) {
  x<-sample_n(x,replace=T, size = n)
  y<-sample_n(x,replace=T, size = n)
  df <- tbl_df(data.frame(x,y))
  colnames(df)<-c("a", "b")
  df2<-df %>% gather("vi", "vd")
  model <- t.test(formula = vd ~ vi,data =  df2)
  return (round(model$statistic,1))
}

repli<-function(n){
 o<- as.data.frame(as.matrix(replicate(1000, t_res(n=n))))
 oo<-gather(o,"boot", "t")
 df <- data.frame(sample = 1:nrow(oo), t = oo$t)
 df_ani <- df %>% 
   split(.$sample) %>% 
   accumulate(~ bind_rows(.x, .y)) %>% 
   bind_rows(.id = "frame") %>% 
   mutate(frame = as.integer(frame))
 bw = 0.3
 n_obs = sum(!is.na(df_ani$t))
 p_anim <- ggplot(data = df_ani, aes(x = t)) +
   geom_histogram(fill = "grey", col ="black", binwidth = bw)+
   labs(title = paste("n =",n), x = "T distribution")+
   scale_x_continuous(breaks=(seq(-15,15,1)), limits = c(-4,4))+ 
   # stat_function(fun = function(x) 
   #   dnorm(x, mean = mean(df_ani$t), sd = sd(df_ani$t)) * bw * n_obs, col = "red",
   #   size = 1, lty = "dashed")+
   theme_bw(base_size = 18)
   
 
 anim <- p_anim + transition_manual(frame) +
   ease_aes("linear") +
   enter_fade() +
   exit_fade()
 anim
 anim_save(paste0("t100_n",n,".gif") , anim)
}
size<-c(10, 20, 30, 60)
for(i in size){repli(i)}
