#t-------------
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggforce)
x<-as_tibble(rnorm(mean = 520, sd = 50, n = 1000))
y<-as_tibble(rnorm(mean = 520, sd = 50, n = 1000))

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
t_res(n=12)
n=12
repli<-function(n){
 o<- as.data.frame(as.matrix(replicate(10000, t_res(n=n))))
 oo<-gather(o,"boot", "t")
 df <- data.frame(sample = 1:nrow(oo), t = oo$t)
 df_ani <- df %>% 
   split(.$sample) %>% 
   accumulate(~ bind_rows(.x, .y)) %>% 
   bind_rows(.id = "frame") %>% 
   mutate(frame = as.integer(frame))
 bw = 0.2
 n_obs = sum(!is.na(df_ani$t))
 p_anim <- ggplot(data = df_ani, aes(x = t)) +
   # geom_histogram(fill = "grey", col ="black", binwidth = bw)+
    geom_dotplot(binwidth=bw, method='histodot', col = "black", fill="grey") +
   labs(title = paste("n =",n, ", samples = 10 000"), x = "T distribution")+
   scale_x_continuous(breaks=(seq(-15,15,1)), limits = c(-4,4))+ 
    scale_y_continuous(limits = c(0,30000))+
   # stat_function(fun = function(x) 
   #   dnorm(x, mean = mean(df_ani$t), sd = sd(df_ani$t)) * bw * n_obs, col = "red",
   #   size = 1, lty = "dashed")+
   theme_bw(base_size = 18)
   
 p_anim
 anim <- p_anim + transition_manual(frame) +
   ease_aes("linear") +
   enter_fade() 

anim_save(paste0("t500_n",n,".gif") , 
          animate(anim, duration =40, renderer = gifski_renderer(loop = F)))
 }
# size<-c(10, 20, 30, 60)
# for(i in size){repli(i)}
repli(n = 12)
