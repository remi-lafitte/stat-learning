# library
library(ggplot2)
library(here)
library(infer)
library(patchwork)
library(magrittr)
library(tidyverse)
# function
exo1<-function(x1, x2, n1,n2){
  h<-as_tibble(rnorm(mean = x1, sd = 20, n = n1))
  f<-as_tibble(rnorm(mean = x2, sd = 20, n = n2))
  m<-cbind(h,f)
  colnames(m)<-c("homme", "femme")
  pop<-m %>% gather(genre, tr)
  data<-pop
  p<-ggplot(data, aes(x = genre, y =tr, col = genre, fill = genre))+
    geom_jitter(size = 3, alpha = 0.5, width = 0.05)+
    stat_summary(aes(group = 1),fun = "mean", geom = "line", col = "black")+
    stat_summary(fun = mean,
                 geom = "errorbar",
                 # fun.max = function(x) mean(x) + sd(x),
                 # fun.min = function(x) mean(x) - sd(x),
                 fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
                 fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)),
                 width = 0.05, col = "black", size=1)+
    stat_summary(fun = "mean", geom = "point", stroke = 1.2, shape = 21, col = "black",
                 size=1)+
    scale_y_continuous(limits = c(400,650))+
    stat_summary(aes(label=..y..), fun = function(x) round(mean(x),1), geom="text", size=8, col = "black",vjust =-4)+
    labs(title = paste0("n =", nrow(data), "| ET [H = ", round(sd(h$value),1),
                        "; F = ",
                        round(sd(f$value),1),"]"), y = "Temps de réaction")+
    guides(col = F, fill =F)+
    theme_bw(base_size = 14)
  print(p)
}

#plot
# # H0
# png(filename = "exo1_h0.png", width = 40, height = 30, units = "cm", res = 100)
# exo1(520, 520, n1 = 5000, n2 = 5000)+
# exo1(520, 520,n1 = 1000, n2 = 1000)+
# exo1(520, 520,n1 = 100, n2 = 100)+
# exo1(520, 520,n1 = 50, n2 = 50)+
# exo1(520, 520,n1 = 12, n2 = 12)+
# exo1(520, 520,n1 = 5, n2 = 5)+
#   plot_annotation(title = expression("Univers H0" ~","~ mu ~ "=520" ~","~ sigma ~ "=20"))
# dev.off()

# H1------------------------
# png(filename = "exo1_h1.png", width = 40, height = 30, units = "cm", res = 100)
# exo1(525, 515, n1 = 5000, n2 = 5000)+
#   exo1(525, 515,n1 = 1000, n2 = 1000)+
#   exo1(525, 515,n1 = 100, n2 = 100)+
#   exo1(525, 515,n1 = 50, n2 = 50)+
#   exo1(525, 515,n1 = 12, n2 = 12)+
#   exo1(525, 515,n1 = 5, n2 = 5)+
#   plot_annotation(title = expression(
#     "Univers H1" ~","~ mu[Homme] ~ "=515" ~","~ mu[Femme] ~ "=525" ~","~ sigma ~ "=20"))
# dev.off()


# H1-----------------
png(filename = "exo1_h1_se.png", width = 40, height = 30, units = "cm", res = 100)
exo1(525, 515, n1 = 5000, n2 = 5000)+
  exo1(525, 515,n1 = 1000, n2 = 1000)+
  exo1(525, 515,n1 = 100, n2 = 100)+
  exo1(525, 515,n1 = 50, n2 = 50)+
  exo1(525, 515,n1 = 12, n2 = 12)+
  exo1(525, 515,n1 = 5, n2 = 5)+
  plot_annotation(title = expression(
    "Univers H1" ~","~ mu[Homme] ~ "=515" ~","~ mu[Femme] ~ "=525" ~","~ sigma ~ "=20"))
dev.off()


# H0-----------------
png(filename = "exo1_h0_sd.png", width = 40, height = 30, units = "cm", res = 100)
fm<-520
hm<-520
  exo1(fm, hm, n1 = 5000, n2 = 5000)+
  exo1(fm, hm,n1 = 1000, n2 = 1000)+
  exo1(fm, hm,n1 = 100, n2 = 100)+
  exo1(fm, hm,n1 = 50, n2 = 50)+
  exo1(fm, hm,n1 = 12, n2 = 12)+
  exo1(fm, hm,n1 = 5, n2 = 5)+
  plot_annotation(title = expression(
    "Univers H0" ~","~ mu[Homme] ~ "=520" ~","~ mu[Femme] ~ "=520" ~","~ sigma ~ "=20"))
dev.off()