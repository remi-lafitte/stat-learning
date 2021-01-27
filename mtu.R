mtu<-read.csv(here::here("mtu_2021.csv"), sep = ";", dec = ",")
summary(mtu$note)

sd(mtu$note)
str(mtu)
hist(mtu$note)
p<-ggplot(mtu, aes(x =note))+
         geom_histogram(col = "black", fill = "grey")+
  scale_x_continuous(breaks=(seq(0,20,1)))+
  theme_bw(base_size = 18)
png(filename = "mtu_hist.png")
p
dev.off()
       