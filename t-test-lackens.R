# " le degré de liberté seul" n'est pas la cause du t plus grand. Quand ddl augmente, sd augmete naturellement !


x = seq(-4.5,4.5,length=100)
y<-dnorm(x,0,1)
plot(x, y, type="l", col="black", lwd=5)
lines(x,dt(x,df=10),col='red',type='l')
lines(x,dt(x,df=5),col='blue',type='l')
legend(-4,0.4, # places a legend at the appropriate place
       c("Normal","t(10)", "t(5)"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5),col=c("black", "red","blue")) # format lines and colors


rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }


myfriends = rnorm2(n = 12, mean = 532, sd = 40)  
mywifesfriends = rnorm2(n = 12, mean = 508, sd = 40)



d1 <- density(myfriends)
d2 <- density(mywifesfriends)
plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", xlab = "LOTR Rating", ylab = "Density")
polygon(d1, col=rgb(0,0,1,1/4), border=rgb(0,0,1))
polygon(d2, col=rgb(1,0,0,1/4), border=rgb(1,0,0,1))



t.test(myfriends,mywifesfriends, var.equal = TRUE)



x=seq(-5,5,length=100)
plot(x,dt(x,df=22),col='black',type='l')
abline(v=1.4697, lwd=2)
x=seq(1.4697,5,length=100)
z<-(dt(x,df=22))
polygon(c(1.4697,x,8),c(0,z,0),col="blue")
abline(v= -qt(0.05,22),col="red",lty=3)

x=seq(-5,5,length=100)
plot(x,dt(x,df=10),col='black',type='l')
abline(v=1.4697, lwd=2)
x=seq(1.4697,5,length=100)
z<-(dt(x,df=10))
polygon(c(1.4697,x,8),c(0,z,0),col="blue")
abline(v= -qt(0.025,22),col="red",lty=3)

x=seq(-5,5,length=100)
plot(x,dt(x,df=4),col='black',type='l')
abline(v=1.4697, lwd=2)
x=seq(1.4697,5,length=100)
z<-(dt(x,df=4))
polygon(c(1.4697,x,8),c(0,z,0),col="blue")
abline(v= -qt(0.025,22),col="red",lty=3)

####------------------------

myfriends2 = rnorm2(n = 12, mean = 532, sd = 80)  
mywifesfriends2 = rnorm2(n = 12, mean = 508, sd = 80)


d1 <- density(myfriends2)
d2 <- density(mywifesfriends2)
plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", xlab = "LOTR Rating", ylab = "Density")
polygon(d1, col=rgb(0,0,1,1/4), border=rgb(0,0,1))
polygon(d2, col=rgb(1,0,0,1/4), border=rgb(1,0,0,1))



t.test(myfriends2,mywifesfriends2, var.equal = TRUE)



x=seq(-5,5,length=100)
plot(x,dt(x,df=22),col='black',type='l')
abline(v=0.73485, lwd=2)
x=seq(0.73485,5,length=100)
z<-(dt(x,df=22))
polygon(c(0.73485,x,8),c(0,z,0),col="blue")
abline(v= -qt(0.05,22),col="red",lty=3)




t.power = function(nsamp=c(10,10),nsim=1000,means=c(0,0),sds=c(1,1)){
  lower = qt(.025,df=sum(nsamp) - 2)
  upper = qt(.975,df=sum(nsamp) - 2)
  ts = replicate(nsim,
                 t.test(rnorm(nsamp[1],mean=means[1],sd=sds[1]),
                        rnorm(nsamp[2],mean=means[2],sd=sds[2]))$statistic)
  
  sum(ts < lower | ts > upper) / nsim
}


t.power(means=c(0,1))




