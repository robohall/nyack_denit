

library(lubridate)

iso_aug<-read_csv("Well_Aug26_2019_Final.csv")
iso_aug<- iso_aug %>% filter (!grepl("Std", Location)) #lose the standards


iso_aug$add_Time <- as.numeric( (hms(iso_aug$Collection_Time) - hms("10:55:00")) ) /60


plot( iso_aug$add_Time, iso_aug$InWellConductivity_uS_cm)

#some math
salt_add<- 50 *2100  # 50 g salt, 2100 uS per gram
bkg_salt<- mean(iso_aug$InWellConductivity_uS_cm[1:5])
n15_add <- (54*20) / (86/15)  # mg 15N

iso_aug$n15_conc <- (n15_add / salt_add) * (iso_aug$InWellConductivity_uS_cm - bkg_salt)

n30_bkg_ratio <- 0.003678^2
mims30_bkg_ratio <- mean( iso_aug$X30.28[1:5])

f01<- 0.01
N_30_conc_01 <- 0.5*f01* iso_aug$n15_conc #why 0.5?
N_30_ratio_01 <- N_30_conc_01 / iso_aug$X28.Conc

f001<- 0.001
N_30_conc_001 <- 0.5*f001* iso_aug$n15_conc
N_30_ratio_001 <- N_30_conc_001 / iso_aug$X28.Conc

plot( iso_aug$add_Time, N_30_ratio_01 + mims30_bkg_ratio, type="l", ylim=c(0.00016,0.00032), 
      ylab="(m/z 30) / (m/z 28)", xlab = "Time from addition start (min)", col="red", lwd=1.5  )
lines(iso_aug$add_Time, N_30_ratio_001 + mims30_bkg_ratio, col="blue", lwd=1.5)
points (iso_aug$add_Time, iso_aug$X30.28, pch= 16,col="black")
legend(200, 0.00030, legend=c("1% denitrified", "0.1% denitrified"),
       col=c("red", "blue"), lty=1, cex=0.8)

no3<- 0.03
mims29_bkg_ratio<- mean( iso_aug$del15N[1:5])


N_29_conc_001 <- N_30_conc_001*(-1 + ((N_30_conc_001+2*no3*f001)/N_30_conc_001)^0.5)
N_29_conc_01 <- N_30_conc_01*(-1 + ((N_30_conc_01+2*no3*f01)/N_30_conc_01)^0.5)
N_29_ratio_001 <- N_29_conc_001 / iso_aug$X28.Conc
N_29_ratio_01 <- N_29_conc_01 / iso_aug$X28.Conc

N_29_del_001<- (((N_29_ratio_001+0.003678)/0.003678)-1)*1000
N_29_del_01<- (((N_29_ratio_01+0.003678)/0.003678)-1)*1000

plot( iso_aug$add_Time, N_29_del_01 + mims29_bkg_ratio, type="l", ylim=c(-2,5), 
      ylab="del 15N", xlab = "Time from addition start (min)", col="red", lwd=1.5  )
lines(iso_aug$add_Time, N_29_del_001 + mims29_bkg_ratio, col="blue", lwd=1.5)
points (iso_aug$add_Time, iso_aug$del15N, pch= 16,col="black")
legend(200, 0.00030, legend=c("1% denitrified", "0.1% denitrified"),
       col=c("red", "blue"), lty=1, cex=0.8)

1/1000



#########

iso_jul19<-read_csv("Well_Jul19_2019_Final.csv")
iso_jul19<- iso_jul19 %>% filter (!grepl("Std", Location)) #lose the standards


iso_jul19$add_Time <- as.numeric( (hms(iso_jul19$Collection_Time) - hms("11:21:00")) ) /60


plot( iso_jul19$add_Time, iso_jul19$InWellConductivity_uS_cm)

#some math
salt_add<- 50 *2100  # 50 g salt, 2100 uS per gram
bkg_salt<- mean(iso_aug$InWellConductivity_uS_cm[1:5])
n15_add <- (28*20) / (86/15)  # mg 15N

iso_jul19$n15_conc <- (n15_add / salt_add) * (iso_jul19$InWellConductivity_uS_cm - bkg_salt)

n30_bkg_ratio <- 0.003678^2
mims30_bkg_ratio <- mean( iso_jul19$X30.28[1:5])

f01<- 0.01
N_30_conc_01 <- 0.5*f01* iso_jul19$n15_conc
N_30_ratio_01 <- N_30_conc_01 / iso_jul19$X28.Conc

f001<- 0.001
N_30_conc_001 <- 0.5*f001* iso_jul19$n15_conc
N_30_ratio_001 <- N_30_conc_001 / iso_jul19$X28.Conc

plot( iso_jul19$add_Time, N_30_ratio_01 + mims30_bkg_ratio, type="l", ylim=c(0.00024,0.00040), 
      ylab="(m/z 30) / (m/z 28)", xlab = "Time from addition start (min)", col="red", lwd=1.5  )
lines(iso_jul19$add_Time, N_30_ratio_001 + mims30_bkg_ratio, col="blue", lwd=1.5)
points (iso_jul19$add_Time, iso_jul19$X30.28, pch= 16,col="black")
legend(200, 0.00030, legend=c("1% denitrified", "0.1% denitrified"),
       col=c("red", "blue"), lty=1, cex=0.8)

no3<- 0.03
mims29_bkg_ratio<- mean( iso_jul19$del15N[1:5])


N_29_conc_001 <- N_30_conc_001*(-1 + ((N_30_conc_001+2*no3*f001)/N_30_conc_001)^0.5)
N_29_conc_01 <- N_30_conc_01*(-1 + ((N_30_conc_01+2*no3*f01)/N_30_conc_01)^0.5)
N_29_ratio_001 <- N_29_conc_001 / iso_jul19$X28.Conc
N_29_ratio_01 <- N_29_conc_01 / iso_jul19$X28.Conc

N_29_del_001<- (((N_29_ratio_001+0.003678)/0.003678)-1)*1000
N_29_del_01<- (((N_29_ratio_01+0.003678)/0.003678)-1)*1000

plot( iso_jul19$add_Time, N_29_del_01 + mims29_bkg_ratio, type="l", ylim=c(-2,5), 
      ylab="del 15N", xlab = "Time from addition start (min)", col="red", lwd=1.5  )
lines(iso_jul19$add_Time, N_29_del_001 + mims29_bkg_ratio, col="blue", lwd=1.5)
points (iso_jul19$add_Time, iso_jul19$del15N, pch= 16,col="black")
legend(200, 0.00030, legend=c("1% denitrified", "0.1% denitrified"),
       col=c("red", "blue"), lty=1, cex=0.8)


#########

iso_jul05<-read_csv("Well_Jul5_2019_Final.csv")
iso_jul05<- iso_jul05 %>% filter (!grepl("Std", Location)) #lose the standards


iso_jul05$add_Time <- as.numeric( (hms(iso_jul05$Collection_Time) - hms("11:21:00")) ) /60


plot( iso_jul05$add_Time, iso_jul05$InWellConductivity_uS_cm)

#some math
salt_add<- 50 *2100  # 50 g salt, 2100 uS per gram
bkg_salt<- mean(iso_aug$InWellConductivity_uS_cm[1:2])
n15_add <- (10*1) / (86/15)  # mg 15N

iso_jul05$n15_conc <- (n15_add / salt_add) * (iso_jul05$InWellConductivity_uS_cm - bkg_salt)

n30_bkg_ratio <- 0.003678^2
mims30_bkg_ratio <- mean( iso_jul05$X30.28[1:2])
mims29_bkg_ratio<- mean( iso_jul05$del15N[1:2])

f01<- 0.01
N_30_conc_01 <- 0.5*f01* iso_jul05$n15_conc
N_30_ratio_01 <- N_30_conc_01 / iso_jul05$X28.Conc

f001<- 0.001
N_30_conc_001 <- 0.5*f001* iso_jul05$n15_conc
N_30_ratio_001 <- N_30_conc_001 / iso_jul05$X28.Conc

plot( iso_jul05$add_Time, N_30_ratio_01 + mims30_bkg_ratio, type="l", ylim=c(.0005,.00071), 
      ylab="(m/z 30) / (m/z 28)", xlab = "Time from addition start (min)", col="red", lwd=1.5  )
lines(iso_jul05$add_Time, N_30_ratio_001 + mims30_bkg_ratio, col="blue", lwd=1.5)
points (iso_jul05$add_Time, iso_jul05$X30.28, pch= 16,col="black")
legend(200, 0.00030, legend=c("1% denitrified", "0.1% denitrified"),
       col=c("red", "blue"), lty=1, cex=0.8)


no3<- 0.03
  
N_29_conc_001 <- N_30_conc_001*(-1 + ((N_30_conc_001+2*no3*f001)/N_30_conc_001)^0.5)
N_29_conc_01 <- N_30_conc_01*(-1 + ((N_30_conc_01+2*no3*f01)/N_30_conc_01)^0.5)
N_29_ratio_001 <- N_29_conc_001 / iso_jul05$X28.Conc
N_29_ratio_01 <- N_29_conc_01 / iso_jul05$X28.Conc

N_29_del_001<- (((N_29_ratio_001+0.003678)/0.003678)-1)*1000
N_29_del_01<- (((N_29_ratio_01+0.003678)/0.003678)-1)*1000

plot( iso_jul05$add_Time, N_29_del_01 + mims29_bkg_ratio, type="l", ylim=c(-15,5), 
      ylab="del 15N", xlab = "Time from addition start (min)", col="red", lwd=1.5  )
lines(iso_jul05$add_Time, N_29_del_001 + mims29_bkg_ratio, col="blue", lwd=1.5)
points (iso_jul05$add_Time, iso_jul05$del15N, pch= 16,col="black")
legend(200, 0.00030, legend=c("1% denitrified", "0.1% denitrified"),
       col=c("red", "blue"), lty=1, cex=0.8)


##LINX data



#linxK <- scan()
#linxK<-data.frame(K=linxK)
#write.csv(linxK, file ="linxK.csv")
linxK<-read.csv("linxK.csv")

ggplot(linxK, aes(K)) +               # Histogram with log10 axis
  geom_histogram(bins = 20, colour="black", fill="lightblue")  +
  scale_x_log10() + geom_vline(aes(xintercept=0.002),
                             color="blue", linetype="dashed", size=1) +
  labs(x="Denitrification rate (1/h) ", y = "Count") +
  theme_classic()+
  theme(text=element_text(size=15),  plot.margin = unit(c(1, 1, 1, 1), "cm"  ) )

###ignore
b<-0.0018
d<-0.02/12



N29<- b*(-1 + ((b+2*d)/b)^0.5)


N29/15

mu<-0.5
phi<-1

a<-mu*phi
b<-  (1-mu)*phi

x<- seq(0.01,0.99,0.01)
plot(x, dbeta(x, a,b), type="l")
