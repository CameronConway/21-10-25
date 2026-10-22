#tuesday
library(vegan)
library(tidyverse)
library(betapart)
dat.all <- read.csv("C:/Users/camer/Downloads/PaciFlora/PaciFlora/Species_list_full_2905.csv", header= T, sep= ';')
which(is.na(dat.all$species)) #which species have NAs
which(is.na(dat.all$island)) #which islands have NAs
dat.all<-dat.all[-which(is.na(dat.all$species)),]
dat.all<-dat.all[-which(is.na(dat.all$island)),]

dat.soc<- dat.all[which(dat.all$islandgroup=='Society'),]
dat.mar<- dat.all[which(dat.all$islandgroup=='Marquesas'),]
dat.haw<- dat.all[which(dat.all$islandgroup=='Hawaiian'),]
dat.sam<- dat.all[which(dat.all$islandgroup=='Samoa'),]
dat.fij<- dat.all[which(dat.all$islandgroup=='Fiji'),]
dat.comb<- rbind(dat.soc,dat.mar,dat.haw,dat.sam,dat.fij)
?merge

dat.soc.red <- dat.soc[,c('species','island')]
dat.soc.red$presence <- 1
dat.soc.pa <- dat.soc.red %>%
pivot_wider(names_from=species, values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.soc.pa)))
names(list0) <- names(dat.soc.pa)
dat.soc.pa <- as.data.frame(dat.soc.pa %>% replace_na(list0))
row.names(dat.soc.pa) <- dat.soc.pa$island
dat.soc.pa <- dat.soc.pa[,-1]

dat.mar.red <- dat.mar[,c('species','island')]
dat.mar.red$presence <- 1
dat.mar.pa <- dat.mar.red %>%
  pivot_wider(names_from=species, values_from=c(presence))
list1 <- as.list(rep(0,ncol(dat.mar.pa)))
names(list1) <- names(dat.mar.pa)
dat.mar.pa <- as.data.frame(dat.mar.pa %>% replace_na(list1))
row.names(dat.mar.pa) <- dat.mar.pa$island
dat.mar.pa <- dat.soc.pa[,-1]

dat.haw.red <- dat.haw[,c('species','island')]
dat.haw.red$presence <- 1
dat.haw.pa <- dat.haw.red %>%
  pivot_wider(names_from=species, values_from=c(presence))
list2 <- as.list(rep(0,ncol(dat.haw.pa)))
names(list2) <- names(dat.haw.pa)
dat.haw.pa <- as.data.frame(dat.haw.pa %>% replace_na(list2))
row.names(dat.haw.pa) <- dat.haw.pa$island
dat.haw.pa <- dat.haw.pa[,-1]

dat.sam.red <- dat.sam[,c('species','island')]
dat.sam.red$presence <- 1
dat.sam.pa <- dat.sam.red %>%
  pivot_wider(names_from=species, values_from=c(presence))
list3 <- as.list(rep(0,ncol(dat.sam.pa)))
names(list3) <- names(dat.sam.pa)
dat.sam.pa <- as.data.frame(dat.sam.pa %>% replace_na(list3))
row.names(dat.sam.pa) <- dat.sam.pa$island
dat.sam.pa <- dat.sam.pa[,-1]

dat.fij.red <- dat.fij[,c('species','island')]
dat.fij.red$presence <- 1
dat.fij.pa <- dat.fij.red %>%
  pivot_wider(names_from=species, values_from=c(presence))
list4 <- as.list(rep(0,ncol(dat.fij.pa)))
names(list4) <- names(dat.fij.pa)
dat.fij.pa <- as.data.frame(dat.fij.pa %>% replace_na(list4))
row.names(dat.fij.pa) <- dat.fij.pa$island
dat.fij.pa <- dat.fij.pa[,-1]
dat.soc.pa
View(dat.soc.pa)

dat.comb.red <- dat.comb[,c('species','island')]
dat.comb.red$presence <- 1
dat.comb.pa <- dat.comb.red %>%
  pivot_wider(names_from=species, values_from=c(presence))
list5 <- as.list(rep(0,ncol(dat.comb.pa)))
names(list5) <- names(dat.comb.pa)
dat.comb.pa <- as.data.frame(dat.comb.pa %>% replace_na(list5))
row.names(dat.comb.pa) <- dat.comb.pa$island
dat.comb.pa <- dat.comb.pa[,-1]
dat.comb.pa

gamma.soc <- ncol(dat.soc.pa) #gamma
gamma.mar <- ncol(dat.mar.pa)
gamma.haw <- ncol(dat.haw.pa)
gamma.sam <- ncol(dat.sam.pa)
gamma.fij <- ncol(dat.fij.pa)

alpha.soc <- mean(rowSums(dat.soc.pa)) #alpha
alpha.mar <- mean(rowSums(dat.mar.pa))
alpha.haw <- mean(rowSums(dat.haw.pa))
alpha.sam <- mean(rowSums(dat.sam.pa))
alpha.fij <- mean(rowSums(dat.fij.pa))

?specaccum
socsar<- specaccum(dat.soc.pa)
plot(socsar)
marsar<- specaccum(dat.mar.pa)
plot(marsar)
hawsar<- specaccum(dat.haw.pa)
plot(hawsar)
samsar<- specaccum(dat.sam.pa)
plot(samsar)
fijsar<- specaccum(dat.fij.pa)
plot(fijsar)

?poolaccum
poolsoc<- poolaccum(dat.soc.pa)
plot(poolsoc)
poolmar<- poolaccum(dat.mar.pa)
plot(poolmar)
poolhaw<- poolaccum(dat.haw.pa)
plot(poolhaw)
poolsam<- poolaccum(dat.sam.pa)
plot(poolsam)
poolfij<- poolaccum(dat.fij.pa)
plot(poolfij)

??betapair
beta.pair(dat.soc.pa)
beta.pair(dat.mar.pa)
beta.pair(dat.haw.pa)
beta.pair(dat.sam.pa)
beta.pair(dat.fij.pa)
beta.comb<-beta.pair(dat.comb.pa)
cmdscale(beta.comb)
?cmdscale

coord.comb.sim <- data.frame(cmdscale(beta.comb$beta.sim))
coord.comb.sim$col <-
  c(rep("blue",nrow(dat.soc.pa)),rep("red",nrow(dat.haw.pa)),rep("darkgreen",nrow(dat.sam.pa)),rep("orange",nrow(dat.mar.pa)),rep("purple"
                                                            ,nrow(dat.fij.pa)))
coord.comb.sim$pch <-
  c(rep(0,nrow(dat.soc.pa)),rep(1,nrow(dat.haw.pa)),rep(2,nrow(dat.sam
                                                               .pa)),rep(3,nrow(dat.mar.pa)),rep(4,nrow(dat.fij.pa)))
coord.comb.sor <- data.frame(cmdscale(beta.comb$beta.sor))
coord.comb.sor$col <-
  c(rep("blue",nrow(dat.soc.pa)),rep("red",nrow(dat.haw.pa)),rep("dark
green",nrow(dat.sam.pa)),rep("orange",nrow(dat.mar.pa)),rep("purple"
                                                            ,nrow(dat.fij.pa)))
coord.comb.sor$pch <-
  c(rep(0,nrow(dat.soc.pa)),rep(1,nrow(dat.haw.pa)),rep(2,nrow(dat.sam
                                                               .pa)),rep(3,nrow(dat.mar.pa)),rep(4,nrow(dat.fij.pa)))
par(mfrow=c(1,3))
plot(coord.comb.sim$X1,coord.comb.sim$X2,col=coord.comb.sim$col,pch=
       coord.comb.sim$pch,lwd=2,main="Simpson")
plot(coord.comb.sor$X1,coord.comb.sor$X2,col=coord.comb.sor$col,pch=
       coord.comb.sor$pch,lwd=2,main="Sorensen")
plot.new()
legend(x="topleft",legend=c("Society","Hawai’i","Samoa","Marquesas",
                            "Fiji"),col =
         c("blue","red","darkgreen","orange","purple"),pch=0:4,lwd=2,bty="n",
       lty=0,cex=2)