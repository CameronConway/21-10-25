install.packages('devtools', repos='https://cloud.r-project.org')
devtools::install_github('jarrodhadfield/sda')
library(sda)
View(collage_photo)
#a
#responsy~ predx
lma<-lm(y ~ speed, data=collage_photo)
coef(lma)
#0.1319 so 13% more
#b
?rep
pred<-rep(collage_photo$person)
pred
lmb<-lm(y~pred, data=collage_photo)
lmb
#c
#not looking at just speed, as people had previous 5 second ones to get used to it
#so not a fair comparison between them
#d
plot(lmb)
#in groups instead of spread out
#e
#glm w loglink poisson
model<-glm(y~speed, data=collage_photo, family=poisson)
coef(model)
#13% 
#f
model1<-glm(y~speed, data=collage_photo)
#log doesnt work
#g
#binomial logit link
model2<- glm(y~ speed, data=collage_photo, family=binomial)
#cant 