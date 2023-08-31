####temp analysis
library(lme4)
library(effects)
library(ggplot2)
library(tidyverse)
library(tibble)
library(dplyr)

dat<-read.csv("FINAL_Field_for_R.csv", header=T)

dat$meanCz<-scale(dat$meanC)
dat$Year<-as.factor(dat$Year)

### Testing size differences in different stages
##Look at ML estimates of different model fits
mod1<-lmer(TL~meanCz*Stage+(1|Year),data=dat)
mod2<-lmer(TL~meanCz*Stage+(meanCz|Year),data=dat)

AIC(mod1)
AIC(mod2)

x<-effect(term="meanCz*Stage",mod=mod2)
plot(x)

dat$pred1<-predict(mod2)
ggplot(dat,aes(meanCz,TL))+
  geom_point()+
  facet_grid(.~Stage)+
  geom_line(colour="blue",aes(y=pred1,group=Year))

###Testing for Years with the lowest and highest temp
model<-lm(meanC~Year,data=dat)
summary(model)
boxplot(dat$meanCz~dat$Year)
x<-effect(term="Year",mod=model)
plot(x)
dat1<-subset(dat,(Year==2018|Year==2019))

ggplot(data=dat1, aes(x=meanC, group=Year, fill=Year)) +
  geom_density(adjust=5, alpha=.5)

ggplot(data=dat1, aes(x=TL, group=Year, fill=Year)) +
  geom_density(adjust=5, alpha=.5)





#### Try again averaging by site collection
dat2<-read.csv("Means_for_R.csv", header=T)

dat2$meanCz<-scale(dat2$meanC)
dat2$Year<-as.factor(dat2$Year)

### Testing size differences in different stages
##Look at ML estimates of different model fits
mod3<-lmer(mTL~meanCz*Stage+(1|Year),data=dat2)
mod4<-lmer(mTL~meanCz*Stage+(meanCz|Year),data=dat2)

AIC(mod3)
AIC(mod4)

x<-effect(term="meanCz*Stage",mod=mod4)
plot(x)

dat2$pred1<-predict(mod4)

ggplot(dat2,aes(meanCz,mTL))+
  geom_point()+
  facet_grid(.~Stage)+
  geom_line(colour="blue",aes(y=pred1,group=Year))


########## Testing RM as Random Effect
dat2$RM<-as.factor(dat2$RM)
mod5<-lmer(mTL~meanCz*Stage+(1|Year)+(1|RM),data=dat2)
mod6<-lmer(mTL~meanCz*Stage+(meanCz|Year)+(1|RM),data=dat2)

AIC(mod5)
AIC(mod6)

x<-effect(term="meanCz*Stage",mod=mod6)
plot(x)

summary(dat2$Year)

        


























