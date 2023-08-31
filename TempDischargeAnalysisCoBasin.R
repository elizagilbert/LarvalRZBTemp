library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(brms)
library(tidybayes)
library(cowplot)

setwd("R:/San_Juan/Working_Projects/RZB_Temperature_Study/Field_Temp_Anlyses")
SJR<-read.csv("SJR_temps_cfs.csv",header=T)

SJR$year<-as.factor(SJR$year)

SJR2<-SJR %>%
  group_by(period,DOY) %>%
  summarize(mean_cms=mean(cms),sd_cms=sd(cms))

SJR2$period<-factor(SJR2$period,levels=c("Pre","Post"),labels=c("Pre dam","Post dam"))

tiff("SJR_cfs.jpg", units="in", width=5, height=5, res=600)
P_SJR<-ggplot(SJR2,aes(x=DOY,y=mean_cms,group=period))+
  geom_line(aes(x=DOY,y=mean_cms,color=period),size=1)+
  geom_ribbon(aes(ymin=mean_cms-sd_cms,ymax=mean_cms+sd_cms,fill=period),alpha=.3)+
  scale_color_manual(values=c("black","blue"))+
  scale_fill_manual(values=c("black","blue"))+
  coord_cartesian(ylim=c(0,400),expand=F)+
  labs(title="San Juan River",y=expression("Discharge" ~(m^3/s)),x="Day of Year")+
  theme_bw(base_size = 18,base_line_size = 1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right","top"),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
P_SJR
dev.off()
###Subset the data to only high discharge
aggregate(SJR$cfs,by=list(SJR$period), mean)


SJR3<-subset(SJR,season=="Spawn")
SJR_flood<-subset(SJR3,cfs>5649)
SJR_flood<-SJR_flood[!is.na(SJR_flood$temp),]

######## set priors
get_prior(temp ~ DOY*period+(1|year),
          data = SJR_flood)

prs <-  c(set_prior("normal(0,1)",class = "b",coef="DOY"),
          set_prior("normal(0,10)",class = "b",coef="periodPre"),
          set_prior("normal(0,1)",class = "b",coef="DOY:periodPre"),
          set_prior("cauchy(0,1)", class = "sd"),
          set_prior("normal(0,10)",class="Intercept"))

Pr_dist<-brm(temp ~ DOY*period+(1|year), data=SJR_flood,
             prior = prs,
             chains = 1,
             iter=1000,
             sample_prior = "only")
plot(Pr_dist)
conditional_effects(Pr_dist,effects="DOY:period")


### Run the Bayes model
m.SJR<-brm(temp ~ DOY*period+(1|year),data=SJR_flood,
         prior = prs,
         chains = 3,
         warmup = 500,
         iter=4000,
         cores = 4,
         sample_prior = TRUE)

plot(m.SJR)
pp_check(m.SJR,nsamples=100)
print(summary(m.SJR,prob=.95),digits=4)
conditional_effects(m.SJR,effects="DOY:period")


saveRDS(m.SJR, file = here("Z:/Working_Projects/RZB_Temperature_Study/Field_Temp_Anlyses/m.SJR.rds"))
m.SJR <- readRDS(here("Z:/Working_Projects/RZB_Temperature_Study/Field_Temp_Anlyses/m.SJR.rds"))


SJR1<-conditional_effects(m.SJR, prob=0.95, points=TRUE,effects="DOY:period")

p<-plot(SJR1,plot=FALSE,
        points=TRUE,color="period")[[1]]

mycol <- rgb(0, 0, 255, max = 255, alpha = 25, names = "blue50")

tiff("SJR_temp.jpg", units="in", width=5, height=5, res=600)
plot1<-p+
  ylab("Temperature (C)")+xlab("Day of Year")+
  theme_classic(base_size = 18,
                base_line_size = 1)+
  scale_fill_manual(values = c("#0000FF19","black"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_color_manual(values = c("#0000FF19","lightgrey"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_x_continuous(breaks=c(90,110,130,150,170,190,210))+
  scale_y_continuous(breaks=c(10,15,20,25,30))+
  theme(legend.position = c(.01,1),
  legend.justification = c("left","top"))+ 
  guides(color = guide_legend(reverse = TRUE))+
  guides(fill = guide_legend(reverse = TRUE))

plot1
dev.off()

###### Get specific estimates at differing times of year
ps<-posterior_samples(m.SJR)

quantile(ps$b_Intercept+ps$b_DOY*100,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*100,probs=c(0.5,0.025,0.975))

quantile(ps$b_Intercept+ps$b_DOY*150,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*150,probs=c(0.5,0.025,0.975))

quantile(ps$b_Intercept+ps$b_DOY*200,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*200,probs=c(0.5,0.025,0.975))

####Green River analysis

GR<-read.csv("GreenRiver_temp_cfs.csv",header=T)

GR$year<-as.factor(GR$year)

GR2<-GR %>%
  group_by(period,DOY) %>%
  summarize(mean_cms=mean(cms),sd_cms=sd(cms))
GR2<-GR2[2:1099,]

GR2$period<-factor(GR2$period,levels=c("Pre","Post","TCD"),labels=c("Pre dam","Post dam","Post TCD"))

tiff("GR_cfs.jpg", units="in", width=5, height=5, res=600)
P_GR<-ggplot(GR2,aes(x=DOY,y=mean_cms,group=period))+
  geom_line(aes(x=DOY,y=mean_cms,color=period),size=1)+
  geom_ribbon(aes(ymin=mean_cms-sd_cms,ymax=mean_cms+sd_cms,fill=period),alpha=.3)+
  scale_color_manual(values=c("black","blue","green"))+
  scale_fill_manual(values=c("black","blue","green"))+
  coord_cartesian(ylim=c(0,1000),expand=F)+
  labs(title="Green River",y=expression("Discharge" ~(m^3/s)),x="Day of Year")+
  theme_bw(base_size = 18,base_line_size = 1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right","top"),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
P_GR
dev.off()

####Subset the data to only floods

###Subset the data to only high discharge
aggregate(GR$cfs,by=list(GR$period), mean)

GR_spawn<-subset(GR,season=="spawn")
GR_flood<-subset(GR_spawn,cfs>5500)

#### Setting priors
get_prior(temp ~ DOY*period+(1|year), data=GR_flood)

prs <-  c(set_prior("normal(0,1)",class = "b",coef="DOY"),
          set_prior("normal(0,10)",class = "b",coef="periodPre"),
          set_prior("normal(0,10)",class = "b",coef="periodTCD"),
          set_prior("normal(0,1)",class = "b",coef="DOY:periodPre"),
          set_prior("normal(0,1)",class = "b",coef="DOY:periodTCD"),
          set_prior("cauchy(0,1)", class = "sd"),
          set_prior("normal(0,10)",class="Intercept"))

Pr_dist<-brm(temp ~ DOY*period+(1|year), data=GR_flood,
             prior = prs,
             chains = 1,
             iter=1000,
             sample_prior = "only")

conditional_effects(Pr_dist)

GR_flood$pperiod<-factor(GR_flood$period,levels=c("aPre","bPost","cTCD"))


### Run the Bayes model
m.GR<-brm(temp ~ DOY*period+(1|year),data=GR_flood,
         prior = prs,
         chains = 3,
         warmup = 500,
         iter=4000,
         sample_prior = TRUE)

plot(m.GR)
pp_check(m.GR,nsamples=100)
print(summary(m.GR,prob=.95),digits=4)
conditional_effects(m.GR,effects="DOY:period")

saveRDS(m.GR, file = here("Z:/Working_Projects/RZB_Temperature_Study/Field_Temp_Anlyses/m.GR.rds"))
m.GR <-readRDS(here("Z:/Working_Projects/RZB_Temperature_Study/Field_Temp_Anlyses/m.GR.rds"))

GR1<-conditional_effects(m.GR, prob=0.95, points=TRUE,effects="DOY:period")

p_GR<-plot(GR1,plot=FALSE,
        points=TRUE,color="period")[[1]]

mycol <- rgb(0, 255, 0, max = 255, alpha = 25, names = "blue50")

tiff("GR_temp.jpg", units="in", width=5, height=5, res=600)
plot_GR<-p_GR+
  ylab("Temperature (C)")+xlab("Day of Year")+
  theme_classic(base_size = 18,
                base_line_size = 1)+
  scale_fill_manual(values = c("black","#0000FF19","#00FF0019"),name="",
                    breaks=c("Pre","Post","TCD"),
                    labels=c("Pre"="Pre dam","Post"="Post dam","TCD"="Post TCD"))+
  scale_color_manual(values = c("lightgrey","#0000FF19","#00FF0019"),name="",
                     breaks=c("Pre","Post","TCD"),
                    labels=c("Pre"="Pre dam","Post"="Post dam","TCD"="Post TCD"))+
  scale_x_continuous(breaks=c(90,110,130,150,170,190,210))+
  scale_y_continuous(breaks=c(10,15,20,25,30))+
  theme(legend.position = c(0.01,1),
        legend.justification = c("left","top"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

plot_GR
dev.off()

###### Get specific estimates at differing times of year
ps<-posterior_samples(m.GR)

quantile(ps$b_Intercept+ps$b_DOY*100,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*100,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodTCD+(ps$b_DOY+ps$`b_DOY:periodTCD`)*100,probs=c(0.5,0.025,0.975))

quantile(ps$b_Intercept+ps$b_DOY*150,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*150,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodTCD+(ps$b_DOY+ps$`b_DOY:periodTCD`)*150,probs=c(0.5,0.025,0.975))

quantile(ps$b_Intercept+ps$b_DOY*200,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*200,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodTCD+(ps$b_DOY+ps$`b_DOY:periodTCD`)*200,probs=c(0.5,0.025,0.975))

####Lees Ferry analysis

LF<-read.csv("lees_temp_cfs.csv",header=T)

LF$year<-as.factor(LF$year)

LF2<-LF %>%
  group_by(period,DOY) %>%
  summarize(mean_cms=mean(cms),sd_cms=sd(cms))

LF2$period<-factor(LF2$period,levels=c("Pre","Post"),labels=c("Pre dam","Post dam"))

tiff("LF_cfs.jpg", units="in", width=5, height=5, res=600)
P_LF<-ggplot(LF2,aes(x=DOY,y=mean_cms,group=period))+
  geom_line(aes(x=DOY,y=mean_cms,color=period),size=1)+
  geom_ribbon(aes(ymin=mean_cms-sd_cms,ymax=mean_cms+sd_cms,fill=period),alpha=.3)+
  scale_color_manual(values=c("black","blue"))+
  scale_fill_manual(values=c("black","blue"))+
  coord_cartesian(ylim=c(0,3000),expand=F)+
  labs(title="Colorado River",y=expression("Discharge" ~(m^3/s)),x="Day of Year")+
  theme_bw(base_size = 18,base_line_size = 1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right","top"),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
P_LF
dev.off()


###Subset for floods
aggregate(LF$cfs,by=list(LF$period), mean)

LF3<-subset(LF,season=="spawn")
LF_flood<-subset(LF3,cfs>40000)


#### Setting priors
get_prior(temp ~ DOY*period+(1|year), data=LF_flood)

prs <-  c(set_prior("normal(0,1)",class = "b",coef="DOY"),
          set_prior("normal(0,10)",class = "b",coef="periodPre"),
          set_prior("normal(0,1)",class = "b",coef="DOY:periodPre"),
          set_prior("cauchy(0,1)", class = "sd"),
          set_prior("normal(0,10)",class="Intercept"))

Pr_dist<-brm(temp ~ DOY*period+(1|year), data=LF_flood,
             prior = prs,
             chains = 1,
             iter=1000,
             sample_prior = "only")

conditional_effects(Pr_dist,effects="DOY:period")
### Run the Bayes model

m.LF<-brm(temp ~ DOY*period+(1|year),data=LF_flood,
         prior = prs,
         chains = 3,
         warmup = 500,
         iter=4000,
         sample_prior = TRUE)

plot(m.LF)

pp_check(m.LF,nsamples=100)
conditional_effects(m.LF)
print(summary(m.LF,prob=.95),digits=4)

saveRDS(m.LF, file = here("Z:/Working_Projects/RZB_Temperature_Study/Field_Temp_Anlyses/m.LF.rds"))
m.LF <- readRDS(here("Z:/Working_Projects/RZB_Temperature_Study/Field_Temp_Anlyses/m.LF.rds"))

LF1<-conditional_effects(m.LF, prob=0.95, points=TRUE,effects="DOY:period")

p3<-plot(LF1,plot=FALSE,
        points=TRUE,color="period")[[1]]

tiff("LF_temp.jpg", units="in", width=5, height=5, res=600)
LF1<-p3+
  ylab("Temperature (C)")+xlab("Day of Year")+
  theme_classic(base_size = 18,
                base_line_size = 1)+
  scale_fill_manual(values = c("#0000FF19","black"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_color_manual(values = c("#0000FF19","lightgrey"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_x_continuous(breaks=c(90,110,130,150,170,190,210))+
  scale_y_continuous(breaks=c(10,15,20,25,30))+
  theme(legend.position = c(.01,1),
        legend.justification = c("left","top"))+ 
  guides(color = guide_legend(reverse = TRUE))+
  guides(fill = guide_legend(reverse = TRUE))

LF1
dev.off()

###### Get specific estimates at differing times of year
ps<-posterior_samples(m.LF)

quantile(ps$b_Intercept+ps$b_DOY*100,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*100,probs=c(0.5,0.025,0.975))

quantile(ps$b_Intercept+ps$b_DOY*150,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*150,probs=c(0.5,0.025,0.975))

quantile(ps$b_Intercept+ps$b_DOY*200,probs=c(0.5,0.025,0.975))
quantile(ps$b_Intercept+ps$b_periodPre+(ps$b_DOY+ps$`b_DOY:periodPre`)*200,probs=c(0.5,0.025,0.975))

###Combine figures
grid.arrange(plot1,plot2,plot3,ncol=3)


plot_grid(P_SJR,P_GR,P_LF, 
          ncol = 1, 
          nrow = 3)

plot_grid(plot1,plot_GR,LF1, 
          ncol = 1, 
          nrow = 3)

plot_grid(P_SJR,P_GR,P_LF,plot1,plot_GR,LF1,
          ncol = 3, 
          nrow = 2,
          label_size = 10)

###Removing lower panels' legends
plot1b<-p+
  ylab("Temperature (C)")+xlab("Day of Year")+
  theme_classic(base_size = 12,
                base_line_size = 1)+
  scale_fill_manual(values = c("#0000FF19","black"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_color_manual(values = c("#0000FF19","lightgrey"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_x_continuous(breaks=c(90,110,130,150,170,190,210))+
  scale_y_continuous(breaks=c(10,15,20,25,30))+
  theme(legend.position = "none")

plot1b

plot_GRb<-p_GR+
  ylab("")+xlab("Day of Year")+
  theme_classic(base_size = 12,
                base_line_size = 1)+
  scale_fill_manual(values = c("black","#0000FF19","#00FF0019"),name="",
                    breaks=c("Pre","Post","TCD"),
                    labels=c("Pre"="Pre dam","Post"="Post dam","TCD"="Post TCD"))+
  scale_color_manual(values = c("lightgrey","#0000FF19","#00FF0019"),name="",
                     breaks=c("Pre","Post","TCD"),
                     labels=c("Pre"="Pre dam","Post"="Post dam","TCD"="Post TCD"))+
  scale_x_continuous(breaks=c(90,110,130,150,170,190,210))+
  scale_y_continuous(breaks=c(10,15,20,25,30))+
  theme(legend.position = "none")
plot_GRb

LF1b<-p3+
  ylab("")+xlab("Day of Year")+
  theme_classic(base_size = 12,
                base_line_size = 1)+
  scale_fill_manual(values = c("#0000FF19","black"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_color_manual(values = c("#0000FF19","lightgrey"),name="",labels=c("Pre"="Pre dam","Post"="Post dam"))+
  scale_x_continuous(breaks=c(90,110,130,150,170,190,210))+
  scale_y_continuous(breaks=c(10,15,20,25,30))+
  theme(legend.position = "none")

LF1b

plot_grid(P_SJR,P_GR,P_LF,plot1b,plot_GRb,LF1b,
          ncol = 3, 
          nrow = 2,
          label_size = 10)









