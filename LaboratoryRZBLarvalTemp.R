library(brms)
library(here)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggnewscale)
library(cowplot)

options(mc.cores = parallel::detectCores())

dat2<-read.csv("FINAL_for_R.csv", header=T)

### Summarize data for results section
dat2 %>% 
  group_by(time) %>% 
  summarize(mean(sl),min(sl),max(sl))

dat2 %>% 
  group_by(time) %>% 
  summarize(mean(total_rays2),min(total_rays2),max(total_rays2))

######## Non regression approach
###############################
get_prior(sl ~ 0+temp*time+(1|female)+(1|tank),
          data = dat2,family="lognormal")

hist(rnorm(1000, 2, 5))

pr1 <- c(set_prior("normal(2,5)",class = "b"))

### Fit interaction model
m.size<-brm(sl ~ 0+temp*time+(1|female)+(1|tank), data=dat2,
             prior = pr1,
             chains = 3,
             warmup = 1000,
             iter=5000,
            family="lognormal",
            cores = 4,
             sample_prior = TRUE)

plot(m.size)
print(m.size,digits=4)
summary(m.size,digits=4)
fixef(m.size)
saveRDS(m.size, "m.size.rds")
m.size <- readRDS("m.size.rds")

conditional_effects(m.size,effect="time:temp")
pp_check(m.size)

### Plot the results
ps<-conditional_effects(m.size,effect="time:temp",probs = c(0.025,0.975))
np<-ps[[1]]

labs<-c("10 Days","20 Days","42 Days")
np$temp <- factor(np$temp, levels = c("20C", "18C", "16C"))


p<-ggplot(np,aes(x=time,y=estimate__,col=temp,group=temp,shape=temp,fill=temp))+
  geom_point(size=1.5, position=position_dodge(width=.3))+
  geom_errorbar(aes(x=time,ymax=upper__,ymin=lower__),
                position=position_dodge(width=0.3),
                width=.1)

p1<-p+ylab("Standard Length (mm)")+xlab("Time since hatch")+
  scale_fill_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_color_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_shape_manual(values=c(24,21,25))+
  theme_classic(base_size = 12,
                base_line_size = 1)+
  scale_x_discrete(labels=labs)+
  scale_y_continuous(limits=c(8,18),breaks=c(8,10,12,14,16,18))+
  theme(legend.position = "none")
p1

### Posterior predictions for size
nd<-crossing(temp=c("16C","18C","20C"),
             time=c("time1","time2","time3"))


size.preds<-posterior_epred(m.size,
                              newdata = nd,
                              re_formula = NA,
                              ndraws=10000)

size.preds<-as.data.frame(size.preds)

#time 1
quantile(size.preds$V1,probs=c(0.5,0.025,.975))
quantile(size.preds$V4,probs=c(0.5,0.025,.975))
quantile(size.preds$V7,probs=c(0.5,0.025,.975))

sum(size.preds$V4-size.preds$V1 > 0)/10000
sum(size.preds$V7-size.preds$V1 > 0)/10000
sum(size.preds$V7-size.preds$V4 > 0)/10000

#time2
quantile(size.preds$V2,probs=c(0.5,0.025,.975))
quantile(size.preds$V5,probs=c(0.5,0.025,.975))
quantile(size.preds$V8,probs=c(0.5,0.025,.975))

sum(size.preds$V5-size.preds$V2 > 0)/10000
sum(size.preds$V8-size.preds$V2 > 0)/10000
sum(size.preds$V8-size.preds$V5 > 0)/10000

#time3
quantile(size.preds$V3,probs=c(0.5,0.025,.975))
quantile(size.preds$V6,probs=c(0.5,0.025,.975))
quantile(size.preds$V9,probs=c(0.5,0.025,.975))

sum(size.preds$V6-size.preds$V3 > 0)/10000
sum(size.preds$V9-size.preds$V3 > 0)/10000
sum(size.preds$V9-size.preds$V6 > 0)/10000

########### Rays
hist(rnorm(1000,30,50))

pr2 <- c(set_prior("normal(20,40)",class = "b"))

m.rays2<-brm(total_rays2 ~ 0+temp*time+(1|female)+(1|tank), data=dat2,
            prior = pr2,
            chains = 3,
            warmup = 1000,
            iter=5000,
            cores = 4,
            sample_prior = TRUE)

pp_check(m.rays2)
print(m.rays2, digits=4)

conditional_effects(m.rays2,effect="time:temp")
pp_check(m.rays)
fixef(m.rays2)

saveRDS(m.rays2, "m.rays2.rds")
m.rays2 <- readRDS("m.rays2.rds")

### Plot the results
ps<-conditional_effects(m.rays2,effect="time:temp",probs = c(0.025,0.975))
np<-ps[[1]]

labs<-c("10 Days","20 Days","42 Days")
np$temp <- factor(np$temp, levels = c("20C", "18C", "16C"))

p<-ggplot(np,aes(x=time,y=estimate__,col=temp,group=temp,fill=temp,shape=temp))+
  geom_point(size=1.5, position=position_dodge(width=.3))+
  geom_errorbar(aes(x=time,ymax=upper__,ymin=lower__),
                position=position_dodge(width=0.3),
                width=.1)

p2<-p+ylab("Total number of rays")+xlab("Time since hatch")+
  scale_fill_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_color_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_shape_manual(values=c(24,21,25))+
  theme_classic(base_size = 12,
                base_line_size = 1)+
  scale_y_continuous(breaks=c(0,10,20,30,40))+
  coord_cartesian(ylim=c(0,40))+
  scale_x_discrete(labels=labs)+
  theme(legend.position = "none")
p2

### Posterior predictions for fin rays
nd<-crossing(temp=c("16C","18C","20C"),
             time=c("time1","time2","time3"))


fin.preds<-posterior_epred(m.rays2,
                            newdata = nd,
                            re_formula = NA,
                            ndraws=10000)

fin.preds<-as.data.frame(fin.preds)

#time 1
quantile(fin.preds$V1,probs=c(0.5,0.025,.975))
quantile(fin.preds$V4,probs=c(0.5,0.025,.975))
quantile(fin.preds$V7,probs=c(0.5,0.025,.975))

sum(fin.preds$V4-fin.preds$V1 > 0)/10000
sum(fin.preds$V7-fin.preds$V1 > 0)/10000
sum(fin.preds$V7-fin.preds$V4 > 0)/10000

#time2
quantile(fin.preds$V2,probs=c(0.5,0.025,.975))
quantile(fin.preds$V5,probs=c(0.5,0.025,.975))
quantile(fin.preds$V8,probs=c(0.5,0.025,.975))

sum(fin.preds$V5-fin.preds$V2 > 0)/10000
sum(fin.preds$V8-fin.preds$V2 > 0)/10000
sum(fin.preds$V8-fin.preds$V5 > 0)/10000

#time3
quantile(fin.preds$V3,probs=c(0.5,0.025,.975))
quantile(fin.preds$V6,probs=c(0.5,0.025,.975))
quantile(fin.preds$V9,probs=c(0.5,0.025,.975))

sum(fin.preds$V6-fin.preds$V3 > 0)/10000
sum(fin.preds$V9-fin.preds$V3 > 0)/10000
sum(fin.preds$V9-fin.preds$V6 > 0)/10000

##### Graphing size and rays
dat2$temp <- factor(dat2$temp, levels = c("20C", "18C", "16C"))

p3<-ggplot(dat2,aes(x=time, y=sl,fill=temp,color=temp,shape=temp))+
  geom_point(size=.8,position = position_jitterdodge(dodge.width = .75,jitter.width = .2))+
scale_fill_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_color_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_shape_manual(values=c(24,21,25),name="Temperature")+
  theme_classic(base_size = 12,
                base_line_size = 1)+
  scale_x_discrete(labels=labs)+
  ylab("Standard Length (mm)")+xlab("Time since hatch")+
  theme(legend.position = c(.25,.80))+
  scale_y_continuous(limits=c(8,18),breaks=c(8,10,12,14,16,18))+
  guides(color = guide_legend(override.aes = list(size = 3)))
p3


p4<-ggplot(dat2,aes(x=time, y=total_rays2,fill=temp,color=temp,shape=temp))+
  geom_point(size=.8,position = position_jitterdodge(dodge.width = .75,jitter.width = .2))+
  scale_fill_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_color_manual(values = c("#CCCCCC", "#333666","#666666"),name="Temperature")+
  scale_shape_manual(values=c(24,21,25))+
  theme_classic(base_size = 12,
                base_line_size = 1)+
  scale_x_discrete(labels=labs)+
  scale_y_continuous(breaks=c(0,10,20,30,40))+
  coord_cartesian(ylim=c(0,40))+
  ylab("Total number of rays")+xlab("Time since hatch")+
  theme(legend.position = "none")
p4

tiff("New_Lab_FIG.jpg", units="in", width=6, height=6, res=300)
plot_grid(p3,p1,p4,p2,
          ncol = 2, 
          nrow = 2,
          label_size = 10)
dev.off()


dat3<-dat2 %>% 
group_by(time,temp,tank,female) %>% 
  summarize(n(),meansl=mean(sl),meanrays=mean(total_rays2))
  

write.csv(dat3,"dat3v2.csv")

