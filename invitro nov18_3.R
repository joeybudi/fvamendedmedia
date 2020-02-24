library(tidyverse)
library(readxl)
invitronov <- read_excel("U:/Fv amended media/nov exp 4/mycgro_sporulation_novexp4_2.xlsx", sheet = "raw_myc_nov_edit")
head(invitronov)

install.packages("Rmisc") #only if needed


detach(package:plyr)
#1. mycelia growth
editinvitronov <- invitronov %>% 
  group_by(crop,day,rep) %>% 
  summarize(ratexy=mean(ratemmperday), mycgrowthxy=mean(mycgrowthxy))
as.factor(editinvitronov$crop)
head(editinvitronov)
str(editinvitronov)

detach("package:Rmisc")
detach("package:plyr") 
library(plyr)
library(dplyr)
library(Rmisc)

#geom point scatter to create linear regression on radial mycelial growth
ggplot(editinvitronov, aes(day, mycgrowthxy, color=crop))+
  facet_wrap(~crop, nrow=2)+
  geom_point(size=1)+
  #geom_point(aes(color=crop), size=3)+
  #geom_point(color="grey90", size=1)+
  geom_smooth(method="lm", se=FALSE)+
  ylab("mycelia diameter (mm)") + 
  scale_x_continuous(name="day",breaks=seq(2,20,2))+
  theme(text=element_text(size=14), axis.text=element_text(size=14))+
  ggtitle("in vitro amended media nov18")+
  geom_text(data=plot.eq.lm.nov, aes(x=10, y=50, label=V1), parse=TRUE, inherit.aes=FALSE)

# growthestimate.lm <- lm(mycgrowthxy ~  day -1  + crop  , data=editinvitronov)
# summary(growthestimate.lm, correlation="T") #alfalfa for some reason did not show, not until put -1 on the model
# anova(growthestimate.lm)

editoats <- subset(editinvitronov, crop=="oats")
head(editoats)
lm.editoats <- lm(mycgrowthxy~day, data=editoats)
lm.editoats  
summary(lm.editoats)  

# growthestimate.cat.lm <- lm(mycgrowthxy ~ factor(crop) *day  -1  , data=editinvitronov)
# growthestimate.cat.lm
# anova(growthestimate.cat.lm)
# growthestimate.cat.lm$coefficients
# 
# library(emmeans)
# a <- emmeans(growthestimate.cat.lm, crop)
# emmeans(a)
# joint_tests(a)
# 
# summary(growthestimate.cat.lm)
# 
# aovest <- aov(mycgrowthxy~factor(crop))
# summary(aovest)
# 
#coef(summary(growthestimate.cat.lm))

plot(growthestimate.lm)

model.matrix(growthestimate.lm)


library(Rmisc)


#summarySE using function on Main.R/Rmisc package
summaryMycgrowthNov<- summarySE(editinvitronov, measurevar="mycgrowthxy", groupvars = c("crop","day"), na.rm="TRUE")
head(summaryMycgrowthNov)

summaryRateNov<- summarySE(editinvitronov, measurevar="ratexy", groupvars = c("crop","day"), na.rm="TRUE")
head(summaryRateNov)
install.packages("ggrepel")

ggplot(summaryMycgrowthNov, aes(day,mycgrowthxy, color=crop)) +
  geom_line(size=1) + 
  geom_errorbar(aes(ymin=mycgrowthxy-se, ymax=mycgrowthxy+se), width=.2) +
  ggrepel::geom_text_repel(data=summaryMycgrowthNov %>% dplyr::filter(day==14), 
                           aes(label=crop), size=4, nudge_x=.8, direction="y")+
  ylab("mycelia diameter (mm)") +
  theme(text=element_text(size=18))+
  guides(color="none")+  ggtitle("mycelial growth")+
  scale_x_continuous(name="day",breaks=seq(2,14,2))+
  theme_bw()

editinvitronov$crop <- factor(editinvitronov$crop, levels=c("control","oats","alfalfa","rye","corn","clover"))
day14novsumm <- summarySE(data=editinvitronov%>%dplyr::filter(day=="14"), 
                          "mycgrowthxy", "crop")
lblmycnov <- c("b","ab","a","ab","ab","a")

ggplot(day14novsumm, aes(y=mycgrowthxy, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mycgrowthxy-se, ymax=mycgrowthxy+se), width=.2) +
  ylab("mycelia diameter (mm)") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=.5, size=4, 
            label=lblmycnov, aes(fontface=2))+
  geom_jitter(data=editinvitronov%>%dplyr::filter(day=="14"),
              aes(x=crop, y=mycgrowthxy,  stroke=.5), alpha=0.3)+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  theme(text = element_text(size=14))+
  ggtitle("mycelia diameter at day 14")+
  theme_bw()

mod.day14novmyc <- aov(mycgrowthxy~crop, editinvitronov%>%dplyr::filter(day=="14"))
summary(mod.day14novmyc)
LSD.test(mod.day14novmyc, "crop", console=TRUE)

#2. sporulation
#sporulation <- rename(sporulation, c("spore/ml"="sporeplate"))
head(sporulation)
View(sporulation)

sporulation_nov <- read_excel("U:/Fv amended media/nov exp 4/mycgro_sporulation_novexp4_2.xlsx", sheet = "sporulation exp nov_raw")
head(sporulation_nov)
sporulation_nov$crop <- factor(sporulation_nov$crop, levels=c("control","oats","alfalfa","rye","corn","clover"))

sporulation_nov2 <- sporulation_nov %>% rename(plate="plate#") %>% group_by(crop, plate) %>% summarise_all(mean)
sporulation_nov2

summarySporulationNov <- Rmisc::summarySE(sporulation_nov, measurevar="totalsporesplate", groupvars="crop", na.rm="TRUE")
head(summarySporulationNov)


signif_label_sporulation <- c("d","ab", "a", "c","bc","a")

ggplot(summarySporulationNov, aes(y=totalsporesplate, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=totalsporesplate-se, ymax=totalsporesplate +se), width=.2) +
  ylab("total spores/plate at day 14") + xlab("crop amendment")+
  #scale_x_discrete(limits=c("control","oats","rye","corn","clover","alfalfa"))+
  theme(legend.position="none", text=element_text(size=14))+
  geom_text(label=signif_label_sporulation, position = position_dodge(0.9),vjust = -1,hjust=-.2, size=5)+ 
  theme_bw()

#stats
install.packages("agricolae")
library(agricolae)
mod.aov.sporulation<- aov(totalsporesplate~crop,data=sporulation_nov)
summary(mod.aov.sporulation)
cv.model(mod.aov.sporulation)
LSD.test(mod.aov.sporulation, "crop", console=TRUE)       

#data transformation attempt

sporulation_nov_transformed <- sporulation_nov %>% mutate(logSporulation=log(totalsporesplate), sqrtSporulation=sqrt(totalsporesplate))
View(sporulation_nov_transformed)

summlogSporulation <- summarySE(sporulation_nov_transformed, measurevar = "logSporulation",groupvars="crop",na.rm=TRUE)  
summsqrtSporulation <- summarySE(sporulation_nov_transformed, measurevar = "sqrtSporulation",groupvars="crop",na.rm=TRUE)  


ggplot(summlogSporulation, aes(y=logSporulation, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=logSporulation-se, ymax=logSporulation+se), width=.2) +
  ylab("log(total spores/plate) at day 20") + xlab("crop amendment")+
  scale_x_discrete(limits=c("control","oats","alfalfa", "rye","corn","clover"))+
  theme(legend.position="none", text=element_text(size=15))+
  theme_bw()
#geom_text(label=signif_label_sporulation, position = position_dodge(0.9),vjust = -1,hjust=-.2, size=5)

ggplot(summsqrtSporulation, aes(y=sqrtSporulation, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=sqrtSporulation-se, ymax=sqrtSporulation+se), width=.2) +
  ylab("sqrt(total spores/plate) at day 20") + xlab("crop amendment")+
  scale_x_discrete(limits=c("control","oats","rye","corn","clover","alfalfa"))+
  theme(legend.position="none", text=element_text(size=15))


#only day20 measurement
editinvitronovday14 <- invitronov %>% 
  filter(day==14) %>%
  group_by(crop,day,blk) %>% 
  summarize(ratexy=mean(ratemmperday), mycgrowthxy=mean(mycgrowth))
View(editinvitronovday14)

as.factor(editinvitronovday14$blk)
summaryMycgrowthday14Nov <- summarySE(editinvitronovday14, measurevar="mycgrowthxy", groupvars=c("crop"), na.rm="TRUE")
View(summaryMycgrowthday14Nov)

signif_label_myc20 <- c("ab","a","b","b","b","b")

ggplot(summaryMycgrowthday14Nov, aes(crop, mycgrowthxy, fill=crop)) +
  geom_bar(position=position_dodge(.9),stat="identity")+
  geom_errorbar(aes(ymin=mycgrowthxy-se, ymax=mycgrowthxy+se), width=.2)+ 
  xlab("crop amendment")+
  ylab("mycelia diameter (mm)")+
  geom_text(label=signif_label_myc20, position = position_dodge(0.9),vjust = -3.25, size=5)+
  theme(text=element_text(size=14), legend.position = "none")

#stats
mod.day20myc <- aov(mycgrowthxy~crop+blk, editinvitronovday14)
summary(mod.day20myc)
LSD.test(mod.day20myc, "crop", console=TRUE)

#3. spore/area
unique(editinvitronov$day)
editinvitronovfin <- editinvitronov %>%
  dplyr::filter(day=="14") %>%
  mutate(mycarea=pi*(mycgrowthxy/2)^2)  

editinvitronovfin

sporulation_nov3 <- sporulation_nov2  %>% mutate(rep=plate%/%10) %>% select(crop, rep, totalsporesplate) 
sporulation_nov3

sporemyc_nov <- left_join(sporulation_nov3, editinvitronovfin, by=c("crop","rep"))
sporemyc_nov <- sporemyc_nov %>% mutate(sporearea = totalsporesplate/mycarea)
sporemyc_nov

sporemyc_nov$crop <- factor(sporemyc_nov$crop, levels=c("control","oats","alfalfa","rye","corn","clover"))

sporemyc_nov.summ <- Rmisc::summarySE(sporemyc_nov, "sporearea", "crop")
sporemyc_nov.summ

lblsporemyc_nov <- c("c","ab","a","bc","ab","ab")

ggplot(sporemyc_nov.summ, aes(y=sporearea, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=sporearea-se, ymax=sporearea+se), width=.2) +
  ylab("spore per unit area") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -1,hjust=.5, size=4, 
            label=lblsporemyc_nov, aes(fontface=2))+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  geom_jitter(data=sporemyc_nov, aes(x=crop, y=sporearea,  stroke=.5), alpha=0.3)+
  theme(text = element_text(size=14))+
  ggtitle("Spore per unit area at day 20")+
  theme_bw()

mod.sporemyc_nov <- lm(sporearea~crop, data=sporemyc_nov)
LSD.test(mod.sporemyc_nov, "crop", console=TRUE)















spore_area <- read_excel("U:/Fv amended media/nov exp 4/mycgro_sporulation_novexp4_2.xlsx", sheet = "spore_area")
View(spore_area)
head(spore_area)

summarySporeArea<- summarySE(spore_area, groupvars="crop",  measurevar = "spore_area")
View(summarySporeArea)

ggplot(summarySporeArea, aes(x=crop,y=spore_area, fill=crop))+
  geom_bar(position=position_dodge(.9), stat="identity")+
  geom_errorbar(aes(ymin=spore_area-se, ymax=spore_area+se), width=.2) +
  scale_x_discrete(limits=c("control","oats","rye","corn","clover","alfalfa"))+
  geom_text(label=signif_label_sporearea, position = position_dodge(0.9),vjust = -1, hjust=-.5, size=5)+
  theme(text=element_text(size=14), legend.position = "none")+
  ylab("number of spores/mm^2")+
  xlab("crop amendment")
  
signif_label_sporearea <- c("a","a","ab","b","ab","b")

  #stats
mod.spore_area<- aov(spore_area~crop, data=spore_area)
summary(mod.spore_area)  
LSD.test(mod.spore_area,"crop",console=TRUE)




#------------
#growth rate per day? 
ggplot(summaryRateNov, aes(day,ratexy, color=crop)) +
  geom_line(size=1) + 
  geom_errorbar(aes(ymin=ratexy-se, ymax=ratexy+se), width=.2) +
  ylab("mycelia growth rate (mm/day)") +
  scale_x_continuous(name="day",breaks=seq(2,20,2)) 

# ggplot(summaryRateNov, aes(day,ratexy, color=crop)) +
#   geom_smooth(size=1, se="FALSE") + 
#   geom_errorbar(aes(ymin=ratexy-se, ymax=ratexy+se), width=.2) +
#   ylab("mycelia growth rate (mm/day)") +
#   scale_x_continuous(name="day",breaks=seq(2,20,2)) 
