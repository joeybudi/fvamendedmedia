#copied from invitro may18
library(readxl)
library(dplyr)
library(ggplot2)

invitromay <- read_excel("U:/Fv amended media/may exp 2/invitro amended media mycelium may 18.xlsx", sheet = "mycelia sort growth rate")
View(invitromay)

#1. mycelia growth
editinvitromay <- invitromay %>% 
  group_by(crop,day,rep) %>% 
  summarize(ratexy=mean(ratemmperday), mycgrowthxy=mean(mycgrowth))
editinvitromay$rep
editinvitromay
View(editinvitromay)
View(invitromay)

detach("package:plyr") 
library(plyr)
library(dplyr)
library(Rmisc)
summaryMycgrowth<- summarySE(editinvitromay, measurevar="mycgrowthxy", groupvars = c("crop","day"), na.rm="TRUE")
View(summaryMycgrowth)

ggplot(summaryMycgrowth, aes(day,mycgrowthxy, color=crop)) +
  facet_grid(. ~ ., labeller=label_both)+
  geom_line(size=1) + 
  geom_errorbar(aes(ymin=mycgrowthxy-se, ymax=mycgrowthxy+se), width=.2) +
  geom_text(data=summaryMycgrowth %>% dplyr::filter(day==14),
            aes(label=crop), size=4, nudge_x=.7, check_overlap = FALSE)+
  ylab("mycelia diameter (mm)") +
  theme(text=element_text(size=18))+
  guides(color="none")+
  ggtitle("mycelial growth")+
  scale_x_continuous(name="day",breaks=seq(4,14,2)) +
  theme_bw()

editinvitromay$crop <- factor(editinvitromay$crop, levels=c("control","oats","oats_roots","alfalfa","rye","corn","clover"))
day14summ <- summarySE(data=editinvitromay%>%dplyr::filter(day=="14"), 
                       "mycgrowthxy", "crop")
day14summ

lblmyc <- c("d","cd","b","a","bc","bc","c")
ggplot(day14summ, aes(y=mycgrowthxy, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mycgrowthxy-se, ymax=mycgrowthxy+se), width=.2) +
  ylab("mycelia diameter (mm)") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -3,hjust=.5, size=4, 
            label=lblmyc, aes(fontface=2))+
  geom_jitter(data=editinvitromay%>%dplyr::filter(day=="14"),
              aes(x=crop, y=mycgrowthxy,  stroke=.5), alpha=0.3)+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  theme(text = element_text(size=14))+
  ggtitle("mycelia diameter at day 14")+
  theme_bw()

#2. sporulation
sporulation <- read_excel("U:/Fv amended media/may exp 2/invitro amended media mycelium may 18.xlsx", sheet = "sporulation")
sporulation <- rename(sporulation, c("spore/ml"="sporeplate"))
sporulation$crop <- factor(sporulation$crop, levels=c("control","oats","oats_roots","alfalfa","rye","corn","clover"))
head(sporulation)
View(sporulation)
sporulation
summarySporulation <- summarySE(sporulation, measurevar="totalsporeperPlate", groupvars="crop", na.rm="TRUE")
summarySporulation

lblsporulation <- c("c","bc","ab","a","bc","abc","c")

#sporulation, log

ggplot(summarySporulation, aes(y=totalsporeperPlate, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=totalsporeperPlate-se, ymax=totalsporeperPlate+se), width=.2) +
  ylab("total spores/plate") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -3,hjust=.5, size=4, 
            label=lblsporulation, aes(fontface=2))+
  geom_jitter(data=sporulation, aes(x=crop, y=totalsporeperPlate,  stroke=.5), alpha=0.3)+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  theme(text = element_text(size=14))+
  ggtitle("Sporulation at day 14")+
  theme_bw()

sporlog <- sporulation %>% mutate(logspore= log(totalsporeperPlate))
sporlog.summ <- summarySE(sporlog, "logspore", "crop")
sporlog.summ

lblsporulation.log <- c("b","b","ab","a","ab","ab","b")
ggplot(sporlog.summ, aes(y=logspore, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=logspore-se, ymax=logspore+se), width=.2) +
  ylab("log(total spores/plate)") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -1,hjust=.5, size=4, 
            label=lblsporulation.log, aes(fontface=2))+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  theme(text = element_text(size=14))+
  ggtitle("Sporulation at day 14\nlog transformed")+
  theme_bw()

library(agricolae)
mod.sporulation.may18.log<- aov(logspore ~ crop, data=sporlog)
summary(mod.sporulation.may18.log)
LSD.test(mod.sporulation.may18.log,"crop",console=TRUE)

mod.finalmyc <- aov(mycgrowthxy ~ crop, data=editinvitromay%>%dplyr::filter(day=="14"))
summary(mod.finalmyc)
LSD.test(mod.finalmyc,"crop",console=TRUE)

#3. sporulation per unit area
#have to used the "mycelia raw" data from the sheet which is not sorted out or handpicked yet

invitromayraw <- read_excel("U:/Fv amended media/may exp 2/invitro amended media mycelium may 18.xlsx", sheet = "mycelia raw")

editinvitromayraw <- invitromayraw %>% 
  group_by(crop,day,rep) %>% 
  summarize( mycgrowthxy=mean(mycgrowth))

editinvitromayfin <- editinvitromayraw %>%
  dplyr::filter(day=="14") %>%
  mutate(mycarea=pi*(mycgrowthxy/2)^2)  

editinvitromayfin
sporulation <- select(sporulation, crop, "plate#", totalsporeperPlate) %>% rename(rep="plate#")
sporulation

sporemyc <- left_join(sporulation, editinvitromayfin)
sporemyc <- sporemyc %>% mutate(sporearea = totalsporeperPlate/mycarea)
sporemyc

sporemyc$crop <- factor(sporemyc$crop, levels=c("control","oats","oats_roots","alfalfa","rye","corn","clover"))

sporemyc.summ <- summarySE(sporemyc, "sporearea", "crop")
sporemyc.summ

lblsporemyc <- c("b","b","ab","a","ab","ab","b")

ggplot(sporemyc.summ, aes(y=sporearea, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=sporearea-se, ymax=sporearea+se), width=.2) +
  ylab("spore per unit area") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -1,hjust=.5, size=4, 
            label=lblsporemyc, aes(fontface=2))+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  geom_jitter(data=sporemyc, aes(x=crop, y=sporearea,  stroke=.5), alpha=0.3)+
  theme(text = element_text(size=14))+
  ggtitle("Spore per unit area at day 14")+
  theme_bw()

mod.sporemyc <- lm(sporearea~crop, data=sporemyc)
LSD.test(mod.sporemyc, "crop", console=TRUE)

#rate of growth

summaryRate<- summarySE(editinvitromay, measurevar="ratexy", groupvars = c("crop","day"), na.rm="TRUE")
View(summaryRate)

ggplot(summaryRate, aes(day,ratexy, color=crop)) +
  geom_line(size=1) + asdfadsfasdfdsaf
geom_errorbar(aes(ymin=ratexy-se, ymax=ratexy+se), width=.2) +
  ylab("mycelia growth rate (mm/day)") +
  scale_x_continuous(name="day",breaks=seq(4,14,2)) 

ggplot(summaryRate, aes(day,ratexy, color=crop)) +
  geom_smooth(size=1,se=FALSE) + 
  ylab("mycelia growth rate (mm/day)") +
  scale_x_continuous(name="day",breaks=seq(4,14,2)) 

