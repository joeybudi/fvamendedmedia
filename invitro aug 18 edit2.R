library(readxl)
library(dplyr)
library(ggplot2)
#RATE OF GROWTH FILE ACCOMPANIMENT IS in Fv amended media R project/1.Rmd

invitroaug <- read_excel("U:/Fv amended media/aug18 exp 3/mycgro_sporulation.xlsx", sheet = "myc2")
invitroaug
  
#1. mycelia growth
editinvitroaug <- invitroaug %>% 
  group_by(crop,day,blk) %>% 
  summarize(ratexy=mean(ratemmperday), mycgrowthxy=mean(mycgrowth))

View(editinvitroaug)
View(invitroaug)
detach("package:Rmisc")
detach("package:plyr") 
library(plyr)
library(dplyr)
library(Rmisc)

summaryMycgrowthaug<- summarySE(editinvitroaug, measurevar="mycgrowthxy", groupvars = c("crop","day"), na.rm="TRUE")
summaryMycgrowthaug
View(summaryMycgrowth)

ggplot(summaryMycgrowthaug, aes(day,mycgrowthxy, color=crop)) +
  facet_grid(. ~ ., labeller=label_both)+
  geom_line(size=1) + 
  geom_errorbar(aes(ymin=mycgrowthxy-se, ymax=mycgrowthxy+se), width=.2) +
  geom_text(data=summaryMycgrowthaug %>% dplyr::filter(day==20),
            aes(label=crop), size=4, nudge_x=1.1, check_overlap = FALSE)+
  ylab("mycelia diameter (mm)") +
  theme(text=element_text(size=18))+
  guides(color="none")+
  ggtitle("mycelial growth")+
  scale_x_continuous(name="day",breaks=seq(4,20,2)) +
  theme_bw()

editinvitroaug$crop <- factor(editinvitroaug$crop, levels=c("control","oats","alfalfa","rye","corn","clover"))
day20summ <- summarySE(data=editinvitroaug%>%dplyr::filter(day=="20"), 
                       "mycgrowthxy", "crop")
day20summ

lblmycaug <- c("b","b","ab","b","b","a")
ggplot(day20summ, aes(y=mycgrowthxy, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mycgrowthxy-se, ymax=mycgrowthxy+se), width=.2) +
  ylab("mycelia diameter (mm)") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=.5, size=4, 
            label=lblmycaug, aes(fontface=2))+
  geom_jitter(data=editinvitroaug%>%dplyr::filter(day=="20"),
              aes(x=crop, y=mycgrowthxy,  stroke=.5), alpha=0.3)+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  theme(text = element_text(size=14))+
  ggtitle("mycelia diameter at day 20")+
  theme_bw()

#2. sporulation
sporulationaug <- read_excel("U:/Fv amended media/aug18 exp 3/mycgro_sporulation.xlsx", sheet = "sporulation to r")
sporulationaug$crop <- factor(sporulationaug$crop, levels=c("control","oats","alfalfa","rye","corn","clover"))
head(sporulationaug)
View(sporulationaug)

summarysporulationaug <- summarySE(sporulationaug, measurevar="totalspores", groupvars="crop", na.rm="TRUE")
summarysporulationaug


#sporulation, log
lblsporulationaug <- c("b","b","a","ab","a","ab")
ggplot(summarysporulationaug, aes(y=totalspores, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=totalspores-se, ymax=totalspores+se), width=.2) +
  ylab("total spores/plate") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -3,hjust=.5, size=4, 
            label=lblsporulationaug, aes(fontface=2))+
  geom_jitter(data=sporulationaug, aes(x=crop, y=totalspores,  stroke=.5), alpha=0.3)+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  theme(text = element_text(size=14))+
  ggtitle("sporulation at day 20")+
  theme_bw()

sporlogaug <- sporulationaug %>% mutate(logspore= log(totalspores))
sporlogaug.summ <- summarySE(sporlogaug, "logspore", "crop")
sporlogaug.summ

#lblsporulationaug log transformed is the same as the non transformed 
ggplot(sporlogaug.summ, aes(y=logspore, x=crop, fill=crop)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=logspore-se, ymax=logspore+se), width=.2) +
  ylab("log(total spores/plate)") + xlab("crop amendment")+
  geom_text(position = position_dodge(0.9),vjust = -1,hjust=.5, size=4, 
            label=lblsporulationaug, aes(fontface=2))+
  #scale_x_discrete(limits=c("control","oats","oats roots","rye","corn","clover","alfalfa"))+
  theme(text = element_text(size=14))+
  ggtitle("sporulationaug at day 20\nlog transformed")+
  theme_bw()

library(agricolae)
mod.myc.aug18<- aov(mycgrowthxy ~ crop, data=editinvitroaug%>%dplyr::filter(day=="20"))
summary(mod.sporulationaug.aug18)
LSD.test(mod.sporulationaug.may18.log,"crop",console=TRUE)

mod.finalmyc.aug18 <- aov(mycgrowthxy ~ crop, data=editinvitroaug%>%dplyr::filter(day=="20"))
summary(mod.finalmyc.aug18)
LSD.test(mod.finalmyc.aug18,"crop",console=TRUE)

mod.sporulation.aug18 <- aov(totalspores ~ crop, data=sporulationaug)
summary(mod.sporulation.aug18)
LSD.test(mod.sporulation.aug18,"crop",console=TRUE)

mod.sporulationlog.aug18 <- aov(logspore ~ crop, data=sporlogaug)
summary(mod.sporulationlog.aug18)
LSD.test(mod.sporulationlog.aug18,"crop",console=TRUE)

#RATE OF GROWTH FILE ACCOMPANIMENT IS in Fv amended media R project/1.Rmd

#rate of growth

summaryRate<- summarySE(data=editinvitroaug%>%dplyr::filter(day=="20"), measurevar="ratexy", groupvars = c("crop","day"), na.rm="TRUE")
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

