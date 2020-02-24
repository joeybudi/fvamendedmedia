
library(stats)

#to do lm functions on list of factors (with warpbreaks example)---------
attach(warpbreaks)
str(warpbreaks)
head(warpbreaks)
warpbreaks
by(warpbreaks[ ,3], wool, summary)
by(warpbreaks[, 1], list(wool=wool, tension=tension), summary)
by(warpbreaks, tension, function(x) lm(breaks ~ wool, data=x))

tmp <- by(warpbreaks, tension, function(x) lm(breaks~wool, data=x))
sapply(tmp, coef)

#now for my fv amended media dataset
attach(editinvitronov)
str(editinvitronov)
head(editinvitronov)
by(editinvitronov)

compact.lm <- by(editinvitronov, crop, function(x) lm(mycgrowthxy ~ day, data=x))
compact.summ.lm <- by(editinvitronov, crop, function(x) summary(lm(mycgrowthxy ~ day, data=x)))
sapply.lm <- sapply(compact.lm, coef)
sapply.summ.lm <- sapply(compact.summ.lm, coef)

sapply.lm

sapply.summ.lm <- as.tibble(sapply.summ.lm)
class(sapply.summ.lm)       
sapply.summ.lm2 <- add_column(sapply.summ.lm, coef = c("intercept","day","intercept","day","intercept","day","intercept","day"))

sapply.summ.lm3 <- add_column(sapply.summ.lm2, param = c("estimate","estimate","std.err","std.err",
                                                         "t-value","t-value","Pr(>|t|)","Pr(>|t|)")) 
sapply.summ.lm3

sapply.summ.lm4 <- gather(sapply.summ.lm3, crop, ,alfalfa:rye)
sapply.summ.lm5 <- spread(sapply.summ.lm4, param, value)
sapply.summ.lm5
sapply.summ.lm6 <- select(sapply.summ.lm5, coef, crop, estimate, std.err, 't-value', 'Pr(>|t|)')
sapply.summ.lm6

sapply.summ.lm7 <- subset(sapply.summ.lm6, coef=="day") 
sapply.summ.lm7
sappply.summ.lm8 <- as.data.frame(sapply.summ.lm7)
sappply.summ.lm8
