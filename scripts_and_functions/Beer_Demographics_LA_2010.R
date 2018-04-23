
rm(list=ls())
cat("\014")

library(dplyr)
library(readr)

Appendix_H_census_Codebook <- read.csv("~/Desktop/Appendix_H_census_Codebook.csv", sep="")
METROPOLITAN_CBSA <- read_csv("~/Desktop/METROPOLITAN_CBSA.csv")

ASEC_2009 <- readRDS("/Volumes/G-DRIVE USB-C/CPSASEC/2009 cps asec.rds")
#names_ASEC_2009 <- data.frame(names(ASEC_2009))
LA_demog_2009 <- filter(ASEC_2009, gtcbsa==31100)
LA_demog_2009 <- select(LA_demog_2009, h_year, gtcbsa, h_seq, ffpos, fpersons, a_age, a_hga, ftotval, prdtrace, pehspnon, prdthsp, penatvty, pemntvty, pefntvty)
LA_demog_2009 <- filter(LA_demog_2009, a_age>=21)

min(LA_demog_2009$a_age)
plot(density(LA_demog_2009$a_age))
mean(LA_demog_2009$a_age)
median(LA_demog_2009$a_age)

plot(density(LA_demog_2009$ftotval))
mean(LA_demog_2009$ftotval)
median(LA_demog_2009$ftotval)


ASEC_2010 <- readRDS("/Volumes/G-DRIVE USB-C/CPSASEC/2010 cps asec.rds")
names_ASEC_2010 <- data.frame(names(ASEC_2010))
LA_demog_2010 <- filter(ASEC_2010, gtcbsa==31100)
LA_demog_2010 <- select(LA_demog_2010, h_year, gtcbsa, h_seq, ffpos, fpersons, a_age, a_hga, ftotval, prdtrace, pehspnon, prdthsp, penatvty, pemntvty, pefntvty)
LA_demog_2010 <- filter(LA_demog_2010, a_age>=21)

min(LA_demog_2010$a_age)
plot(density(LA_demog_2010$a_age))
mean(LA_demog_2010$a_age)
median(LA_demog_2010$a_age)

plot(density(LA_demog_2010$ftotval))
mean(LA_demog_2010$ftotval)
median(LA_demog_2010$ftotval)


ASEC_2011 <- readRDS("/Volumes/G-DRIVE USB-C/CPSASEC/2011 cps asec.rds")
LA_demog_2011 <- filter(ASEC_2011, gtcbsa==31100)
LA_demog_2011 <- select(LA_demog_2011, h_year, gtcbsa, h_seq, ffpos, fpersons, a_age, a_hga, ftotval, prdtrace, pehspnon, prdthsp, penatvty, pemntvty, pefntvty)
LA_demog_2011 <- filter(LA_demog_2011, a_age>=21)

min(LA_demog_2011$a_age); max(LA_demog_2011$a_age)
plot(density(LA_demog_2011$a_age))
mean(LA_demog_2011$a_age)
median(LA_demog_2011$a_age)

plot(density(LA_demog_2011$ftotval))
mean(LA_demog_2011$ftotval)
median(LA_demog_2011$ftotval)
