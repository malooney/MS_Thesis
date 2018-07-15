
# Housekeeping ----------------------------------------------------------------
rm(list=ls())
cat("\014")

library(dplyr)
library(feather)
library(stargazer)

# Load Data -------------------------------------------------------------------
LA_data_2010 <- read_feather("/Users/malooney/Google Drive/digitalLibrary/*MS_Thesis/MS_Thesis/data/LA_data_2010_feather")
Beer_Characteristics_Master_List <- read.csv("~/Desktop/Beer_Characteristics_Master_List.csv", stringsAsFactors=FALSE)

# Add volume measures ---------------------------------------------------------
oz <- round(data.frame(oz=LA_data_2010$VOL_EQ.x* 288))
total_oz <- (oz* LA_data_2010$UNITS); colnames(total_oz) <- "total_oz"

total_gal <- (0.0078125* total_oz); colnames(total_gal) <- "total_gal"

dollarPerGal <- LA_data_2010$DOLLARS/ total_gal; 
colnames(dollarPerGal) <- "dollarPerGal"

LA_data_2010_manip <- cbind(LA_data_2010, oz, total_oz, total_gal, 
                            dollarPerGal)

rm(oz, total_gal, total_oz, dollarPerGal, LA_data_2010)

# Remove zero data ------------------------------------------------------------
LA_data_2010_manip <- filter(LA_data_2010_manip, L5 !="ALL BRAND")
LA_data_2010_manip <- filter(LA_data_2010_manip, dollarPerGal !="Inf")

# Explore Brands, Firms, and percenatges of Brands by Firms -------------------
uniqueBrands <- LA_data_2010_manip
colnames(uniqueBrands)[20] <- "Brands"
uniqueBrands_U <- aggregate(UNITS~Brands, uniqueBrands, sum)
uniqueBrands_Dol <- aggregate(DOLLARS~Brands, uniqueBrands, sum)
uniqueBrands_TotGal <- aggregate(total_gal~Brands, uniqueBrands, sum)
uniqueBrands <- full_join(uniqueBrands_U, uniqueBrands_Dol, by="Brands")
uniqueBrands <- full_join(uniqueBrands, uniqueBrands_TotGal, by="Brands")
uniqueBrands <- arrange(uniqueBrands, desc(UNITS))
rm(uniqueBrands_Dol, uniqueBrands_U, uniqueBrands_TotGal)
prcntBrandRep <- (sum(uniqueBrands[1:60,2]) / sum(uniqueBrands$UNITS)) * 100
par(mfrow=c(1,2))
x <- uniqueBrands[order(uniqueBrands$UNITS[1:60]),]
dotchart(x$UNITS[1:60], labels = x$Brands[1:60], 
         cex=.7,
         main="Total Units by Brand", 
         xlab="Units")
x <- uniqueBrands[order(uniqueBrands$DOLLARS[1:60]),]
dotchart(x$DOLLARS[1:60], labels = x$Brands[1:60], 
         cex=.7,
         main="Total Dollars by Brand", 
         xlab="Dollars")
aggregateDataSummary <- data.frame(Units=uniqueBrands$UNITS[1:60], 
                                   Dollars=uniqueBrands$DOLLARS[1:60], 
                                   Total_Gallons=uniqueBrands$total_gal[1:60])
rownames(aggregateDataSummary) <- uniqueBrands$Brands[1:60]
write(stargazer(aggregateDataSummary, type = "text", summary = F, flip=F, header = F), file="LA_agg_data.txt")
write.csv(aggregateDataSummary, "LA_agg_data.csv")
rm(x)




uniqueChains_all <- data.frame(Chain = rep(LA_data_2010_manip$MskdName, 
                                           LA_data_2010_manip$UNITS), 
                               y = sequence(LA_data_2010_manip$UNITS))
uniqueChains <- data.frame(table(uniqueChains_all$Chain))
uniqueChains <- arrange(uniqueChains, desc(Freq))
prcntChainRep <- (sum(uniqueChains[1:1,2]) / nrow(uniqueChains_all)) * 100

uniqueFirms_all <- data.frame(Firm = rep(LA_data_2010_manip$L4, 
                                           LA_data_2010_manip$UNITS), 
                               y = sequence(LA_data_2010_manip$UNITS))
uniqueFirms <- data.frame(table(uniqueFirms_all$Firm))
uniqueFirms <- arrange(uniqueFirms, desc(Freq))
prcntFirmRep <- (sum(uniqueFirms[1:2,2]) / nrow(uniqueFirms_all)) * 100

uniqueConglomerates <- data.frame(table(LA_data_2010_manip$L3))
uniqueConglomerates <- arrange(uniqueConglomerates, desc(Freq))

rm(uniqueBrands_all, uniqueChains_all, uniqueFirms_all)

# Function - Generate Main Data Frame -----------------------------------------

generate_data_frame <- function()  {
  
  unqWeek <- unique(LA_data_2010_manip$WEEK)
  unqChain <- unique(LA_data_2010_manip$MskdName)
  unqChain <- unqChain[-2] # remove Chain w/missing weeks
  unqBrands <- c("BUD LIGHT", 
                 "BUDWEISER", 
                 "COORS LIGHT", 
                 "MILLER LITE", 
                 "CORONA EXTRA", 
                 "HEINEKEN", 
                 "TECATE", 
                 "MILLER GENUINE DRAFT", 
                 "MILLER HIGH LIFE", 
                 "MODELO ESPECIAL", 
                 "BUD LIGHT LIME", 
                 "NEWCASTLE BROWN ALE", 
                 "MICHELOB ULTRA", 
                 "CORONA LIGHT", 
                 "NATURAL LIGHT", 
                 "COORS", 
                 "PACIFICO CLARA", 
                 "MILLER GENUINE DRAFT LIGHT 64", 
                 "STELLA ARTOIS LAGER", 
                 "BLUE MOON BELGIAN WHITE ALE")

  N <- (2646000/9)* (25.5/52) # LA market size Times per capita consumption / week
  #N <- 250000* (25.5/52)
  tmp <- filter(LA_data_2010_manip, L5 %in% unqBrands)
  tmp_main <- data.frame()
  #i <- 1
  #j <- 1
  k <- 1
  m <- 1
  n <- 1
  
  for(k in seq_along(unqChain)){
    tmp1 <- filter(tmp, MskdName==unqChain[k])
    i <- 1
    for(i in seq_along(unqWeek)){
      tmp2 <- filter(tmp1, WEEK==unqWeek[i])
      if(nrow(tmp2)!=0){
      j <- 1
      
      for(j in seq_along(unqBrands)){
        
        tmp3 <- filter(tmp2, L5==unqBrands[j]) 
        w_dollar <- sum(tmp3$dollarPerGal)
        W <- tmp3$dollarPerGal/w_dollar
        W_mean <- sum(W*tmp3$dollarPerGal)
    
        tmp_main[n, 01] <- m # cdid_week
        tmp_main[n, 02] <- i
        tmp_main[n, 03] <- unqChain[k] # chain
        tmp_main[n, 04] <- tmp2$WEEK[1]
        tmp_main[n, 05] <- as.character(tmp2$`Calendar week starting on`[1])
        tmp_main[n, 06] <- as.character(tmp2$`Calendar week ending on`[1])
        tmp_main[n, 07] <- unqBrands[j] # brand
        tmp_main[n, 08] <- tmp3$L3[1] # Conglomerate
        tmp_main[n, 09] <- tmp3$L4[1] # Firm
        tmp_main[n, 10] <- sum(tmp3$total_gal) # total gallons
        tmp_main[n, 11] <- mean(tmp3$dollarPerGal) # mean price1 ($/gal)
        tmp_main[n, 12] <- W_mean # weighted mean price2 ($/gal)
        tmp_main[n, 13] <- sum(tmp3$total_gal)/N # share
        
        n <- n+1
        j <- j+1
        }
      
      } else{
        j <- 1
        for(j in seq_along(unqBrands)){
          tmp_main[n, 01] <- m
          tmp_main[n, 02] <- i
          tmp_main[n, 03] <- unqChain[k]
          tmp_main[n, 04] <- unqWeek[i]
          tmp_main[n, 05] <- NA
          tmp_main[n, 06] <- NA
          tmp_main[n, 07] <- unqBrands[j]
          tmp_main[n, 08] <- NA
          tmp_main[n, 09] <- NA
          tmp_main[n, 10] <- NA
          tmp_main[n, 11] <- NA
          tmp_main[n, 12] <- NA
          tmp_main[n, 13] <- NA
          
          n <- n+1
          j <- j+1
          }
      }
      
      i <- i+1
      m <- m+1
      }
    k <- k+1
  }
  
  colnames(tmp_main) <- c("cdid", "sub_cdid", "Chain", "week", 
                          "week_start", "week_end", "Brand", "Conglomerate",
                          "Firm",  "total_gallons", "p1_mean", "p2_Wmean",
                          "share")
  return(tmp_main)
  }

LA_2010_aggregate_data <- generate_data_frame()

nChains <- 6
nWeeks <- 52 # number of weeks
nmkt <- nChains * nWeeks # number of markets
nbrn <- 20 # number of brands
constant <- data.frame("constant"= rep(1, times= nmkt* nbrn))

outshr <- function(share, cdid, nmkt, nbrn){ # function to calculate outshr
  
  cdindex <- seq(nbrn, nbrn*nmkt, nbrn) # indexes the markets
  
  temp <- cumsum(share)
  sum1 <- temp[cdindex]
  sum1[2:length(sum1)] <- diff(sum1)
  outshr <- 1- sum1[cdid]
  return(outshr)
}

outshr <- data.frame(outshr= outshr(share=LA_2010_aggregate_data$share, cdid=LA_2010_aggregate_data$cdid, nmkt=nmkt, nbrn=nbrn))

LA_2010_aggregate_data <- cbind(LA_2010_aggregate_data, outshr)

LA_2010_aggregate_data <- left_join(LA_2010_aggregate_data, 
                                     Beer_Characteristics_Master_List, 
                                     by= c("Brand" = "Product_Name"))

rm(outshr, constant)

LA_2010_aggregate_data_subChain <- filter(LA_2010_aggregate_data, 
                                          Chain == "Chain79")# | 
                                          #Chain == "Chain110")# |
                                          #Chain == "Chain3")# |
                                          #Chain == "Chain98")# | 
                                          #Chain == "Chain15")#| 
                                          #Chain == "Chain84")

# LA_2010_aggregate_data_subChain$outshr[LA_2010_aggregate_data_subChain$outshr <
#                                          0] <- 0.0000001

# -----------------------------------------------------------------------------


par(mfrow=c(3,2))

hist(filter(LA_2010_aggregate_data, Chain=="Chain79")$p2_Wmean,
     main="Prices of Chain79", xlim=c(0, 25))
abline(v= mean(filter(LA_2010_aggregate_data, Chain=="Chain79")$p2_Wmean),
       col="blue")

hist(filter(LA_2010_aggregate_data, Chain=="Chain110")$p2_Wmean,
     main="Prices of Chain110", xlim=c(0, 25))
abline(v= mean(filter(LA_2010_aggregate_data, Chain=="Chain110")$p2_Wmean),
       col="blue")

hist(filter(LA_2010_aggregate_data, Chain=="Chain3")$p2_Wmean,
     main="Prices of Chain3", xlim=c(0, 25))
abline(v= mean(filter(LA_2010_aggregate_data, Chain=="Chain3")$p2_Wmean),
       col="blue")

hist(filter(LA_2010_aggregate_data, Chain=="Chain98")$p2_Wmean,
     main="Prices of Chain98", xlim=c(0, 25))
abline(v= mean(filter(LA_2010_aggregate_data, Chain=="Chain98")$p2_Wmean),
       col="blue")

hist(filter(LA_2010_aggregate_data, Chain=="Chain15")$p2_Wmean,
     main="Prices of Chain15", xlim=c(0, 25))
abline(v= mean(filter(LA_2010_aggregate_data, Chain=="Chain15")$p2_Wmean),
       col=
         "blue")

hist(filter(LA_2010_aggregate_data, Chain=="Chain84")$p2_Wmean,
     main="Prices of Chain84", xlim=c(0, 25))
abline(v= mean(filter(LA_2010_aggregate_data, Chain=="Chain84")$p2_Wmean),
       col="blue")

summary(lm.results <- lm( log(share) - log(outshr)~  0 + p2_Wmean + 
                            BUD.LIGHT + 
                            BUDWEISER + 
                            COORS.LIGHT + 
                            MILLER.LITE + 
                            CORONA.EXTRA + 
                            HEINEKEN + 
                            TECATE + 
                            MILLER.GENUINE.DRAFT + 
                            MILLER.HIGH.LIFE + 
                            MODELO.ESPECIAL + 
                            BUD.LIGHT.LIME + 
                            NEWCASTLE.BROWN.ALE + 
                            MICHELOB.ULTRA + 
                            CORONA.LIGHT + 
                            NATURAL.LIGHT + 
                            COORS + 
                            PACIFICO.CLARA + 
                            MILLER.GENUINE.DRAFT.LIGHT.64 + 
                            STELLA.ARTOIS.LAGER + 
                            BLUE.MOON.BELGIAN.WHITE.ALE, 
                          data= LA_2010_aggregate_data_subChain))


summary(lm.results1 <- lm( log(share) - log(outshr) ~ 1 + p2_Wmean + 
                             ABV + 
                             #IBU + 
                             #SRM +
                             Calories_oz + 
                             Carbs_oz + 
                             USA + 
                             Mexico# + 
                             # Netherlands 
                           ,data= LA_2010_aggregate_data_subChain))








