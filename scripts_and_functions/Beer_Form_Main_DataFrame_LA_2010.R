
# Housekeeping ----------------------------------------------------------------
rm(list=ls())
cat("\014")

library(dplyr)
library(feather)

# Load Data -------------------------------------------------------------------
LA_data_2010 <- read_feather("/Users/malooney/Google Drive/digitalLibrary/*MS_Thesis/MS_Thesis/data/LA_data_2010_feather")
Beer_Characteristics_Master_List <- read.csv("~/Desktop/Beer_Characteristics_Master_List.csv", stringsAsFactors=FALSE)

# Add volume measures ---------------------------------------------------------
oz <- round(data.frame(oz=LA_data_2010$VOL_EQ.x* 288))
total_oz <- (oz* LA_data_2010$UNITS); 
colnames(total_oz) <- "total_oz"
total_gal <- (0.0078125* total_oz); 
colnames(total_gal) <- "total_gal"
dollarPerGal <- LA_data_2010$DOLLARS/ total_gal; 
colnames(dollarPerGal) <- "dollarPerGal"

LA_data_2010_manip <- cbind(LA_data_2010, oz, total_oz, total_gal, 
                            dollarPerGal)

rm(oz, total_gal, total_oz, dollarPerGal, LA_data_2010)

# Remove zero data ------------------------------------------------------------
LA_data_2010_manip <- filter(LA_data_2010_manip, L5 !="ALL BRAND")
LA_data_2010_manip <- filter(LA_data_2010_manip, dollarPerGal !="Inf")


uniqueBrands <- data.frame(table(LA_data_2010_manip$L5))
uniqueBrands <- arrange(uniqueBrands, desc(Freq))
100 * (sum(uniqueBrands[1:20,2]) / nrow(LA_data_2010_manip))

uniqueChains <- data.frame(table(LA_data_2010_manip$MskdName))
uniqueFirms <- data.frame(table(LA_data_2010_manip$L4))
uniqueConglomerates <- data.frame(table(LA_data_2010_manip$L3))


# Function - Generate Main Data Frame -----------------------------------------
generate_data_frame <- function()  {
  
  unqWeek <- unique(LA_data_2010_manip$WEEK)
  unqChain <- unique(LA_data_2010_manip$MskdName)
  unqChain <- unqChain[-2] # remove Chain w/missing weeks
  unqBrands <- c("BUD LIGHT", "BUDWEISER", "COORS LIGHT", "MILLER LITE",
               "CORONA EXTRA", "HEINEKEN", "TECATE", "MILLER GENUINE DRAFT", 
               "MILLER HIGH LIFE", "MODELO ESPECIAL", "BUD LIGHT LIME", 
               "NEWCASTLE BROWN ALE", "MICHELOB ULTRA", "CORONA LIGHT",
               "NATURAL LIGHT", "COORS", "PACIFICO CLARA", 
               "MILLER GENUINE DRAFT LIGHT 64", "STELLA ARTOIS LAGER", 
               "BLUE MOON BELGIAN WHITE ALE")

  N <- (25.5/52)*2646000 # LA market size, per capita consumption / week
  #N <- (25.5/52)*250000
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
    
        tmp_main[n, 01] <- m
        tmp_main[n, 02] <- i
        tmp_main[n, 03] <- unqChain[k]
        tmp_main[n, 04] <- tmp2$WEEK[1]
        tmp_main[n, 05] <- as.character(tmp2$`Calendar week starting on`[1])
        tmp_main[n, 06] <- as.character(tmp2$`Calendar week ending on`[1])
        tmp_main[n, 07] <- unqBrands[j]
        tmp_main[n, 08] <- mean(tmp3$dollarPerGal)
        tmp_main[n, 09] <- sum(tmp3$total_gal)/N
        tmp_main[n, 10] <- sum(tmp3$total_gal)
        tmp_main[n, 11] <- W_mean
    
        n <- n+1
        j <- j+1}
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
          
          n <- n+1
          j <- j+1}
        }
      i <- i+1
      m <- m+1
      }
    k <- k+1
  }
  
  colnames(tmp_main) <- c("cdid", "sub_cdid", "Chain", "week", "week_start",
                          "week_end", "Brand", "price1", "share", 
                          "total_gallons", "price2")
  return(tmp_main)
  }

LA_2010_aggregate_data <- generate_data_frame()

nmkt <- 364;
nbrn <- 20;
constant <- data.frame("constant"= rep(1, times=nmkt*nbrn))

outshr <- function(share, cdid, nmkt, nbrn){ # function to calculate outshr
  
  cdindex <- seq(nbrn, nbrn*nmkt, nbrn) # indexes the markets
  
  temp <- cumsum(share)
  sum1 <- temp[cdindex]
  sum1[2:length(sum1)] <- diff(sum1)
  outshr <- 1- sum1[cdid]
  return(outshr)
}

outshr <- data.frame(outshr= outshr(share=LA_2010_aggregate_data$share, cdid=LA_2010_aggregate_data$cdid, nmkt=nmkt, nbrn=nbrn))

price3 <- data.frame(price3=LA_2010_aggregate_data$price2/10)

LA_2010_aggregate_data <- cbind(LA_2010_aggregate_data, outshr, price3)

LA_2010_aggregate_data <- left_join(LA_2010_aggregate_data, 
                                     Beer_Characteristics_Master_List, 
                                     by= c("Brand" = "Product_Name"))

rm(outshr, price3, constant)

summary(lm.results <- lm( log(share)- log(outshr)~ 0 + price2+ 
                            `BUD.LIGHT`+ 
                            `BUDWEISER`+ 
                            `COORS.LIGHT`+ 
                            `MILLER.LITE`+ 
                            `CORONA.EXTRA`+ 
                            `HEINEKEN`+ 
                            `TECATE`+ 
                            `MILLER.GENUINE.DRAFT`+ 
                            `MILLER.HIGH.LIFE`+ 
                            `MODELO.ESPECIAL`+ 
                            `BUD.LIGHT.LIME`+ 
                            NEWCASTLE.BROWN.ALE+ 
                            MICHELOB.ULTRA+ 
                            CORONA.LIGHT+ 
                            NATURAL.LIGHT+ 
                            COORS+ 
                            PACIFICO.CLARA+ 
                            MILLER.GENUINE.DRAFT.LIGHT.64+ 
                            STELLA.ARTOIS.LAGER+ 
                            BLUE.MOON.BELGIAN.WHITE.ALE, 
                          data= LA_2010_aggregate_data))


summary(lm.results1 <- lm( log(share) - log(outshr) ~ 0 + price2+ 
                             ABV+ 
                             IBU+ 
                             Calories_oz+ 
                             Carbs_oz+ 
                             USA+ 
                             Mexico, 
                           data= LA_2010_aggregate_data))








