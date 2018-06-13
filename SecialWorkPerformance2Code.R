rm(list=ls())

# Aggregate Cola Data

#data description:
# "MARKET"
# "CHAIN"             
# "store_type"        
# "iri_key"           = delivery store
# "year"
# "week"             
# "L4"                = vendor           
# "L5"                = brand               
# "VOL_EQ"             = volume equivilant (provides a way to compare units of different sizes)            
# "PACKAGE"           
# "units"
# "dollars"
# "price"             
# "price_deflated"    = adjusted by inflation (?)
# "feature"            = #NONE No feature; 
# FS-C --FSP C (for frequent shopper program members only) 
# C - small ad, usually 1 line of text 
# B – medium size ad 
# A – large size ad 
# A+ ad – also known as “Q” or “R” – retailer coupon or rebate
# "display"            = 0=NO, 1=MINOR, 2=MAJOR. MAJOR includes codes lobby and endaisle
#                         (our interpretation: where the ad is displayed in the store; how visible)
# "total_vol_carbbev"  = (i.e. total carbonated beverages volume in store) 
# "total_rev_carbbev"  = (i.e. total carbonated beverages revenue in store)
# "total_vol_cola"     = total cola volume in store
# "total_rev_cola"     = total cola revenue in store
# "total_vol_l4"       = manufacturer cola volume in store
# "total_rev_l4"       = manufacturer cola revenue in store 

# done by Klapper... 
# "yearfactor"         
# "iri_keyfactor"    
# "display_minor"     
# "display_major"     
# "feature_small"    
# "feature_medium"    
# "feature_large"     
# "coupon"           
# "displayall"       
# "featureall"  


#each observation is one purchase over a time of January 2001 till December 2011
 


data.colaBackup<-read.csv("cola_amm_boston.csv")
#data.cola <- subset(data.cola, data.cola$PACKAGE=="BOTTLE")
#data.cola <- subset(data.cola, data.cola$store_type=="GROC")
data.cola33 <- subset(data.colaBackup, data.colaBackup$CHAIN==33)
data.cola65 <- subset(data.colaBackup, data.colaBackup$CHAIN==65)

#only extracting the wanted chains 33 and 65
data.cola <- rbind(data.cola33, data.cola65)

#data transformation: getting the right types 
data.cola$yearfactor<-as.factor(data.cola$year)
data.cola$iri_keyfactor<-as.factor(data.cola$iri_key) # iri_key means delivery_stores
data.cola$display_minor<- ifelse(data.cola$display==1,1,0)
data.cola$display_major<- ifelse(data.cola$display==2,1,0)
data.cola$feature_small<- ifelse(data.cola$feature=="C",1,0)
data.cola$feature_medium<- ifelse(data.cola$feature=="B",1,0)
data.cola$feature_large<- ifelse(data.cola$feature=="A",1,0)
data.cola$coupon<- ifelse(data.cola$feature=="A+",1,0)
data.cola$displayall<-data.cola$display_minor+data.cola$display_major
data.cola$featureall<-data.cola$feature_small+data.cola$feature_medium+data.cola$feature_large
data.cola.agg<-data.cola
#aggregating revenue from singe price * units sold 
#aggregating all the values for one whole purchase
data.cola.agg$price<-data.cola.agg$price*data.cola.agg$units
data.cola.agg$price_deflated<-data.cola.agg$price_deflated*data.cola.agg$units
data.cola.agg$display_minor<-data.cola.agg$display_minor*data.cola.agg$units
data.cola.agg$display_major<-data.cola.agg$display_major*data.cola.agg$units
data.cola.agg$feature_small<-data.cola.agg$feature_small*data.cola.agg$units
data.cola.agg$feature_medium<-data.cola.agg$feature_medium*data.cola.agg$units
data.cola.agg$feature_large<-data.cola.agg$feature_large*data.cola.agg$units
data.cola.agg$coupon<-data.cola.agg$coupon*data.cola.agg$units
data.cola.agg$displayall<-data.cola.agg$displayall*data.cola.agg$units
data.cola.agg$featureall<-data.cola.agg$featureall*data.cola.agg$units


    



#aggregating units and dollars   (so the result)
units.sum<-aggregate(cbind(units, dollars) ~  MARKET+CHAIN+store_type+year+week+L4+L5+VOL_EQ+PACKAGE, 
                     data = data.cola.agg, sum, na.rm = TRUE)


#aggregating ad specific variables (so the influence)

#interpretation: one row: "all the DMs (just as an example) showed 167 minor displays in that week"
#                         " all the Rewe showed 845 mayor displays in that week"
tmp<- aggregate(cbind(price,price_deflated,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~ MARKET+CHAIN+store_type+year+week+L4+L5+VOL_EQ+PACKAGE, data = data.cola.agg, sum, na.rm = TRUE)


data.cola.agg <- merge(units.sum,tmp,by=c("MARKET","CHAIN","store_type","year","week","L4","L5","VOL_EQ","PACKAGE")) 
data.cola.aggABSOLUTE <- data.cola.agg


#average price over all the stores in that week
data.cola.agg$price<-data.cola.agg$price/data.cola.agg$units
data.cola.agg$price_deflated<-data.cola.agg$price_deflated/data.cola.agg$units

#taking average of erverything
data.cola.agg$display_minor<-data.cola.agg$display_minor/data.cola.agg$units
data.cola.agg$display_major<-data.cola.agg$display_major/data.cola.agg$units
data.cola.agg$feature_small<-data.cola.agg$feature_small/data.cola.agg$units
data.cola.agg$feature_medium<-data.cola.agg$feature_medium/data.cola.agg$units
data.cola.agg$feature_large<-data.cola.agg$feature_large/data.cola.agg$units
data.cola.agg$coupon<-data.cola.agg$coupon/data.cola.agg$units
data.cola.agg$displayall<-data.cola.agg$displayall/data.cola.agg$units
data.cola.agg$featureall<-data.cola.agg$featureall/data.cola.agg$units

head(data.cola.agg,20)


#######Start: produce own stuff and graphics
# VolSizes <- as.factor(data.cola.agg$VOL_EQ)
# str(VolSizes) 

library(dplyr)
testSet <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(revenuePerCHain =sum(dollars))
testSet

# # A tibble: 2 x 2
# CHAIN revenuePerCHain
# <int>           <dbl>
#   1    33         1557509
#   2    65         1575471


plot(testSet)

testSet2 <- data.cola.agg %>% 
  group_by(L5) %>%
  dplyr::summarize(revenue =sum(dollars))
testSet2

# # A tibble: 4 x 2
# L5   revenue
# <fctr>     <dbl>
#   1 COKE CLASSIC 1110099.5
#   2    DIET COKE 1066700.5
#   3   DIET PEPSI  445617.6
#   4        PEPSI  510562.8

testSet2.5 <- data.cola.agg %>% 
  group_by(L5) %>%
  dplyr::summarize(totalunits =sum(units))
testSet2.5


testSet3 <- data.cola.agg %>% 
  group_by(PACKAGE) %>%
  dplyr::summarize(revenue =sum(dollars))
testSet3


# # A tibble: 2 x 2
# PACKAGE revenue
# <fctr>   <dbl>
#   1  BOTTLE 1 158 657
#  2     CAN 1 974 323



####
#also look at amount and 

#sum up units sold in total 

testSet4 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(totalUnits =sum(units))
testSet4

# # A tibble: 2 x 2
# CHAIN totalUnits
# <int>      <dbl>
#   1    33     682321
#   2    65     941663


testSet5 <- data.cola.agg %>% 
  group_by(L5) %>%
  dplyr::summarize(totalUnits =sum(units))
testSet5


#           L5        totalUnits
# <fctr>      <dbl>
#   1 COKE CLASSIC     603533
#   2    DIET COKE     524807
#   3   DIET PEPSI     219488
#   4        PEPSI     276156


testSet6 <- data.cola.agg %>% 
  group_by(PACKAGE) %>%
  dplyr::summarize(totalUnits =sum(units))
testSet6



# # A tibble: 2 x 2
#       PACKAGE   totalUnits
# <fctr>      <dbl>
#   1  BOTTLE    1017120
#  2     CAN     606864



#revenue per one bottle or coke:
#or mean price per bottle and per can 


testSet7 <- data.cola.agg %>% 
  group_by(PACKAGE) %>%
  dplyr::summarize(meanPrice =mean(price))
testSet7


# # A tibble: 2 x 2
# PACKAGE meanPrice
# <fctr>     <dbl>
#   1  BOTTLE  1.329912
#   2     CAN  3.669948

plot(testSet7)








#histogram of weeks and the revenues  and also units  maybe also prices 

testset8 <- data.cola.agg %>% 
  select(CHAIN,year, week,PACKAGE,units, dollars, price, price_deflated) %>% 
  group_by(CHAIN,year) %>%
  dplyr::summarize(revenue =sum(dollars))
  

testset8$year <- as.factor(testset8$year)
testset8$CHAIN <- as.factor(testset8$CHAIN)

#plot of the revenue for the two chains over all years 
png(filename = paste0("outputs/","revenueDevelopment.png"),
    width = 580, height = 480, units = "px")

ggplot(data=testset8, aes(x=year, y=revenue, group=CHAIN)) +
  geom_line(aes(color = CHAIN), size = 1) +
  geom_point() +
  xlab("Years") +
  ylab("Revenue of Cola") +
  ylim(0, max(testset8$revenue)*1.05) +
  theme(axis.text.x = element_text(angle = 90, size=18, vjust=0.5, hjust=0),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18,face="bold"))+
  theme(legend.text=element_text(size=18))

dev.off()
######


#plot over the weeeks 
testset9 <- data.cola.agg %>% 
  select(CHAIN,year, week,PACKAGE,units, dollars, price, price_deflated) %>% 
  group_by(CHAIN,week) %>%
  dplyr::summarize(revenue =sum(dollars))

testset9$week <- as.factor(testset9$week)
testset9$CHAIN <- as.factor(testset9$CHAIN)

ggplot(data=testset9, aes(x=week, y=revenue, group=CHAIN)) +
  geom_line(aes(color = CHAIN), size = 1) +
  geom_point() +
  ylim(0, max(testset9$revenue)*1.05) +
  theme(axis.text.x = element_text(angle = 90, size=18, vjust=0.5, hjust=0),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18,face="bold"))+
  theme(legend.text=element_text(size=18))



#plot of the price changes
testset10 <- data.cola.agg %>% 
  select(CHAIN,year, week,PACKAGE,units, dollars, price, price_deflated) %>% 
  group_by(CHAIN,year) %>%
  dplyr::summarize(meanPrice =mean(price))

testset10$year <- as.factor(testset10$year)
testset10$CHAIN <- as.factor(testset10$CHAIN)


png(filename = paste0("outputs/","priceDevelopment.png"),
    width = 580, height = 480, units = "px")

ggplot(data=testset10, aes(x=year, y=meanPrice, group=CHAIN)) +
  geom_line(aes(color = CHAIN), size = 1) +
  geom_point() +
  xlab("Years") +
  ylab("Mean Price of Cola") +
  ylim(0, max(testset10$meanPrice)*1.05) +
  theme(axis.text.x = element_text(angle = 90, size=18, vjust=0.5, hjust=0),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18,face="bold"))+
  theme(legend.text=element_text(size=18))
dev.off()

#plot of the price changes (deflated)
testset11 <- data.cola.agg %>% 
  select(CHAIN,year, week,PACKAGE,units, dollars, price, price_deflated) %>% 
  group_by(CHAIN,year) %>%
  dplyr::summarize(meanPriceDeflated =mean(price_deflated))


testset11$year <- as.factor(testset11$year)
testset11$CHAIN <- as.factor(testset11$CHAIN)

png(filename = paste0("outputs/","priceDevelopmentDeflated.png"),
    width = 580, height = 480, units = "px")

ggplot(data=testset11, aes(x=year, y=meanPriceDeflated, group=CHAIN)) +
  geom_line(aes(color = CHAIN), size = 1) +
  geom_point() +
  xlab("Years") +
ylab("Mean Price of Cola (deflated)") +
  ylim(0, max(testset11$meanPriceDeflated)*1.05) +
  theme(axis.text.x = element_text(angle = 90, size=18, vjust=0.5, hjust=0),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18,face="bold"))+
  theme(legend.text=element_text(size=18))

dev.off()

####


png(filename = paste0("outputs/","priceComparison.png"),
    width = 580, height = 480, units = "px")



test <- ggplot(data=testset11, aes(x=year, y=meanPriceDeflated, group=CHAIN)) +
  geom_line(aes(color = CHAIN), size = 1, linetype = 2) +
  geom_point() +
  xlab("Years") +
  ylab("Mean Price of Cola (deflated)") +
  ylim(0, max(testset11$meanPriceDeflated)*1.35) +
  theme(axis.text.x = element_text(angle = 90, size=18, vjust=0.5, hjust=0),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18,face="bold"))+
  theme(legend.text=element_text(size=18))

test +  
  geom_line(data=testset10, aes(x=year, y=testset10$meanPrice,color = CHAIN, group=CHAIN), size = 1) +
  geom_point()

dev.off()
####

#plot of the unit changes  
testset12 <- data.cola.agg %>% 
  select(CHAIN,year, week,PACKAGE,units, dollars, price, price_deflated) %>% 
  group_by(CHAIN,year) %>%
  dplyr::summarize(units =sum(units))


testset12$year <- as.factor(testset12$year)
testset12$CHAIN <- as.factor(testset12$CHAIN)

png(filename = paste0("outputs/","unitsDevelopment.png"),
    width = 580, height = 480, units = "px")

ggplot(data=testset12, aes(x=year, y=units, group=CHAIN)) +
  geom_line(aes(color = CHAIN), size = 1) +
  geom_point() +
  xlab("Years") +
  ylab("Units of Cola") +
  ylim(0, max(testset12$units)*1.05) +
  # xlim(-5000, 5000) +
  theme(axis.text.x = element_text(angle = 90, size=18, vjust=0.5, hjust=0),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18,face="bold"))+
  theme(legend.text=element_text(size=18))

dev.off()



#see if the prices change between coke and pepsi 
testset13 <- data.cola.agg %>% 
  select(CHAIN,year, L4, L5, week,PACKAGE,units, dollars, price, price_deflated) %>% 
  # filter(L4 == "COCA COLA CO")  %>% 
  group_by(L5,year) %>%
  dplyr::summarize(revenue =sum(dollars))
testset13

testset13$year <- as.factor(testset13$year)
testset13$L5 <- as.factor(testset13$L5)


png(filename = paste0("outputs/","productsDevelopment.png"),
    width = 580, height = 480, units = "px")

options(scipen=1000)
  

ggplot(data=testset13, aes(x=year, y=revenue, group=L5)) +
  geom_line(aes(color = L5), size = 1) +
  geom_point() +
  xlab("Years") +
  ylab("Revenue of Cola products") +
  ylim(0, max(testset13$revenue)*1.05) +
  theme(axis.text.x = element_text(angle = 90, size=18, vjust=0.5, hjust=0),
        axis.text.y = element_text(size=18),
      axis.title=element_text(size=18,face="bold"))+
  theme(legend.text=element_text(size=18))
  #theme_minimal()

dev.off()


#####
#marketing activities analysis 

#which percentage used coupons ? 
testset14 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(MeanCouponUse =mean(coupon))

testset14


# testset14
# # A tibble: 2 x 2
# CHAIN MeanCouponUse
# <int>         <dbl>
#   1    33    0.02286995
#   2    65    0.00000000    #never used coupons



  #how much did they safe?

    #average price when coupon yes


    #average price when coupon no


    #did they buy more?

#whats the efffect of display ads? 
testset15 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(MeanDisplayAll =mean(displayall))
testset15 #what does that even mean

#measure of efficiency?
# A tibble: 2 x 2
# CHAIN MeanDisplayAll
# <int>          <dbl>
#   1    33      0.4836346  (drug)
#  2     65      0.4515487 (grocery) 
#                           that would mean that ads in grocery stores are more efficient(?)
#                           

#other features?

testset16 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(MeanDisplaymajor =mean(display_major))
testset16 #what does that even mean

testset17 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(MeanDisplayminor =mean(display_minor))
testset17 #what does that even mean

# # A tibble: 2 x 2
# CHAIN MeanDisplaymajor
# <int>            <dbl>
#   1    33        0.1158815
#   2    65        0.4128319

#

# testset17 #what does that even mean
# # A tibble: 2 x 2
# CHAIN MeanDisplayminor
# <int>            <dbl>
#   1    33       0.36775311
#  2    65       0.03871681


#learning: grocery store (65) does much more major advertising than minor advertising
        # drug store the other way arount.

testset18 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(MeanFeatureAll =mean(featureall))
testset18 #what does that even mean
# 
# # A tibble: 2 x 2
# CHAIN MeanFeatureAll
# <int>          <dbl>
#   1    33      0.3482519
#   2    65      0.5294248


testset19 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(Meanfeature_large =mean(feature_large))
testset19 #what does that even mean

testset20 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(Meanfeature_medium =mean(feature_medium))
testset20 #what does that even mean


testset21 <- data.cola.agg %>% 
  group_by(CHAIN) %>%
  dplyr::summarize(Meanfeature_small =mean(feature_small))
testset21 #what does that even mean


# CHAIN Meanfeature_large
# <int>             <dbl>
#   1    33        0.20382621
# 2    65        0.02876106
# 
# CHAIN Meanfeature_medium
# <int>              <dbl>
#   1    33          0.1444256
# 2    65          0.4913717
# 
# 
# A tibble: 2 x 2
# CHAIN Meanfeature_small
# <int>             <dbl>
#   1    33       0.000000000
# 2    65       0.009292035

####### end own stuff 



#important: hint Klapppt: look for specialities in the data like american holidays (thanks giving, christmas, July 4th)

#end first skript


#task 2b:
# Display a scatterplot matrix of the data
#interaction units and price: there is a max price of 6; the lower the price the more units are bought
#                           interpretation: two graphs: different willingess to pay for each store type 
# in the drug store they are willing to pay more than in the grocery store
# idea: give the stores different colors to distinguish them better

pairs(formula =~(units)+(price)+displayall+featureall, data=data.cola.agg)


#due to averages "feature all" can be between 0 and 1
pairs(formula =~log(units)+log(price)+displayall+featureall, data=data.cola.agg)



model.red<-lm(log(units)~log(price),data=data.cola.agg)
model.full<-lm(log(units)~log(price)+displayall+featureall,data=data.cola.agg)

summary(model.red)

# Call:
#   lm(formula = log(units) ~ log(price), data = data.cola.agg)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.4052 -0.5270  0.0181  0.5129  2.9826 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.47443    0.01309  418.23   <2e-16 ***
# log(price)  -0.87617    0.01380  -63.48   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7285 on 9102 degrees of freedom
# Multiple R-squared:  0.3069,	Adjusted R-squared:  0.3068 
# F-statistic:  4030 on 1 and 9102 DF,  p-value: < 2.2e-16

summary(model.full)

# Call:
#   lm(formula = log(units) ~ log(price) + displayall + featureall, 
#      data = data.cola.agg)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.92883 -0.43117  0.02919  0.41203  2.62456 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.94941    0.01428  346.49   <2e-16 ***
#   log(price)  -0.78354    0.01194  -65.63   <2e-16 ***
#   displayall   0.43551    0.01700   25.62   <2e-16 ***
#   featureall   0.57044    0.01440   39.60   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6192 on 9100 degrees of freedom
# Multiple R-squared:  0.4995,	Adjusted R-squared:  0.4993 
# F-statistic:  3027 on 3 and 9100 DF,  p-value: < 2.2e-16


anova(model.red,model.full)
# Analysis of Variance Table
# 
# Model 1: log(units) ~ log(price)
# Model 2: log(units) ~ log(price) + displayall + featureall
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1   9102 4831.1                                  
# 2   9100 3488.5  2    1342.6 1751.1 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





model.custom1<-lm(log(units)~ log(price)+year+ store_type + L4 + L5 + displayall+featureall + coupon + PACKAGE + VOL_EQ,data=data.cola.agg)
summary(model.custom1)
# Call:
#   lm(formula = log(units) ~ log(price) + year + store_type + L4 + 
#        L5 + displayall + featureall + coupon + PACKAGE + VOL_EQ, 
#      data = data.cola.agg)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.22960 -0.23562 -0.01488  0.23563  2.23213 
# 
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -55.918900   2.973449 -18.806  < 2e-16 ***
#   log(price)      -2.604984   0.029867 -87.220  < 2e-16 ***
#   year             0.030893   0.001485  20.798  < 2e-16 ***
#   store_typeGROC   0.052521   0.009007   5.831 5.70e-09 ***
#   L4PEPSICO INC   -0.706420   0.011546 -61.182  < 2e-16 ***
#   L5DIET COKE     -0.088455   0.011404  -7.757 9.66e-15 ***
#   L5DIET PEPSI    -0.170926   0.011398 -14.996  < 2e-16 ***
#   L5PEPSI                NA         NA      NA       NA    
# displayall       0.144126   0.011020  13.079  < 2e-16 ***
#   featureall       0.060847   0.011671   5.214 1.89e-07 ***
#   coupon           0.525111   0.040002  13.127  < 2e-16 ***
#   PACKAGECAN       2.091639   0.032278  64.800  < 2e-16 ***
#   VOL_EQ                 NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#from here on: checking out the new exercise code (30th May etc)


 


