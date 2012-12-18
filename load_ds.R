library(foreach)
library(itertools)
library(xtable)
library(plyr)
library(ggplot2)
library(reshape2)
library(ade4)
library(MASS)
library(randomForest)

#----------------------------------------------------
# This survey data went out to 15k customers but in a
# biasest: picking those with the highest revenue
#
# It would be good to have the list of merchants that
# got the survey but did not respond.
#----------------------------------------------------

#----------------------------------------------------
# The goal of this analysis is to:
# a) Identify which variables are predictors of LTV-CAC
# b) Identify some profitable clusters
#----------------------------------------------------

# Import the survey data
#dat <- read.csv('survey_results.csv', sep="\t")
path <- file.path(Sys.getenv("HOME"), "Documents", "data")
dat <- read.delim(file.path(path, "input", "Survey Results.txt"), stringsAsFactors=TRUE)
# dat <- read.csv('survey_results2.csv')

# Clean up the data and get it into the right data types
dat$StartDate <- strptime(as.character(dat$StartDate), format="%m/%d/%Y %H:%M")
dat$EndDate <- strptime(as.character(dat$StartDate), format="%m/%d/%Y %H:%M")
dat$X_orders_lm <- as.integer(as.character(dat$X_orders_lm))

# Let's assume nulls in orders means 0 orders
dat$X_orders_lm[which(is.na(dat$X_orders_lm))] <- 0

# Order size when no present is NA
dat$X_ordersize_lm <- as.integer(as.character(dat$X_ordersize_lm))

# Reduce countries to a smaller number of regions
dat$Loc.Region <- as.character(dat$country)
dat$Loc.Region[dat$Loc.Region %in% c("AU", "GB", "CA", "NZ", "IE", "ZA")] <- "EnglishNonUS"
# Let's put Israel in Europe
dat$Loc.Region[dat$Loc.Region %in% c("BE", "NL", "DE", "ES", "IL")] <- "Europe"
dat$Loc.Region[dat$Loc.Region %in% c("MX", "CO", "CR")] <- "LatinAmerica"
dat$Loc.Region[dat$Loc.Region %in% c("HK", "JP", "MY", "SG")] <- "Asia"
dat$Loc.Region <- factor(dat$Loc.Region)

# Convert some survey values into logicals
dat$Physical <- ifelse(dat$Physical=='Physical',TRUE,FALSE)
dat$Switched <- ifelse(dat$Switched=='Switched',TRUE,FALSE)
dat$Experienced <- ifelse(dat$Experience_Survey=='Experienced with eCommerce',TRUE,FALSE)

# Convert marketing budget from a factor to numeric values; hard to know what to do for > $2k
dat$Marketing.Budget <- rep(0, length(dat$Marketing_Budget))
dat$Marketing.Budget[dat$Marketing_Budget=="$201 - $500"] <- 350
dat$Marketing.Budget[dat$Marketing_Budget=="$101 - $200"] <- 150
dat$Marketing.Budget[dat$Marketing_Budget=="$51 - $100"] <- 75
dat$Marketing.Budget[dat$Marketing_Budget=="$501 - $1,000"] <- 750
dat$Marketing.Budget[dat$Marketing_Budget=="Over $2,000"] <- 3000

# Let's combine regional and international into a shipping range.
dat$Ship.Range <- rep("Regional", length(dat$Ship.Nationwide))
dat$Ship.Range[dat$Ship.Nationwide=="Nationwide"] <- "Nationwide"
dat$Ship.Range[dat$Ship.International=="International"] <- "International"
dat$Ship.Range <- factor(dat$Ship.Range)
dat$Own.Domain <- ifelse(dat$own_domain==1, TRUE, FALSE)
# dat$Subdomain <- ifelse(dat$domaintype=="subdomain", TRUE, FALSE) # domaintype does not exist
dat$Annual.Plan <- ifelse(dat$billingcycle=="Annually", TRUE, FALSE)

# Interestingly, there are some regional that also ship international
# TODO: This confuses me: we have stores that don't ship nationwide yet ship internationally.  We dont have any that ship only regionally; so my Ship.Range value is suspect
which(dat$Ship.Nationwide=="NotNationWide" & dat$Ship.International=="International")

# This makes me thing that either 'industry' is bogus or that data is joined together wrong.
length(dat[as.character(dat$Industry_Survey)== as.character(dat$industry), c("Industry_Survey")])

# Firmagraphic variables
firmagraphic <- c("Industry_Survey", "Physical", "Switched", "Products_From", "Experienced", "Loc.Region")

# Behavioural variables
behavioral <-c("ShipFromPlaces", "Staff_Total", "Staff_Store", "Marketing.Budget", "Marketing_Spend", "Marketing_Spend_Other", "age", "Own.Domain", "Ship.Range", "plan_name", "Annual.Plan") # "Subdomain",

# Success variables: things that happen as a result of demographics and behaviours; not some success may not be
# observered.  We don't know if a physical values having an on-line store to bring people to the physical store
# (i.e. revenue isn't necessarily the goal)
# Also a store that makes their own products may have higher profit margins.
success <- c("X_revenue_lm", "X_orders_lm", "MajorityBusiness")

# Let's try hierarchical clustering
# http://www.statmethods.net/advstats/cluster.html
# http://stackoverflow.com/questions/5048638/automatically-expanding-an-r-factor-into-a-collection-of-1-0-indicator-variables
firm.dat <- dat[,firmagraphic]
firm.dat.dummy <- acm.disjonctif(firm.dat)
fit <- hclust(dist(firm.dat.dummy), method="ward")
groups <- cutree(fit, k=5)
firm.dat$Cluster <- groups
firm.dat$X_revenue_lm <- dat$X_revenue_lm
firm.dat$X_orders_lm <- dat$X_orders_lm
firm.dat$age <- dat$age

# cluster on revenue
test <- firm.dat.dummy
test$X_revenue_lm <- dat$X_revenue_lm
test$X_orders_lm <- dat$X_orders_lm
test$age <- as.numeric(dat$age)
test[, names(test) %in% c("X_revenue_lm", "X_orders_lm", "age")] <- data.frame(sapply(test[, names(test) %in% c("X_revenue_lm", "X_orders_lm", "age")], scale))
fit <- hclust(dist(test), method="ward")
groups <- cutree(fit, k=3)
plot(fit)
rect.hclust(fit, k=3, border="red")
aggregate(test$X_revenue_lm,list(groups),mean)
aggregate(test$X_revenue_lm,list(groups),sd)
qplot(factor(groups), X_revenue_lm, data=test, geom=c('boxplot'))
qplot(factor(groups), age, data=test, geom=c('boxplot'), ylim=c(0,1))


# Some visualisations:
rev.hist <- qplot(X_revenue_lm, data=firm.dat, geom=c('histogram'), binwidth=1000)
order.hist <- qplot(X_orders_lm, data=firm.dat, geom=c('histogram'), binwidth=100)
rev.hist.trunc <- qplot(X_revenue_lm, data=firm.dat, geom=c('histogram'), binwidth=100, xlim = c(0,5000))
rev.age <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point')) + geom_smooth(method = "lm")
# Stores stablise in revenue at about 400 days
rev.age.loess <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point'), ylim=c(0,5000)) + geom_smooth(method = "loess")
rev.age.physical <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point'), colour=Physical) + geom_smooth(method = "lm")
rev.age.switched <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point'), colour=Switched) + geom_smooth(method = "lm")
rev.age.products <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point'), colour=Products_From) + geom_smooth(method = "lm")
rev.age.experienced <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point'), colour=Experienced) + geom_smooth(method = "lm")
rev.age.region <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point'), colour=Loc.Region) + geom_smooth(method = "lm")
rev.age.cluster <- qplot(age, X_revenue_lm, data=firm.dat, geom=c('point'), colour=Cluster) + geom_smooth(method = "lm")
firm.dat.trim <- firm.dat[firm.dat$age > 400 & firm.dat$age < 800,]
rev.age.industry <- qplot(X_revenue_lm, Industry_Survey, data=firm.dat.trim, geom=c('boxplot'))
firm.dat.trim$Ind.S <- factor(substr(as.character(firm.dat.trim$Industry_Survey),0,2))
rev.industry2 <- qplot(Ind.S, X_revenue_lm, data=firm.dat.trim, geom=c('boxplot'))
rev.industry3 <- qplot(Ind.S, X_revenue_lm, data=firm.dat.trim, geom=c('jitter'))
tb <- table(firm.dat$Industry_Survey)
rev.industry4 <- qplot(Ind.S, X_revenue_lm, data=firm.dat.trim, geom=c('boxplot'), ylim=c(0,5000))

# Which industries have the highest percent physical stores?
percent.physical.stores <- sort(table(firm.dat$Industry_Survey[firm.dat$Physical==TRUE])/table(firm.dat$Industry_Survey),decreasing=TRUE)

# Do some industry have larger order sizes?
ordersize.industry2 <- qplot(Ind.S, X_revenue_lm/X_orders_lm, data=firm.dat.trim, geom=c('boxplot'))

industry.products <- ggplot(firm.dat, aes(x=Industry_Survey)) + geom_bar(position="stack") + facet_grid(. ~ Products_From) + coord_flip()

# TODO: Get Login_OK from dataengine database and see if that's important - add it to behaviour fields; 


# Let's look at the relationship among success metrics
success.dat <- dat[,success]

# Let's test some data integrity:
#   -- we see there are orders with no revenue
#   -- there are four duplicates, which could skew
revenue.no.orders <- with(success.dat, success.dat[X_revenue_lm == 0 & X_orders_lm > 0,])
dup.storeid <- sort(table(dat$StoreID), decreasing=TRUE)
num.dup.storeid <- sum(dup.storeid > 1)

# How important is ordersize?  What are the numbers of high-volume vs. low-volume stores
rev.orders <- qplot(X_orders_lm, X_revenue_lm, data=dat, geom=c('point'))
rev.orders.outliers <- with(dat, dat[X_revenue_lm > 100000 | X_orders_lm > 2000,])

# Who has large orders?
large.orders <- with(dat, dat[is.finite(X_revenue_lm/X_orders_lm) & X_revenue_lm/X_orders_lm > 1000,])

# Is average order size important?
rev.orders2 <- qplot(X_orders_lm, X_revenue_lm, data=dat, geom=c('point'), xlim=c(0,100), ylim=c(0,1000))

# How many stores are getting a large number of orders?  Not many
ordersize.density <- qplot(X_revenue_lm/X_orders_lm, data=dat, geom='histogram', binwidth=10)
high.rev.dat <- dat[dat$X_revenue_lm > 5000,]
ordersize.density2 <- qplot(X_revenue_lm/X_orders_lm, data=high.rev.dat, geom='histogram', binwidth=10)

# Look at behaviour as it relates to demographics
firm.behavior.dat <- dat[,c(firmagraphic, behavioral)]
firm.behavior.dat$X_revenue_lm <- dat$X_revenue_lm
# firm.behavior.dat$X_revenue_lm <- dat$ltv
qplot(Industry_Survey, data=firm.behavior.dat, geom='bar')

# How many do not have their own domain? 42 - possibly slightly more successful
table(firm.behavior.dat$Own.Domain)
rev.domain <- qplot(X_revenue_lm, data=firm.behavior.dat, colour=Own.Domain, geom=c('density'))
rev.domain + scale_x_log10() # See it better

# Any difference with subdomain?
industry.plan <- qplot(Industry_Survey, data=firm.behavior.dat, geom="bar", fill=plan_name, position="fill") + coord_flip()
products.plan <- qplot(Products_From, data=firm.behavior.dat, geom="bar", fill=plan_name, position="fill") + coord_flip()
rev.shipfromplaces <- qplot(ShipFromPlaces, X_revenue_lm, data=firm.behavior.dat, geom=c('point')) + geom_smooth(method = "lm")

# Let's see which variables relate to revenue
model.vars <- c("Physical", "Switched", "Products_From", "Experienced", "Loc.Region", "Marketing.Budget", "age", "Annual.Plan", "Ship.Range", "plan_name", "X_revenue_lm") # "Subdomain",

m1 <- lm(X_revenue_lm ~ ., data=firm.behavior.dat[,model.vars])
anova(m1)
## Analysis of Variance Table

## Response: X_revenue_lm
##                    Df     Sum Sq    Mean Sq F value    Pr(>F)    
## Physical            1 1.1586e+09 1.1586e+09  3.0043 0.0833464 .  
## Switched            1 9.2955e+09 9.2955e+09 24.1038 1.062e-06 ***
## Products_From       2 1.3606e+09 6.8031e+08  1.7641 0.1718677    
## Experienced         1 2.0219e+09 2.0219e+09  5.2429 0.0222391 *  
## Loc.Region          4 1.0359e+09 2.5898e+08  0.6715 0.6117955    
## Marketing.Budget    1 2.0574e+10 2.0574e+10 53.3489 5.610e-13 ***
## age                 1 4.7965e+09 4.7965e+09 12.4374 0.0004395 ***
## Annual.Plan         1 8.6572e+08 8.6572e+08  2.2449 0.1343682    
## Subdomain           1 6.2144e+06 6.2144e+06  0.0161 0.8990113    
## Ship.Range          1 2.8578e+08 2.8578e+08  0.7411 0.3895251    
## plan_name           4 3.0265e+10 7.5664e+09 19.6200 1.425e-15 ***
## Residuals        1023 3.9452e+11 3.8565e+08                      
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
# So important variables, in order, are plan_name, age, Marketing.Budget, Switched, Experienced and Physical, in that order.

# Let's see how well these predict age and therefore LTV
m2 <- lm(age ~ ., data=firm.behavior.dat[,model.vars])
anova(m2)
## Analysis of Variance Table

## Response: age
##                    Df   Sum Sq Mean Sq F value    Pr(>F)    
## Physical            1   380917  380917  6.0383 0.0141641 *  
## Switched            1     1807    1807  0.0286 0.8656524    
## Products_From       2    64050   32025  0.5077 0.6020563    
## Experienced         1  5938089 5938089 94.1303 < 2.2e-16 ***
## Loc.Region          4   636034  159009  2.5206 0.0397126 *  
## Marketing.Budget    1     2243    2243  0.0356 0.8504688    
## Annual.Plan         1  1845778 1845778 29.2592 7.883e-08 ***
## Subdomain           1    27215   27215  0.4314 0.5114449    
## Ship.Range          1    77510   77510  1.2287 0.2679237    
## plan_name           4  1294336  323584  5.1294 0.0004289 ***
## X_revenue_lm        1   570891  570891  9.0497 0.0026916 ** 
## Residuals        1023 64534651   63084                      
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# Doesn't work for classification
m3 <- lda(plan_name ~ ., data=firm.behavior.dat[,model.vars])


# Let's look at the co-variance between these variables

cor.marketing.switched <- with(firm.behavior.dat, cor(Marketing.Budget, Switched, use = "everything"))
cor.marketing.physical <- with(firm.behavior.dat, cor(Marketing.Budget, Physical, use = "everything"))
cor.marketing.experienced <- with(firm.behavior.dat, cor(Marketing.Budget, Experienced, use = "everything"))
cor.switched.experienced <- with(firm.behavior.dat, cor(Switched, Experienced, use = "everything"))
cor.physical.experienced <- with(firm.behavior.dat, cor(Physical, Experienced, use = "everything"))
cor.physical.rev <- with(firm.behavior.dat, cor(Physical, X_revenue_lm, use = "everything"))

# CAC Numbers
# On-line: $350
# Acquisition: $175
# Partner: $160

# Let's look at a measure of LTV based on age and plan_name, assuming.
dat$Plan.Value <- 25
dat$Plan.Value[dat$plan_name=='Diamond Plan'] <- 300
dat$Plan.Value[dat$plan_name=='Gold Plan'] <- 80
dat$Plan.Value[dat$plan_name=='Platinum Plan'] <- 150
dat$Plan.Value[dat$plan_name=='Silver Plan'] <- 40
dat$LTV.proxy <- dat$Plan.Value*dat$age

form <- "LTV.proxy ~ Physical + Switched + Products_From + Experienced + Loc.Region + Staff_Store + Staff_Total + Marketing.Budget + Own.Domain + Subdomain + Ship.Range + X_revenue_lm + X_orders_lm + MajorityBusiness"

m <- randomForest(as.formula(form), dat, importance=TRUE)

importance(m)
##                     %IncMSE IncNodePurity
## Physical          1.7549556   24117074924
## Switched          2.4844550   22131479002
## Products_From    11.8083383   56800466219
## Experienced       5.2041523   22507530899
## Loc.Region        6.1694528   22254441408
## Staff_Store       2.6671560   55204859863
## Staff_Total       5.4893915   70543029268
## Marketing.Budget -0.2603349   55095859508
## Own.Domain        1.2394493    1996433268
## Subdomain         1.4072615   26389991441
## Ship.Range        2.1020595   23295364947
## X_revenue_lm     20.2021409  219334154058
## X_orders_lm      20.5045249  219260168775
## MajorityBusiness  7.8185748   28395502647
## 

# Given the high number of stores open longer than a year without revenue
dat$Viable.Rev <- ifelse(dat$X_revenue_lm > 300, TRUE, FALSE)
age.viable <- qplot(age, data=dat, colour=Viable.Rev, geom=c('density'))
age.viable2 <- qplot(age, data=dat, fill=Viable.Rev, geom='histogram', binwidth=20)

age.switched <- qplot(age, data=dat, colour=Switched, geom=c('density'))


# Given the high number of stores without viable revenue that are old, let's just assume l
#dat$est.lt <- with(dat, age + dat$Viable.Rev*max.lt + (!dat$Viable.Rev)*(Annual.Plan*6 + (!Annual.Plan)*3))
dat$est.lt <-  dat$age*2

# First we note that most respondents had viable revenue.  This is not true in the population.
# So most of these respondents don't churn and have high life-time value to begin with
table(dat$Viable.Rev)
# FALSE  TRUE 
#  368   674 

# What are the personas?

dat$persona <- 'other'

# 1) Switched, marketing spend, revenue, high plan, reseller (Long-life)  (Acquisition)
# There are 29 of these
p1 <- with(dat, Switched==TRUE & Physical==FALSE & Products_From=='I resell products' & X_revenue_lm > 5000 & plan_name %in% c('Diamond Plan', 'Gold Plan', 'Platinum Plan') & age > 200)
dat$persona[p1] <- 'p1'

# 2) Physical, low revenue, high plan?, (Long-life)  (GPS or Heartland) -- 12
p2 <- with(dat, Physical==TRUE & X_revenue_lm < 100 & plan_name %in% c('Diamond Plan', 'Gold Plan', 'Platinum Plan') & age > 200)
dat$persona[p2] <- 'p2'

# 3) Own products, long life, steady revenue -- 66
p3 <- with(dat, Products_From=='I make my own products' & X_revenue_lm > 1000 & plan_name %in% c('Diamond Plan', 'Gold Plan', 'Platinum Plan', 'Silver Plan') & age > 200)
dat$persona[p3] <- 'p3'
                    
# 3) Physical, low revenue, high plan?, (Long-life)
                    
# 4) On-line only - sell own products, high margins, medium revenue, medium volume, long-lifetime

# 5) Low plan, low revenue, medium products, drop ship, short life 

# What do we need to know about them?

# TODO: What we need is LTV:CAC ratio -- we need to look at stores in these buckets and work out the true LTV and CAC.

