# load libraries
library(ggplot2)
library(grid)
library(gridExtra)

#===
# EXPLORATION
#===

# set path to data input and output and load data
path <- file.path(Sys.getenv("HOME"), "Documents", "data")
dat <- read.delim(file.path(path, "input", "Survey Results.txt"), stringsAsFactors=TRUE)

# plan name and revenue
dat$plan_name <- factor(dat$plan_name, levels=c("Bronze Plan", "Silver Plan", "Gold Plan", "Platinum Plan", "Diamond Plan"), ordered=TRUE)
qplot(plan_name, X_revenue_lm, data=dat, geom="boxplot")
val <- aggregate(data.frame(total_value_per_month=dat$plan_value), by=list(plan_name=dat$plan_name), sum)
grid.arrange(qplot(plan_name, data=dat, geom="bar") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()),  
	qplot(plan_name, total_value_per_month, data=val, geom="bar") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()),
	qplot(plan_name, X_revenue_lm, data=dat, geom="boxplot", ylim=c(0, 100000)), nrow=3)
anova(lm(X_revenue_lm ~ plan_name, data=dat))
qplot(plan_value, X_revenue_lm, data=dat, geom="point") + geom_smooth(method = "lm")
with(dat, cor(plan_value, X_revenue_lm, use="everything"))

# marketing budget
dat$Marketing.Budget <- rep(0, length(dat$Marketing_Budget))
dat$Marketing.Budget[dat$Marketing_Budget=="$51 - $100"] <- 75
dat$Marketing.Budget[dat$Marketing_Budget=="$101 - $200"] <- 150
dat$Marketing.Budget[dat$Marketing_Budget=="$201 - $500"] <- 350
dat$Marketing.Budget[dat$Marketing_Budget=="$501 - $1,000"] <- 750
dat$Marketing.Budget[dat$Marketing_Budget=="$1,001 - $2,000"] <- 1500
dat$Marketing.Budget[dat$Marketing_Budget=="Over $2,000"] <- 3000
qplot(plan_value, Marketing.Budget, data=dat, geom=c("point")) + geom_smooth(method = "lm")
with(dat, cor(plan_value, Marketing.Budget, use="everything"))
with(dat[dat$plan_name != "Diamond Plan", ], cor(plan_value, Marketing.Budget, use="everything"))
dat$Marketing_Budget <- factor(dat$Marketing_Budget, levels=c("$0", "$1-$10", "$11 - $50", "$51 - $100", "$101 - $200", "$201 - $500", "$501 - $1,000", "$1,001 - $2,000", "Over $2,000"), ordered=TRUE)
dat$Marketing_Budget <- as.character(dat$Marketing_Budget)
ggplot(dat, aes(plan_name, fill=Marketing_Budget)) + geom_bar(position="dodge")

# customer value and age
qplot(customer_value, age, data=dat, colour=plan_name)

# customer plan and age
qplot(plan_value, age, data=dat) + geom_smooth(method = "lm")
qplot(plan_name, age, data=dat, geom=c("boxplot")) + coord_flip()
anova(lm(age ~ plan_name, data=dat))
anova(lm(age ~ plan_name, data=dat[dat$plan_name != "Diamond Plan", ]))

# maybe LTV can be log of revenue last month multiplied by plan value
plot(dat$X_revenue_lm, log(dat$X_revenue_lm + 1))
plot(log(dat$X_revenue_lm + exp(1))*dat$plan_value)
dat$success_metric <- log(dat$X_revenue_lm + exp(1))*dat$plan_value
grid.arrange(qplot(dat$plan_value, geom="histogram", binwidth=10),
	qplot(dat$X_revenue_lm, geom="histogram", binwidth=10000),
	qplot(log(dat$X_revenue_lm + exp(1)), geom="histogram", binwidth=1),
	qplot(log(dat$X_revenue_lm + exp(1))*dat$plan_value, geom="histogram", binwidth=70))
# dat$success_metric <- dat$X_revenue_lm
# dat$success_metric <- dat$plan_value
qplot(plan_name, success_metric, data=dat, geom="boxplot")
qplot(plan_name, success_metric, data=dat, geom="violin")
qplot(dat$success_metric, geom="histogram", binwidth=70)
qplot(Products_From, success_metric, data=dat, geom=c("boxplot")) + coord_flip()
qplot(Physical, plan_value, data=dat, geom=c("boxplot")) + coord_flip()

#===
# MODEL
#===

# reduce countries to a smaller number of regions
dat$Loc.Region <- as.character(dat$country)
dat$Loc.Region[dat$Loc.Region %in% c("AU", "GB", "CA", "NZ", "IE", "ZA")] <- "EnglishNonUS"
dat$Loc.Region[dat$Loc.Region %in% c("BE", "NL", "DE", "ES", "IL")] <- "Europe"
dat$Loc.Region[dat$Loc.Region %in% c("MX", "CO", "CR")] <- "LatinAmerica"
dat$Loc.Region[dat$Loc.Region %in% c("HK", "JP", "MY", "SG")] <- "Asia"
dat$Loc.Region <- factor(dat$Loc.Region)

# combine regional and international into a shipping range.
dat$Ship.Range <- rep("Regional", length(dat$Ship.Nationwide))
dat$Ship.Range[dat$Ship.Nationwide == "Nationwide"] <- "Nationwide"
dat$Ship.Range[dat$Ship.International == "International"] <- "International"
dat$Ship.Range <- factor(dat$Ship.Range)

# do something with blanks in Physical_Before
levels(dat$Physical_Before)[levels(dat$Physical_Before) == ""] <- NA # should be NA

# make some fields logical
dat$Own.Domain <- ifelse(dat$own_domain == 1, TRUE, FALSE)
dat$Ship.From.Places <- ifelse(dat$ShipFromPlaces > 1, "More than one", "One")

# staff (numbers don't match - it should be a character field so just split on one employee)
dat$Staff_Store_Ratio <- dat$Staff_Store/dat$Staff_Total
dat$Staff_Store_Ratio <- ifelse(dat$Staff_Store_Ratio < 1, "Low", "High")
dat$Staff.Store <- ifelse(dat$Staff_Store > 1, "More than one", "One")
dat$Staff.Total <- ifelse(dat$Staff_Total > 1, "More than one", "One")

# age
dat$Age <- ifelse(dat$age > 365, "Above one year", "Under one year")

# create data for model
dat_model <- dat
model.vars <- c("success_metric",
				"Physical",
				"Switched",
				"Products_From",
				"Experience_Survey",
				"Loc.Region",
				"Marketing_Budget", # maybe try Marketing.Budget
				# "age",
				"Age",
				"billingcycle",
				"Ship.Range",
				"Own.Domain",
				# "Marketing_Spend",
				# "X_ordersize_lm",
				"Industry_Survey",
				"MajorityBusiness",
				# "ShipFromPlaces",
				"Ship.From.Places",
				"cac",
				# "channel",
				# "experience",
				# "industry" # it might be bogus - as.character(dat$Industry_Survey)[as.character(dat$industry) == "Education"]
				"Staff.Store",
				"Staff_Store_Ratio",
				"Staff.Total"
				)
dat_model <- dat_model[names(dat_model) %in% model.vars]

# feature scaling of numeric data
ind <- sapply(dat_model, class) == "numeric" | sapply(dat_model, class) == "integer"
dat_model[, ind] <- data.frame(sapply(dat_model[ind], scale))

# just remove NA rows for now
# r <- ((which(is.na(dat_model)) - 1) %% dim(dat_model)[1]) + 1
# dat_model <- dat_model[!1:dim(dat_model)[1] %in% r, ]

# lm and anova
model <- lm(success_metric ~ ., data=dat_model)
anova(model)
summary(model)

# randomly arrange the data and divide it into a training and test
n <- dim(dat_model)[1]
n_train <- floor(n*0.6)
dat_model <- dat_model[sample(1:n), ]
train <- dat_model[1:n_train,]
n_cv <- floor((dim(dat_model)[1] - n_train)/2)
cv <- dat_model[(n_train+1):(n_train+1+n_cv), ]
n_test <- floor(dim(dat_model)[1] - n_train - n_cv)
test <- dat_model[(n_train+1+n_test+1):dim(dat_model)[1], ]

# predict
model <- lm(success_metric ~ ., data=train)
pred_success_metric <- predict(model, newdata=x_newdata, type="response")

# check residuals
plot(cv$success_metric - pred_success_metric) # check for outliers
hist(cv$success_metric - pred_success_metric, 100)
rms <- sqrt(mean((cv$success_metric - pred_success_metric)^2))

#===
# CLUSTERING
#===

# k-means on success metric
cl <- kmeans(dat$success_metric, 3)
dat$cluster <- cl$cluster
dat <- merge(dat, aggregate(data.frame(success_metric_median=dat$success_metric), list(cluster=cl$cluster), median))
dat$success_metric_cluster <- cl$cluster
qplot(success_metric, rnorm(length(success_metric)), data=dat, colour=as.character(cluster), xlim=c(0, max(dat$success_metric)))
jitter <- rnorm(length(dat$success_metric))
qplot(dat$success_metric, geom="histogram", binwidth=70, xlim=c(0, 3500)) +
		theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())
grid.arrange(qplot(success_metric, jitter, data=dat, colour=as.character(cluster), xlim=c(0, max(dat$success_metric))) +
		theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()), # legend.position="none", 
	qplot(success_metric, jitter, data=dat, colour=plan_name, xlim=c(0, max(dat$success_metric))), # + theme(legend.position="none"),
nrow=2)

# boxplots
dat$Marketing <- ifelse(is.na(match(dat$Marketing_Budget, c("$1,001 - $2,000", "Over $2,000"))), "Mark.Low", "Mark.High")
dat$Industry <- ifelse(is.na(match(dat$Industry_Survey, c("Art & Crafts", "Education", "Music, Books & DVDs", "Non-Profit", "Toys and Games", "Automotive & Motorcross", "Home & Garden", "Wedding & Bridal"))), "Ind.Low", "Ind.High")
dat$cluster_group <- ifelse(dat$success_metric_median > 400, "Clust.High", "Clust.Low")
dat$resell_group <- ifelse(dat$Products_From == "I resell products", "Resell", "Not.Resell")
plot(table(dat$cluster_group, dat$Industry))
plot(table(dat$cluster_group, dat$Marketing))
plot(table(dat$cluster_group, dat$Physical))
plot(table(dat$cluster_group, dat$resell_group))
plot(table(dat$Products_From, dat$plan_name))
plot(table(dat$cluster_group, dat$resell_group, dat$Industry))
plot(table(dat$cluster_group, dat$resell_group, dat$Marketing))

# agg success metric using median - reseller, industry
agg <- aggregate(data.frame(success_metric_median=dat$success_metric), list(reseller=dat$resell_group, industry=dat$Industry), median)
agg$group <- paste(agg$industry, agg$reseller, sep="_")
agg <- agg[order(agg$success_metric_median), ]
dat$group <- factor(paste(dat$Industry, dat$resell_group, sep="_"), levels=agg$group)
agg <- merge(agg, aggregate(data.frame(count=dat$group), list(group=dat$group), length))
qplot(group, success_metric, data=dat, geom="boxplot") + coord_flip()
qplot(group, success_metric, data=dat, geom="violin") + coord_flip()
agg$success_metric_median <- round(agg$success_metric_median)
agg <- agg[rev(order(agg$success_metric_median)), ]
write.table(agg, file=file.path(path, "output", "agg_reseller_industry.txt"), sep="\t", quote=FALSE, row.names=FALSE)

# agg success metric using median - marketing, industry
agg <- aggregate(data.frame(success_metric_median=dat$success_metric), list(marketing=dat$Marketing, industry=dat$Industry), median)
agg$group <- paste(agg$industry, agg$marketing, sep="_")
agg <- agg[order(agg$success_metric_median), ]
dat$group <- factor(paste(dat$Industry, dat$Marketing, sep="_"), levels=agg$group)
agg <- merge(agg, aggregate(data.frame(count=dat$group), list(group=dat$group), length))
qplot(group, success_metric, data=dat, geom="boxplot") + coord_flip()
qplot(group, success_metric, data=dat, geom="violin") + coord_flip()
agg$success_metric_median <- round(agg$success_metric_median)
agg <- agg[rev(order(agg$success_metric_median)), ]
write.table(agg, file=file.path(path, "output", "agg_marketing_industry.txt"), sep="\t", quote=FALSE, row.names=FALSE)

# agg success metric using median - marketing, reseller
agg <- aggregate(data.frame(success_metric_median=dat$success_metric), list(marketing=dat$Marketing, reseller=dat$resell_group), median)
agg$group <- paste(agg$reseller, agg$marketing, sep="_")
agg <- agg[order(agg$success_metric_median), ]
dat$group <- factor(paste(dat$resell_group, dat$Marketing, sep="_"), levels=agg$group)
agg <- merge(agg, aggregate(data.frame(count=dat$group), list(group=dat$group), length))
qplot(group, success_metric, data=dat, geom="boxplot") + coord_flip()
qplot(group, success_metric, data=dat, geom="violin") + coord_flip()
agg$success_metric_median <- round(agg$success_metric_median)
agg <- agg[rev(order(agg$success_metric_median)), ]
write.table(agg, file=file.path(path, "output", "agg_marketing_reseller.txt"), sep="\t", quote=FALSE, row.names=FALSE)

# k-means on revenue
dat_filt <- dat[dat$X_revenue_lm < 100000, ]
cl <- kmeans(dat_filt$X_revenue_lm, 4)
dat_filt$cluster <-cl$cluster
grid.arrange(qplot(dat_filt$X_revenue_lm, geom="histogram", binwidth=70, xlim=c(0, 3500)) +
		theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()),
	qplot(X_revenue_lm, rnorm(length(X_revenue_lm)), data=dat_filt, colour=as.character(cluster), xlim=c(0, max(dat_filt$X_revenue_lm))) +
		theme(legend.position="none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()),
	qplot(X_revenue_lm, rnorm(length(X_revenue_lm)), data=dat_filt, colour=as.character(plan_name), xlim=c(0, max(dat_filt$X_revenue_lm))) +
		theme(legend.position="none"), nrow=3)

# scatter plot matrix
# splom( ~ dat[names(dat %in% c(""))], groups=dat$plan_name, auto.key=TRUE)

#===
# TOP DOWN
#===

# create top-down segments
loser_larry <- dat[dat$Marketing == "Mark.Low" & dat$Staff_Total == 1 & dat$X_revenue_lm == 0, ]
hobby <- dat[dat$Marketing == "Mark.Low" & dat$Physical == "NotPhysical" & dat$Staff_Total == 1 & dat$X_revenue_lm > 0, ]
small_business <- dat[dat$Marketing == "Mark.Low" & dat$Physical == "NotPhysical" & dat$Staff_Total > 1 & dat$Staff_Total <= 4, ]
medium_business <- dat[dat$Marketing == "Mark.Low" & dat$Staff_Total == 5, ]
large_business <- dat[dat$Marketing == "Mark.High" & dat$Staff_Total == 10, ]

# count
persona_count <- data.frame(count=c(dim(loser_larry)[1], dim(hobby)[1], dim(small_business)[1], dim(medium_business)[1], dim(large_business)[1]))
persona_count$persona <- c("loser_larry", "hobby", "small_business", "medium_business", "large_business")
ggplot(persona_count, aes(x=persona, y=count)) + geom_bar(stat = "identity") + coord_flip()

# plot revenue
grid.arrange(ggplot(hobby, aes(x=X_revenue_lm)) + geom_histogram(binwidth=0.2) + scale_x_log10(limits=c(1e-1,1e7)) + theme(axis.title.x=element_blank()),
	ggplot(small_business, aes(x=X_revenue_lm)) + geom_histogram(binwidth=0.2) + scale_x_log10(limits=c(1e-1,1e7)) + theme(axis.title.x=element_blank()),
	ggplot(medium_business, aes(x=X_revenue_lm)) + geom_histogram(binwidth=0.2) + scale_x_log10(limits=c(1e-1,1e7)) + theme(axis.title.x=element_blank()),
	ggplot(large_business, aes(x=X_revenue_lm)) + geom_histogram(binwidth=0.2) + scale_x_log10(limits=c(1e-1,1e7)), nrow=4)

# plot age
grid.arrange(ggplot(loser_larry, aes(x=age)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 1250)) + theme(axis.title.x=element_blank()),
	ggplot(hobby, aes(x=age)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 1250)) + theme(axis.title.x=element_blank()),
	ggplot(small_business, aes(x=age)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 1250)) + theme(axis.title.x=element_blank()),
	ggplot(medium_business, aes(x=age)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 1250)) + theme(axis.title.x=element_blank()),
	ggplot(large_business, aes(x=age)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 1250)), nrow=5)

# plot success_metric
grid.arrange(ggplot(loser_larry, aes(x=success_metric)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 3400)) + theme(axis.title.x=element_blank()),
	ggplot(hobby, aes(x=success_metric)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 3400)) + theme(axis.title.x=element_blank()),
	ggplot(small_business, aes(x=success_metric)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 3400)) + theme(axis.title.x=element_blank()),
	ggplot(medium_business, aes(x=success_metric)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 3400)) + theme(axis.title.x=element_blank()),
	ggplot(large_business, aes(x=success_metric)) + geom_histogram(binwidth=50) + expand_limits(x=c(0, 3400)), nrow=5)

# plot plan_value
grid.arrange(ggplot(loser_larry, aes(x=plan_value)) + geom_histogram(binwidth=10) + expand_limits(x=c(0, 300)) + theme(axis.title.x=element_blank()),
	ggplot(hobby, aes(x=plan_value)) + geom_histogram(binwidth=10) + expand_limits(x=c(0, 300)) + theme(axis.title.x=element_blank()),
	ggplot(small_business, aes(x=plan_value)) + geom_histogram(binwidth=10) + expand_limits(x=c(0, 300)) + theme(axis.title.x=element_blank()),
	ggplot(medium_business, aes(x=plan_value)) + geom_histogram(binwidth=10) + expand_limits(x=c(0, 300)) + theme(axis.title.x=element_blank()),
	ggplot(large_business, aes(x=plan_value)) + geom_histogram(binwidth=10) + expand_limits(x=c(0, 300)), nrow=5)

#===
# REGRESSION RESULTS
#===

# Call:
# lm(formula = success_metric ~ ., data = dat_model)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -2.0261 -0.5009 -0.1519  0.2475  5.5827 

# Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -0.6802171  0.3925922  -1.733 0.083473 .  
# Industry_SurveyAntiques & Collectibles  0.5310498  0.3533263   1.503 0.133158    
# Industry_SurveyApparel & Clothing       0.1918148  0.1691016   1.134 0.256936    
# Industry_SurveyArt & Crafts             0.7389134  0.1895360   3.899 0.000103 ***
# Industry_SurveyAutomotive & Motorcross  0.4660770  0.2076823   2.244 0.025041 *  
# Industry_SurveyBusiness                 0.1000732  0.2131455   0.470 0.638811    
# Industry_SurveyComputer & Software      0.1050944  0.2739364   0.384 0.701324    
# Industry_SurveyEducation                0.8416343  0.3016330   2.790 0.005368 ** 
# Industry_SurveyElectronics              0.1656400  0.2048709   0.809 0.418992    
# Industry_SurveyFood & Beverage          0.3683194  0.1968972   1.871 0.061693 .  
# Industry_SurveyFurniture               -0.0144983  0.2942985  -0.049 0.960719    
# Industry_SurveyGifts & Specialty        0.3339427  0.1797614   1.858 0.063508 .  
# Industry_SurveyHealth & Beauty          0.2065689  0.1771056   1.166 0.243750    
# Industry_SurveyHome & Garden            0.4885579  0.1771098   2.759 0.005914 ** 
# Industry_SurveyIndustrial Equipment     0.2268674  0.2448287   0.927 0.354341    
# Industry_SurveyJewelry & Accessories    0.2904749  0.1929554   1.505 0.132540    
# Industry_SurveyMusic, Books & DVDs      0.5597301  0.2318679   2.414 0.015959 *  
# Industry_SurveyNon-Profit               0.7302112  0.3166441   2.306 0.021311 *  
# Industry_SurveyOffice & Stationary     -0.0775473  0.3618777  -0.214 0.830364    
# Industry_SurveySports and Recreation    0.2926570  0.1795119   1.630 0.103358    
# Industry_SurveyToys and Games           0.5209196  0.2115802   2.462 0.013984 *  
# Industry_SurveyTravel                   0.1333850  0.4775521   0.279 0.780065    
# Industry_SurveyWedding & Bridal         0.8569973  0.4304983   1.991 0.046787 *  
# Industry_SurveyWholesale                0.1023585  0.2467624   0.415 0.678374    
# Experience_SurveyNew to eCommerce      -0.0304668  0.0666647  -0.457 0.647761    
# PhysicalPhysical                        0.1610724  0.0688858   2.338 0.019573 *  
# SwitchedSwitched                        0.1865967  0.0621548   3.002 0.002748 ** 
# MajorityBusinessInternational           0.0167159  0.1535592   0.109 0.913339    
# MajorityBusinessNationwide              0.1193554  0.0922835   1.293 0.196190    
# Products_FromI make my own products     0.1025813  0.0885513   1.158 0.246964    
# Products_FromI resell products          0.5085738  0.0823159   6.178 9.45e-10 ***
# Marketing_Budget$1-$10                 -0.2010847  0.2129266  -0.944 0.345203    
# Marketing_Budget$1,001 - $2,000         0.3918343  0.1311447   2.988 0.002879 ** 
# Marketing_Budget$101 - $200            -0.1229932  0.1090931  -1.127 0.259840    
# Marketing_Budget$11 - $50              -0.2971755  0.1250036  -2.377 0.017627 *  
# Marketing_Budget$201 - $500             0.0039369  0.1092602   0.036 0.971264    
# Marketing_Budget$501 - $1,000           0.1311092  0.1211072   1.083 0.279255    
# Marketing_Budget$51 - $100             -0.1311167  0.1114923  -1.176 0.239871    
# Marketing_BudgetOver $2,000             0.4604487  0.1313822   3.505 0.000478 ***
# cac                                     0.0137285  0.0289164   0.475 0.635059    
# billingcycleMonthly                    -0.2782723  0.1335383  -2.084 0.037431 *  
# Loc.RegionEnglishNonUS                  0.0164109  0.2729871   0.060 0.952075    
# Loc.RegionEurope                        0.0354778  0.4387441   0.081 0.935568    
# Loc.RegionLatinAmerica                 -0.3823467  0.5902083  -0.648 0.517254    
# Loc.RegionUS                            0.1143048  0.2724556   0.420 0.674916    
# Ship.RangeNationwide                   -0.0989877  0.0634909  -1.559 0.119296    
# Own.DomainTRUE                          0.2846618  0.1467701   1.940 0.052723 .  
# Ship.From.PlacesOne                    -0.0944392  0.0677081  -1.395 0.163389    
# Staff_Store_RatioLow                    0.0009567  0.0873518   0.011 0.991264    
# Staff.StoreOne                         -0.1248522  0.0787001  -1.586 0.112961    
# Staff.TotalOne                         -0.1666793  0.1114098  -1.496 0.134948    
# AgeUnder one year                      -0.1775349  0.0609084  -2.915 0.003639 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# Residual standard error: 0.894 on 990 degrees of freedom
# Multiple R-squared: 0.2398,	Adjusted R-squared: 0.2007 
# F-statistic: 6.125 on 51 and 990 DF,  p-value: < 2.2e-16