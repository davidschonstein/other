bigcommerce_personas <- function() {

	# load libraries
	library(ggplot2)

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
	dat$Marketing_Budget <- factor(dat$Marketing_Budget, levels=c("$0", "$1-$10", "$11 - $50", "$51 - $100", "$101 - $200", "$201 - $500", "$501 - $1,000", "$1,001 - $2,000", "Over $2,000" ), ordered=TRUE)
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
	qplot(plan_name, success_metric, data=dat, geom="boxplot")
	qplot(plan_name, success_metric, data=dat, geom="violin")
	hist(dat$success_metric, 100) # break this into three clusters and then look at what median values/percentage categories are in clusters

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
	dat$Own.Domain <- ifelse(dat$own_domain==1, TRUE, FALSE)

	# create data for model
	dat_model <- dat
	model.vars <- c("success_metric",
					# "Physical",
					"Switched",
					"Products_From",
					"Experience_Survey",
					"Loc.Region",
					"Marketing_Budget", # maybe try Marketing.Budget
					"age",
					"billingcycle",
					"Ship.Range",
					"Own.Domain",
					"Marketing_Spend",
					"X_ordersize_lm",
					"Industry_Survey",
					"MajorityBusiness",
					"ShipFromPlaces"
					# "Staff_Store",
					# "Staff_Total"
					)
	dat_model <- dat_model[names(dat_model) %in% model.vars]

	# feature scaling of numeric data
	ind <- sapply(dat_model, class) == "numeric" | sapply(dat_model, class) == "integer"
	dat_model[, ind] <- data.frame(sapply(dat_model[ind], scale))
	r <- ((which(is.na(dat_model)) - 1) %% dim(dat_model)[1]) + 1 # just remove NA rows for now
	dat_model <- dat_model[!1:dim(dat_model)[1] %in% r, ]

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

	


	# scatter plot matrix
	splom( ~ dat[names(dat %in% c(""))], groups=dat$plan_name, auto.key=TRUE)

}