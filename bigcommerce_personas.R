bigcommerce_personas <- function() {

	# load libraries
	library(ggplot2)

	# set path to data input and output
	path <- file.path(Sys.getenv("HOME"), "Documents", "data")

	# load data
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
	dat$Marketing.Budget[dat$Marketing_Budget=="$201 - $500"] <- 350
	dat$Marketing.Budget[dat$Marketing_Budget=="$101 - $200"] <- 150
	dat$Marketing.Budget[dat$Marketing_Budget=="$51 - $100"] <- 75
	dat$Marketing.Budget[dat$Marketing_Budget=="$501 - $1,000"] <- 750
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

	# which variables influence this success metric

	# scatter plot matrix
	splom( ~ dat[names(dat %in% c(""))], groups=dat$plan_name, auto.key=TRUE)

}