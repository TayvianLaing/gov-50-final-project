### R code from vignette source 'article3.Rnw'

###################################################
### code chunk number 1: loadlibs
###################################################
library(stargazer)
library(ggplot2)
library(tables)
booktabs()
library(causalsens)
textAnd <- function(x)paste(paste(x[-length(x)],collapse=", ")," and ",x[length(x)],sep="")


###################################################
### code chunk number 2: bigchunk
###################################################


dat <- read.csv("union-with-ranks.csv",
	header=T)

### Create facts of listings
dat$App1.SameYearListed <- dat$App2.SameYearListed <-dat$App3.SameYearListed <-dat$App4.SameYearListed <-dat$App5.SameYearListed <-NA
dat$Resp1.SameYearListed <- dat$Resp2.SameYearListed <-dat$Resp3.SameYearListed <-dat$Resp4.SameYearListed <-dat$Resp5.SameYearListed <-NA

dat$App1.FollowingYearListed <- dat$App2.FollowingYearListed <-dat$App3.FollowingYearListed <-dat$App4.FollowingYearListed <-dat$App5.FollowingYearListed <-NA
dat$Resp1.FollowingYearListed <- dat$Resp2.FollowingYearListed <-dat$Resp3.FollowingYearListed <-dat$Resp4.FollowingYearListed <-dat$Resp5.FollowingYearListed <-NA

dat$App1.PrecedingYearListed <- dat$App2.PrecedingYearListed <-dat$App3.PrecedingYearListed <-dat$App4.PrecedingYearListed <-dat$App5.PrecedingYearListed <-NA
dat$Resp1.PrecedingYearListed <- dat$Resp2.PrecedingYearListed <-dat$Resp3.PrecedingYearListed <-dat$Resp4.PrecedingYearListed <-dat$Resp5.PrecedingYearListed <-NA

dat$App1.AllTimeListed <- dat$App2.AllTimeListed <-dat$App3.AllTimeListed <-dat$App4.AllTimeListed <-dat$App5.AllTimeListed <-NA
dat$Resp1.AllTimeListed <- dat$Resp2.AllTimeListed <-dat$Resp3.AllTimeListed <-dat$Resp4.AllTimeListed <-dat$Resp5.AllTimeListed <-NA


for (i in 1:5) {
	target.var <- paste("App",i,".SameYearListed",sep="")
	source.var <- paste("App",i,".SameYearRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))
	target.var <- paste("Resp",i,".SameYearListed",sep="")
	source.var <- paste("Resp",i,".SameYearRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))

	target.var <- paste("App",i,".FollowingYearListed",sep="")
	source.var <- paste("App",i,".FollowingYearRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))
	target.var <- paste("Resp",i,".FollowingYearListed",sep="")
	source.var <- paste("Resp",i,".FollowingYearRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))

	target.var <- paste("App",i,".PrecedingYearListed",sep="")
	source.var <- paste("App",i,".PrecedingYearRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))
	target.var <- paste("Resp",i,".PrecedingYearListed",sep="")
	source.var <- paste("Resp",i,".PrecedingYearRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))

	target.var <- paste("App",i,".AllTimeListed",sep="")
	source.var <- paste("App",i,".AllTimeRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))
	target.var <- paste("Resp",i,".AllTimeListed",sep="")
	source.var <- paste("Resp",i,".AllTimeRank",sep="")
	dat[,target.var] <- as.numeric(!is.element(dat[,source.var],c(1,8)))
}

### Appellants, average same-year rank
dat$App.SameYearRank.avg <- rowMeans(dat[,
	c("App1.SameYearRank","App2.SameYearRank","App3.SameYearRank","App4.SameYearRank","App5.SameYearRank")],
	na.rm=TRUE)
### Respondents, average same year rank
dat$Resp.SameYearRank.avg <- rowMeans(dat[,
	c("Resp1.SameYearRank","Resp2.SameYearRank","Resp3.SameYearRank","Resp4.SameYearRank","Resp5.SameYearRank")],
	na.rm=TRUE)

### Appellants, average Following-year rank
dat$App.FollowingYearRank.avg <- rowMeans(dat[,
	c("App1.FollowingYearRank","App2.FollowingYearRank","App3.FollowingYearRank","App4.FollowingYearRank","App5.FollowingYearRank")],
	na.rm=TRUE)
### Respondents, average Following year rank
dat$Resp.FollowingYearRank.avg <- rowMeans(dat[,
	c("Resp1.FollowingYearRank","Resp2.FollowingYearRank","Resp3.FollowingYearRank","Resp4.FollowingYearRank","Resp5.FollowingYearRank")],
	na.rm=TRUE)

### Appellants, average Preceding-year rank
dat$App.PrecedingYearRank.avg <- rowMeans(dat[,
	c("App1.PrecedingYearRank","App2.PrecedingYearRank","App3.PrecedingYearRank","App4.PrecedingYearRank","App5.PrecedingYearRank")],
	na.rm=TRUE)
### Respondents, average Preceding year rank
dat$Resp.PrecedingYearRank.avg <- rowMeans(dat[,
	c("Resp1.PrecedingYearRank","Resp2.PrecedingYearRank","Resp3.PrecedingYearRank","Resp4.PrecedingYearRank","Resp5.PrecedingYearRank")],
	na.rm=TRUE)

### Appellants, average all-time rank
dat$App.AllTimeRank.avg <- rowMeans(dat[,
	c("App1.AllTimeRank","App2.AllTimeRank","App3.AllTimeRank","App4.AllTimeRank","App5.AllTimeRank")],
	na.rm=TRUE)
### Respondents, average all-time rank
dat$Resp.AllTimeRank.avg <- rowMeans(dat[,
	c("Resp1.AllTimeRank","Resp2.AllTimeRank","Resp3.AllTimeRank","Resp4.AllTimeRank","Resp5.AllTimeRank")],
	na.rm=TRUE)


### Appellants, max same-year rank
dat$App.SameYearRank.max <- apply(dat[,
	c("App1.SameYearRank","App2.SameYearRank","App3.SameYearRank","App4.SameYearRank","App5.SameYearRank")],1,max,
	na.rm=TRUE)
### Respondents, max same year rank
dat$Resp.SameYearRank.max <- apply(dat[,
	c("Resp1.SameYearRank","Resp2.SameYearRank","Resp3.SameYearRank","Resp4.SameYearRank","Resp5.SameYearRank")],1,max,
	na.rm=TRUE)

### Appellants, max Following-year rank
dat$App.FollowingYearRank.max <- apply(dat[,
	c("App1.FollowingYearRank","App2.FollowingYearRank","App3.FollowingYearRank","App4.FollowingYearRank","App5.FollowingYearRank")],1,max,
	na.rm=TRUE)
### Respondents, max Following year rank
dat$Resp.FollowingYearRank.max <- apply(dat[,
	c("Resp1.FollowingYearRank","Resp2.FollowingYearRank","Resp3.FollowingYearRank","Resp4.FollowingYearRank","Resp5.FollowingYearRank")],1,max,
	na.rm=TRUE)

### Appellants, max Preceding-year rank
dat$App.PrecedingYearRank.max <- apply(dat[,
	c("App1.PrecedingYearRank","App2.PrecedingYearRank","App3.PrecedingYearRank","App4.PrecedingYearRank","App5.PrecedingYearRank")],1,max,
	na.rm=TRUE)
### Respondents, max Preceding year rank
dat$Resp.PrecedingYearRank.max <- apply(dat[,
	c("Resp1.PrecedingYearRank","Resp2.PrecedingYearRank","Resp3.PrecedingYearRank","Resp4.PrecedingYearRank","Resp5.PrecedingYearRank")],1,max,
	na.rm=TRUE)

### Appellants, max all-time rank
dat$App.AllTimeRank.max <- apply(dat[,
	c("App1.AllTimeRank","App2.AllTimeRank","App3.AllTimeRank","App4.AllTimeRank","App5.AllTimeRank")],1,max,
	na.rm=TRUE)
### Respondents, max all-time rank
dat$Resp.AllTimeRank.max <- apply(dat[,
	c("Resp1.AllTimeRank","Resp2.AllTimeRank","Resp3.AllTimeRank","Resp4.AllTimeRank","Resp5.AllTimeRank")],1,max,
	na.rm=TRUE)

### Appellants, average same-year Listed
dat$App.SameYearListed.avg <- rowMeans(dat[,
	c("App1.SameYearListed","App2.SameYearListed","App3.SameYearListed","App4.SameYearListed","App5.SameYearListed")],
	na.rm=TRUE)
### Respondents, average same year Listed
dat$Resp.SameYearListed.avg <- rowMeans(dat[,
	c("Resp1.SameYearListed","Resp2.SameYearListed","Resp3.SameYearListed","Resp4.SameYearListed","Resp5.SameYearListed")],
	na.rm=TRUE)

### Appellants, average Following-year Listed
dat$App.FollowingYearListed.avg <- rowMeans(dat[,
	c("App1.FollowingYearListed","App2.FollowingYearListed","App3.FollowingYearListed","App4.FollowingYearListed","App5.FollowingYearListed")],
	na.rm=TRUE)
### Respondents, average Following year Listed
dat$Resp.FollowingYearListed.avg <- rowMeans(dat[,
	c("Resp1.FollowingYearListed","Resp2.FollowingYearListed","Resp3.FollowingYearListed","Resp4.FollowingYearListed","Resp5.FollowingYearListed")],
	na.rm=TRUE)

### Appellants, average Preceding-year Listed
dat$App.PrecedingYearListed.avg <- rowMeans(dat[,
	c("App1.PrecedingYearListed","App2.PrecedingYearListed","App3.PrecedingYearListed","App4.PrecedingYearListed","App5.PrecedingYearListed")],
	na.rm=TRUE)
### Respondents, average Preceding year Listed
dat$Resp.PrecedingYearListed.avg <- rowMeans(dat[,
	c("Resp1.PrecedingYearListed","Resp2.PrecedingYearListed","Resp3.PrecedingYearListed","Resp4.PrecedingYearListed","Resp5.PrecedingYearListed")],
	na.rm=TRUE)

### Appellants, average all-time Listed
dat$App.AllTimeListed.avg <- rowMeans(dat[,
	c("App1.AllTimeListed","App2.AllTimeListed","App3.AllTimeListed","App4.AllTimeListed","App5.AllTimeListed")],
	na.rm=TRUE)
### Respondents, average all-time Listed
dat$Resp.AllTimeListed.avg <- rowMeans(dat[,
	c("Resp1.AllTimeListed","Resp2.AllTimeListed","Resp3.AllTimeListed","Resp4.AllTimeListed","Resp5.AllTimeListed")],
	na.rm=TRUE)


### Appellants, max same-year Listed
dat$App.SameYearListed.max <- apply(dat[,
	c("App1.SameYearListed","App2.SameYearListed","App3.SameYearListed","App4.SameYearListed","App5.SameYearListed")],1,max,
	na.rm=TRUE)
### Respondents, max same year Listed
dat$Resp.SameYearListed.max <- apply(dat[,
	c("Resp1.SameYearListed","Resp2.SameYearListed","Resp3.SameYearListed","Resp4.SameYearListed","Resp5.SameYearListed")],1,max,
	na.rm=TRUE)

### Appellants, max Following-year Listed
dat$App.FollowingYearListed.max <- apply(dat[,
	c("App1.FollowingYearListed","App2.FollowingYearListed","App3.FollowingYearListed","App4.FollowingYearListed","App5.FollowingYearListed")],1,max,
	na.rm=TRUE)
### Respondents, max Following year Listed
dat$Resp.FollowingYearListed.max <- apply(dat[,
	c("Resp1.FollowingYearListed","Resp2.FollowingYearListed","Resp3.FollowingYearListed","Resp4.FollowingYearListed","Resp5.FollowingYearListed")],1,max,
	na.rm=TRUE)

### Appellants, max Preceding-year Listed
dat$App.PrecedingYearListed.max <- apply(dat[,
	c("App1.PrecedingYearListed","App2.PrecedingYearListed","App3.PrecedingYearListed","App4.PrecedingYearListed","App5.PrecedingYearListed")],1,max,
	na.rm=TRUE)
### Respondents, max Preceding year Listed
dat$Resp.PrecedingYearListed.max <- apply(dat[,
	c("Resp1.PrecedingYearListed","Resp2.PrecedingYearListed","Resp3.PrecedingYearListed","Resp4.PrecedingYearListed","Resp5.PrecedingYearListed")],1,max,
	na.rm=TRUE)

### Appellants, max all-time Listed
dat$App.AllTimeListed.max <- apply(dat[,
	c("App1.AllTimeListed","App2.AllTimeListed","App3.AllTimeListed","App4.AllTimeListed","App5.AllTimeListed")],1,max,
	na.rm=TRUE)
### Respondents, max all-time Listed
dat$Resp.AllTimeListed.max <- apply(dat[,
	c("Resp1.AllTimeListed","Resp2.AllTimeListed","Resp3.AllTimeListed","Resp4.AllTimeListed","Resp5.AllTimeListed")],1,max,
	na.rm=TRUE)


### Now create treatments

dat$SameYearTreatment.AvgRank <- dat$App.SameYearRank.avg > dat$Resp.SameYearRank.avg
dat$FollowingYearTreatment.AvgRank <- dat$App.FollowingYearRank.avg > dat$Resp.FollowingYearRank.avg
dat$PrecedingYearTreatment.AvgRank <- dat$App.PrecedingYearRank.avg > dat$Resp.PrecedingYearRank.avg
dat$AllTimeTreatment.AvgRank <-  dat$App.AllTimeRank.avg > dat$Resp.AllTimeRank.avg

dat$SameYearTreatment.MaxRank <- dat$App.SameYearRank.max > dat$Resp.SameYearRank.max
dat$FollowingYearTreatment.MaxRank <- dat$App.FollowingYearRank.max > dat$Resp.FollowingYearRank.max
dat$PrecedingYearTreatment.MaxRank <- dat$App.PrecedingYearRank.max > dat$Resp.PrecedingYearRank.max
dat$AllTimeTreatment.MaxRank <-  dat$App.AllTimeRank.max > dat$Resp.AllTimeRank.max

dat$SameYearTreatment.AvgListed <- dat$App.SameYearListed.avg > dat$Resp.SameYearListed.avg
dat$FollowingYearTreatment.AvgListed <- dat$App.FollowingYearListed.avg > dat$Resp.FollowingYearListed.avg 
dat$PrecedingYearTreatment.AvgListed <- dat$App.PrecedingYearListed.avg > dat$Resp.PrecedingYearListed.avg
dat$AllTimeTreatment.AvgListed <-  dat$App.AllTimeListed.avg > dat$Resp.AllTimeListed.avg

dat$SameYearTreatment.MaxListed <- dat$App.SameYearListed.max > dat$Resp.SameYearListed.max
dat$FollowingYearTreatment.MaxListed <- dat$App.FollowingYearListed.max > dat$Resp.FollowingYearListed.max
dat$PrecedingYearTreatment.MaxListed <- dat$App.PrecedingYearListed.max > dat$Resp.PrecedingYearListed.max
dat$AllTimeTreatment.MaxListed <-  dat$App.AllTimeListed.max > dat$Resp.AllTimeListed.max

treatments <- c("SameYearTreatment.AvgRank",
	"FollowingYearTreatment.AvgRank",
	"PrecedingYearTreatment.AvgRank",
	"AllTimeTreatment.AvgRank",
	"SameYearTreatment.MaxRank",
	"FollowingYearTreatment.MaxRank",
	"PrecedingYearTreatment.MaxRank",
	"AllTimeTreatment.MaxRank",
	"SameYearTreatment.AvgListed",
	"FollowingYearTreatment.AvgListed",
	"PrecedingYearTreatment.AvgListed",
	"AllTimeTreatment.AvgListed",
	"SameYearTreatment.MaxListed",
	"FollowingYearTreatment.MaxListed",
	"PrecedingYearTreatment.MaxListed",
	"AllTimeTreatment.MaxListed")

dat$app.nlawyers <- apply(dat[,c("AppLawyer1","AppLawyer2","AppLawyer3","AppLawyer4","AppLawyer5")],1,function(x)length(na.omit(x)))
dat$resp.nlawyers <- apply(dat[,c("RespLawyer1","RespLawyer2","RespLawyer3","RespLawyer4","RespLawyer5")],1,function(x)length(na.omit(x)))
dat$deltaLawyers <- dat$app.nlawyers - dat$resp.nlawyers

dat$appellant.wins <- as.numeric(dat$Outcome == 3)

dat$Govt <- ifelse(dat$AppType %in% c("govt","othergovt"),1,
	ifelse(dat$RespType %in% c("govt","othergovt"),-1,0))
dat$Govt[which(dat$AppType %in% c("govt","othergovt") & dat$RespType %in% c("govt","othergovt"))] <- 0

dat$Individual <- ifelse(dat$AppType=="individual",1,
	ifelse(dat$RespType=="individual",-1,0))
dat$Individual[which(dat$AppType %in% c("individual") & dat$RespType %in% c("individual"))] <- 0

dat$Ltd <- ifelse(dat$AppType=="ltd",1,
	ifelse(dat$RespType=="ltd",-1,0))
dat$Ltd[which(dat$AppType %in% c("ltd") & dat$RespType %in% c("ltd"))] <- 0

dat$PLC <- ifelse(dat$AppType=="plc",1,
	ifelse(dat$RespType=="plc",-1,0))
dat$PLC[which(dat$AppType %in% c("plc") & dat$RespType %in% c("plc"))] <- 0

dat$Trust <- ifelse(dat$AppType=="trust",1,
	ifelse(dat$RespType=="trust",-1,0))
dat$Trust[which(dat$AppType %in% c("trust") & dat$RespType %in% c("trust"))] <- 0

dat$SoleTrader <- ifelse(dat$AppType=="soletrader",1,
	ifelse(dat$RespType=="soletrader",-1,0))
dat$SoleTrader[which(dat$AppType %in% c("soletrader") & dat$RespType %in% c("soletrader"))] <- 0

dat$Individual <- ifelse(dat$AppType %in% c("individual","soletrader"),1,
	ifelse(dat$RespType %in% c("individual","soletrader"),-1,0))
dat$Individual[which(dat$AppType %in% c("individual","soletrader") & dat$RespType %in% c("individual","soletrader"))] <- 0

dat$Govt.2 <- dat$AppType %in% "govt"
dat$Individual.2 <- dat$AppType %in% c("individual","soletrader")
dat$Trust.2 <-  dat$AppType %in% "trust"
dat$Ltd.2 <-  dat$AppType %in% "ltd"
dat$PLC.2 <-  dat$AppType %in% "plc"

dat$Complexity <- scale(log(dat$words))
dat$Level <- factor(dat$Level)
dat$HighCt <- dat$Level == "-1"
dat$ApexCt <- dat$Level == "1"
dat$post2008 <- dat$Year > 2008


###################################################
### code chunk number 3: origrankings
###################################################
### Load the rank information
rankings <- read.csv("Tax rankings.csv",header=T)
### convert ranks to order
rankings$rank.num <- as.numeric(as.character(car:::recode(rankings$Rank,
	"c('Juniors 1','Juniors - 1')=6;
	c('Juniors 2','Juniors - 2')=5;
	c('Juniors 3','Juniors - 3')=4;
	c('Juniors - 4') = 3;
	c('Juniors - star','Juniors - Star')=7;
	c('Juniors - Up-and-coming')=2;
	c('New Silks') = 9;
	c('Silks 1','Silks - 1') = 13;
	c('Silks 2','Silks - 2') = 12;
	c('Silks 3','Silks - 3') = 11;
	c('Silks 4','Silks - 4') = 10;
	c('Silks - star','Silks - Star') = 14")))



###################################################
### code chunk number 4: rankdistr
###################################################
rank.mat <- as.matrix(dat[,c("app.rank1","app.rank2","app.rank3","app.rank4","app.rank5",
	"resp.rank1","resp.rank2","resp.rank3","resp.rank4","resp.rank5")])
rank.distrib <- na.omit(as.vector(rank.mat))



###################################################
### code chunk number 5: splitjoins
###################################################
njoins <- length(names(which(table(dat$refno)>2)))
splitjoins <- names(which(by(dat$appellant.wins,dat$refno,var)>0))


###################################################
### code chunk number 6: smrytab
###################################################
stargazer(dat[,c("Govt.2","Ltd.2","Trust.2","PLC.2","Individual.2","deltaLawyers","words","HighCt","ApexCt","post2008")],
	summary = TRUE,
	title = "Summary statistics",
	label ="tab:smry",
	covariate.labels = c("Govt. is appellant",
		"Limited company is appellant",
		"Trust is appellant",
		"Publicly-traded company is appellant",
		"Individual or sole trader is appellant",
		"App. lawyers - resp. lawyers",
		"Complexity (wordcount)",		
		"High Court or equiv.",
		"Final court (HoL/UKSC/PC)",
		"Post 2008"))


###################################################
### code chunk number 7: out1
###################################################

dat$Representation <- ifelse(dat$SameYearTreatment.AvgRank,"Appellant counsel better","Appellant counsel not better")
dat$Outcome <- ifelse(dat$appellant.wins==1,"Appellant wins","Appellant loses")
outtab1 <- table(
	dat$Representation,
	dat$Outcome)

latex(tabular(Heading() * factor(Representation) ~ Heading() * factor(Outcome)*((n=1)+Percent("row")),
	data=dat))
dat$Representation <- NULL
dat$Outcome <- NULL



###################################################
### code chunk number 8: out2
###################################################

dat$Representation <- ifelse(dat$SameYearTreatment.AvgListed,"Appellant counsel better","Appellant counsel not better")
dat$Outcome <- ifelse(dat$appellant.wins==1,"Appellant wins","Appellant loses")
outtab2 <- table(
	dat$Representation,
	dat$Outcome)

latex(tabular(Heading() * factor(Representation) ~ Heading() * factor(Outcome)*((n=1)+Percent("row")),
	data=dat))
dat$Representation <- NULL
dat$Outcome <- NULL



###################################################
### code chunk number 9: rankmods
###################################################
sel.formula <- as.formula("SameYearTreatment.AvgRank ~ 
	Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(log(words)) + HighCt + ApexCt + post2008")
outcome.formula <- as.formula("appellant.wins ~ 
	SameYearTreatment.AvgRank + Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(deltaLawyers) + scale(log(words)) + HighCt + ApexCt + post2008")
sel.mod <- glm(sel.formula,data = dat, family=binomial)
outcome.mod <- lm(outcome.formula,data = dat)
outcome.glm <- glm(outcome.formula,data = dat,family=binomial)

stargazer(outcome.mod,
	# outcome.glm,
	sel.mod,
	dep.var.labels = c("Appellant wins","Treated"),
	title = "Regression models, rank-based measure",
	label = "tab:rankreg",
	font.size = "footnotesize",
	no.space = TRUE,
	covariate.labels = c("Better-ranked lawyers","Appellant: Government","Appellant: Trust","Appellant: Public limited company","Appellant: Individual or sole trader","No. appellant lawyers - no. respondent lawyers","log(Complexity)","High Court v. Court of Appeal","HoL/UKSC/PC v. Court of Appeal","Post 2008","Constant"),
	# notes = "Coefficient values for continuous variables are standardized", 
	intercept.bottom = TRUE)


###################################################
### code chunk number 10: listedmods
###################################################
sel.formula <- as.formula("SameYearTreatment.AvgListed ~ 
	Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(log(words)) + HighCt + ApexCt + post2008")
outcome.formula <- as.formula("appellant.wins ~ 
	SameYearTreatment.AvgListed + Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(deltaLawyers) + scale(log(words)) + HighCt + ApexCt + post2008")
sel.mod <- glm(sel.formula,data = dat, family=binomial)
outcome.mod <- lm(outcome.formula,data = dat)
outcome.glm <- glm(outcome.formula,data = dat,family=binomial)

stargazer(outcome.mod,
	# outcome.glm,
	sel.mod,
	dep.var.labels = c("Appellant wins","Treated"),
	title = "Regression models, listing-based measure",
	label = "tab:listreg",
	font.size = "footnotesize",
	no.space = TRUE,
	covariate.labels = c("Better-ranked lawyers","Appellant: Government","Appellant: Trust","Appellant: Public limited company","Appellant: Individual or sole trader","No. appellant lawyers - no. respondent lawyers","log(Complexity)","High Court v. Court of Appeal","HoL/UKSC/PC v. Court of Appeal","Post 2008","Constant"),
	# notes = "Coefficient values for continuous variables are standardized", 
	intercept.bottom = TRUE)


###################################################
### code chunk number 11: sensitivity
###################################################
dat$Complexity <- scale(log(dat$words))
dat$deltaLawyers.scaled <- scale(dat$deltaLawyers)
tr <- "SameYearTreatment.AvgRank" 
dat[,tr] <- as.numeric(dat[,tr])
	sel.formula <- as.formula(
		paste(tr,"~Govt.2 + Trust.2 + PLC.2 + Individual.2 + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	outcome.formula <- as.formula(
		paste("appellant.wins~",tr,"+Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	sel.mod <- glm(sel.formula,data = dat, family=binomial)
	out.mod <- lm(outcome.formula,data = dat)

alpha <- seq(max(abs(coef(out.mod)[-1])) * -1.5, max(abs(coef(out.mod)[-1])) * 1.5, length.out = 101)

rank.one.side.sens <- causalsens(out.mod, sel.mod, ~ Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008, data = dat,
                      alpha = alpha, confound = one.sided)

tr <- "SameYearTreatment.AvgListed" 
dat[,tr] <- as.numeric(dat[,tr])
	sel.formula <- as.formula(
		paste(tr,"~Govt.2 + Trust.2 + PLC.2 + Individual.2 + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	outcome.formula <- as.formula(
		paste("appellant.wins~",tr,"+Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	sel.mod2 <- glm(sel.formula,data = dat, family=binomial)
	out.mod2 <- lm(outcome.formula,data = dat)

alpha <- seq(max(abs(coef(out.mod2)[-1])) * -1.5, max(abs(coef(out.mod2)[-1])) * 1.5, length.out = 101)

list.one.side.sens <- causalsens(out.mod2, sel.mod2, 
	~ Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008, 
	data = dat,
	alpha = alpha, confound = one.sided)


lowest.rank <- alpha[max(which(rank.one.side.sens$sens$lower>0))]
lowest.list <- alpha[max(which(list.one.side.sens$sens$lower>0))]


if (FALSE) {
	par(mfrow=c(2,2),mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
	plot(rank.one.side.sens, type = "raw",main = "Rank-based measure", xlab = expression(paste("Amount of confounding ", alpha)))

	arrows(x0=lowest.rank,x1=lowest.rank,y0=-0.15,y1=0,length=0.05)

	text(x=lowest.rank,y=-0.2,labels="Rank-based\n measure\nbecomes\nsignificant",cex=.75)
	points(x= abs(coef(out.mod)[-1]),
		y = rep(0,length(coef(out.mod))-1),
		pch = 4)
	points(x= abs(coef(out.mod)[-1]) * -1,
		y = rep(0,length(coef(out.mod))-1),
		pch = 4)

	plot(list.one.side.sens, type = "raw",main = "Listing-based measure", xlab = expression(paste("Amount of confounding ", alpha)))
	points(x= abs(coef(out.mod2)[-1]),
		y = rep(0,length(coef(out.mod))-1),
		pch = 4)
	points(x= abs(coef(out.mod2)[-1]) * -1,
		y = rep(0,length(coef(out.mod))-1),
		pch = 4)

}
pdf(file="article3-sensitivity.pdf")
plot(rank.one.side.sens, type = "r.squared",main = "Rank-based measure")
legend("topright", 
	pch = "× = ",
	bty = "n",
	legend = "Variance explained \n by variables in model")
	
dev.off()
### plot(list.one.side.sens, type = "r.squared",main = "Listing-based measure")



###################################################
### code chunk number 12: othercoefs
###################################################
othercoefs <- read.csv("coefs.csv",header=T)
### just get dummies that are not intercepts and are significant
othercoefs$tval <- abs(othercoefs$beta / othercoefs$se)
othercoefs <- subset(othercoefs,isIntercept == 0 & grepl("dummy",Type,ignore.case=TRUE))
othercoefs$predprob <- abs(othercoefs$beta) / 4
rank.comparison <- round(mean(abs(lowest.rank) > othercoefs$predprob)*100,1)
list.comparison <- round(mean(abs(lowest.list) > othercoefs$predprob)*100,1)


###################################################
### code chunk number 13: robustnesscheck
###################################################
tr <- NULL
treats <- grep("Treat",names(dat),value=TRUE)
coef.mat <- matrix(NA,nrow=length(treats),ncol=3)
for (tr in treats) {
	out.formula <- as.formula(
		paste("appellant.wins~",tr,"+Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008",
			sep=" "))
	out.mod <- lm(out.formula,data = dat)
	coef.mat[which(tr==treats),1] <- coef(out.mod)[charmatch(tr,names(coef(out.mod)))]
	coef.mat[which(tr==treats),2:3] <- confint(out.mod)[charmatch(tr,names(coef(out.mod))),]
}
coef.df <- as.data.frame(coef.mat)
coef.df$config <- treats
coef.df$Basis <- "Basis: Rank-based"
coef.df$Basis[grepl("List",coef.df$config)] <- "Basis: Listing-based"
coef.df$Aggregation <- ifelse(grepl("Max",coef.df$config),"Aggregation: Maximum","Aggregation: Average")
coef.df$Temporal <- "Temporal: Same year"
coef.df$Temporal[grepl("AllTime",coef.df$config)] <- "Temporal: All-time"
coef.df$Temporal[grepl("Following",coef.df$config)] <- "Temporal: Following year"
coef.df$Temporal[grepl("Preceding",coef.df$config)] <- "Temporal: Preceding year"

ggplot(coef.df,aes(x=Temporal,y=V1,ymin=V2,ymax=V3)) + geom_pointrange() + 
	scale_y_continuous("Coefficient value") + 
	facet_grid(Aggregation~Basis) + 
	geom_hline(yintercept=0,col='gray',lty=2) + 
	coord_flip() + 
	theme_classic()


###################################################
### code chunk number 14: rankmods2
###################################################
sel.formula2 <- as.formula("SameYearTreatment.AvgRank ~ 
	Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(log1p(CasesReferredInJudgment)) + HighCt + ApexCt + post2008")
outcome.formula2 <- as.formula("appellant.wins ~ 
	SameYearTreatment.AvgRank + Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(deltaLawyers) + scale(log1p(CasesReferredInJudgment)) + HighCt + ApexCt + post2008")
sel.mod2 <- glm(sel.formula2,data = dat, family=binomial)
outcome.mod2 <- lm(outcome.formula2,data = dat)
outcome.glm2 <- glm(outcome.formula2,data = dat,family=binomial)


stargazer(outcome.mod2,
	# outcome.glm,
	sel.mod2,
	dep.var.labels = c("Appellant wins","Treated"),
	title = "Regression models, ranking-based measure",
	label = "tab:rankregaux",
	font.size = "footnotesize",
	no.space = TRUE,
	covariate.labels = c("Better-ranked lawyers","Appellant: Government","Appellant: Trust","Appellant: Public limited company","Appellant: Individual or sole trader","No. appellant lawyers - no. respondent lawyers","log(Complexity (alt.))","High Court v. Court of Appeal","HoL/UKSC/PC v. Court of Appeal","Post 2008","Constant"),
	# notes = "Coefficient values for continuous variables are standardized", 
	intercept.bottom = TRUE)


###################################################
### code chunk number 15: listedmods2
###################################################

sel.formula2 <- as.formula("SameYearTreatment.AvgListed ~ 
	Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(log1p(CasesReferredInJudgment)) + HighCt + ApexCt + post2008")
outcome.formula2 <- as.formula("appellant.wins ~ 
	SameYearTreatment.AvgListed + Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(deltaLawyers) + scale(log1p(CasesReferredInJudgment)) + HighCt + ApexCt + post2008")
sel.mod2 <- glm(sel.formula2,data = dat, family=binomial)
outcome.mod2 <- lm(outcome.formula2,data = dat)
outcome.glm2 <- glm(outcome.formula2,data = dat,family=binomial)

stargazer(outcome.mod2,
	# outcome.glm,
	sel.mod2,
	dep.var.labels = c("Appellant wins","Treated"),
	title = "Regression models, listing-based measure",
	label = "tab:listregaux",
	font.size = "footnotesize",
	no.space = TRUE,
	covariate.labels = c("Better-ranked lawyers","Appellant: Government","Appellant: Trust","Appellant: Public limited company","Appellant: Individual or sole trader","No. appellant lawyers - no. respondent lawyers","log(Complexity (alt.))","High Court v. Court of Appeal","HoL/UKSC/PC v. Court of Appeal","Post 2008","Constant"),
	# notes = "Coefficient values for continuous variables are standardized", 
	intercept.bottom = TRUE)


###################################################
### code chunk number 16: altsensitivity
###################################################
dat$Complexity <- scale(log1p(dat$CasesReferredInJudgment))

dat$deltaLawyers.scaled <- scale(dat$deltaLawyers)
tr <- "SameYearTreatment.AvgRank" 
dat[,tr] <- as.numeric(dat[,tr])
	sel.formula <- as.formula(
		paste(tr,"~Govt.2 + Trust.2 + PLC.2 + Individual.2 + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	outcome.formula <- as.formula(
		paste("appellant.wins~",tr,"+Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	sel.mod <- glm(sel.formula,data = dat, family=binomial)
	out.mod <- lm(outcome.formula,data = dat)

alpha <- seq(max(abs(coef(out.mod)[-1])) * -1.5, max(abs(coef(out.mod)[-1])) * 1.5, length.out = 101)

rank.one.side.sens <- causalsens(out.mod, sel.mod, ~ Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008, data = dat,
                      alpha = alpha, confound = one.sided)

tr <- "SameYearTreatment.AvgListed" 
dat[,tr] <- as.numeric(dat[,tr])
	sel.formula <- as.formula(
		paste(tr,"~Govt.2 + Trust.2 + PLC.2 + Individual.2 + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	outcome.formula <- as.formula(
		paste("appellant.wins~",tr,"+Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008",
			sep=" "))

	sel.mod2 <- glm(sel.formula,data = dat, family=binomial)
	out.mod2 <- lm(outcome.formula,data = dat)

alpha <- seq(max(abs(coef(out.mod2)[-1])) * -1.5, max(abs(coef(out.mod2)[-1])) * 1.5, length.out = 101)

list.one.side.sens <- causalsens(out.mod2, sel.mod2, 
	~ Govt.2 + Trust.2 + PLC.2 + Individual.2 + deltaLawyers.scaled + Complexity + HighCt + ApexCt + post2008, 
	data = dat,
	alpha = alpha, confound = one.sided)


lowest.rank <- alpha[max(which(rank.one.side.sens$sens$lower>0))]
lowest.list <- alpha[max(which(list.one.side.sens$sens$lower>0))]
par(mfrow=c(2,2),mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
plot(rank.one.side.sens, type = "raw",main = "Rank-based measure", xlab = expression(paste("Amount of confounding ", alpha)))

arrows(x0=lowest.rank,x1=lowest.rank,y0=-0.15,y1=0,length=0.05)

text(x=lowest.rank,y=-0.2,labels="Rank-based\n measure\nbecomes\nsignificant",cex=.75)
points(x= abs(coef(out.mod)[-1]),
	y = rep(0,length(coef(out.mod))-1),
	pch = 4)
points(x= abs(coef(out.mod)[-1]) * -1,
	y = rep(0,length(coef(out.mod))-1),
	pch = 4)

plot(list.one.side.sens, type = "raw",main = "Listing-based measure", xlab = expression(paste("Amount of confounding ", alpha)))
points(x= abs(coef(out.mod2)[-1]),
	y = rep(0,length(coef(out.mod))-1),
	pch = 4)
points(x= abs(coef(out.mod2)[-1]) * -1,
	y = rep(0,length(coef(out.mod))-1),
	pch = 4)

plot(rank.one.side.sens, type = "r.squared",main = "Rank-based measure")
plot(list.one.side.sens, type = "r.squared",main = "Listing-based measure")



###################################################
### code chunk number 17: rankmodshighct
###################################################
sel.formula <- as.formula("SameYearTreatment.AvgRank ~ 
	Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(log(words)) + ApexCt + post2008")
outcome.formula <- as.formula("appellant.wins ~ 
	SameYearTreatment.AvgRank + Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(deltaLawyers) + scale(log(words)) + ApexCt + post2008")
sel.mod <- glm(sel.formula,data = subset(dat,HighCt==FALSE), family=binomial)
outcome.mod <- lm(outcome.formula,data = subset(dat,HighCt==FALSE))
outcome.glm <- glm(outcome.formula,data = subset(dat,HighCt==FALSE),family=binomial)

stargazer(outcome.mod,
	sel.mod,
	dep.var.labels = c("Appellant wins","Treated"),
	title = "Regression models, rank-based measure, Court of Appeal and Lords only",
	label = "tab:highrankreg",
	font.size = "footnotesize",
	no.space = TRUE,
	covariate.labels = c("Better-ranked lawyers","Appellant: Government","Appellant: Trust","Appellant: Public limited company","Appellant: Individual or sole trader","No. appellant lawyers - no. respondent lawyers","log(Complexity)","HoL/UKSC/PC v. Court of Appeal","Post 2008","Constant"),
	intercept.bottom = TRUE)


###################################################
### code chunk number 18: listedmodshighct
###################################################
sel.formula <- as.formula("SameYearTreatment.AvgListed ~ 
	Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(log(words)) + ApexCt + post2008")
outcome.formula <- as.formula("appellant.wins ~ 
	SameYearTreatment.AvgListed + Govt.2 + Trust.2 + PLC.2 + Individual.2 + scale(deltaLawyers) + scale(log(words)) + ApexCt + post2008")
sel.mod <- glm(sel.formula,data = subset(dat,HighCt==FALSE), family=binomial)
outcome.mod <- lm(outcome.formula,data = subset(dat,HighCt==FALSE))
outcome.glm <- glm(outcome.formula,data = subset(dat,HighCt==FALSE),family=binomial)

stargazer(outcome.mod,
	sel.mod,
	dep.var.labels = c("Appellant wins","Treated"),
	title = "Regression models, listing-based measure, Court of Appeal and Lords only",
	label = "tab:highlistreg",
	font.size = "footnotesize",
	no.space = TRUE,
	covariate.labels = c("Better-ranked lawyers","Appellant: Government","Appellant: Trust","Appellant: Public limited company","Appellant: Individual or sole trader","No. appellant lawyers - no. respondent lawyers","log(Complexity)","HoL/UKSC/PC v. Court of Appeal","Post 2008","Constant"),
	intercept.bottom = TRUE)

### Alt outcome mod
summary(update(outcome.mod, . ~ . - SameYearTreatment.AvgListed + I(App.SameYearRank.avg - Resp.SameYearRank.avg)))


