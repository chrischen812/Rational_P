rm(list=ls())

setwd("C:/Users/Christopher/Documents/Projects/MeetUp/MeetUp - EMR")

#install.packages("survey")
#install.packages("plyr")

library(survey)
library(plyr)

data(api)

# keep only the data set we need
rm(list = ls()[!grepl("apistrat", ls())])

##-- basic exploration of data set
# head, names/variable meaning

# select variables to explore --- t(names(apistrat)) to
d <- apistrat[,-c(1,3, 5:9,8,10,14,16:18,24:26,28,37)]

# create/specify survey design
survey_design <- svydesign(ids=~1, strata = ~stype, fpc = ~fpc, weights = ~pw, data = d)

# interest: positive growth or not --- updating a design object
d$growth.cat = factor(1*(d$growth>0))
survey_design <- update(survey_design, growth.cat = d$growth.cat)
d$growth.label <- sapply(d$growth, function(x){ifelse(x>0, "Positive Growth", "Negative Growth")})
survey_design <- update(survey_design, growth.label=d$growth.label)


#################
# scatter plot of api for 1999 and 2000, and change in API - scatterplot is adjusted for sampling weights
svyplot(~api99+api00, survey_design, basecol="blue")
svyhist(~api99, survey_design, col=3)
svyhist(~api00, survey_design, col=3)

# positive growth and percent of students tested
svyboxplot(pcttest~growth.cat, design = survey_design, all.outliers = T, col=c(2,3), ylab="Percent Tested")

# awards and positive growth - dotchart. 1. svyby; 2. dotchart
dotchart(svyby(~awards, ~growth.label, design=survey_design, svymean), xlab="Proportions", col=c("red", "blue"))

# Percentage qualified for subsidized meals
svyhist(~meals,design=survey_design, col="blue")
svyplot(~growth+meals, design=survey_design, basecol="blue")
svyboxplot(meals~growth.cat, design=survey_design, all.outliers = T, col=c(2,3))

# 'English Language Learners' (percent)
svymean(~ell, survey_design)
svyhist(~ell, design = survey_design, col="blue", xlab="English Language Learners") 
svyplot(~growth+ell,survey_design,basecol = "blue")

# Year-round school
svyby(~growth, ~yr.rnd, survey_design, svymean)
svyboxplot(growth~yr.rnd, survey_design, all.outliers = T, col=c(2,3), ylab="Growth", xlab="Year Round School")



# summary statistics and unadjusted odds ratio
getSummaryStats <- function(stats, svyModel, dp = 4){
  n.levels <- nrow(stats)/2
  
  stats.val <- stats[1:n.levels, ]
  stats.se <- stats[(n.levels+1):(2*n.levels),]
  
  dp.format <- paste("%.",dp,"f",sep="")
  
  stats2 <- matrix(paste(sprintf(dp.format, stats.val),paste("(",sprintf(dp.format,stats.se), ")", sep="")), ncol=2)
  
  conf.interval <- round(exp(confint(svyModel)),dp)
  odds <- sprintf(dp.format, exp(svyModel$coefficients))
  odds <- paste(odds, " [", conf.interval[,1], ", ", conf.interval[,2], "]", sep="")
  
  return.val <- cbind(names(svyModel$coefficients[ifelse(n.levels==1, 2, 1):ifelse(n.levels==1,2,n.levels)]), stats2, odds[ifelse(n.levels==1,2,n.levels)])
  return (return.val)
}


#vars <- names(dd)
include <- names(d)[-c(1,2,5:7,21:24)]
summaryStatistics <- NULL

# recode summary statistics to within group percentages and using confint for CI
univPvalue <- NULL
for (varName in include){
  #if (varName %in% exclude){next}
  m1 <- svyglm(as.formula(paste("growth.cat~", varName)), design=survey_design, family=quasibinomial)
  univPvalue <- rbind(univPvalue, cbind(varName, regTermTest(m1,varName, method="LRT", df = Inf)$p))
  
  stats <- t(svyby(as.formula(paste("~", varName,sep="")), ~growth.cat, survey_design, svymean)[,-1])
  
  summaryStatistics <- rbind(summaryStatistics, getSummaryStats(stats, m1))
}


write.table(as.data.frame(summaryStatistics), "../Inpatient/painSummaryStatisticsFCDCmeeting.csv", sep=",", row.names=F)
write.table(as.data.frame(univPvalue), "../Inpatient/painRaoScottPvaluesFCDCmeeting.csv", sep=",", row.names=F)



startModel <- svyglm(paste("growth.cat ~", paste(include, collapse = "+")), design=survey_design, family=quasibinomial())
source("svyaic.R")
xx <- svystep(startModel)
