####### HELPER FUNCTIONS #######

# for pretty regression tables
# http://stackoverflow.com/questions/30195718/stargazer-save-to-file-dont-show-in-console
mod_stargazer <- function(title, output.file, append, ...) {
  if(append == FALSE & file.exists(output.file)) {
    file.remove(output.file)
  }
  cat("<h1>", title, "</h1>\n", file=output.file, append=TRUE)
  output <- capture.output(stargazer(ci = TRUE, ...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}

# for automatically installing missing packages
# http://www.salemmarafi.com/code/install-r-package-automatically/
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


####### SETUP #######
# packages
usePackage('lme4')
usePackage('longpower')
usePackage('stargazer')
usePackage('influence.ME')
usePackage('tidyr')
usePackage('car')
usePackage('plyr')
usePackage('printr')
usePackage('ggplot2')
usePackage('sjPlot') # table functions

# which polar-scores? default or count-based
## default
sink("data-files/2016_election_results.txt")

# get data
df <- read.csv('data-files/merged_long.csv', header = TRUE, sep = ",", quote = "\"",
               dec = ".", fill = TRUE, comment.char = "")

## count-based
# sink("data-files/2016_election_results_counts.txt", append = FALSE, split = TRUE)
# 
# # get data
# df <- read.csv('data-files/weekly_averages_long_counts.csv', header = TRUE, sep = ",", quote = "\"",
#                dec = ".", fill = TRUE, comment.char = "")

df.dems <- subset(df, party == "Democrat")
df.reps <- subset(df, party == "Republican")


####### DESCRIPTIVE STATS #######
# show summary stats
summary(df)

# summarize by handle
df.handlemeans <- spread(df, key = week, value = abs)
summary(df.handlemeans)

# summarize by party
by_party <- table(df$week,df$party)
by_party

# summarize by party and week
by_party_week <- ddply(df, .(week, party), summarize,
                       mean = round(mean(abs, na.rm=TRUE), 4),
                       sd = round(sd(abs, na.rm=TRUE), 2),
                       median = round(median(abs, na.rm=TRUE), 2))
by_party_week

# spaghetti plots
take_dems <- sample(unique(df.dems$handle), 15)
df.dem_sample = df.dems[df.dems$handle %in% take_dems, ]
p.dems <- ggplot(data = df.dem_sample, aes(x = week, y = abs, group = handle, colour = handle))
p.dems + geom_line()

take_reps <- sample(unique(df.reps$handle), 15)
df.rep_sample = df.reps[df.reps$handle %in% take_reps, ]
p.reps <- ggplot(data = df.rep_sample, aes(x = week, y = abs, group = handle, colour = handle))
p.reps + geom_line()

####### REGRESSION MODELS #######
# linear mixed-effects models
# REML vs ML 
# https://stats.stackexchange.com/questions/48671/what-is-restricted-maximum-likelihood-and-when-should-it-be-used
lmm.null <- lmer(abs ~ 1 + (1|handle), data = df, REML = FALSE)
summary(lmm.null)
lmm1a <- lmer(abs ~ week + ( 1 | handle ), data = df, REML = FALSE)
summary(lmm1a)
lmm1b <- lmer(abs ~ party + (1|handle), data = df, REML = FALSE)
summary(lmm1a)
lmm2a <- lmer(abs ~ week + (week | handle), data = df, REML = FALSE)
summary(lmm2a)
lmm2b <- lmer(abs ~ party + (week | handle), data = df, REML = FALSE)
summary(lmm2b)
lmm3 <- lmer(abs ~ party + week + (1|handle), data = df, REML = FALSE)
summary(lmm3)
lmm4 <- lmer(abs ~ party * week + (1|handle), data = df, REML = FALSE)
summary(lmm4)
lmm5 <- lmer(abs ~ party * week + (1+week|handle), data = df, REML = FALSE)
summary(lmm5)
lmm5.null <- lmer(abs ~ party + week + (1+week|handle), data = df, REML = FALSE)
summary(lmm5.null)
lmm5.res <- resid(lmm5)

# NOTE: LH 5/11 - no reason to run these?
# lmm4 <- lmer(abs ~ party * week + (week | handle), data = df)
# summary(lmm3)
# lmm3b <- lmer(abs ~ party * week + (1+week | handle), data = df)
# summary(lmm3b)
# lmm6 <- lm(abs ~ party * week, data = df)
# summary(lmm6)

# null vs week
anova(lmm.null, lmm1a, refit=FALSE)

# week vs RE for week
anova(lmm1a, lmm2a, refit=FALSE)

# week vs week & party
anova(lmm1a, lmm3, refit=FALSE)

# additive vs interactions
anova(lmm3, lmm4, refit=FALSE)

# random slopes
anova(lmm5.null, lmm5, refit=FALSE)

lmmpower(lmm5, pct.change = 0.10, t = seq(0,9,1), power = 0.90)

# check assumptions
# plot(df$handle, lmm5.res, ylab="Residuals", xlab="Week", main="Partisanship")
# abline(0,0)

plot(residuals(lmm5))
hist(residuals(lmm5))
qqnorm(residuals(lmm5))

# # Q-Q plot looked skewed, so do some checking on outliers
# estex.lmm5 <- influence(lmm5, "handle")
# 
# # dfbetas(estex.lmm5, parameters=c(2,3))
# 
# # plot(estex.lmm5,
# #      which="dfbetas",
# #      parameters=c(2,3),
# #      xlab="DFbetaS",
# #      ylab="handle")
# 
# df.cooks <- cooks.distance(estex.lmm5, parameter = 3, sort = TRUE)
# 
# # plot(estex.lmm5, which="cook",
# #      cutoff=.009, sort=TRUE,
# #      xlab="Cook´s Distance",
# #      ylab="handle")
# 
# # which(df.cooks > 4/444)
# 
# # df.cooks
# 
# # leave out outliers
# print(lmm5, cor=FALSE)
# lmm.exclude1 <- exclude.influence(lmm5,
#                                   "handle", "RepThompson")
# print(lmm.exclude1, cor=FALSE)
# lmm.exclude2 <- exclude.influence(lmm5,
#                                   "handle", "RepDennyHeck")
# print(lmm.exclude2, cor=FALSE)
# lmm.exclude3 <- exclude.influence(lmm5,
#                                   "handle", "RepLawrence")
# print(lmm.exclude3, cor=FALSE)
# lmm.exclude4 <- exclude.influence(lmm5,
#                                   "handle", "RepPaulTonko")
# print(lmm.exclude4, cor=FALSE)
# lmm.exclude5 <- exclude.influence(lmm5,
#                                   "handle", "repdonbeyer")
# print(lmm.exclude5, cor=FALSE)
# lmm.exclude6 <- exclude.influence(lmm5,
#                                   "handle", "DonaldNorcross")
# print(lmm.exclude6, cor=FALSE)

# separate for Dems and Reps to get at interaction term
lmm_dem.null <- lmer(abs ~ (1+week|handle), data = subset(df, party == "Democrat"), REML = FALSE)
summary(lmm_dem.null)
lmm_dem <- lmer(abs ~ week + (1+week|handle), data = subset(df, party == "Democrat"), REML = FALSE)
summary(lmm_dem)
anova(lmm_dem.null, lmm_dem)
lmm_rep.null <- lmer(abs ~ (1+week|handle), data = subset(df, party == "Republican"), REML = FALSE)
summary(lmm_rep.null)
lmm_rep <- lmer(abs ~ week + (1+week|handle), data = subset(df, party == "Republican"), REML = FALSE)
summary(lmm_rep)
anova(lmm_rep.null, lmm_rep)

# make pretty tables
# mod_stargazer("Linear Mixed Models", "mixed_models.html",
#               lmm5, lmm_dem, lmm_rep,
#               type = "html",
#               append = FALSE)
sjt.lmer(lmm5)
sjt.lmer(lmm_dem, lmm_rep)

# run again with just weeks 3-9
# week 2 had a gun control debate that throws off all Dems
df.last_7_weeks <- df[ which(df$week > 2), ]
lmm5.last_7_weeks <- lmer(abs ~ party * week + (1+week|handle), data = df.last_7_weeks, REML = FALSE)
summary(lmm5.last_7_weeks)
lmm5.null.last_7_weeks <- lmer(abs ~ party + week + (1+week|handle), data = df.last_7_weeks, REML = FALSE)
summary(lmm5.null.last_7_weeks)

anova(lmm5.null.last_7_weeks, lmm5.last_7_weeks)

# separate for Dems and Reps to get at interaction term (last 6 weeks)
lmm_dem.last_7_weeks.null <- lmer(abs ~ (1+week|handle), data = subset(df.last_7_weeks, party == "Democrat"), REML = FALSE)
summary(lmm_dem.last_7_weeks.null)
lmm_dem.last_7_weeks <- lmer(abs ~ week + (1+week|handle), data = subset(df.last_7_weeks, party == "Democrat"), REML = FALSE)
summary(lmm_dem.last_7_weeks)
anova(lmm_dem.last_7_weeks.null, lmm_dem.last_7_weeks)
lmm_rep.last_7_weeks.null <- lmer(abs ~ (1+week|handle), data = subset(df.last_7_weeks, party == "Republican"), REML = FALSE)
summary(lmm_rep.last_7_weeks.null)
lmm_rep.last_7_weeks <- lmer(abs ~ week + (1+week|handle), data = subset(df.last_7_weeks, party == "Republican"), REML = FALSE)
summary(lmm_rep.last_7_weeks)
anova(lmm_rep.last_7_weeks.null, lmm_rep.last_7_weeks)

sjt.lmer(lmm5.last_7_weeks)
sjt.lmer(lmm_dem.last_7_weeks, lmm_rep.last_7_weeks)