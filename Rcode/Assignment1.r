# Question 1 ----
# Library----
library(s20x)
library(gplots)
library(ggplot2)
library(tidyverse)
library(nortest)
library(car)
library(RColorBrewer)
library(biotools)
library(PMCMRplus)

# Load Data ----
df.weight <- read.csv("Data/DIET.csv") %>% 
  mutate(gender = factor(gender),
         Diet = factor(Diet),
         gender = case_when(
           gender == 0 ~ "female",
           TRUE ~ "male")
         )

# Data check
dim(df.weight)
head(df.weight)
str(df.weight)

# Viewing Data ----
weightdiff <- df.weight$pre.weight - df.weight$weight6weeks

ggplot(df.weight, aes(gender, weightdiff))+geom_boxplot()

ggplot(df.weight, aes(Diet, weightdiff))+geom_boxplot()

ggplot(data = df.weight)+geom_boxplot(aes(x = Diet, y = weightdiff)) + facet_wrap(~gender)

#KS test
lillie.test(weightdiff)
lillie.test(weightdiff[df.weight$gender == 'female'])
lillie.test(weightdiff[df.weight$gender == 'male'])
#SW test 
shapiro.test(weightdiff)

#Two-way ANOVA ----
#Assumption Check

fit.weight <- aov(weightdiff~gender*Diet, data = df.weight)
summary(fit.weight)

#Tukey
TukeyHSD(fit.weight)

# Interaction plot

interaction.plot(df.weight$Diet, df.edit$gender, weightdiff, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Diet type", 
                 ylab="estimated marginal means", 
                 main="Interaction Plot")

# Question 2 ----
# Load Data ----
df.stat <- read.csv("Data/STATPAK.csv") %>% 
  mutate(StatPak = factor(StatPak))

# Data check
head(df.stat)
str(df.stat)

# SW test ----
shapiro.test(df.stat$Time)
shapiro.test(df.stat$Satisfaction)

# Box plot/Hist/QQplot/Density ----
boxplot(Time ~ StatPak, data = df.stat)
boxplot(Satisfaction ~ StatPak, data = df.stat)

## Time Viz
qqline(df.stat$Time)
hist(df.stat$Time)
plot(density(df.stat$Time))

## Satisfaction Viz
qqline(df.stat$Satisfaction)
hist(df.stat$Satisfaction)
plot(density(df.stat$Satisfaction))

#Outliers detection and removal ----
 
olTime <- boxplot(Time ~ StatPak, data = df.stat)$out
olSatisfaction <- boxplot(Satisfaction ~ StatPak, data = df.stat)$out

olTime
olSatisfaction

out <- df.stat[df.stat$StatPak == "BMDP" & (df.stat$Time == 15 | df.stat$Time == 44 | df.stat$Time == 8),]
out

# remove outliers
df.NoOutlier <- df.stat[-which(x$No %in% outAll$No),] %>% 
  mutate(Satisfaction = as.numeric(Satisfaction))

# Test homogenity of variance and covariance ----

## Levene test
leveneTest(Time ~ StatPak, data = df.NoOutlier)
leveneTest(Satisfaction ~ StatPak, data = df.NoOutlier)

## Box's M test 
boxM(data = df.NoOutlier[,6:7], group = df.NoOutlier$StatPak)

# One way manova
RespoVari <- cbind(df.NoOutlier$Time,df.NoOutlier$Satisfaction)
fit.stat <- manova(RespoVari ~ StatPak, data = df.NoOutlier)
summary(fit.stat, test = 'Pillai')

# Tamhane test for Time

fit.T2 <- tamhaneT2Test(df.NoOutlier$Time, df.NoOutlier$StatPak)
summary(fit.T2)

# Tukey test for satisfaction

?TukeyHSD

fit.TK <- aov(Satisfaction ~ StatPak, data = df.NoOutlier)
TukeyHSD(fit.TK)
