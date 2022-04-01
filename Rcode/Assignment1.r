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

# Load Data ----
df <- read.csv("Data/DIET.csv") %>% 
  mutate(gender = factor(gender),
         Diet = factor(Diet))
df.edit <- df %>% 
  mutate(gender = case_when(
                            gender == 0 ~ "female",
                            TRUE ~ "male")
         )

# Data check
df
head(df)
names(df)

# Viewing Data ----
weightdiff <- df$pre.weight - df$weight6weeks

ggplot(df.edit, aes(gender, weightdiff))+geom_boxplot()

ggplot(df.edit, aes(Diet, weightdiff))+geom_boxplot()

ggplot(data = df.edit)+geom_boxplot(aes(x = Diet, y = weightdiff)) + facet_wrap(~gender)

#KS test
lillie.test(weightdiff)
lillie.test(weightdiff[df.edit$gender == 'female'])

#SW test 
shapiro.test(weightdiff)

#Two-way ANOVA ----
#Assumption Check

fit <- aov(weightdiff~gender*Diet, data = df.edit)
summary(fit)

#Tukey
TukeyHSD(fit)

# Interaction plot

interaction.plot(df.edit$Diet, df.edit$gender, weightdiff, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Diet type", 
                 ylab="estimated marginal means", 
                 main="Interaction Plot")

# Question 2 ----
# Load Data ----
x <- read.csv("Data/STATPAK.csv")

# Data check
x
head(x)
str(x)

# SW test ----
shapiro.test(x$Time)
shapiro.test(x$Satisfaction)

# Box plot/Hist/QQplot/Density ----
boxplot(Time ~ StatPak, data = x)
boxplot(Satisfaction ~ StatPak, data = x)

## Time Viz
qqnorm(x$Time)
qqline(x$Time)
hist(x$Time)
plot(density(x$Time))

## Satisfaction Viz
qqnorm(x$Satisfaction)
qqline(x$Satisfaction)
hist(x$Satisfaction)
plot(density(x$Satisfaction))

#Outliers detection and removal ----
 
olTime <- boxplot(Time ~ StatPak, data = x)$out
olSatisfaction <- boxplot(Satisfaction ~ StatPak, data = x)$out

print(olTime)
print(olSatisfaction)

out <- x[x$StatPak == "BMDP" & (x$Time == 15 | x$Time == 44 | x$Time == 8),]
print(out)

outAll <- rbind(out)
print(outAll)

# remove outliers
x1 <- x[-which(x$No %in% outAll$No),] %>% 
  mutate(Satisfaction = as.numeric(Satisfaction))

# Test homogenity of variance and covariance ----

## Levene test
leveneTest(Time ~ StatPak, data = x1)
leveneTest(Satisfaction ~ StatPak, data = x1)

## Box's M test 
boxM(data = x1[,6:7], group = x1$StatPak)

# One way manova
Y <- cbind(x1$Time,x1$Satisfaction)
fit <- manova(Y ~ StatPak, data = x1)
summary(fit)

#?????????????????????
