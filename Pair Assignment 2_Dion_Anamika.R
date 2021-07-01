TB_budget_2020 <- read.csv("TB_budget_2021-02-09.csv")  # imported data from files
TB_budget_2020
dim(TB_budget_2020) # Dimensions


# call out libraries
library(tidyverse)
library(ggplot2)
library(pastecs)
library(gridExtra)
library(psych)

# dataframe - country, year 2020, # patients receiving tb treatment, budget

data <-TB_budget_2020 %>%
  select(country, year, budget_tpt, tx_tpt) %>%
  filter(year == 2020, budget_tpt > 0, tx_tpt > 0)

# inner join variables to remove N/As
df <- data[!is.na(data$budget_tpt) & !is.na(data$tx_tpt),]
df

dim(df) # observations after cleaning
str(df) # data type
summary(df) # summary statistics

# Export csv
write.csv(df,"tb_budget_2020_pairassignment2.csv")

# Outliers
# scatter plot 
ggplot(df, aes(x=budget_tpt,y=tx_tpt)) + geom_point() 

# histograms
ggplot(df, aes(budget_tpt)) + geom_histogram() # budget
ggplot(df, aes(tx_tpt)) + geom_histogram() # people receiving treatment 

# cleaning - remove outlier
df.nooutliers <- df %>%
  filter(tx_tpt <= 1200000 & budget_tpt <= 500000)

df.nooutliers
describe(df.nooutliers)

ggplot(df.nooutliers, aes(x=budget_tpt,y=tx_tpt)) + 
  geom_point()

# histograms
ggplot(df.nooutliers, aes(budget_tpt)) + geom_histogram() # budget
ggplot(df.nooutliers, aes(tx_tpt)) + geom_histogram() # people receiving treatment

# boxplots
ggplot(df.nooutliers, aes(budget_tpt)) + geom_boxplot() # budget
ggplot(df.nooutliers, aes(tx_tpt)) + geom_boxplot() # people receiving treatment

# Q-Q plots
df.nooutliers %>%
  ggplot(aes(sample=budget_tpt)) + stat_qq() # budget

df.nooutliers %>% 
  ggplot(aes(sample=tx_tpt)) + stat_qq() # people receiving treatment


# LOG TRANSFORMATION - Budget
budget.log.histogram <- df.nooutliers %>%
  mutate(budget.log = log(budget_tpt)) %>%
  ggplot(aes(x=budget.log)) + theme(legend.position = "none") +
  geom_histogram(colour = "black", fill = "lightblue", aes(y=..density..)) 

grid.arrange(ggplot(df.nooutliers, aes(budget_tpt)) + geom_histogram(fill = "lightblue", colour = "black"),budget.log.histogram, ncol=2)


# LOG TRANSFORMATION - Treatment
treatment.log.histogram <- df.nooutliers %>%
  mutate(treatment.log = log(tx_tpt)) %>%
  ggplot(aes(x=treatment.log)) + theme(legend.position = "none") +
  geom_histogram(colour = "black", fill = "red", aes(y=..density..)) 

grid.arrange(ggplot(df.nooutliers, aes(tx_tpt)) + geom_histogram(fill = "red", colour = "black"),treatment.log.histogram, ncol=2)

# QQ Log - Budget
budget.log.qq <- df.nooutliers %>% 
  mutate(budget.log = log(budget_tpt)) %>% 
  ggplot(aes(sample=budget.log)) + stat_qq() + geom_qq_line(aes(color="red")) + theme(legend.position = "none")
grid.arrange(df.nooutliers %>%
               ggplot(aes(sample=budget_tpt)) + stat_qq(), budget.log.qq, ncol=2)


# QQ Log - Treatment
treatment.log.qq <- df.nooutliers %>% 
  mutate(treatment.log = log(tx_tpt)) %>% 
  ggplot(aes(sample=treatment.log)) + stat_qq() + geom_qq_line(aes(color="red")) + theme(legend.position = "none")
grid.arrange(df.nooutliers %>%
               ggplot(aes(sample=tx_tpt)) + stat_qq(), treatment.log.qq, ncol=2)


df.nooutliers.log <- df.nooutliers %>% mutate(budget.log = log(budget_tpt)) %>% mutate(treatment.log = log(tx_tpt))

df.nooutliers.log
dim(df.nooutliers.log)
str(df.nooutliers.log)
summary(df.nooutliers.log)
describe(df.nooutliers.log)

# Test - Budget
stat.desc(df.nooutliers.log$budget_tpt, basic=FALSE, norm=TRUE)

stat.desc(df.nooutliers.log$budget.log, basic=FALSE, norm=TRUE)

# Test - Treatment
stat.desc(df.nooutliers.log$tx_tpt, basic=FALSE, norm=TRUE)

stat.desc(df.nooutliers.log$treatment.log, basic=FALSE, norm=TRUE)


# Correlation
ggplot(df.nooutliers.log, aes(x = budget.log, y = treatment.log)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color ="red")

# Correlation test
df.nooutliers.log %>% 
  select(budget.log, treatment.log) %>% 
  cor(use="complete.obs", method = "pearson")

# Correlation test
cor.test(df.nooutliers.log$budget.log, df.nooutliers.log$treatment.log)

