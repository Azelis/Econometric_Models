CCAPM <- read.delim("Data/CCAPM.txt")
attach(CCAPM)

# CCAMP - Consumer Capital Asset Pricing Model # 

# It shows financial instrument dependency of consumers
# r1 is smaller  company and their return of investment
# r2 is a bigger company with a lower possible return and risk and etc.

# The goal is to create 10 regression models to identify beta values (risk)

# Part of analysis is to create autocoreglama, investigate autocorrelation and
# apply reasonable Lag to solve autocorrelation problem. 
# Autocorrelation means data correlated with itself over specific time

library(Hmisc)

acf(r1)
# There is an autocorellation with 1, 7, 8, 12 months. 
# Lag 8 (8 month) is not included since p value is to high and it does not required to solve
# autocorrelation problem
m1 <- lm(r1 ~ cons + Lag(r1, 1) + Lag(r1, 7) + Lag(r1, 12))
summary(m1)
acf(residuals(m1))
# By residuals autocoreglama, Lag problem is solved

acf(r2)
# Autocorrelation with 1, 8, 12 months
m2 <- lm(r2 ~ cons + Lag(r2, 1) + Lag(r2, 8) + Lag(r2, 12))
summary(m2)
acf(residuals(m2))

acf(r3)
# Autocorrelation with 1, 8, 12 months
m3 <- lm(r3 ~ cons + Lag(r3, 1) + Lag(r3, 8) + Lag(r3, 12))
summary(m3)
acf(residuals(m3))
# Lags of more than 12 months are not very significant for given regression model

acf(r4)
# Autocorrelation with 1, 8, 12 months
m4 <- lm(r4 ~ cons + Lag(r4, 1) + Lag(r4, 8) + Lag(r4, 12))
summary(m4)
acf(residuals(m4))
# P value for Lag 12 is higher than 0.05, but lower than 0.10, so we will keep it
# since it is really important to solve autocorrelation case

acf(r5)
# Autocorrelation with 1, 8, 12 months
# Important to mention that autocorrelation with 12 month influence is decreasing
# with every bigger company, lag with 12 month is decreasing
m5 <- lm(r5 ~ cons + Lag(r5, 1) + Lag(r5, 8) + Lag(r5, 12))
summary(m5)
acf(residuals(m5))

acf(r6)
# Autocorrelation with 1, 8 months, 12 isnot significant anymore
m6 <- lm(r6 ~ cons + Lag(r6, 1) + Lag(r6, 8))
summary(m6)
acf(residuals(m6))

acf(r7)
# Autocorrelation with 1, 8 months
m7 <- lm(r7 ~ cons + Lag(r7, 1) + Lag(r7, 8))
summary(m7)
acf(residuals(m7))
# Can be spotted that 5 month is very closed to be autocorrelated

acf(r8)
# Autocorrelation with only 8 month
m8 <- lm(r8 ~ cons + Lag(r8, 8))
summary(m8)
acf(residuals(m8))
# Lag 8 has 10.7% p-value, assumsing it is still significant, since it is around 10%

acf(r9)
# Autocorrelation with 8 month
m9 <- lm(r9 ~ cons + Lag(r9, 8))
summary(m9)
acf(residuals(m9))

acf(r10)
# Autocorrelation with 5 month
m10 <- lm(r10 ~ cons + Lag(r10, 5))
summary(m10)
acf(residuals(m10))

# With