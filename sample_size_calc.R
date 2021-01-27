#install.packages("pwr")
library(pwr)

# For a one-way ANOVA comparing 5 groups, calculate the
# sample size needed in each group to obtain a power of
# 0.80, when the effect size is low to moderate (0.2) and a
# significance level of 0.05 is employed.

pwr.anova.test(k=4,f=.6,sig.level=.05,power=.8)