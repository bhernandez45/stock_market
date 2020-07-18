######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################
######HEY THERE LOSER!!!!!!!!!!!!!!!!!!!!#######################

install.packages("lme4")
library(lme4)


corr(finance_data)

# reccodes and cleaning
#finance_data$time=
#finance_data$success[finance_data$close>0]=1
#finance_data$success[finance_data$close<0]=0


# unconditional growth model
m0=lmer(success~ 1 + (1|time), data=finance_data, family="binomial")

m1=lmer(success~1 + (1|time), data=finance_data, family="binomial")

icc_mod_fin=
