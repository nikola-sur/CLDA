# Some simple examples to check that functions are compiling

rm(list=ls())

library(CLDA)

CLDA::testfun(10) # 55

data(zip_train)
data(zip_test)
