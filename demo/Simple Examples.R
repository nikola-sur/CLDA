# Some simple examples to check that functions are compiling

rm(list=ls())

library(CLDA)

data(zip_train)
# data(zip_test)

x <- as.matrix(unname(zip_train[, -1]))
y <- as.numeric(zip_train[, 1])

CLDA::CLDA(x = x, y = y, linear = TRUE, type = "full", m = nrow(x), gamma = 0.01)
