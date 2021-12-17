# Some simple examples to check that functions are compiling

rm(list=ls())

library(CLDA)

data(zip_train)
data(zip_test)

x <- as.matrix(unname(zip_train[, -1]))
y <- as.numeric(zip_train[, 1])

x_test <- as.matrix(unname(zip_test[, -1]))
y_test <- as.numeric(zip_test[, 1])

# Fit models
set.seed(2947640)
mod_L1 <- CLDA::CLDA(x = x, y = y, linear = TRUE, type = "full", gamma = 1e-4)
mod_L2 <- CLDA::CLDA(x = x, y = y, linear = TRUE, type = "compressed", m = floor(nrow(x)/10), s = 0.01, gamma = 1e-4)
mod_L3 <- CLDA::CLDA(x = x, y = y, linear = TRUE, type = "projected", m = floor(nrow(x)/10), s = 0.01, gamma = 1e-4)
mod_L4 <- CLDA::CLDA(x = x, y = y, linear = TRUE, type = "subsampled", m = floor(nrow(x)/10), gamma = 1e-4)
# mod_L5 <- CLDA::CLDA(x = x, y = y, linear = TRUE, type = "FRF", m = floor(nrow(x)/10), s = 0.01, gamma = 1e-4)

mod_Q1 <- CLDA::CLDA(x = x, y = y, linear = FALSE, type = "full", gamma = 1e-4)
mod_Q2 <- CLDA::CLDA(x = x, y = y, linear = FALSE, type = "compressed", m = floor(nrow(x)/10), s = 0.01, gamma = 1e-4)


# Make predictions
preds_L1 <- predict.CLDA(mod = mod_L1, x = x_test)
preds_L2 <- predict.CLDA(mod = mod_L2, x = x_test)
preds_L3 <- predict.CLDA(mod = mod_L3, x = x_test)
preds_L4 <- predict.CLDA(mod = mod_L4, x = x_test)
# preds_L5 <- predict.CLDA(mod = mod_L5, x = x_test)

preds_Q1 <- predict.CLDA(mod = mod_Q1, x = x_test)
preds_Q2 <- predict.CLDA(mod = mod_Q2, x = x_test)
