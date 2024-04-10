
# scoff

require(rstan)
require(loo)

load("result_scoff_alpha.Rdata")

fit1 <- fit

load("result_scoff.Rdata")

fit2 <- fit

loo(fit1)
loo(fit2)


# base

require(rstan)
require(loo)

load("result_base_alpha.Rdata")

fit1 <- fit

load("result_base.Rdata")

fit2 <- fit

loo(fit1)
loo(fit2)