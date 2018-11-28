
library(parallel)

design <- factor.design(~cond1[a,b]+cond2[a,b]+subject(cond2)*5+item(cond1)*20)

# minimal design consists of 10 subjects and 40 items

str(output.design(design))

fixefs <- default.fixed.means(design)


fixefs[1] = 1 # set true intercept
fixefs["cond1b"] = 1 # set true fixed effect for cond1b

ranefs <- default.random.cov(design, sd=.4)
ranefs$subject <- covmat(c(.4,.2), matrix(c(1,.3,.3,1),ncol=2), set.names = colnames(ranefs$subject))


sig <- 5.0 # true residual sd

cl <- makeForkCluster(detectCores())

pwr4 <- design.power.curve(
  design, # for which design to simulate
  modelClass="lmer",
  formula=response~cond1b*cond2b+(cond1b|subject)+(cond2b|item),
  cl=cl, # parallel cluster (if not given, evaluated sequentially)
  along="subject", # random factor along which to replicate design
  nsim=500, # no. of power simulations for each configuration
  ns=1:10, # vector of replication sizes to test
  fixefs = fixefs, # true fixed effects (group means)
  varcov = ranefs, # true variance-covariance matrices
  residual.sd = sig, # true residual error
  alpha = .05, # alpha level
  tests = tests( # test(s) to perform on each simulation
    t("(Intercept)"),
    t(cond1b),
    simr(fixed, cond1b, t),
    t(cond2b)
  )
)

pwr2 <- design.power.curve(
  design, # for which design to simulate
  modelClass="lmer",
  formula=response~cond1b*cond2b+(cond1b|subject)+(cond2b|item),
  cl=cl, # parallel cluster (if not given, evaluated sequentially)
  along="item", # random factor along which to replicate design
  nsim=500, # no. of power simulations for each configuration
  ns=1:5, # vector of replication sizes to test
  fixefs = fixefs, # true fixed effects (group means)
  varcov = ranefs, # true variance-covariance matrices
  residual.sd = sig, # true residual error
  alpha = .05, # alpha level
  tests = tests( # test(s) to perform on each simulation
    t("(Intercept)"),
    t(cond1b),
    t(cond2b)
  )
)


stopCluster(cl)

pwr4
pwr2

gridExtra::grid.arrange(
  plot(pwr4, plot.error = "r", plot.mean = "l"),
  plot(pwr2, plot.error = "r", plot.mean = "l")
)
