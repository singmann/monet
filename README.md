[![Travis-CI Build Status](https://travis-ci.org/singmann/monet.svg?branch=master)](https://travis-ci.org/singmann/monet)

# monet

`monet` provides an interface for Type III tests of model terms for any 
user-provided estimation function: `test_terms`.  
This is achieved by estimating a full model including all model terms, as well
as restricted models in which the parameters corresponding to one model term are
withhold (i.e., set to zero). The test between the full model and the restricted
model can be  performed via any user-provided model comparison function, the
default uses the generic `anova` function.
In some sense, `monet` provides a generalization to `afex::mixed`.

The name `monet` is basically a portmanteau based on the fact that this package 
provides MOdel comparisons for NEsTed models and of course a play on the name of [French painter Claude Monet](https://en.wikipedia.org/wiki/Claude_Monet).
 

## Installation

You can install `monet` from github with:


``` r
# install.packages("devtools")
devtools::install_github("singmann/monet")
```

## Example

The main function is `test_terms`:

``` r
set_sum_contrasts() ## quite important, currently coding is not checked

data("Machines", package = "MEMSS")

# ignoring repeated-measures
m1 <- test_terms(score ~ Machine, data=Machines,
                 est_fun = lm)
m1
# lm Anova Table (Type III tests)
# 
# Model: score ~ Machine
# Data: Machines
#    Effect Df 1 Df 0         F Pr(>F)
# 1 Machine   51    2 26.30 *** <.0001
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1
nice(m1)
anova(m1)

# simple model with random-slopes for repeated-measures factor
m3 <- test_terms(score ~ Machine, data=Machines,
                 extra_formula = ~ (Machine|Worker),
                 est_fun = lme4::lmer, arg_est = list(REML = FALSE),
                 arg_test = list(model.names=c("f", "r")))
m3
# lme4::lmer Anova Table (Type III tests)
# 
# Model: score ~ Machine + (Machine | Worker)
# Data: Machines
#    Effect Df 1 Df 0     Chisq Pr(>Chisq)
# 1 Machine   10    2 17.14 ***      .0002
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1
anova(m3)

## replicates:
afex::mixed(score ~ Machine + (Machine|Worker), data=Machines, method = "LRT")
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: score ~ Machine + (Machine | Worker)
# Data: Machines
# Df full model: 10
#    Effect df     Chisq p.value
# 1 Machine  2 17.14 ***   .0002
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1
```
