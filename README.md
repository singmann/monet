[![Travis-CI Build Status](https://travis-ci.org/singmann/monet.svg?branch=master)](https://travis-ci.org/singmann/monet)

# monet

`monet` provides an interface for Type III tests of model terms for any 
user-provided fit function via function `test_terms()`. This is achieved 
by estimating a full model including all model terms, as well as restricted 
models in which the parameters corresponding to one model term are withhold 
(i.e., set to zero). The test between the full model and the restricted model 
can be  performed via any user-provided model comparison function, the default 
uses the generic `anova` function. `monet` essentially provides a generalization
of `afex::mixed` to arbitrary user defined estimation and testing functions.

The name `monet` is basically a portmanteau based on the fact that this package 
provides MOdel comparisons for NEsTed models and of course a play on the name of
[French painter Claude Monet](https://en.wikipedia.org/wiki/Claude_Monet).
 

## Installation

You can install `monet` from github with:


``` r
# install.packages("devtools")
devtools::install_github("singmann/monet")
```

## Example

The main function is `test_terms` which works with many different fit functions:

``` r
library("monet")
set_sum_contrasts() ## quite important, currently coding is not checked

test_terms(formula = count~spp * mined, 
           extra_formula =  ~(1|site), 
           fit_fun = glmmTMB::glmmTMB, 
           fit_arg = list(zi=~spp * mined, family="poisson"), 
           data = glmmTMB::Salamanders)
# glmmTMB::glmmTMB Anova Table (Type III tests)
# Model: count ~ spp * mined + (1 | site)
# Data: glmmTMB::Salamanders
#      Effect Df 1 Df 0     Chisq Pr(>Chisq)
# 1       spp   29    6   14.53 *        .02
# 2     mined   29    1 19.98 ***     <.0001
# 3 spp:mined   29    6   13.85 *        .03
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1


data("Machines", package = "MEMSS") ## example data from MEMSS package

# ignoring repeated-measures
m1 <- test_terms(score ~ Machine, data=Machines,
                 fit_fun = lm)
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
                 fit_fun = lme4::lmer, fit_arg = list(REML = FALSE),
                 test_arg = list(model.names=c("f", "r")))
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

## emmeans support is provided automatically for the returned objects:
emmeans::emmeans(m3, "Machine")
#  Machine   emmean       SE  df lower.CL upper.CL
#  A       52.35556 1.680711 7.2 48.40357 56.30754
#  B       60.32222 3.528546 7.2 52.02529 68.61916
#  C       66.27222 1.806273 7.2 62.02500 70.51945
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
```
