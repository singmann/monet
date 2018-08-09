context("test glmmTMB_interface")

test_that("Examples work: vignette", {
  testthat::skip_if_not_installed("glmmTMB")
  testthat::skip_on_cran()
  if (require("glmmTMB")) {
    Owls <- transform(Owls,
                      Nest=reorder(Nest,NegPerChick),
                      NCalls=SiblingNegotiation,
                      FT=FoodTreatment)
    ## see: https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
    fit_zipoisson <- glmmTMB(NCalls~(FT+ArrivalTime)*SexParent+
                               offset(log(BroodSize))+(1|Nest),
                             data=Owls,
                             ziformula=~1,
                             family=poisson)
    zipp_test <- test_terms(formula = NCalls~(FT+ArrivalTime)*SexParent +
                              offset(log(BroodSize)),
                            data = Owls, extra_formula = ~ (1|Nest),
                            fit_fun = glmmTMB,
                            fit_arg = list(ziformula=~1, family=poisson)
    )
    expect_is(zipp_test, "monet")
    sum1 <- summary(fit_zipoisson)
    sum2 <- summary(zipp_test$full_model)
    expect_equivalent(sum1[names(sum1) != "call"], sum2[names(sum2) != "call"])

    fit_zipoisson <- glmmTMB(NCalls~(FT+scale(ArrivalTime))*SexParent+
                               offset(log(BroodSize))+(1|Nest),
                             data=Owls,
                             ziformula=~1,
                             family=poisson)
    zipp_test <- test_terms(formula = NCalls~(FT+scale(ArrivalTime))*SexParent +
                              offset(log(BroodSize)),
                            data = Owls, extra_formula = ~ (1|Nest),
                            fit_fun = glmmTMB,
                            fit_arg = list(ziformula=~1, family=poisson)
    )
    expect_is(zipp_test, "monet")
    sum1 <- summary(fit_zipoisson)
    sum2 <- summary(zipp_test$full_model)
    expect_equivalent(sum1[names(sum1) != "call"], sum2[names(sum2) != "call"])
  }

})


test_that("Examples work: ?glmmTMB", {
  testthat::skip_if_not_installed("glmmTMB")
  testthat::skip_if_not_installed("lme4")
  testthat::skip_on_cran()
  if (require("glmmTMB")) {

    m1 <- glmmTMB(count~ mined + (1|site), zi=~mined, family=poisson,
                  data=Salamanders)
    m0 <- glmmTMB(count~ 1 + (1|site), zi=~mined, family=poisson,
                  data=Salamanders)
    m1_test <- test_terms(count~ mined, extra_formula = ~ (1|site),
                          fit_fun = glmmTMB,
                          fit_arg = list(zi=~mined, family=poisson),
                          data=Salamanders)
    man_test <- anova(m0, m1)

    expect_is(m1_test, "monet")
    sum1 <- summary(m1)
    sum2 <- summary(m1_test$full_model)
    expect_equivalent(sum1[names(sum1) != "call"], sum2[names(sum2) != "call"])
    expect_equivalent(anova(m1_test)[,c("Chisq", "Pr(>Chisq)")],
                      as.data.frame(man_test)[2,c("Chisq", "Pr(>Chisq)")])

    data(cbpp, package="lme4")
    tmbm1 <- glmmTMB(cbind(incidence, size-incidence) ~ period + (1 | herd),
                      data=cbpp, family=binomial)
    tmbm0 <- glmmTMB(cbind(incidence, size-incidence) ~ 1 + (1 | herd),
                      data=cbpp, family=binomial)

    test_tmbm1 <- test_terms(cbind(incidence, size-incidence) ~ period,
                             extra_formula = ~ (1 | herd),
                             data=cbpp, fit_fun = glmmTMB,
                             fit_arg = list(family=binomial))

    man_test <- anova(tmbm0, tmbm1)

    expect_is(test_tmbm1, "monet")
    sum1 <- summary(tmbm1)
    sum2 <- summary(test_tmbm1$full_model)
    expect_equivalent(sum1[names(sum1) != "call"], sum2[names(sum2) != "call"])
    expect_equivalent(anova(test_tmbm1)[,c("Chisq", "Pr(>Chisq)")],
                      as.data.frame(man_test)[2,c("Chisq", "Pr(>Chisq)")])

  }

})
