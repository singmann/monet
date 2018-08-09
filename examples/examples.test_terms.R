
set_sum_contrasts() ## quite important, currently coding is not checked

data("Machines", package = "MEMSS")

# ignoring repeated-measures
m1 <- test_terms(score ~ Machine, data=Machines,
                 est_fun = lm)
m1
nice(m1)
anova(m1)

## fixed-effects model
m2 <- test_terms(score ~ Machine*Worker, data=Machines,
                 est_fun = lm)
m2


# simple model with random-slopes for repeated-measures factor
m3 <- test_terms(score ~ Machine, data=Machines,
                 extra_formula = ~ (Machine|Worker),
                 est_fun = lme4::lmer, arg_est = list(REML = FALSE),
                 arg_test = list(model.names=c("f", "r")))
m3
anova(m3)

## specify colnames in anova() output by hand instead of automatically:
m3b <- test_terms(score ~ Machine, data=Machines,
                 extra_formula = ~ (Machine|Worker),
                 est_fun = lme4::lmer, arg_est = list(REML = FALSE),
                 arg_test = list(model.names=c("f", "r"),
                                 colnames = list("Chi Df", "Chisq", "Pr(>Chisq)")))
m3b


\dontrun{
# using an example from afex
data("sk2011.2", package = "afex")
# use only affirmation problems (S&K also splitted the data like this)
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

library("lme4")
# not a particularly reasonable non-maximal model
sk_m1 <- test_terms(response ~ instruction*inference*type,
                             sk2_aff,
                             extra_formula = ~(inference|id),
                    est_fun = lmer, arg_est = list(REML = FALSE),
                    arg_test = list(model.names=c("f", "r")))

nice(sk_m1)
anova(sk_m1)

### matches:
afex::mixed(response ~ instruction*inference*type + (inference|id),
                             sk2_aff, method = "LRT")

## if corresponding method exist, emmeans support is provided automatically:
emmeans::emmeans(sk_m1, c("instruction", "type"))

}

\dontrun{
## It works also with glmmTMB
## see: https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
Owls <- transform(glmmTMB::Owls,
                  Nest=reorder(Nest,NegPerChick),
                  NCalls=SiblingNegotiation,
                  FT=FoodTreatment)
zipp_test <- test_terms(formula = NCalls~(FT+ArrivalTime)*SexParent,
                        data = Owls,
                        extra_formula = ~ offset(log(BroodSize)) + (1|Nest),
                        est_fun = glmmTMB::glmmTMB,
                        arg_est = list(ziformula=~1, family=poisson)
)
zipp_test
}
