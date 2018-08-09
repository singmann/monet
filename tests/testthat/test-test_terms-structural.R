context("test_terms: Structural tests")

test_that("mixed: Maxell & Delaney (2004), Table 16.4, p. 842: Type 3", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("afex")
  data(md_16.4, package = "afex")
  md_16.4b <- md_16.4
  md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)
  contrasts(md_16.4b$cond) <- "contr.sum"
  mixed4_2 <- test_terms(induct ~ cond*cog, md_16.4b,
                         extra_formula = ~ (cog|room:cond),
                         fit_fun = lme4::lmer, fit_arg = list(REML = FALSE),
                 test_arg = list(model.names=c("f", "r")))
  lmer4_full <- lme4::lmer(induct ~ cond*cog + (cog|room:cond), md_16.4b,
                           REML = FALSE)
  lmer4_small <- lme4::lmer(induct ~ cond+cog + (cog|room:cond), md_16.4b,
                            REML = FALSE)
  expect_that(lme4::fixef(mixed4_2$full_model),
              equals(lme4::fixef(lmer4_full)))
  expect_that(mixed4_2$full_model, is_equivalent_to(lmer4_full))
  expect_that(lme4::fixef(mixed4_2$restricted_models$`cond:cog`),
              is_equivalent_to(lme4::fixef(lmer4_small)))
})
