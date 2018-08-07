
library("devtools")
load_all()
document()

library(usethis) ## see: https://github.com/r-lib/usethis

use_package("afex", "Suggests")
use_package("MEMSS", "Suggests")
use_package("stats", "Imports")

use_test("test_terms")
use_readme_md()

#### basic use

data("sk2011.2", package = "afex")

# use only affirmation problems (S&K also splitted the data like this)
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

afex::set_sum_contrasts()
# set up model with maximal by-participant random slopes
sk_m1 <- nested_model_formulas(response ~ instruction*inference*type,
                             sk2_aff,
                             extra_formula = ~(inference|id))
str(sk_m1, 2)

library("lme4")

sk_m1$formulas[[2]]

lmer(sk_m1$formulas[[1]], data = sk_m1$data, REML = FALSE)

all_fit <- lapply(sk_m1$formulas, function(x)
  lmer(x, data = sk_m1$data, REML = FALSE))

lapply(all_fit[-1], function(x) anova(all_fit[[1]], x, refit = FALSE))

anova_df(all_fit[[1]], all_fit[[2]])

broom::tidy(anova(all_fit[[1]], all_fit[[2]]))

afex::mixed(response ~ instruction*inference*type + (inference|id), sk2_aff,
            method = "LRT")
