
library("devtools")
load_all()
document()
test()
library("testthat")

library(usethis) ## see: https://github.com/r-lib/usethis

use_package("afex", "Suggests")
use_package("MEMSS", "Suggests")
use_package("emmeans", "Suggests")
use_package("stats", "Imports")

use_test("test_terms")
use_test("test_glmmTMB_interface")
use_readme_md()


