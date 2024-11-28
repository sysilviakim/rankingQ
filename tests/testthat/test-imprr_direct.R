test_that("plug-in estimation works", {

data("identity")

example_direct <- imprr_direct(identity,
                               J = 4,
                               main_q = "app_identity",
                               anc_correct = "anc_correct_identity",
                               n_bootstrap = 10)


})

