
# D. Reproducibility

test_that("same seed gives identical output (D1)", {
    run_lil <- function() { set.seed(30); lil(n_sims = 8, summ = "time") }
    expect_identical(run_lil(), run_lil())
    run_big <- function() { set.seed(31); big(n_sims = 2, summ = "all") }
    expect_identical(run_big(), run_big())
    run_spt <- function() {
        set.seed(32)
        sim_plant_types(50, 50, n_virus = 1, n_pseudo = 500, n_lands = 4,
                        wt_pp = 3)
    }
    expect_identical(run_spt(), run_spt())
})

test_that("number of threads does not change output (D2)", {
    run_lil <- function(nt) {
        set.seed(33); lil(n_sims = 8, summ = "time", n_threads = nt)
    }
    expect_identical(run_lil(1L), run_lil(4L))
    run_big <- function(nt) {
        set.seed(34); big(n_sims = 4, summ = "all", n_threads = nt)
    }
    expect_identical(run_big(1L), run_big(4L))
    run_spt <- function(nt) {
        set.seed(35)
        sim_plant_types(50, 50, n_virus = 1, n_pseudo = 500, n_lands = 8,
                        wt_pp = 3, n_threads = nt)
    }
    expect_identical(run_spt(1L), run_spt(4L))
})
