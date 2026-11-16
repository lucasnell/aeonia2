
# A. Input checks

test_that("sim_plant_types() rejects bad sizes and counts (A1)", {
    expect_error(sim_plant_types(0, 10, n_virus = 1, n_pseudo = 1), "n_x")
    expect_error(sim_plant_types(10, 0, n_virus = 1, n_pseudo = 1), "n_y")
    expect_error(sim_plant_types(1, 1, n_virus = 1, n_pseudo = 0), "n_x \\* n_y")
    expect_error(sim_plant_types(3, 3, n_virus = 10, n_pseudo = 0), "n_virus")
    expect_error(sim_plant_types(3, 3, n_virus = 0, n_pseudo = 10), "n_pseudo")
    expect_error(sim_plant_types(3, 3, n_virus = 0, n_pseudo = 0), "cannot be < 1")
    expect_error(sim_plant_types(3, 3, n_virus = 1, n_pseudo = 1, n_lands = 0),
                 "n_lands")
})

test_that("make_disease_ptr() rejects out-of-range values (A2)", {
    dp <- function(...) {
        args <- list(radius = 1, virus_attract = 1, pseudo_repel = 1,
                     p_load_alate = 0.5, p_load_plant = 0.5)
        other <- list(...)
        args[names(other)] <- other
        do.call(make_disease_ptr, args)
    }
    expect_identical(typeof(dp()), "externalptr")  # baseline works
    expect_error(dp(radius = 0.5), "radius")
    expect_error(dp(p_load_alate = 1.5), "p_load_alate")
    expect_error(dp(p_load_alate = -0.1), "p_load_alate")
    expect_error(dp(p_load_plant = 1.5), "p_load_plant")
    expect_error(dp(w = 2), "w < 0.0001 \\|\\| w > 1")
    expect_error(dp(w = 1e-5), "w < 0.0001 \\|\\| w > 1")
    expect_error(dp(w = 0.2, epsilon = 10), "w\\*epsilon > 1")
    expect_error(dp(w = 0.001, epsilon = 0.01), "w\\*epsilon < 0.0001")
    expect_error(dp(total_exp_days = 0L), "total_exp_days")
})

test_that("plantscape wrappers reject bad landscapes and densities (A3)", {
    # N0 of zero is not allowed:
    expect_error(lil(N0 = 0), "N0 must be > 0")
    expect_error(lil(N0 = -1), "N0")
    # Wrong landscape dimensions or values for big_plantscape:
    land_bad_dim <- array(0L, c(99, 100, 1))
    land_bad_dim[1, 1, 1] <- 1L
    expect_error(big_plantscape(n_sims = 1, landscape = land_bad_dim,
                                virus_attract = 1, pseudo_repel = 1,
                                pseudo_surv = 1, zeta = 0.5, N0 = 55, sd_N = 0,
                                Y0 = 1, summ = "all"))
    land_bad_val <- array(0L, c(100, 100, 1))
    land_bad_val[1, 1, 1] <- 4L
    expect_error(big_plantscape(n_sims = 1, landscape = land_bad_val,
                                virus_attract = 1, pseudo_repel = 1,
                                pseudo_surv = 1, zeta = 0.5, N0 = 55, sd_N = 0,
                                Y0 = 1, summ = "all"))
    # Dispersal output without summ = "all" in big_plantscape:
    land <- array(0L, c(100, 100, 1))
    land[1, 1, 1] <- 1L
    expect_error(big_plantscape(n_sims = 1, landscape = land,
                                virus_attract = 1, pseudo_repel = 1,
                                pseudo_surv = 1, zeta = 0.5, N0 = 55, sd_N = 0,
                                Y0 = 1, out_dispersals = "in"),
                 "summ` must be \"all\"")
})

test_that("alate_prop() rejects negative densities (A4)", {
    expect_error(alate_prop(-1), "z < 0")
})

test_that("lil_plantscape() checks n_pseudo against spat_config (A3)", {
    # Fixed layouts allow 0 or 3 Pseudomonas plants:
    for (sc in c("diagonal", "near virus", "far virus", "over virus")) {
        expect_no_error(lil(n_sims = 2, n_pseudo = 0, spat_config = sc))
        expect_no_error(lil(n_sims = 2, n_pseudo = 3, spat_config = sc))
        expect_error(lil(n_sims = 2, n_pseudo = 5, spat_config = sc),
                     "exactly 0 or 3")
    }
    # Random layouts allow other counts:
    expect_no_error(lil(n_sims = 2, n_pseudo = 5, spat_config = "random"))
    expect_no_error(lil(n_sims = 2, n_pseudo = 5, spat_config = "no virus"))
})

test_that("starting alates work with the default distr_0 (A3)", {
    # Previously failed with "alates X_0 sums to <= 0".
    set.seed(50)
    out <- lil(n_sims = 2, n_pseudo = 0, W0 = 7, summ = "none",
               out_stages = "all", max_t = 2L)
    first <- out[!is.na(out$x) & out$time == 1, ]
    alate_cols <- grep("^alates_", names(first), value = TRUE)
    expect_equal(rowSums(first[alate_cols]), rep(7, nrow(first)),
                 tolerance = 1e-10)
    expect_true(all(unlist(first[alate_cols]) >= 0))
    ip <- make_insects_ptr(pseudo_surv = 1, zeta = 0)
    expect_no_error(test_insect_pops(5, N0 = 10, W0 = 5, M0 = 0, Y0 = 0, ip))
})
