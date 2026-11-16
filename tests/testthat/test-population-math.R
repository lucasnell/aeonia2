
# C. Single-plant dynamics and helper functions compared against
# calculations done independently in R.


test_that("logit and inv_logit are inverses with known values (C3)", {
    p <- c(0.001, 0.1, 0.5, 0.9, 0.999)
    expect_equal(inv_logit(logit(p)), p, tolerance = 1e-12)
    expect_equal(logit(p), log(p / (1 - p)), tolerance = 1e-12)
    expect_equal(inv_logit(0), 0.5)
})

test_that("alate_prop() equals inv_logit(b0 + b1 * z) (C3)", {
    z <- c(0, 10, 100, 1000, 5000)
    expect_equal(alate_prop(z, -5, 0.0022), plogis(-5 + 0.0022 * z),
                 tolerance = 1e-12)
    expect_equal(alate_prop(z, 1, -0.01), plogis(1 - 0.01 * z),
                 tolerance = 1e-12)
})

test_that("sad_leslie() returns the normalized dominant eigenvector (C2)", {
    L <- apterous_leslie()
    v <- as.numeric(sad_leslie(L))
    lambda <- dom_eigen(L)
    expect_true(all(v >= 0))
    expect_equal(sum(v), 1, tolerance = 1e-12)
    expect_equal(as.numeric(L %*% v), lambda * v, tolerance = 1e-10)
})

test_that("low-density growth equals lambda * pseudo_surv * pred_surv (C2)", {
    # No alate production (alate_prop = 0), no parasitoids,
    # and density dependence negligible (huge K).
    for (ps in c(1, 0.8)) for (pr in c(0.9, 0.95)) {
        ip <- make_insects_ptr(pseudo_surv = ps, zeta = 0, K = 1e15,
                               pred_surv = pr, alate_b0 = -1000)
        out <- test_insect_pops(10, N0 = 10, W0 = 0, M0 = 0, Y0 = 0, ip)
        rate <- dom_eigen(apterous_leslie()) * ps * pr
        expect_equal(out$aphids[-1] / head(out$aphids, -1),
                     rep(rate, 10), tolerance = 1e-8)
        expect_true(all(out$alates == 0))
    }
})

test_that("equilibrium matches carrying_capacity() (C1)", {
    # Beverton-Holt density dependence: z* = K * (lambda * ps * pred - 1)
    pars <- expand.grid(ps = c(1, 0.9), pr = c(0.9, 0.95), K = c(5e3, 12.5e3))
    for (i in seq_len(nrow(pars))) {
        ps <- pars$ps[i]; pr <- pars$pr[i]; K <- pars$K[i]
        ip <- make_insects_ptr(pseudo_surv = ps, zeta = 0, K = K,
                               pred_surv = pr, alate_b0 = -1000)
        out <- test_insect_pops(5000, N0 = 50, W0 = 0, M0 = 0, Y0 = 0, ip)
        cc <- carrying_capacity(pseudo_surv = ps, K = K, pred_surv = pr)
        expect_gt(cc, 0)
        expect_equal(tail(out$aphids, 1), cc, tolerance = 1e-6)
    }
})

test_that("wasps without hosts decline as Y0 * s_y^t (C4)", {
    ip <- make_insects_ptr(pseudo_surv = 1, zeta = 0)
    out <- test_insect_pops(20, N0 = 0, W0 = 0, M0 = 0, Y0 = 100, ip)
    expect_equal(out$wasps, 100 * an_populations$s_y^(0:20), tolerance = 1e-12)
})

test_that("mummies develop and emerge as wasps as written in R (C5)", {
    ps <- 1; pr <- 0.9; s_y <- an_populations$s_y
    sex_ratio <- an_populations$sex_ratio
    n_mum <- an_dev_times$mum_days[2]  # mummy development time (days)
    M0 <- 30; Y0 <- 4; max_t <- 10
    ip <- make_insects_ptr(pseudo_surv = ps, zeta = 0, pred_surv = pr)
    out <- test_insect_pops(max_t, N0 = 0, W0 = 0, M0 = M0, Y0 = Y0, ip)
    # Independent recurrence:
    M <- rep(M0 / n_mum, n_mum)
    Y <- Y0
    M_tot <- sum(M); Y_all <- Y
    for (t in 1:max_t) {
        old_mums <- M[n_mum]
        M[2:n_mum] <- pr * M[1:(n_mum - 1)]
        M[1] <- 0  # no new mummies without aphids
        Y <- s_y * Y + sex_ratio * old_mums
        M_tot <- c(M_tot, sum(M)); Y_all <- c(Y_all, Y)
    }
    expect_equal(out$mummies, M_tot, tolerance = 1e-12)
    expect_equal(out$wasps, Y_all, tolerance = 1e-12)
})

test_that("attack survival matches negative-binomial equation (C6)", {
    # Meisner et al. (2014), eq. 6:
    # A_bar = rel_attack * a * Y / (h * x + 1)
    # Pr(survive) = Pr(0 attacks) + Pr(1 attack) * s1 + Pr(>=2 attacks) * s2
    rel <- c(0.1, 0.2, 0.3, 0.4)
    a <- 2.3; k <- 0.35; h <- 0.03
    for (Y in c(0, 1, 10, 100)) for (x in c(0, 50, 1000)) {
        A_bar <- rel * a * Y / (h * x + 1)
        for (s in list(c(0, 0), c(0.4, 0.1))) {
            r_out <- aeonia:::attack_surv_test_cpp(rel, a, k, h, Y, x, s)
            if (Y == 0) {
                expected <- rep(1, length(rel))
            } else {
                p0 <- dnbinom(0, size = k, mu = A_bar)
                p1 <- dnbinom(1, size = k, mu = A_bar)
                expected <- p0 + p1 * s[1] + (1 - p0 - p1) * s[2]
            }
            expect_equal(as.numeric(r_out), expected, tolerance = 1e-10)
        }
    }
})
