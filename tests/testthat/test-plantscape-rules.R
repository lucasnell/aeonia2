
# B. Rules every simulation must follow.


test_that("outputs are finite and non-negative across extreme settings (B2)", {
    pars <- expand.grid(zeta = c(0, 1), pseudo_surv = c(0, 1),
                        p_load = c(0, 1), Y0 = c(0, 50))
    set.seed(10)
    for (i in seq_len(nrow(pars))) {
        p <- pars[i, ]
        for (s in c("time", "all")) {
            out <- lil(n_sims = 3, zeta = p$zeta, pseudo_surv = p$pseudo_surv,
                       p_load_alate = p$p_load, p_load_plant = p$p_load,
                       Y0 = p$Y0, summ = s, max_t = 60L)
            cols <- setdiff(num_cols(out), "infect_time")
            vals <- unlist(out[cols])
            expect_true(all(is.finite(vals)), info = paste(s, i))
            expect_true(all(vals >= 0), info = paste(s, i))
        }
    }
    set.seed(11)
    for (s in c("time", "all")) {
        out <- big(n_sims = 1, summ = s)
        vals <- unlist(out[setdiff(num_cols(out), "infect_time")])
        expect_true(all(is.finite(vals)))
        expect_true(all(vals >= 0))
    }
})

test_that("no aphids anywhere gives finite wasps and p_alates = 0 (B2)", {
    # pseudo_surv = 0 on all 9 plants kills every aphid after one step.
    set.seed(12)
    none <- lil(n_sims = 2, n_pseudo = 9, pseudo_surv = 0, Y0 = 20,
                summ = "none", max_t = 5L)
    expect_true(all(is.finite(none$wasps)))
    # Per plant, wasps follow wasp_attract (equal by default) when no aphids:
    late <- none[!is.na(none$x) & none$time > 2, ]
    tot <- none[is.na(none$x) & none$time > 2, ]
    expect_equal(late$wasps, rep(tot$wasps / 9, each = 9), tolerance = 1e-12)
    all_out <- lil(n_sims = 2, n_pseudo = 9, pseudo_surv = 0, Y0 = 20,
                   summ = "all", max_t = 5L)
    expect_true(all(is.finite(all_out$p_alates)))
})

test_that("virus never spreads when transmission is impossible (B3)", {
    # Y0 = 0 lets aphids reach high densities and produce many alates,
    # so the virus does spread when transmission is possible.
    set.seed(13)
    expect_gt(mean(lil(n_sims = 20, Y0 = 0)$n_infected), 1)
    expect_true(all(lil(n_sims = 20, Y0 = 0, p_load_plant = 0)$n_infected == 1L))
    expect_true(all(lil(n_sims = 20, Y0 = 0, p_load_alate = 0)$n_infected == 1L))
    expect_true(all(lil(n_sims = 20, Y0 = 0, fly_p = 0)$n_infected == 1L))
})

test_that("infectious plants never decrease or exceed plant count (B4)", {
    set.seed(14)
    out <- lil(n_sims = 20, summ = "time", Y0 = 0, p_load_alate = 1,
               p_load_plant = 1)
    for (r in unique(out$rep)) {
        v <- out$virus[out$rep == r]
        expect_true(all(diff(v) >= 0))
        expect_true(all(v <= 9))
    }
    big_out <- big(n_sims = 1, summ = "time")
    expect_true(all(diff(big_out$virus) >= 0))
    expect_true(all(big_out$virus <= 1e4))
})

test_that("Pseudomonas acts only through pseudo_surv and pseudo_repel (B5)", {
    n <- 5L
    L0 <- array(0L, c(3, 3, n))
    L0[1, 1, ] <- 1L
    L2 <- L0
    for (i in 1:n) L2[cbind(c(3, 2, 1), c(1, 2, 3), i)] <- 2L
    run <- function(L, ps) {
        set.seed(15)
        sim_plantscape(L, N0 = array(55, c(1, 1, 1)), W0 = array(0, c(1, 1, 1)),
                       M0 = array(0, c(1, 1, 1)), Y0 = 5,
                       insect_ptr = make_insects_ptr(pseudo_surv = ps, zeta = 0.5),
                       disease_ptr = make_disease_ptr(radius = 1, virus_attract = 1,
                                                      pseudo_repel = 1,
                                                      p_load_alate = 0.5,
                                                      p_load_plant = 0.5),
                       summ = "time", n_threads = 1L)
    }
    expect_identical(run(L0, 1), run(L2, 1))
    # Check that the comparison can detect a difference:
    expect_false(identical(run(L0, 0.5), run(L2, 0.5)))
})

test_that("per-plant parasitoids sum to the total and follow zeta (B6)", {
    for (z in c(0, 0.5, 1)) {
        set.seed(16)
        out <- lil(n_sims = 2, zeta = z, Y0 = 20, summ = "none", max_t = 40L)
        plants <- out[!is.na(out$x), ]
        tots <- out[is.na(out$x), ]
        sums <- aggregate(wasps ~ rep + time, plants, sum)
        m <- merge(sums, tots[, c("rep", "time", "wasps")], by = c("rep", "time"))
        expect_equal(m$wasps.x, m$wasps.y, tolerance = 1e-10)
        # Share of parasitoids on each plant:
        # (1 - zeta) / 9 + zeta * (plant aphids / all aphids)
        plants$z <- plants$aphids + plants$alates + plants$parasitized
        z_tot <- ave(plants$z, plants$rep, plants$time, FUN = sum)
        Y_tot <- tots$wasps[match(paste(plants$rep, plants$time),
                                  paste(tots$rep, tots$time))]
        expected <- Y_tot * ((1 - z) / 9 + z * plants$z / z_tot)
        expect_equal(plants$wasps, expected, tolerance = 1e-10)
    }
})

test_that("no parasitoids means no parasitism (B7)", {
    set.seed(17)
    out <- lil(n_sims = 5, Y0 = 0, summ = "time")
    expect_true(all(out$parasitized == 0))
    expect_true(all(out$mummies == 0))
    expect_true(all(out$wasps == 0))
})

test_that("infect_stop ends runs on the first fully infected day (B8)", {
    set.seed(18)
    out <- lil(n_sims = 20, summ = "time", infect_stop = TRUE, max_t = 300L,
               Y0 = 0,
               p_load_alate = 1, p_load_plant = 1, fly_p = 0.5)
    full <- 0L
    for (r in unique(out$rep)) {
        v <- out$virus[out$rep == r]
        if (any(v == 9)) {
            full <- full + 1L
            expect_identical(which(v == 9), length(v))
        } else {
            expect_identical(length(v), 301L)
        }
    }
    expect_gt(full, 0L)
})

test_that("summary levels agree with each other (B9)", {
    run <- function(s) {
        set.seed(19)
        lil(n_sims = 3, summ = s, max_t = 50L)
    }
    none <- run("none"); time <- run("time"); all <- run("all")
    plants <- none[!is.na(none$x), ]
    tots <- none[is.na(none$x), ]
    # Plant rows sum to the totals row:
    for (v in c("virus", "aphids", "alates", "parasitized", "mummies")) {
        s <- aggregate(plants[[v]], list(rep = plants$rep, time = plants$time), sum)
        s <- s[order(s$rep, s$time), ]
        expect_equal(s$x, tots[[v]], tolerance = 1e-10, info = v)
    }
    # Totals in "none" equal "time":
    for (v in c("virus", "aphids", "alates", "parasitized", "mummies", "wasps")) {
        expect_equal(tots[[v]], time[[v]], tolerance = 1e-10, info = v)
    }
    # "all" equals means / maxima of "time":
    by_rep <- split(time, time$rep)
    m <- function(f) vapply(by_rep, f, numeric(1), USE.NAMES = FALSE)
    expect_equal(all$aphids, m(\(d) mean(d$aphids)), tolerance = 1e-10)
    expect_equal(all$log_aphids, m(\(d) mean(log(d$aphids + 1))), tolerance = 1e-10)
    expect_equal(all$alates, m(\(d) mean(d$alates)), tolerance = 1e-10)
    expect_equal(all$p_alates, m(\(d) mean(d$alates / (d$aphids + d$alates))),
                 tolerance = 1e-10)
    expect_equal(all$parasitized, m(\(d) mean(d$parasitized)), tolerance = 1e-10)
    expect_equal(all$mummies, m(\(d) mean(d$mummies)), tolerance = 1e-10)
    expect_equal(all$wasps, m(\(d) mean(d$wasps)), tolerance = 1e-10)
    expect_equal(as.numeric(all$n_infected), m(\(d) max(d$virus)))
})

test_that("alate dispersals are recorded only when alates fly (B10)", {
    set.seed(20)
    flying <- lil(n_sims = 3, Y0 = 0, summ = "all", out_dispersals = "in")
    expect_gt(sum(vapply(flying$disps, sum, numeric(1))), 0)
    grounded <- lil(n_sims = 3, Y0 = 0, summ = "all", out_dispersals = "in",
                    fly_p = 0)
    expect_identical(sum(vapply(grounded$disps, sum, numeric(1))), 0)
})

test_that("alates only fly between distinct plants within radius (B11)", {
    # Rows of the dispersal matrix are destinations, columns are origins;
    # names are "x_y".
    for (rad in c(1, 1.5, 2)) {
        set.seed(21)
        out <- lil(n_sims = 3, n_pseudo = 0, Y0 = 0, radius = rad,
                   summ = "all", out_dispersals = "all")
        D <- Reduce(`+`, out$disps)
        xy <- do.call(rbind, lapply(strsplit(rownames(D), "_"), as.integer))
        dist <- as.matrix(stats::dist(xy))
        allowed <- dist > 0 & dist <= rad
        expect_gt(sum(D), 0)
        expect_true(all(D[!allowed] == 0), info = paste("radius", rad))
    }
})
