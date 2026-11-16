
# E. Statistical checks with fixed seeds. These check direction and
# proportionality of random outcomes, not exact values.
# (E1 is in test-plant-types.R.)

test_that("higher loading probabilities increase outbreak size (E2)", {
    set.seed(40)
    # Y0 = 0 so that aphids produce many alates; max_t = 40 so that
    # outbreaks don't reach all 9 plants under both settings.
    lo <- lil(n_sims = 300, Y0 = 0, max_t = 40L, p_load_alate = 0.1,
              p_load_plant = 0.1)
    hi <- lil(n_sims = 300, Y0 = 0, max_t = 40L, p_load_alate = 0.9,
              p_load_plant = 0.9)
    expect_gt(mean(hi$n_infected), mean(lo$n_infected) + 1)
})

test_that("dispersal destinations follow alighting weights (E3)", {
    # radius = 3 makes every plant in a 3 x 3 landscape a neighbour of every
    # other plant. With pseudo_repel = 4, Pseudomonas plants have weight 1/4
    # and all others weight 1 (virus_attract = 1).
    set.seed(41)
    out <- lil(n_sims = 20, n_pseudo = 3, spat_config = "far virus", Y0 = 0,
               radius = 3, pseudo_repel = 4, virus_attract = 1,
               summ = "all", out_dispersals = "all")
    D <- Reduce(`+`, out$disps)  # rows = destination, cols = origin
    pseudo_ids <- c("2_3", "3_2", "3_3")
    wts <- ifelse(rownames(D) %in% pseudo_ids, 1 / 4, 1)
    names(wts) <- rownames(D)
    expected <- D * 0
    for (j in colnames(D)) {
        w <- wts; w[j] <- 0  # alates never "fly" to the plant they're on
        expected[, j] <- sum(D[, j]) * w / sum(w)
    }
    expect_true(all(diag(D) == 0))
    expect_gt(sum(D), 1e4)
    # Pearson chi-square goodness of fit; 8 destinations per origin,
    # 9 origins -> 9 * 7 = 63 df.
    keep <- expected > 0
    chisq <- sum((D[keep] - expected[keep])^2 / expected[keep])
    p <- pchisq(chisq, df = 63, lower.tail = FALSE)
    expect_gt(p, 0.001)
})
