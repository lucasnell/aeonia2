
# B1 and E1: simulated arrangements of virus and Pseudomonas plants.
# Landscape codes: bit 1 = virus, bit 2 = Pseudomonas (0, 1, 2, or 3).

test_that("sim_plant_types() places exactly n_virus and n_pseudo plants (B1)", {
    set.seed(1)
    L <- sim_plant_types(20, 20, n_virus = 3, n_pseudo = 50, n_lands = 10)
    expect_identical(dim(L), c(20L, 20L, 10L))
    expect_true(all(L %in% 0:3))
    for (i in seq_len(dim(L)[3])) {
        expect_identical(sum(bitwAnd(L[, , i], 1L) > 0), 3L)
        expect_identical(sum(bitwAnd(L[, , i], 2L) > 0), 50L)
    }
})

test_that("sim_plant_types() honours starting positions (B1)", {
    set.seed(2)
    L <- sim_plant_types(20, 20, n_virus = 1, n_pseudo = 10, n_lands = 10,
                         virus_starts = cbind(1, 1),
                         pseudo_starts = cbind(20, 20))
    expect_true(all(bitwAnd(L[1, 1, ], 1L) > 0))
    expect_true(all(bitwAnd(L[20, 20, ], 2L) > 0))
})

test_that("tiny wt_vp keeps virus and Pseudomonas apart; shared start overlaps (B1)", {
    # Same settings as the large-landscape simulations, fewer landscapes.
    set.seed(3)
    L_apart <- sim_plant_types(100, 100, n_virus = 1, n_pseudo = 9000,
                               n_lands = 5, wt_vp = 1e-6, wt_pp = 1,
                               virus_starts = cbind(1, 1),
                               pseudo_starts = cbind(100, 100))
    expect_false(any(L_apart == 3L))
    L_over <- sim_plant_types(100, 100, n_virus = 1, n_pseudo = 1000,
                              n_lands = 5, wt_vp = 100, wt_pp = 1,
                              virus_starts = cbind(1, 1),
                              pseudo_starts = cbind(1, 1))
    expect_true(all(L_over[1, 1, ] == 3L))
})

test_that("wt_pp > 1 clusters Pseudomonas plants (E1)", {
    # Mean number of Pseudomonas plants among the 8 neighbours of each
    # Pseudomonas plant.
    pp_neighbours <- function(L) {
        P <- (L %/% 2L) %% 2L == 1L  # keeps matrix dimensions
        n_x <- nrow(P); n_y <- ncol(P)
        cnt <- matrix(0L, n_x, n_y)
        for (dx in -1:1) for (dy in -1:1) {
            if (dx == 0 && dy == 0) next
            xs <- max(1, 1 + dx):min(n_x, n_x + dx)
            ys <- max(1, 1 + dy):min(n_y, n_y + dy)
            cnt[xs - dx, ys - dy] <- cnt[xs - dx, ys - dy] + P[xs, ys]
        }
        mean(cnt[P])
    }
    set.seed(4)
    unif <- sim_plant_types(100, 100, n_virus = 1, n_pseudo = 1000,
                            n_lands = 5, wt_pp = 1)
    clust <- sim_plant_types(100, 100, n_virus = 1, n_pseudo = 1000,
                             n_lands = 5, wt_pp = 3)
    m_unif <- mean(sapply(1:5, \(i) pp_neighbours(unif[, , i])))
    m_clust <- mean(sapply(1:5, \(i) pp_neighbours(clust[, , i])))
    # Uniform placement: expected ~ 8 * 999 / 9999 ~ 0.8 (less at edges).
    expect_lt(abs(m_unif - 0.8), 0.15)
    expect_gt(m_clust, 1.5 * m_unif)
})

test_that("placement weights reach neighbours in non-square landscapes (B1)", {
    # With a huge wt_pp, the second Pseudomonas plant must be next to the
    # first one (including diagonally).
    for (st in list(c(1, 1), c(5, 12), c(3, 7), c(1, 12), c(5, 1))) {
        set.seed(5)
        L <- sim_plant_types(5, 12, n_virus = 0, n_pseudo = 2, n_lands = 30,
                             wt_pp = 1e9, pseudo_starts = rbind(st))
        for (i in seq_len(dim(L)[3])) {
            xy <- which(L[, , i] == 2L, arr.ind = TRUE)
            other <- xy[!(xy[, 1] == st[1] & xy[, 2] == st[2]), , drop = FALSE]
            expect_identical(nrow(other), 1L)
            expect_lte(max(abs(other[1, ] - st)), 1,
                       label = paste("start", paste(st, collapse = ",")))
        }
    }
})
