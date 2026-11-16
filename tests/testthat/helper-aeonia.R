
# Shared helpers for tests.
# Large landscapes are only ever simulated with `summ = "time"` or
# `summ = "all"` because `summ = "none"` uses too much memory.


# Small (3 x 3) landscape simulation with defaults used across tests.
lil <- function(n_sims = 10, n_pseudo = 3, pseudo_surv = 0.85, zeta = 0.5,
                N0 = 55, sd_N = 0, Y0 = 5, summ = "all", n_threads = 1L, ...) {
    lil_plantscape(n_sims = n_sims, n_pseudo = n_pseudo,
                   pseudo_surv = pseudo_surv, zeta = zeta, N0 = N0,
                   sd_N = sd_N, Y0 = Y0, summ = summ, n_threads = n_threads, ...)
}

# Large (100 x 100) landscape simulation; never uses summ = "none".
big <- function(n_sims = 2, n_pseudo = 1000, zeta = 0.5, max_t = 30L,
                summ = "all", n_threads = 1L, ...) {
    stopifnot(summ %in% c("time", "all"))
    land <- sim_plant_types(100, 100, n_virus = 1, n_pseudo = n_pseudo,
                            n_lands = n_sims)
    big_plantscape(n_sims = n_sims, landscape = land, virus_attract = 1,
                   pseudo_repel = 1, pseudo_surv = 0.85, zeta = zeta,
                   N0 = 55, sd_N = 0, Y0 = 250, max_t = max_t, summ = summ,
                   n_threads = n_threads, ...)
}

# Apterous Leslie matrix with the same defaults as `make_insects_ptr()`
# (same approach as `carrying_capacity()`).
apterous_leslie <- function(...) {
    arg_names <- names(formals(make_insects_ptr))
    arg_names <- arg_names[arg_names %in% names(formals(aeonia:::aphid_pop))]
    args <- formals(make_insects_ptr)[arg_names]
    args[["name"]] <- "aphid"
    args[["pseudo_surv"]] <- 1.0
    args[[".RETURN_LESLIE"]] <- TRUE
    other <- list(...)
    for (n in names(other)) args[[n]] <- other[[n]]
    args <- lapply(args, eval)
    do.call(aeonia:::aphid_pop, args)
}

# Dominant eigenvalue of a matrix
dom_eigen <- function(L) max(Mod(eigen(L, only.values = TRUE)$values))

# Columns of a data frame that are numeric (not list columns)
num_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]
