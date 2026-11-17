
#'
#' Virus attraction plots for larger landscape simulations.
#'

source("_scripts/00-preamble.R")

.overwrite <- FALSE



get_args <- function(n_pseudo,
                     wasp_resp,
                     p_load,
                     fly_p,
                     virus_attract,
                     n_sims) {

    if (n_pseudo > 0) {
        landscape <- "_scripts/interm-data/large-landscapes.rds" |>
            read_rds() |>
            filter(n_pseudo == .env$n_pseudo,
                   # uniform, virus starts on uninhabited plant:
                   wt_pp == 1, wt_vp < 1) |>
            getElement("landscape") |>
            getElement(1)
        landscape <- landscape[,,1:n_sims,drop=FALSE]
    } else landscape <- array(c(1L, rep(0L, 9999L)), c(100L, 100L, n_sims))

    zeta <- switch(wasp_resp, weak = 0.1, moderate = 0.5, strong = 0.9)

    args <- list(landscape = landscape,
                 virus_attract = virus_attract,
                 fly_p = fly_p,
                 zeta = zeta,
                 p_load_alate = p_load,
                 p_load_plant = p_load,
                 sd_N = 0,
                 pseudo_repel = 1,
                 Y0 = 250,
                 N0 = 55,
                 K = 12.5e3,
                 pseudo_surv = 0.85,
                 n_sims = dim(landscape)[3])

    return(args)
}


# large landscape simmer, just focusing on mean peak infected plants
large_ninf_simmer <- function(n_pseudo,
                              wasp_resp,
                              p_load,
                              fly_p,
                              virus_attract,
                              n_sims = 12L) {

    # n_pseudo = 0L; wasp_resp = "moderate"; p_load = 1; fly_p = 0.1
    # virus_attract = 1; n_sims = 12L
    # rm(n_pseudo, wasp_resp, p_load, fly_p, virus_attract, n_sims)
    # rm(landscape, zeta, args, sims)

    args <- get_args(n_pseudo, wasp_resp, p_load, fly_p, virus_attract, n_sims)
    args[["summ"]] <- "all"

    sims <- do.call(big_plantscape, args) |>
        select(rep, n_infected)
    return(sims)
}


# Takes ~0.5 min
set.seed(1703275641)
avg_ninf_sims <- c(1, 5, 100) |>
    set_names() |>
    map(\(virus_attract) {
        large_ninf_simmer(n_pseudo = 0L,
                          wasp_resp = "moderate",
                          p_load = 1,
                          fly_p = 0.1,
                          virus_attract = virus_attract,
                          n_sims = 12L) |>
            mutate(virus_attract = .env$virus_attract) |>
            select(virus_attract, everything())
    }, .progress = .prog_args) |>
    list_rbind()

set.seed(1985361637)
avg_ninf_sims |>
    group_by(virus_attract) |>
    summarize(boot = list(booter(n_infected)),
              mean = mean(n_infected)) |>
    mutate(lo = map_dbl(boot, \(x) x[["Lower"]]),
           hi = map_dbl(boot, \(x) x[["Upper"]])) |>
    select(-boot)

#   virus_attract  mean    lo    hi
#           <dbl> <dbl> <dbl> <dbl>
# 1             1 4590. 4136. 5058.
# 2             5 6787. 6339. 7269.
# 3           100 5675. 5017. 6301.





# large landscape simmer, just focusing on infections and incoming alates
# through space
large_inf_disp_simmer <- function(n_pseudo,
                                  wasp_resp,
                                  p_load,
                                  fly_p,
                                  virus_attract,
                                  n_sims = 12L) {

    # n_pseudo = 0L; wasp_resp = "moderate"; p_load = 1; fly_p = 0.1
    # virus_attract = 1; n_sims = 12L
    # rm(n_pseudo, wasp_resp, p_load, fly_p, virus_attract, n_sims)
    # rm(args, sims, sim_summs, .rep)

    args <- get_args(n_pseudo, wasp_resp, p_load, fly_p, virus_attract, n_sims)

    args[["out_dispersals"]] <- "in"
    args[["force_disps"]] <- TRUE
    args[["summ"]] <- "none"
    args[["out_stages"]] <- "two"

    sims <- do.call(big_plantscape, args) |>
        filter(!is.na(x)) |>
        mutate(virus = as.integer(virus),
               aphids = aphids_juv + aphids_adu + alates_juv + alates_adu + parasitized,
               alates = alates_adu) |>
        select(rep, time, x, y, virus, aphids, alates, disps)

    # If >1 rep simulated, choose the one that is closest to the mean:
    if (n_sims > 1) {
        sim_summs <- sims |>
            filter(time == max(time)) |>
            group_by(rep) |>
            summarize(virus = sum(virus), .groups = "drop") |>
            mutate(adiff = abs(virus - mean(virus)))
        # Tie breaker is just the first one:
        .rep <- sim_summs$rep[which(sim_summs$adiff == min(sim_summs$adiff))][1]
        sims <- sims |>
            filter(rep == .rep)
    }

    sims <- sims |> select(-rep)

    return(sims)

}





# Takes ~2 min
set.seed(876826518)
inf_sims <- c(1, 5, 100) |>
    set_names() |>
    map(\(virus_attract) {
        large_inf_disp_simmer(n_pseudo = 0L,
                              wasp_resp = "moderate",
                              p_load = 1,
                              fly_p = 0.1,
                              virus_attract = virus_attract,
                              n_sims = 12L) |>
            mutate(virus_attract = .env$virus_attract)
    }, .progress = .prog_args) |>
    list_rbind()



# Difference in number of infected with virus attraction minus that without.
# Positive numbers mean greater infection with virus attraction.
inf_sims |>
    filter(time == max(time)) |>
    group_by(virus_attract) |>
    summarize(virus = sum(virus), .groups = "drop_last") |>
    summarize(diff = (\(va, vi) {
        .virus_attract <- va[va > 1]
        .d_virus <- vi[va > 1] - vi[va == 1]
        list(tibble(virus_attract = .virus_attract,
                    virus = vi[va > 1],
                    d_virus = .d_virus))
    })(virus_attract, virus)) |>
    unnest(diff)

#   virus_attract virus d_virus
#           <dbl> <int>   <int>
# 1             5  6635    2229
# 2           100  5926    1520





MAX_ALATES <- inf_sims |>
    group_by(virus_attract, time) |>
    summarize(alates = sum(alates), .groups = "drop") |>
    getElement("alates") |> max()





infection_plotter <- function(va_vals) {

    # va_vals = c(1, 100)
    # rm(va_vals, max_disps, max_bot, f, g, d_virus_df, inf_t)
    # rm(.title, spat_p, time_p)

    max_disps <- inf_sims |>
        filter(virus_attract %in% va_vals) |>
        getElement("disps") |>
        max()
    max_bot <- inf_sims |>
        filter(virus_attract %in% va_vals) |>
        group_by(virus_attract, time) |>
        summarize(virus = sum(virus),
                  alates = sum(alates),
                  .groups = "drop") |>
        mutate(d_virus = virus - lag(virus)) |>
        summarize(across(alates:d_virus, \(x) max(x, na.rm = TRUE))) |>
        as.list()
    # alates --> d_virus scale
    f <- function(x) x / max_bot$alates * max_bot$d_virus
    # d_virus --> alates scale
    g <- function(x) x / max_bot$d_virus * max_bot$alates

    plot_list <- map(va_vals, \(va) {
        d_virus_df <- inf_sims |>
            filter(virus_attract == va) |>
            select(-virus_attract) |>
            group_by(time) |>
            summarize(virus = sum(virus),
                      alates = sum(alates),
                      .groups = "drop") |>
            mutate(d_virus = virus - lag(virus),
                   alates = f(alates))
        # top three times for max increase in new infected plants
        inf_t <- d_virus_df |>
            mutate(dd_virus = d_virus - lag(d_virus)) |>
            arrange(desc(dd_virus)) |>
            head(n = 3) |>
            getElement("time") |>
            sort()
        .title <- serify("No virus attraction (", "<i>&nu;</i> = 1", ")")
        if (va > 1) {
            .title <- .title |>
                str_replace("No virus", "Virus") |>
                str_replace("= 1", paste("=", va))
        }
        spat_p <- inf_sims |>
            filter(virus_attract == va) |>
            filter(virus == 1) |>
            filter(time %in% (inf_t-7L)) |>
            arrange(disps) |>
            ggplot(aes(x, y)) +
            geom_point(aes(size = disps, color = disps)) +
            scale_y_reverse() +
            scale_size_area("Incoming<br>alates",
                            limits = c(0, max_disps),
                            max_size = 2) +
            scale_color_viridis_c("Incoming<br>alates",
                                  limits = c(0, max_disps),
                                  option = "plasma", direction = -1,
                                  end = 0.9,
                                  guide = guide_colourbar(theme = theme(
                                      # legend.key.size = unit(12, "in"),
                                      legend.key.height = unit(1.2, "in"),
                                      legend.key.width  = unit(0.24, "in")
                                  ))) +
            facet_wrap(~ time, labeller = label_both, nrow = 1, axes = "all") +
            coord_equal(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
            labs(title = .title) +
            # This fixes a very annoying bug where using
            # `legend.text = ggtext::element_markdown(...)` makes the legend
            # really large and doesn't allow me to change its size
            replace_theme(theme_get(),
                          legend.text = element_text(color = "black", size = 9)) +
            theme(# legend.position = "top",
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                plot.margin = margin(0,0,0,0))
        # spat_p
        time_p <- d_virus_df |>
            ggplot(aes(time, d_virus)) +
            geom_hline(yintercept = 0, color = "gray70") +
            # geom_area(aes(y = alates), fill = "dodgerblue", alpha = 0.5) +
            geom_line(aes(y = alates), color = "dodgerblue", linewidth = 1) +
            geom_line(linewidth = 1, na.rm = TRUE) +
            # geom_vline(xintercept = inf_t -7L, linetype = "solid", color = "gray80",
            #            linewidth = 0.75) +
            geom_vline(xintercept = inf_t, linetype = "22", color = "gray60",
                       linewidth = 0.75) +
            scale_y_continuous(sec.axis = sec_axis(g, "Adult alates")) +
            coord_cartesian(xlim = c(0, 100), ylim = c(0, max_bot$d_virus)) +
            labs(x = "Time (days)", y = "New infected plants") +
            theme(plot.margin = margin(0,0,0,0),
                  axis.title.y.right = element_markdown(color = "dodgerblue"),
                  axis.text.y.right = element_markdown(color = "dodgerblue"),
                  axis.ticks.y.right = element_line(color = "dodgerblue"))
        # time_p
        if (va > 1) {
            time_p <- time_p + theme(axis.text.y = element_blank())
        }
        if (va <= 1) {
            time_p <- time_p + theme(axis.text.y.right = element_blank())
        }
        # spat_p / time_p
        return(list(spat_p, time_p))
    }) |>
        do.call(what = c)

    return(plot_list)

}




inf_p <- infection_plotter(c(1, 5)) |>
    base::`[`(c(1,3,2,4)) |>
    wrap_plots(design = "A#B\nC#D", axis_titles = "collect", guides = "collect",
               widths = c(1, 0.05, 1)) +
    plot_annotation(tag_levels = "A")

#
# NOTE: check to make sure the legends look okay (not weirdly large).
# Creating `inf_p` again fixes this.
#
# inf_p




if (.overwrite || !file.exists("_plots/supp/virus-attract.pdf")) {
    save_plot("_plots/supp/virus-attract.pdf", inf_p, width = 7.5, height = 4)
}

