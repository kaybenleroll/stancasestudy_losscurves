
get_character_index <- function(x) match(x, sort(unique(x)))


read_claim_datafile <- function(path, progress = FALSE) {
    lob <- path %>% basename
    lob <- gsub("_pos\\.csv", "", lob)

    data_tbl <- read_csv(path, progress = progress)

    col_names <- data_tbl %>% names %>% tolower
    col_names <- gsub("_..?", "", col_names)

    names(data_tbl) <- col_names

    data_tbl <- data_tbl %>% mutate(lob = lob)

    return(data_tbl)
}


create_stanfit <- function(stan_model, usedata_tbl, model_id) {
    cohort_maxtime <- usedata_tbl %>%
        group_by(acc_year) %>%
        summarise(maxtime = max(dev_lag)) %>%
        arrange(acc_year) %>%
        .[[2]]

    cohort_premium <- usedata_tbl %>%
        group_by(acc_year) %>%
        summarise(premium = unique(premium)) %>%
        .[[2]]

    t_values <- usedata_tbl %>%
        select(dev_lag) %>%
        arrange(dev_lag) %>%
        unique %>%
        .[[1]]

    standata_lst <- list(
        growthmodel_id = 1   # Use weibull rather than loglogistic
       ,n_data         = usedata_tbl %>% nrow
       ,n_time         = usedata_tbl %>% select(dev_lag)  %>% unique %>% nrow
       ,n_cohort       = usedata_tbl %>% select(acc_year) %>% unique %>% nrow
       ,cohort_id      = get_character_index(usedata_tbl$acc_year)
       ,cohort_maxtime = cohort_maxtime
       ,t_value        = t_values
       ,t_idx          = get_character_index(usedata_tbl$dev_lag)
       ,premium        = cohort_premium
       ,loss           = usedata_tbl$cum_loss
    )


    lc_1_draws       <- extract(lc_1_stanfit, permuted = FALSE, inc_warmup = TRUE)
    lc_1_monitor_tbl <- as.data.frame(monitor(lc_1_draws, print = FALSE))
    lc_1_monitor_tbl <- lc_1_monitor_tbl %>%
        mutate(variable  = rownames(lc_1_monitor_tbl)
              ,parameter = gsub("\\[.*]", "", variable)
               )

    convergence_plot <- ggplot(lc_1_monitor_tbl) +
        aes(x = parameter, y = Rhat, color = parameter) +
        geom_jitter(height = 0, width = 0.2, show.legend = FALSE) +
        geom_hline(aes(yintercept = 1), size = 0.5) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        ylab(expression(hat(italic(R))))


    neff_plot <- ggplot(lc_1_monitor_tbl) +
        aes(x = parameter, y = n_eff, color = parameter) +
        geom_jitter(height = 0, width = 0.2, show.legend = FALSE) +
        expand_limits(y = 0) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        xlab("Parameter") +
        ylab(paste0("Effective Sample Count (n_eff)"))

    param_root <- c("omega", "theta", "LR", "gf", "loss_sd")

    use_vars <- lc_1_monitor_tbl %>%
        filter(parameter %in% param_root) %>%
        .[["variable"]]

    trace_1_plot <- rstan::traceplot(lc_1_stanfit, pars = c("omega", "theta", "LR")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

    trace_2_plot <- rstan::traceplot(lc_1_stanfit, pars = c("gf", "loss_sd")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

    plotdata_tbl <- lc_1_monitor_tbl %>%
        filter(variable %in% use_vars) %>%
        select(mean, `25%`, `50%`, `75%`) %>%
        mutate(variable = factor(use_vars, levels = use_vars))

    expectation_plot <- ggplot(plotdata_tbl) +
        geom_point(aes(x = variable, y = mean)) +
        geom_errorbar(aes(x = variable, ymin = `25%`, ymax = `75%`), width = 0) +
        expand_limits(y = 0) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        xlab("Parameter") +
        ylab("Value")


}
