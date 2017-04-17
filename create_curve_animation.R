library(tidyverse)
#library(gganimate)


weibull_func     <- function(t, om, th) t^om / (t^om + th^om)
loglogistic_func <- function(t, om, th) 1 - exp(-(t/th)^om)

t_seq <- seq(0, 15, by = 0.01)
om_seq <- seq(0.1, 2.5, by = 0.1)
th_seq <- seq(0.1, 6, by=1)

weibull_tbl <- expand.grid(
    label = 'Weibull'
    ,t    = t_seq
    ,om   = om_seq
    ,th   = th_seq
) %>% mutate(value = weibull_func(t, om, th))

loglogistic_tbl <- expand.grid(
    label = 'Log-logistic'
   ,t     = t_seq
   ,om    = om_seq
   ,th    = th_seq
) %>% mutate(value = loglogistic_func(t, om, th))


plot_tbl <- bind_rows(weibull_tbl, loglogistic_tbl)


p < -ggplot(plot_tbl) +
    geom_line(aes(x = t, y = value, colour = label, frame = om)) +
    xlab(expression(t)) +
    facet_wrap(~th, ncol = 3)

gganimate(p, interval = .2, "anim.gif")
