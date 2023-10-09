######################################################################
## Supplemental code                                                ##
## Intermediate disturbance from swidden.                           ##
## farming increases tree diversity in Indigenous Maya 	            ##
## forests" by Downey et al. Compiled on 9/8/2022.                  ##
## Sean S. Downey*, Matt Walker, Jacob Moschler, Filiberto Penados, ##
## Rongjun Qin, William Peterman, Shane A. Scaggs, Suang Song.      ##
## (Accepted, 2023). Intermediate swidden disturbance increases     ##
## spectral diversity in Maya community forests. Communications     ##
## Earth and Environment.                                           ##
######################################################################


#load packages
library(openxlsx)
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(patchwork)
library(modelr)
library(scales)
library(plyr)
library(dplyr)

# rstan options
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
cstats <- read.xlsx(xlsxFile = "swidden.xlsx", sheet = 1, skipEmptyRows = TRUE)
d <- as.data.frame(read.xlsx(xlsxFile = "swidden.xlsx", sheet = 2, skipEmptyRows = TRUE))
colnames(d) <- c("prid","ed","ed_z","shdi","shdi_exp","pland.pasture","ppast_z","village","pland.pf","ppf_z","dvill","droad")
d <- d  %>% mutate_at(c('ed_z', 'ppast_z','ppf_z'), as.numeric)
W <- as.matrix(read.xlsx(xlsxFile = "swidden.xlsx", sheet = 3, colNames = F, skipEmptyRows = TRUE))
ss <- read.xlsx(xlsxFile = "swidden.xlsx", sheet = 4, skipEmptyRows = TRUE)
colnames(ss) <- c("class","sid","count")

# Helper function for projecting scaled data
proj_unscale <- function(scl_dat, orig_dat){
    unscl_dat <- (scl_dat * sd(orig_dat)) + mean(orig_dat)
    return(unscl_dat)
}

# A theme for PNAS draft 
PNAS_xy <- theme(
    # plot panels and backgrounds
    plot.background = element_rect(fill='white', color=NA), 
    panel.border = element_rect(fill = NA, color = 'black', size=2),
    panel.background = element_rect(fill='grey97',color='black',size=2), 
    panel.grid = element_blank(), 
    # axes
    axis.ticks = element_line(size=1.25, color='black'), 
    axis.ticks.length = unit(0.15, 'in'), 
    axis.text = element_text(family='sans',size=11, color='black'), 
    # facet strips 
    strip.background = element_rect(color='black',fill='black',size=2),
    strip.text = element_text(color='#ccee00', size=11), 
    # legend
    legend.key = element_rect(fill='white'), 
    # general text
    text = element_text(size = 12)
)

# Figure 1 ----

# plot land classes
p1.B1 <- cstats %>% 
    filter(class < 8) %>% 
    filter(metric == 'area_cv') %>%
    ggplot() + PNAS_xy + 
    geom_bar(aes(y=value, 
                 x=factor(class,c(7,1:6), labels=c("P","C","1","2-4","5-11","12-19","20+")), 
                 fill=factor(class,c(7,1:6), labels=c("P","C","1","2-4","5-11","12-19","20+"))),
             color = 'black', stat = 'identity', lwd=0.33, width=0.6) +
    scale_y_continuous(breaks = c(0,450,900)) + 
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = scico::scico(7, palette='tokyo', begin=1, end=0)) + 
    scale_color_manual(values = scico::scico(7, palette='tokyo', begin=1, end=0)) + 
    labs(x=NULL, y="Patch area coef.\nof variation (ha)", fill='Land\nclass', color='Land\nclass') + 
    theme(legend.position = 'none')

p1.B2 <- cstats %>% 
    filter(class < 8) %>% 
    filter(metric == 'area_mn') %>%
    ggplot() + PNAS_xy + 
    geom_bar(aes(y=value, 
                 x=factor(class,c(7,1:6), labels=c("P","C","1","2-4","5-11","12-19","20+")), 
                 fill=factor(class,c(7,1:6), labels=c("P","C","1","2-4","5-11","12-19","20+"))),
             color = 'black', stat = 'identity', lwd=0.33, width=0.6) +
    scale_y_continuous(breaks = c(0,15,30)) + 
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = scico::scico(7, palette='tokyo', begin=1, end=0)) + 
    scale_color_manual(values = scico::scico(7, palette='tokyo', begin=1, end=0)) + 
    labs(x=NULL, y="Patch area\nmean (ha)", fill='Land\nclass', color='Land\nclass') + 
    theme(legend.position = 'none')

p1.B3 <- cstats %>% 
    filter(class < 8) %>% 
    filter(metric == 'ed') %>%
    ggplot() + PNAS_xy + 
    geom_bar(aes(y=value, 
                 x=factor(class,c(7,1:6), labels=c("P","C","1","2-4","5-11","12-19","20+")), 
                 fill=factor(class,c(7,1:6), labels=c("P","C","1","2-4","5-11","12-19","20+"))),
             color = 'black', stat = 'identity', lwd=0.33, width=0.6) +
    scale_y_continuous(breaks = c(0,20,40)) + 
    #scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = scico::scico(7, palette='tokyo', begin=1, end=0)) + 
    scale_color_manual(values = scico::scico(7, palette='tokyo', begin=1, end=0)) + 
    labs(x='Land class', y="Edge density\n(m/ha)", fill='Land\nclass', color='Land\nclass') + 
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1)) + 
    theme(legend.position = 'none')

# plot spectral species composition for each land class
melt_ss <- plyr::ddply(ss,plyr::.(class))%>% 
    group_by(class) %>% 
    dplyr::mutate(rescale = scale((count / sum(count))) )%>% 
    ungroup() %>% group_by(sid) %>% 
    dplyr::mutate(rescale=scale(rescale)) #rescales values within and between land classes

p1.C <-ggplot(melt_ss)+
    geom_tile(aes(
        y=factor(sid,20:1),
        x=factor(class,c("P","C","1","2-4","5-11","12-19","20+")),
        fill=rescale,))+
    xlab("Land class")+
    ylab("Spectral species identification number") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1)) +
    scale_fill_gradientn(
        "Relative\nabundance",
        colors=c('#AD003F','#BB440C','#D2A52E','#86DE67','#00C994'),
    )+
    PNAS_xy
# Final Figure 1
(p1.B1 / p1.B2 / p1.B3) | p1.C +  plot_annotation(tag_levels = 'A')

# Figure 2 ----

# FIGURE 2: Model effect of distance on edge density as a hurdle-gamma process 
d$dvill.s <- scale(d$dvill) # scales values
d$dist <- pmin(d$droad, d$dvill) # calculates min of dist from road and village
d$dist.s <- scale(d$dist)

# model formula 
form <- bf( ed ~ dist.s, 
            shape ~ dist.s
)

# model fit
fit <- brm(form, data = d, family = hurdle_gamma())

# model predictions
nd <- tibble(dist.s = seq(min(d$dist.s)-0.1, max(d$dist.s)+0.1, length=1000))
mu_summary <- 
    fitted(fit, newdata = nd, probs = c(0.055,0.945)) %>%
    as_tibble() %>%
    bind_cols(nd)

preds <- 
    predict(fit, newdata = nd, probs = c(0.055,0.945)) %>%
    as_tibble() %>%
    bind_cols(nd)

labs = round(proj_unscale(c( -1.35823228,0,1.35823228), d$dist))

# plot ed as a function of dist to village or road
p2 <- d %>%
    #mutate(f = '89% fitted and\npredicted intervals') %>%
    ggplot(aes(x=dist.s)) + PNAS_xy + 
    geom_ribbon(data=preds, 
                aes(ymin=Q5.5, ymax=Q94.5), 
                fill='black', alpha=0.2) + 
    geom_smooth(data=mu_summary, 
                aes(y=Estimate, ymin=Q5.5, ymax=Q94.5), 
                stat='identity', 
                fill='black', color='#3300ff', alpha=0.2) + 
    geom_point(aes(y=ed), pch=21) + 
    geom_rug(aes(y=ed),alpha=0.5) + 
    scale_x_continuous(breaks = c( -1.35823228,0,1.35823228), 
                       #expand = c(0.005,0.005), 
                       labels = round(labs)) + 
    xlab("Distance to nearest village/road (m)") + 
    ylab("Edge density (m/ha)") 

# Final Figure 2
p2

# Figure 3 ----

# Fitting tightened car model (full model)
# Note: adjust parameters to suit specs of your machine

# standardized by maximum possible edge value 
d$ppast_std = d$pland.pasture / 100
d$ppf_std = d$pland.pf / 100


# model formula
form = bf(shdi_exp ~ 1 + ed_z + I(ed_z^2) + ppast_std  + ppf_std + (ed_z + I(ed_z^2) + ppast_std + ppf_std | village ) + car(W,gr=prid,type='icar'))

# priors  
my_prior = c( prior(student_t(3, 14.4, 2.5), class = Intercept),
              prior(normal(0, 5), class = b, coef = ed_z),
              prior(normal(0, 5), class = b, coef = Ied_zE2),
              prior(normal(0, 5), class = b, coef = ppast_std),
              prior(normal(0, 5), class = b, coef = ppf_std),
              prior(exponential(2), class = sd),
              prior(lkj(1), class = cor) )

b.car <- 
    brm(formula = form,  
        data    = d, 
        data2   = list(W=W), 
        family  = gaussian,
        prior   = my_prior,
        iter    = 5000, 
        warmup  = 3500,
        thin    = 1,
        chains  = 2, 
        cores   = 2,
        control = list(adapt_delta = 0.999, max_treedepth = 15),
        save_warmup = FALSE, 
        seed = 42 ) 

#slow (~10min)
#Use these for testing:  iter=5000,  warmup=3500, chains/cores = 2; expect low bulk and total ESS and low BFI
#use these to reproduce: iter=14000, warmup=7000, chains/cores = 4

# Basic diagnostics (as reported in Supplemental Information)
# stat2d
pp_check(b.car, 'stat_2d', ndraws = 500) + PNAS_xy + ggtitle('Tightened CAR')
# density overlay
pp_check(b.car, 'dens_overlay', ndraws = 200) + PNAS_xy + ggtitle('Tightened CAR')

# Plot predictions from tight CAR model 
v_labs <- c(`1` = 'Village A', 
            `2` = 'Village B')  # facet labels 

# Compare curves between two village A and B 
d2 = d

vp = d %>%
    data_grid( village   = village, 
               prid      = prid,
               ppast_std = mean(d$ppast_std),
               ppf_std   = mean(d$ppf_std), 
               ed_z      = seq(min(d$ed_z)-0.1, max(d$ed_z)+0.1, length=12)) %>%
    add_predicted_draws(b.car, ndraws = 10) %>% # slow; set to 10 for testing, but used 2,000 for production runs
    ggplot() + PNAS_xy + 
    stat_lineribbon(aes(x = ed_z, y = .prediction), 
                    color='#3300ff', .width = c(0.99,0.95,0.8,0.5)) + 
    geom_point(data = d2, aes(d2$ed_z, d2$shdi_exp), pch=21, size=1.2) +
    geom_rug(data = d2, aes(d2$ed_z, d2$shdi_exp), alpha=0.5) +
    facet_wrap(~village, labeller = as_labeller(v_labs), ncol=2) + 
    scale_fill_grey(start = 0.85, end = 0.45) + 
    scale_x_continuous(limits = c(min(d$ed_z)-0.1, max(d$ed_z)+0.1), 
                       expand = c(0.05,0.0005), 
                       labels = c(0,82,164), 
                       breaks = c(-1,0,1)) + 
    scale_y_continuous(limits = c(4,20), 
                       breaks = c(0,6,12,18)) + 
    labs(x = 'Edge density (m/ha)', 
         y = 'Spectral species diversity', 
         fill  = 'Credible\nintervals', 
         color = 'Credible\nintervals')
vp


