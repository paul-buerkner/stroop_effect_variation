stroop <- read.csv("stroop.csv")
stroop$ID <- factor(stroop$ID)


# do it with Jeffs function
source("jeffs_functions.R")
stroop$y <- stroop$rt
stroop$sub <- stroop$ID
myF(stroop)


# do it with lme4
library(lme4)
fit_lmm_pp <- lmer(rt ~ congruency + (congruency | ID), data = stroop) 
summary(fit_lmm_pp)
AIC(fit_lmm_pp)

fit_lmm_cp <- lmer(rt ~ congruency + (1 | ID), data = stroop) 
summary(fit_lmm_cp)

# compare both models
anova(fit_lmm_pp, fit_lmm_cp)


# ---------- do it with brms --------------
# requires dev version for pp_check plots
# devtools::install_github("paul-buerkner/brms")
library(brms)

# ---------- normal distribution -----------
prior <- c(set_prior("normal(0, 0.2)", class = "sd", group = "ID",
                     coef = "Intercept"),
           set_prior("normal(0, 0.04)", class = "sd", group = "ID",
                     coef = "congruencyincongruent"),
           set_prior("normal(0, 0.2)", class = "sigma"))

# complete pooling
fit_cp <- brm(rt ~ congruency + (1 | ID), data = stroop,
              rior = prior, sample_prior = TRUE) 
summary(fit_cp)

# partial pooling
fit_pp <- brm(rt ~ congruency + (congruency | ID), data = stroop, 
                    prior = prior, sample_prior = TRUE) 
summary(fit_pp)
ggsave("plots/plot_normal.pdf", width = 9, height = 7,
       plot = plot(fit_pp, N = 6, newpage = FALSE)[[1]])
ggsave("plots/pp_check_normal.pdf", width = 7, height = 7,
       plot = pp_check(fit_pp))

# compare both models
LOO(fit_cp, fit_pp, pointwise = FALSE)

# further analyses
hyp <- paste("sd_ID_congruencyincongruent^2 / (sd_ID_Intercept^2 +",
             "sd_ID_congruencyincongruent^2 + sigma^2) = 0")
(hyp <- hypothesis(fit_pp, hyp, class = NULL))
plot(hyp)

(hyp2 <- hypothesis(fit_pp, "sd_ID_congruencyincongruent > 0.025",
                    class = NULL))
plot(hyp2)

conditions <- data.frame(ID = unique(stroop$ID))
rownames(conditions) <- conditions$ID
me <- marginal_effects(fit_pp, effects = "congruency", 
                       conditions = conditions, re_formula = NULL)
me_plot <- plot(me)[[1]]
ggsave("marginal_effects_ID.pdf", plot = me_plot, height = 12, width = 8,
       useDingbats=FALSE)


# -------- lognormal distribution ----------
# TODO: amend priors for lognormal models?
prior_ln <- c(set_prior("normal(0, 0.2)", class = "sd", group = "ID",
                     coef = "Intercept"),
              set_prior("normal(0, 0.04)", class = "sd", group = "ID",
                     coef = "congruencyincongruent"),
              set_prior("normal(0, 0.2)", class = "sigma"))

# complete pooling
fit_cp_ln <- brm(rt ~ congruency + (1 | ID), 
                 data = stroop, family = lognormal(),
                 prior = prior_ln, sample_prior = TRUE) 
summary(fit_cp_ln)
plot(fit_cp_ln)
pp_check(fit_cp_ln)
marginal_effects(fit_cp_ln)

# partial pooling
fit_pp_ln <- brm(rt ~ congruency + (congruency | ID), 
                 data = stroop, family = lognormal(),
                 prior = prior_ln, sample_prior = TRUE) 
summary(fit_pp_ln)
ggsave("plots/plot_lognormal.pdf", width = 9, height = 7,
       plot = plot(fit_pp_ln, N = 6, newpage = FALSE)[[1]])
ggsave("plots/pp_check_lognormal.pdf", width = 7, height = 7,
       plot = pp_check(fit_pp_ln))
marginal_effects(fit_pp_ln)

# compare both models
LOO(fit_cp_ln, fit_pp_ln, pointwise = FALSE)
