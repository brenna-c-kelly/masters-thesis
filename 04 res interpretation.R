

library(tibble)
# coefficient interpretation


res



density(pop_tox$black)

log(summary(pop_tox$black + 0.001))


summary(pop_tox$black)
exp(2.19 + 1) - exp(2.19)

(2.19 + 1) / (1 + 2.19 + 1)

1.116 / (1 + 1.116)
4.009 / (1 + 4.009)

pop_tox[which(pop_tox$geoid == "42001" & pop_tox$year == 2012), ]

101352

exp(-0.0001104008 + log(mean(pop_tox$total)))

4.009 / (1 + 4.009)



logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(res$summary.fixed)

install.packages("inlabru")
sum(predict(res, type = "prob")[,1])


logit_seq <- seq(-10, 10, by = 2)
prob_seq <- round(logit2prob(logit_seq), 3)

df <- data.frame(Logit = logit_seq,
                 Probability = prob_seq)

ggplot(df) +
  aes(x = logit_seq, y = prob_seq) +
  geom_point(size = 2, alpha = 1) +
  labs(x = "logit", y = "probability of success")


exp(-4.455150099) / (1 + exp(-4.455150099))
exp(4.524385452) / (1 + exp(4.524385452))
exp(4.524385452 + -4.455150099) / (1 + exp(4.524385452 + -4.455150099))

fixed <- data.frame(res$summary.fixed)

fixed <- tibble::rownames_to_column(fixed, "coef")

fixed_z <- df %>%
  filter(grepl("z", coef))
fixed_y <- df %>%
  filter(grepl("y", coef))

odds <- data.frame(fixed_z[, 1], round(exp(fixed_z[, 2:8]), 3))
prob_z <- data.frame(fixed_z[, 1], round(logit2prob(fixed_z[, 2:8]), 3))
summary(fixed_z)
logit_seq <- seq(-5, 5, by = 1)
df <- data.frame(Logit = logit_seq,
                 Probability = prob_z[1])
c(29, 32:39)

fixed
fixed(which())
fixed[which(fixed$coef == "mu_z"), ]


for(i in c(2:18)) {
  int <- fixed_z[which(fixed_z$coef == "mu_z"), "mean"]
  fx <- fixed_z[i, "mean"]
  coef <- fixed_z[i, 1]
  prop_fx <- exp(int + fx) / (1 + exp(int + fx))
  df <- data.frame(coef = coef, prob= prop_fx)
  print(df)
}

exp(-4.455150099 + 4.008750278) / (1 + exp(-4.455150099 + 4.008750278))

-4.731872223 / (1 + -4.731872223)

for(i in c(29, 32:39)) {
  summary(exp(logit2prob(pop_tox[i]))
}

for(i in c(6:15)) {
  exp(pop_tox[i])
  print(i)
  print(head(pop_tox[i]))
}

raw_z <- exp(logit_seq + log(mean(pop_tox[, 6:15]),))

summary(pop_tox$pov_lc)
summary(exp(pop_tox$pov_lc + log(mean(pop_tox$pov))))
summary(pop_tox$pov)


logit_seq <- seq(-8, 5, by = 1)
prob_seq <- round(logit2prob(logit_seq), 3)
raw <- exp(logit_seq + log(mean(pop_tox$total)))
df <- data.frame(Logit = logit_seq,
                 Probability = prob_seq,
                 Raw = raw)
df
res$summary.fixed

