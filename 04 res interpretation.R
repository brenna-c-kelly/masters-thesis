

library(tibble)
# coefficient interpretation

summary(pop_tox)


summary(res)



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

fixed_z

odds <- data.frame(fixed_z[, 1], round(exp(fixed_z[5, 2:8]), 3))
prob_z <- data.frame(fixed_z[, 1], round(logit2prob(fixed_z[5, 2:8]), 3))
summary(fixed_z)
logit_seq <- seq(-10, 10, by = 4)
df <- data.frame(Logit = logit_seq,
                 Probability = prob_z)
c(29, 32:39)

fixed
fixed(which())
fixed[which(fixed$coef == "mu_z"), ]

fx_list = vector("list", length = 11)
for(i in c(2:11)) {
  int <- fixed_z[which(fixed_z$coef == "mu_z"), "mean"]
  fx <- fixed_z[i, "mean"]
  coef <- fixed_z[i, 1]
  prop_fx <- exp(int + fx) / (1 + exp(int + fx))
  sig <- ifelse((fixed_z[i, "X0.025quant"] > 0 & fixed_z[i, "X0.975quant"] > 0) |
                  (fixed_z[i, "X0.025quant"] < 0 & fixed_z[i, "X0.975quant"] < 0),
                "sig",
                "not sig")
  dir <- ifelse(sig == "sig" & fixed_z[i, "mean"] > 0, "+", "-")
  df <- data.frame(coef = coef, 
                   prob = prop_fx,
                   sig = sig,
                   dir = dir)
  fx_list[[i]] <- df
}
fx_list = do.call(rbind, fx_list)
fx_list

intx_list = vector("list", length = 17)
for(i in c(12:18)) {
  int <- fixed_z[which(fixed_z$coef == "mu_z"), "mean"]
  intx_name_1 <- substr(fixed_z[i, "coef"], 1, 7)
  intx_name_2 <- substr(fixed_z[i, "coef"], 9, 15)
  #print(intx_name_2)
  intx <- fixed_z[i, "mean"]
  intx_name <- ifelse(intx_name_1 == "x_pov_z", intx_name_2, intx_name_1)
  fx_1 <- fixed_z[which(fixed_z$coef == intx_name), "mean"]
  fx_2 <- fixed_z[which(fixed_z$coef == "x_pov_z"), "mean"]
  
  prop_fx <- exp(int + intx + fx_1 + fx_2) / (1 + exp(int + intx + fx_1 + fx_2))
  sig <- ifelse((fixed_z[i, "X0.025quant"] > 0 & fixed_z[i, "X0.975quant"] > 0) |
                  (fixed_z[i, "X0.025quant"] < 0 & fixed_z[i, "X0.975quant"] < 0),
                "sig",
                "not sig")
  dir <- ifelse(sig == "sig" & fixed_z[i, "mean"] > 0, "+", "-")
  df <- data.frame(coef = fixed_z[i, "coef"], 
                   prob = prop_fx,
                   sig = sig,
                   dir = dir)
  intx_list[[i]] <- df
}
intx_list = do.call(rbind, intx_list)
intx_list

fixed_list <- rbind(fx_list, intx_list)
fixed_list
#fixed_z



# odds
fx_list = vector("list", length = 11)
for(i in c(2:11)) {
  int <- fixed_z[which(fixed_z$coef == "mu_z"), "mean"]
  fx <- fixed_z[i, "mean"]
  coef <- fixed_z[i, 1]
  odds_fx <- exp(int + fx)
  sig <- ifelse((fixed_z[i, "X0.025quant"] > 0 & fixed_z[i, "X0.975quant"] > 0) |
                  (fixed_z[i, "X0.025quant"] < 0 & fixed_z[i, "X0.975quant"] < 0),
                "sig",
                "not sig")
  dir <- ifelse(fixed_z[i, "mean"] > 0, "+", "-")
  df <- data.frame(coef = coef, 
                   odds = odds_fx,
                   sig = sig,
                   dir = dir)
  fx_list[[i]] <- df
}
fx_list = do.call(rbind, fx_list)
fx_list

intx_list = vector("list", length = 17)
for(i in c(12:18)) {
  int <- fixed_z[which(fixed_z$coef == "mu_z"), "mean"]
  intx_name_1 <- substr(fixed_z[i, "coef"], 1, 7)
  #intx_name_2 <- substr(fixed_z[i, "coef"], 9, 15)
  #print(intx_name_2)
  intx <- fixed_z[i, "mean"]
  print(intx)
  fx_1 <- fixed_z[which(fixed_z$coef == intx_name_1), "mean"]
  fx_2 <- fixed_z[which(fixed_z$coef == "x_pov_z"), "mean"]
  
  odds_fx <- exp(int + intx + fx_1 + fx_2)
  sig <- ifelse((fixed_z[i, "X0.025quant"] > 0 & fixed_z[i, "X0.975quant"] > 0) |
                  (fixed_z[i, "X0.025quant"] < 0 & fixed_z[i, "X0.975quant"] < 0),
                "sig",
                "not sig")
  dir <- ifelse(fixed_z[i, "mean"] > 0, "+", "-")
  df <- data.frame(coef = fixed_z[i, "coef"], 
                   odds = odds_fx,
                   sig = sig,
                   dir = dir)
  intx_list[[i]] <- df
}
intx_list = do.call(rbind, intx_list)
intx_list

fixed_list <- rbind(fx_list, intx_list)
fixed_list
round(fixed_z$mean, 2)

fx_list







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

hist(pop_tox$pov_lc)
summary(exp(pop_tox$pov_lc + log(mean(pop_tox$pov))))
summary(pop_tox$pov)
table(pop_tox$pov == 0)

par(mfrow = c(2, 1))
hist(pop_tox$pov)
hist(log(pop_tox$pov))


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
} # convert logit to probability

logit_seq <- seq(-3, 3, by = 1) # range of logit
prob_seq <- round(logit2prob(logit_seq), 3) # apply fx to range
raw <- exp(logit_seq + log(mean(pop_tox$nhpi))) # get variable distribution
df <- data.frame(Logit = logit_seq, # put it all together
                 Probability = prob_seq,
                 Raw = raw,
                 race = "nhpi")
names(df) <-c("Log Unit Change", "AIAN", "Black",
                "Asian", "Other", "Two or More", "Hispanic",
                "NHPI")

df_1 <- cbind(df_1, df_2)
df_1 <- df_1[, c(1, 3, 5, 7, 9, 11, 13, 17)]
names(df_1) <-c("Log Unit Change", "AIAN", "Black",
                "Asian", "Other", "Two or More", "Hispanic",
                "NHPI")
df_1
#df_1 <- df_1[, c(1:4, 7:8, 11:12, 19:20, 23:24, 27:28)]
#pop_tox$
names(df_1) <- c("Log Unit Change", "prob", "AIAN", "tx1", "Black", "tx2",
                 "Asian", "tx3", "Other", "tx4", "Two or More", "tx5", "Hispanic", "tx6")
df_1 <- df_1[, c(1, 3, 5, 7, 9, 11, 13)]

#df_1 <- df[-c(6:35), ]

test <- df %>%
  spread(race, Probability)

summary(pop_tox$other)
res$summary.fixed

head(pop_tox$name)
tops <- pop_tox %>%
  filter(name %in% c("Clay County, Florida",
                     "Salt Lake County, Utah",
                     "Montgomery County, Virginia",
                     "Catawba County, North Carolina",
                     "Harris County, Texas"))
one <- tops %>%
  filter(name == "Harris County, Texas")
sum(one$rsei.score) / sum(pop_tox$rsei.score)
