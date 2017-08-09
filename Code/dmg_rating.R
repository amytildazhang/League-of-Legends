#Riot's approach:
# - last 10 weeks of data
# - minutes spent around 1+ opponent
# - champions that have at least 5 games (w/o player of interest)
# - adjusted for dmg in wins
# - they used dmg/min
# - reworks




library(here)
library(tidyverse)
library(readxl)
library(stringr)
library(forcats)
library(magrittr)

#---------------------------------------------------Read in data
matchdata <- here("Data/") %>% 
  paste0(list.files(.)) %>%
  map_df(read_xlsx, col_types=c(rep("text", 4), "date", 
                                rep("text", 14),
                                rep("numeric", 72))) %>%
  filter(player != "Team") %>%
  select(gameid, league, split, week, game, team, side, player, 
         position, champion, gamelength, result, k, a, teamkills, 
         firedrakes, dmgtochamps, dmgtochampsperminute,
         earnedgpm, goldspent, totalgold, dmgshare) %>%
  filter(!(week %in% c("QF", "T", "F", "R1", "R2", 
                                 "R3", "RR", "SF", "3P"))) %>%
  mutate(week=week %>% as.numeric) %>%
  filter(split=="2017-2" | week >= 9) %>%
  filter(!is.na(dmgtochamps)) %>%
  mutate(otherkills=teamkills-k-a,
         killshare=ifelse(teamkills==0, 0, k/teamkills),
         otherkillshare=ifelse(teamkills==0, 0, otherkills/teamkills))

savefolder <- here("Output/dmg_rating/")


#---------------------------------------------------filter champions
#excluded champions
excluded_champs <- matchdata %>% group_by(position, player, champion) %>%
  summarise(n_games=n()) %>%
  group_by(position, champion) %>%
  summarise(n_total=sum(n_games),
            max_by_one=max(n_games),
            min_leftover=n_total-max_by_one) %>%
  filter(min_leftover < 12) #this is 0??

matchdata <- matchdata %>% 
  group_by(position, player, champion) %>%
  summarise(n_games=n()) %>%
  group_by(position, champion) %>%
  summarise(n_total=sum(n_games),
            max_by_one=max(n_games),
            min_leftover=n_total-max_by_one) %>%
  filter(min_leftover >= 12) %>%
  inner_join(matchdata)

matchdata <- matchdata %>% group_by(player) %>%
  summarise(p_games=n()) %>%
  right_join(matchdata)



#---------------------------------------------------EDA
base <- ggplot(matchdata, aes(y=dmgtochamps)) +
  facet_grid(. ~ position, scales='free')

base + geom_boxplot(aes(x=champion))

base + geom_boxplot(aes(x=league)) 

base + geom_boxplot(aes(x=factor(firedrakes))) 

base + geom_boxplot(aes(x=side)) 

base + geom_point(aes(x=gamelength))

base + geom_boxplot(aes(x=factor(result)))

base + geom_point(aes(x=goldspent))

base + geom_point(aes(x=teamkills))

base + geom_point(aes(x=dmgshare))

base + geom_point(aes(x=killshare))


ggplot(matchdata, aes(y=gamelength, x=goldspent)) +
  facet_grid(. ~ position, scales='free') +
  geom_point()

ggplot(matchdata, aes(y=gamelength, x=league)) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()

ggplot(matchdata, aes(y=goldspent, x=factor(firedrakes))) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()

ggplot(matchdata, aes(y=goldspent, x=factor(result))) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()

ggplot(matchdata, aes(y=teamkills, x=factor(result))) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()

ggplot(matchdata, aes(y=dmgshare, x=factor(result))) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()

ggplot(matchdata, aes(y=killshare, x=factor(result))) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()

ggplot(matchdata, aes(y=k, x=factor(result))) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()


ggplot(matchdata, aes(y=otherkills, x=factor(result))) +
  facet_grid(. ~ position, scales='free') +
  geom_boxplot()

#---------------------------------------------------analysis
#by position: top
mse_pred <- function(pred, actual){
  (pred - actual)^2 %>% sum %>% `/`(sum(test_idx))
}



formulas <- c(
  dmgtochamps ~ champion + goldspent,
  sqrt(dmgtochamps) ~ champion + goldspent,
  sqrt(dmgtochamps) ~ champion + log(goldspent)
)

top <- matchdata %>% filter(position=="Top")

#cross-validation bc, although total dataset is large,
#individual champions don't necessarily have much data
set.seed(710)
blocks <- sample.int(nrow(top), nrow(top))
b_size <- nrow(top)/10
b_size
cutoffs <- c(1, seq(from=200, by=200, length.out=9), nrow(top))




cv_models <- function(formulas, cutoffs, data){
  
  
  for (i in 1:(length(cutoffs) - 1)) {
    test_idx <- 1:nrow(data) %in% (cutoffs[i] + 1):cutoffs[i+1]
    train_df <- data[!test_idx,]
    test_df <- data[test_idx,]
    
    for (model in formulas){
      fit <- lm(model, data=train_df)
      
    }

  }
}

#Nvm too lazy for cross-validation


#gold adjustment
goldfit <- lm(sqrt(dmgtochamps) ~ champion + log(goldspent), data=sup)
boxplot(goldfit$residuals ~ sup$champion)
plot(goldfit$residuals ~ sup$goldspent)
abline(h=0)


#see if it seems likely we need to include gamelength and/or result
plot(goldfit$residuals ~ sup$gamelength)
abline(h=0) #not likely

boxplot(goldfit$residuals ~ factor(sup$result)) #maybe possibly slightly

plot(goldfit$residuals ~ sup$teamkills)



#try including result
result_gf <- lm(sqrt(dmgtochamps) ~ champion + log(goldspent) + result,
            data=sup)
plot(result_gf)

boxplot(result_gf$residuals ~ sup$champion)
boxplot(result_gf$residuals ~ factor(sup$result)) 
plot(result_gf$residuals ~ sup$goldspent)
abline(h=0)

summary(result_gf) #
plot(result_gf$residuals ~ sup$teamkills) #still informative



#teamkills
tk_gf <- lm(sqrt(dmgtochamps) ~ champion + log(goldspent) + sqrt(teamkills),
            data=sup)

plot(tk_gf)
summary(tk_gf)

boxplot(tk_gf$residuals ~ sup$champion)
boxplot(tk_gf$residuals ~ factor(sup$result)) 
boxplot(tk_gf$residuals ~ factor(sup$firedrakes))

plot(tk_gf$residuals ~ sup$goldspent)
abline(h=0)
plot(tk_gf$residuals ~ sqrt(sup$teamkills))
abline(h=0)




#otherkills
fd_gf <- lm(sqrt(dmgtochamps) ~ champion + log(goldspent) + log(otherkills + .1),
            data=sup)

plot(fd_gf)
summary(fd_gf)

boxplot(fd_gf$residuals ~ sup$champion)
boxplot(fd_gf$residuals ~ factor(sup$result)) 
boxplot(fd_gf$residuals ~ factor(sup$firedrakes))


plot(fd_gf$residuals ~ sup$goldspent)
abline(h=0)
plot(fd_gf$residuals ~ log(sup$otherkills + 0.1))
abline(h=0)


models <- c(sqrt(dmgtochamps) ~ champion + log(goldspent) + log(otherkills + .1),
            sqrt(dmgtochamps) ~ champion + log(goldspent),
            sqrt(dmgtochamps) ~ champion + log(goldspent) + result,
            sqrt(dmgtochamps) ~ champion + log(goldspent) + league,
            sqrt(dmgtochamps) ~ champion*side + log(goldspent) + league)


#compare prediction
set.seed(710)
train_idx <- sample.int(nrow(sup), floor(0.7*nrow(sup)))
test_idx <- setdiff(1:nrow(sup), train_idx)

supfit1 <- lm(models[[1]], data=sup[train_idx,])
supfit2 <- lm(models[[2]], data=sup[train_idx,])

pred1 <- predict(supfit1, newdata=sup[test_idx,])
pred2 <- predict(supfit2, newdata=sup[test_idx,])

mse_pred(pred1, sup[test_idx,]$dmgtochamps)
mse_pred(pred2, sup[test_idx,]$dmgtochamps)


fit <- lm(models[[1]], sup)
eval_fit(fit, sup)


fit <- lm(models[[2]], sup)
eval_fit(fit, sup)

fit <- lm(models[[4]], sup)
eval_fit(fit, sup)


fit <- lm(models[[5]], sup)
eval_fit(fit, sup)

#---------------------------------------------------analysis
#by position: JUNGLE

eval_fit <- function(fit, dataset){
  par(mfrow=c(2,2))
  plot(fit, ask=F)
  a <- dataset$otherkills/dataset$teamkills
  par(mfrow=c(1,1))
  boxplot(fit$residuals ~ dataset$champion, main="Resid vs champion")
  boxplot(fit$residuals ~ factor(dataset$result), main="Resid vs result") 
  boxplot(fit$residuals ~ factor(dataset$firedrakes), main="Resid vs firedrakes")
  # boxplot(fit$residuals ~ factor(dataset$league), main="Resid vs league")
  # boxplot(fit$residuals ~ factor(dataset$side), main="Resid vs side")
  
  plot(fit$residuals ~ log(dataset$gamelength), main="Resid vs log(gamelength)")
  abline(h=0)
  plot(fit$residuals ~ dataset$goldspent, main="Resid vs goldspent")
  abline(h=0)
  plot(fit$residuals ~ log(dataset$otherkills + 0.1), main="Resid vs log(otherkills)")
  abline(h=0)
  plot(fit$residuals ~ log(dataset$teamkills + 0.1), main="Resid vs log(teamkills)")
  abline(h=0)
  plot(fit$residuals ~ dataset$killshare, main="Resid vs killshare")
  abline(h=0)
  summary(fit)
}

compare_pred <- function(model, dataset, actual){
  set.seed(710)
  train_idx <- sample.int(nrow(dataset), floor(0.7*nrow(dataset)))
  test_idx <- setdiff(1:nrow(dataset), train_idx)

  fit1 <- lm(model, data=dataset[train_idx,])
  pred1 <- predict(fit1, newdata=dataset[test_idx,])
  
  paste0(mse_pred(pred1, actual[test_idx])) %>% print
  }

jungle <- matchdata %>% filter(position=="Jungle")
jungfit1 <- lm(models[[1]], jungle)
eval_fit(jungfit1, jungle)
summary(jungfit1)

jungfit2 <- lm(models[[2]], jungle)
eval_fit(jungfit2, jungle)

compare_pred(models, jungle)



#---------------------------------------------------analysis
#by position: MID

mid <- matchdata %>% filter(position=="Middle")
fit1 <- lm(models[[1]], mid)
eval_fit(fit1, mid)

fit2 <- lm(models[[2]], mid)
eval_fit(fit2, mid)

fit2 <- lm(models[[3]], mid)
eval_fit(fit2, mid)

compare_pred(models, mid)


#---------------------------------------------------analysis
#by position: ADC

adc <- matchdata %>% filter(position=="ADC")
fit1 <- lm(models[[1]], adc)
eval_fit(fit1, adc)

fit2 <- lm(models[[2]], adc)
eval_fit(fit2, adc)

fit2 <- lm(models[[3]], adc)
eval_fit(fit2, adc)

compare_pred(models, adc)



#---------------------------------------------------analysis
#by position: Support

sup <- matchdata %>% filter(position=="Support")
fit1 <- lm(models[[1]], sup)
eval_fit(fit1, sup)

fit2 <- lm(models[[2]], sup)
eval_fit(fit2, sup)

fit2 <- lm(models[[3]], sup)
eval_fit(fit2, sup)

compare_pred(models, sup)



compare_pred(models, sup) #3, 2, 1
compare_pred(models, jungle) #1, 2, 3
compare_pred(models, mid) #3, 1, 2
compare_pred(models, adc) #3, 2, 1
compare_pred(models, sup) #1, 2, 3


#---------------------------------------------------results
matchdata <- matchdata %>% arrange(champion)
top <- matchdata %>% filter(position=="Top")
jungle <- matchdata %>% filter(position=="Jungle")
mid <- matchdata %>% filter(position=="Middle")
adc <- matchdata %>% filter(position=="ADC")
sup <- matchdata %>% filter(position=="Support")


model <- models[[2]]
topfit <- lm(model, top)
jungfit <- lm(model, jungle)
midfit <- lm(model, mid)
adcfit <- lm(model, adc)
supfit <- lm(model, sup)



save_dmg_rating <- function(dataset, model){
  fit <- lm(model, dataset)
  a <- dataset %>% mutate(
    perc_diff=fit$residuals/fit$fitted.values) 
  a %>%
    group_by(league, team, player) %>%
    summarise(dmg_performance = mean(perc_diff)*100) %>% 
    arrange(league, desc(dmg_performance)) %T>% 
    write_csv(paste0(savefolder, dataset$position[1], "_dmg_ratings.csv"))
  
  ggplot(a,  aes(x=fct_reorder(paste0(player, " (", p_games, ")"), perc_diff, mean), y=perc_diff)) +
    ylim(-1,1) +
    coord_flip() +
    facet_grid(league ~., scales="free", space="free") +
    geom_boxplot() +
    labs(x=NULL, y="% difference of actual damage from expected") +
    theme_minimal() +
    geom_hline(aes(yintercept = 0), color="red") +
    theme(axis.text.y = element_text(size=8))
  
  ggsave(paste0(savefolder, dataset$position[1], "_graph.png"),
         width=6, height=14)
  return(a)
}

all_dmg <- map_df(list(top, jungle, mid, adc, sup), 
                  save_dmg_rating, model=model)
all_dmg %>% group_by(position) %>% summarise(mean=mean(perc_diff))


#-----------------------
#fine-tuning
#quantify "result" effect

models <- c(
  sqrt(dmgtochamps) ~ position*champion + position*log(goldspent),
  log(dmgtochamps) ~ position*champion + position*log(goldspent)
)


fit <- lm(models[[2]], matchdata)
par(mfrow=c(1,1))
hist(fit$residuals)
par(mfrow=c(2,2))
plot(fit, ask=F) #qqplot indicates left-skew


set.seed(710)
train_idx <- sample.int(nrow(matchdata), floor(0.7*nrow(matchdata)))
test_idx <- setdiff(1:nrow(matchdata), train_idx)

fit1 <- lm(models[[1]], data=matchdata[train_idx,])
pred1 <- predict(fit1, newdata=matchdata[test_idx,])

mse_pred(pred1^2, matchdata$dmgtochamps[test_idx])

fit2 <- lm(models[[2]], data=matchdata[train_idx,])
pred2 <- predict(fit2, newdata=matchdata[test_idx,])

mse_pred(exp(pred2), matchdata$dmgtochamps[test_idx])
#----------------------------------------------


models <- c(
  sqrt(dmgtochamps) ~ position*champion + position*log(goldspent),
  log(dmgtochamps) ~ position*champion + position*log(goldspent),
  log(dmgtochamps) ~ position*log(goldspent) + champion*log(goldspent),
  log(dmgtochamps) ~ position*log(goldspent) + champion*log(goldspent) + result
)


fit <- lm(models[[3]], matchdata)
par(mfrow=c(1,1))
hist(fit$residuals)
par(mfrow=c(2,2))
plot(fit, ask=F) #qqplot indicates left-skew


fit3 <- lm(models[[3]], data=matchdata[train_idx,])
pred3 <- predict(fit3, newdata=matchdata[test_idx,])

mse_pred(exp(pred3), matchdata$dmgtochamps[test_idx])


fit4 <- lm(models[[4]], data=matchdata[train_idx,])
pred4 <- predict(fit4, newdata=matchdata[test_idx,])

mse_pred(exp(pred4), matchdata$dmgtochamps[test_idx])

fit <- lm(models[[4]], data=matchdata)

par(mfrow=c(1,1))
hist(fit$residuals)
par(mfrow=c(2,2))
plot(fit, ask=F) 

resultless <- lm(models[[3]], data=matchdata)
summary(fit)

#---------------------different residuals
r_models <- c(
  log(dmgtochamps) ~ champion + position*goldspent,
  log(dmgtochamps) ~ champion + result,
  log(dmgtochamps) ~ champion + position*goldspent + result,
  log(dmgtochamps) ~ champion + position*log(goldspent), #
  
  log(dmgtochamps) ~ champion*goldspent + position*goldspent + result,
  log(dmgtochamps) ~ champion + position*gamelength,
  log(dmgtochamps) ~ champion + position*log(gamelength), #
  log(dmgtochamps) ~ champion*log(gamelength) + position*log(gamelength),
  
  log(dmgtochamps) ~ champion*log(gamelength) + position*log(gamelength) + result,
  log(dmgtochamps) ~ champion*log(gamelength) + position*log(gamelength) + poly(otherkillshare, 3),#
  log(dmgtochamps) ~ champion + position*log(gamelength) + poly(otherkillshare, 3),#
  log(dmgtochamps) ~ champion + position*poly(log(gamelength), 2) + poly(otherkillshare, 3)#
)

i <- 12
r_fit <- lm(r_models[[i]], matchdata)
r_fit$residuals <- (matchdata$dmgtochamps - exp(r_fit$fitted.values))/
  matchdata$dmgtochamps
par(mfrow=c(1,1))
hist(r_fit$residuals)
par(mfrow=c(2,2))
plot(r_fit, ask=F) 
summary(r_fit)


set.seed(715)
train_idx <- sample.int(nrow(matchdata), floor(0.7*nrow(matchdata)))
test_idx <- setdiff(1:nrow(matchdata), train_idx)

map_dbl(r_models, function(model){
  tfit <- lm(model, data=matchdata[train_idx,])
  pred <- predict(tfit, newdata=matchdata[test_idx,])
  actual <- matchdata$dmgtochamps[test_idx]
  ((exp(pred) - actual)/actual) %>% mean
}) 

#we're interested in 4, 7, 11
i <- 11
r_fit <- lm(r_models[[i]], matchdata)
r_fit$residuals <- (matchdata$dmgtochamps - exp(r_fit$fitted.values))/
  matchdata$dmgtochamps
par(mfrow=c(1,1))

a <- matchdata$otherkills/matchdata$teamkills
plot(r_fit$residuals ~ matchdata$otherkillshare, main="Other kill share", ylim=c(-2, 1))
abline(h=0)

boxplot(r_fit$residuals ~ matchdata$champion, main="Resid vs champion", ylim=c(-2, 1))
boxplot(r_fit$residuals ~ factor(matchdata$result), main="Resid vs result", ylim=c(-2, 1)) 
boxplot(r_fit$residuals ~ factor(matchdata$firedrakes), main="Resid vs firedrakes", ylim=c(-2, 1))
# boxplot(r_fit$residuals ~ factor(matchdata$league), main="Resid vs league")
# boxplot(r_fit$residuals ~ factor(matchdata$side), main="Resid vs side")

plot(r_fit$residuals ~ log(matchdata$gamelength), main="Resid vs log(gamelength)", ylim=c(-2, 1))
abline(h=0)
plot(r_fit$residuals ~ matchdata$goldspent, main="Resid vs goldspent", ylim=c(-2, 1))
abline(h=0)
plot(r_fit$residuals ~ log(matchdata$otherkills + 0.1), main="Resid vs log(otherkills)", ylim=c(-2, 1))
abline(h=0)
plot(r_fit$residuals ~ log(matchdata$teamkills + 0.1), main="Resid vs log(teamkills)", ylim=c(-2, 1))
abline(h=0)
plot(r_fit$residuals ~ matchdata$killshare, main="Resid vs killshare", ylim=c(-2, 1))
abline(h=0)




#----------------------more sanity checks

fit <- lm(r_models[[12]], matchdata)
fit$residuals <- (matchdata$dmgtochamps - exp(fit$fitted.values))/
  matchdata$dmgtochamps

matchdata$resid <- fit$residuals
oks_curve <- (fit$model$`poly(otherkillshare, 3)`[,1]*-9.729924 +
                fit$model$`poly(otherkillshare, 3)`[,2]*-6.199987 +
                fit$model$`poly(otherkillshare, 3)`[,3]*6.086321) 

oks <- tibble(
  curve = oks_curve,
  val = exp(matchdata$otherkillshare)
) %>%
  arrange(val)

oks <- oks[-duplicated(oks),]
ggplot(matchdata, aes(x=exp(otherkillshare), y=dmgtochamps)) +
  geom_point(alpha=0.2) 


hv <- tibble(
  hatvals = hatvalues(fit),
  position = matchdata$position,
  champion = matchdata$champion) %>% 
  group_by(position, champion) 
hv_mean <- hv %>%
  summarise(meanhv = mean(hatvals)) %>%
  full_join(hv) %>%
  mutate(
    hvratio = hatvals/meanhv
  )
nrow(hv_mean) == nrow(hv)
nrow(hv) == nrow(matchdata)

matchdata$highhat <- hv_mean$hvratio

#create confidence intervals
avgoks <- matchdata %>% group_by(position, champion) %>%
  summarise(
    otherkillshare = median(otherkillshare)
  )
predictions <- tibble(
  goldspent = matchdata$goldspent, 
  gamelength= matchdata$gamelength,
  dmgtochamps = fit$fitted.values %>% exp, #predicted
  resid = fit$residuals,
  champion = matchdata$champion,
  position = matchdata$position) %>%
  left_join(avgoks)

conf <- predict(fit, newdata=predictions, interval="confidence")
predictions <- predictions %>% mutate(
  lwr = conf[,2] %>% exp,
  upr = conf[,3] %>% exp,
  dmgtochamps = conf[,1] %>% exp
)
predictions$resid = (predictions$dmgtochamps - matchdata$dmgtochamps)/
  matchdata$dmgtochamps





walk(c("Top", "Jungle", "Middle", "ADC", "Support"), function(pos, fit){
  
  n_champs <- length(unique(matchdata$champion[matchdata$position==pos]))  
  
  matchdata %>% filter(position==pos) %>%
    ggplot(aes(x=gamelength, y=dmgtochamps)) +
    geom_point(alpha=0.7) +
    geom_line(aes(group=factor(otherkillshare)),
              data=predictions %>% filter(position==pos)) +
    facet_wrap(~champion) +
    geom_ribbon(aes(ymin=lwr, ymax=upr), fill=yellow, 
                data=predictions %>% filter(position==pos),
                alpha=0.7
                ) +
    theme_minimal() +
    theme(axis.text=element_text(size=8)) +
    labs(title=paste0("OKS set to median"))
  
  ggsave(paste0(savefolder, "prediction_graph_", pos, ".png"),
         height=ceiling(n_champs/4)*1.5, width=8.5)

  ggplot(predictions %>% filter(position==pos), 
         aes(x=dmgtochamps, y=resid)) +
    facet_wrap(~champion, scales="free") +
    geom_point(alpha=0.7) + 
    geom_hline(aes(yintercept=0), color="gray") +
    theme_minimal() + 
    theme(axis.text=element_text(size=8))
  
  ggsave(paste0(savefolder, "residual_graph_", pos, ".png"),
         height=ceiling(n_champs/4)*1.5, width=8.5)  
  }, fit)



walk(unique(matchdata$position), function(pos){
  a <- matchdata %>% filter(position == pos) 
  a %>%
    group_by(league, team, player) %>%
    summarise(dmg_performance = mean(resid)*100) %>% 
    arrange(league, desc(dmg_performance)) %T>% 
    write_csv(paste0(savefolder, dataset$position[1], "_dmg_ratings.csv"))
  
  ggplot(a,  aes(x=fct_reorder(paste0(player, " (", p_games, ")"), resid, mean),
                 y=resid)) +
    ylim(-1,1) +
    coord_flip() +
    facet_grid(league ~., scales="free", space="free") +
    geom_boxplot() +
    labs(x=NULL, y="% difference of actual damage from expected") +
    theme_minimal() +
    geom_hline(aes(yintercept = 0), color="red") +
    theme(axis.text.y = element_text(size=8))
  
  ggsave(paste0(savefolder, dataset$position[1], "_graph.png"),
         width=6, height=14)
  })





base <- ggplot(matchdata, aes(y=resid)) +
  facet_grid(. ~ position, scales="free", space="free")

base + geom_boxplot(aes(x=champion)) + coord_flip() +
  facet_grid(position ~ ., scales="free", space="free")

base + geom_boxplot(aes(x=factor(result)))


base + geom_boxplot(aes(x=factor(firedrakes)))

boxplot(fit$residuals ~ factor(dataset$firedrakes), main="Resid vs firedrakes")
# boxplot(fit$residuals ~ factor(dataset$league), main="Resid vs league")
# boxplot(fit$residuals ~ factor(dataset$side), main="Resid vs side")

plot(fit$residuals ~ log(dataset$gamelength), main="Resid vs log(gamelength)")
abline(h=0)
plot(fit$residuals ~ dataset$goldspent, main="Resid vs goldspent")
abline(h=0)
plot(fit$residuals ~ log(dataset$otherkills + 0.1), main="Resid vs log(otherkills)")
abline(h=0)
plot(fit$residuals ~ log(dataset$teamkills + 0.1), main="Resid vs log(teamkills)")
abline(h=0)
plot(fit$residuals ~ dataset$killshare, main="Resid vs killshare")
abline(h=0)
summary(fit)

