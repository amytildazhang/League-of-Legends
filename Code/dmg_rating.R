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
         killshare=k/teamkills)

savefolder <- here("Output/dmg_rating/")


#---------------------------------------------------filter champions
matchdata <- matchdata %>% 
  group_by(position, player, champion) %>%
  summarise(n_games=n()) %>%
  group_by(position, champion) %>%
  summarise(n_total=sum(n_games),
            max_by_one=max(n_games),
            min_leftover=n_total-max_by_one) %>%
  filter(min_leftover >= 5) %>%
  inner_join(matchdata)

matchdata <- matchdata %>% group_by(player) %>% 
  summarise(p_games=n()) %>%
  filter(p_games > 10) %>%
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

compare_pred <- function(models, dataset){
  set.seed(710)
  train_idx <- sample.int(nrow(dataset), floor(0.7*nrow(dataset)))
  test_idx <- setdiff(1:nrow(dataset), train_idx)

  walk(1:length(models), function(i){
    fit1 <- lm(models[[i]], data=dataset[train_idx,])
    pred1 <- predict(fit1, newdata=dataset[test_idx,])

    paste0("Fit ", i, ": ", mse_pred(pred1, dataset[test_idx,]$dmgtochamps)) %>% print

  })
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
