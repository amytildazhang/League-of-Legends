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
library(ggrepel)

#---------------------------------------------------Read in data
matchdata <- here("Data/") %>% 
    paste0(list.files(.)) %>%
    map_df(read_xlsx, col_types = c(rep("text", 4), "date", 
                                    rep("text", 14),
                                    rep("numeric", 72))) %>%
    filter(player != "Team") %>%
    select(gameid, league, split, week, game, team, side, player, 
           position, champion, gamelength, result, k, a, teamkills, 
           firedrakes, dmgtochamps, dmgtochampsperminute,
           earnedgpm, goldspent, totalgold, dmgshare) %>%
    filter(!(week %in% c("QF", "T", "F", "R1", "R2", 
                         "R3", "RR", "SF", "3P")),
           league %in% c("NALCS", "EULCS", "LCK", "LMS")) %>%
    mutate(week = week %>% as.numeric) %>%
    filter(split == "2017-2" | week > 9) %>%
    filter(!is.na(dmgtochamps)) %>%
    mutate(otherkills = teamkills-k-a,
           killshare = ifelse(teamkills == 0, 0, k/teamkills),
           otherkillshare = ifelse(teamkills == 0, 0, otherkills/teamkills),
           position = fct_relevel(position,
                                  c("Top", "Jungle", "Middle", "ADC", "Support")))

savefolder <- here("Damage_Rating/Output/")


#---------------------------------------------------filter champions
#excluded champions
excluded_champs <- matchdata %>% group_by(position, player, champion) %>%
    summarise(n_games = n()) %>%
    group_by(position, champion) %>%
    summarise(n_total = sum(n_games),
              max_by_one = max(n_games),
              min_leftover = n_total - max_by_one) %>%
    filter(min_leftover < 12) #this is 0??

matchdata <- matchdata %>% 
    group_by(position, player, champion) %>%
    summarise(n_games = n()) %>%
    group_by(position, champion) %>%
    summarise(n_total = sum(n_games),
              max_by_one = max(n_games),
              min_leftover = n_total - max_by_one) %>%
    filter(min_leftover >=  12) %>%
    inner_join(matchdata, by = c("position", "champion"))

matchdata <- matchdata %>% group_by(player) %>%
    summarise(p_games = n()) %>%
    right_join(matchdata)



#---------------------------------------------------EDA
#commented out to save time when re-running code
# base <- ggplot(matchdata, aes(y = dmgtochamps)) +
#  facet_grid(. ~ position, scales = 'free')
# 
# base + geom_boxplot(aes(x = champion))
# 
# base + geom_boxplot(aes(x = league)) 
# 
# base + geom_boxplot(aes(x = factor(firedrakes))) 
# 
# base + geom_boxplot(aes(x = side)) 
# 
# base + geom_point(aes(x = gamelength))
# 
# base + geom_boxplot(aes(x = factor(result)))
# 
# base + geom_point(aes(x = goldspent))
# 
# base + geom_point(aes(x = teamkills))
# 
# base + geom_point(aes(x = dmgshare))
# 
# base + geom_point(aes(x = killshare))
# 
# 
# ggplot(matchdata, aes(y = gamelength, x = goldspent)) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_point()
# 
# ggplot(matchdata, aes(y = gamelength, x = league)) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()
# 
# ggplot(matchdata, aes(y = goldspent, x = factor(firedrakes))) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()
# 
# ggplot(matchdata, aes(y = goldspent, x = factor(result))) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()
# 
# ggplot(matchdata, aes(y = teamkills, x = factor(result))) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()
# 
# ggplot(matchdata, aes(y = dmgshare, x = factor(result))) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()
# 
# ggplot(matchdata, aes(y = killshare, x = factor(result))) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()
# 
# ggplot(matchdata, aes(y = k, x = factor(result))) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()
# 
# 
# ggplot(matchdata, aes(y = otherkills, x = factor(result))) +
#  facet_grid(. ~ position, scales = 'free') +
#  geom_boxplot()

#---------------------different residuals
r_models <- c( #regression models that are being considered
    sqrt(dmgtochamps) ~ champion + position*goldspent,
    sqrt(dmgtochamps) ~ champion + result,
    sqrt(dmgtochamps) ~ champion + position*goldspent + result,
    sqrt(dmgtochamps) ~ champion + position*log(goldspent), #
    
    sqrt(dmgtochamps) ~ champion*goldspent + position*goldspent + result,
    sqrt(dmgtochamps) ~ champion + position*gamelength,
    sqrt(dmgtochamps) ~ champion + position*log(gamelength), #
    sqrt(dmgtochamps) ~ champion*log(gamelength) + position*log(gamelength),
    
    sqrt(dmgtochamps) ~ champion*log(gamelength) + position*log(gamelength) + result,
    sqrt(dmgtochamps) ~ champion*log(gamelength) + position*log(gamelength) + poly(otherkillshare, 3),#
    sqrt(dmgtochamps) ~ champion + position*log(gamelength) + poly(otherkillshare, 3),#
    sqrt(dmgtochamps) ~ champion + position*poly(log(gamelength), 2) + poly(otherkillshare, 3),
    
    dmgtochampsperminute ~ champion*factor(result),
    sqrt(dmgtochamps) ~ champion*factor(result) + position,
    sqrt(dmgtochamps) ~ champion*league + position*log(goldspent) #
    
)


#choose model based on performance over test set
set.seed(715)
train_idx <- sample.int(nrow(matchdata), floor(0.7*nrow(matchdata)))
test_idx <- setdiff(1:nrow(matchdata), train_idx)

map_dbl(1:length(r_models), function(i){
    print(i)
    model <- r_models[[i]]
    tfit <- lm(model, data = matchdata[train_idx,])
    pred <- predict(tfit, newdata = matchdata[test_idx,])
    actual <- matchdata$dmgtochamps[test_idx]
    if (i == 13) {
        pred <- pred*matchdata$gamelength[test_idx] #so it's on the same scale
        return(((actual - pred)/pred) %>% mean)
    }
    ((actual - pred^2)/pred^2) %>% mean
}) 


i <- 14
fit <- lm(r_models[[i]], matchdata)

sink(paste0(savefolder, i, "/Diagnostics/lm_summary.txt"))
summary(fit)
sink()

sink(paste0(savefolder, i, "/Diagnostics/lm_markdown.md"))
knitr::kable(s$coefficients, format = "markdown")
sink()

fit_model_with_resid <- function(i, matchdata, r_models){
    fit <- lm(r_models[[i]], matchdata)
    if (i  == 13) {
        fit$residuals <- (matchdata$dmgtochampsperminute - fit$fitted.values) /
            fit$fitted.values
    } else {
        fit$residuals <- (matchdata$dmgtochamps - fit$fitted.values^2) /
            fit$fitted.values^2
    }
    return(fit)
}

fit <- fit_model_with_resid(4, matchdata, r_models)

fit <- lm(r_models[[11]], matchdata)
fit$residuals <- (matchdata$dmgtochamps - fit$fitted.values^2) /
    fit$fitted.values^2
par(mfrow = c(2,2))
plot(fit, ask = F)



output_diagnostics <- function(i, matchdata, r_models){
    red <- "#FF020A"
    blue <- "#0980B2"
    yellow <- "#C4A20A"
    
    fit <- lm(r_models[[i]], matchdata)
    s = summary(fit)
    sink(paste0(savefolder, i, "/Diagnostics/lm_summary.txt"))
    s
    sink()
    
    sink(paste0(savefolder, i, "/Diagnostics/lm_markdown.md"))
    knitr::kable(s$coefficients, format = "markdown")
    sink()
    
    #adjust fit$residuals so they are % of expected
    if (i  == 13) {
        fit$residuals <- (matchdata$dmgtochampsperminute - fit$fitted.values) /
            fit$fitted.values
        matchdata$predicted <- fit$fitted.values
    } else {
        fit$residuals <- (matchdata$dmgtochamps - fit$fitted.values^2) /
            fit$fitted.values^2
        matchdata$predicted <- fit$fitted.values^2
    }
    
    matchdata$resid <- fit$residuals
    
    
    #plot % residuals vs potential predictors
    pdf(paste0(savefolder, i, "/Diagnostics/diagnostics.pdf"))
    par(mfrow = c(1,1))
    hist(fit$residuals)
    par(mfrow = c(2,2))
    plot(fit, ask = F) 
    
    plot(fit$residuals ~ matchdata$otherkillshare, main = "Other kill share")
    abline(h = 0)
    
    boxplot(fit$residuals ~ matchdata$champion, main = "% Resid vs champion")
    
    boxplot(fit$residuals ~ factor(matchdata$result), main = "% Resid vs result") 
    
    boxplot(fit$residuals ~ factor(matchdata$firedrakes), main = "% Resid vs firedrakes")
    # boxplot(fit$residuals ~ factor(matchdata$league), main = "Resid vs league")
    # boxplot(fit$residuals ~ factor(matchdata$side), main = "Resid vs side")
    
    plot(fit$residuals ~ log(matchdata$gamelength), main = "% Resid vs log(gamelength)")
    abline(h = 0)
    
    plot(fit$residuals ~ matchdata$goldspent, main = "% Resid vs goldspent")
    abline(h = 0)
    
    plot(fit$residuals ~ log(matchdata$otherkills + 0.1), main = "% Resid vs log(otherkills)")
    abline(h = 0)
    
    plot(fit$residuals ~ log(matchdata$teamkills + 0.1), main = "% Resid vs log(teamkills)")
    abline(h = 0)
    
    plot(fit$residuals ~ matchdata$killshare, main = "% Resid vs killshare")
    abline(h = 0)
    
    dev.off()
    
    
    #separate residual graphs across position & champion
    if (i  == 12) {
        oks_curve <- (fit$model$`poly(otherkillshare, 3)`[,1]*-501.3220 +
                          fit$model$`poly(otherkillshare, 3)`[,2]*-220.4526 +
                          fit$model$`poly(otherkillshare, 3)`[,3]*286.9827) 
        
        oks <- tibble(
            curve = oks_curve,
            val = matchdata$otherkillshare
        ) %>%
            arrange(val)
        
        oks <- oks[-duplicated(oks),]
        ggplot(matchdata, aes(x = otherkillshare, y = dmgtochamps)) +
            geom_point(alpha = 0.1) +
            geom_line(aes(x = val, y = curve + 127.3797),
                      data = oks, size = 1) +
            labs(title = "Fitted curve for OKS") +
            ggsave(paste0(savefolder, i, "/Diagnostics/oks_fit.png"),
                   height = 6, with = 8)
        
    }
    
    if (i  == 13) {
        predictions <- tibble(
            result = factor(matchdata$result),
            resid = fit$residuals,
            champion = matchdata$champion,
            position = matchdata$position,
            gamelength = matchdata$gamelength,
            dmgtochampsperminute = fit$fitted.values,
            dmgtochamps = dmgtochampsperminute
        ) 
    } else {
        #create confidence intervals
        avgoks <- matchdata %>% group_by(position, champion) %>%
            summarise(
                otherkillshare = median(otherkillshare)
            )
        avgoks <- rbind(avgoks, avgoks)
        avgoks$result = rep(c(0,1), each = (nrow(avgoks)/2))
        predictions <- tibble(
            goldspent = matchdata$goldspent, 
            gamelength = matchdata$gamelength,
            result = matchdata$result,
            dmgtochamps = NA, #predicted
            resid = fit$residuals,
            champion = matchdata$champion,
            position = matchdata$position,
            dmgtochampsperminute = NA) %>%
            full_join(avgoks)
        
        
        conf <- predict(fit, newdata = predictions, interval = "confidence")
        predictions <- predictions %>% mutate(
            lwr = conf[,2]^2,
            upr = conf[,3]^2,
            dmgtochamps = conf[,1]^2
        )
        
    }
    
    
    walk(c("Top", "Jungle", "Middle", "ADC", "Support"), function(pos, fit){
        
        n_champs <- length(unique(matchdata$champion[matchdata$position == pos])) 
        
        if (i == 13) {
            matchdata %>% filter(position == pos) %>%
                ggplot(aes(x = gamelength, y = dmgtochampsperminute)) +
                geom_point(aes(shape = factor(result), color = factor(result)), 
                           alpha = 0.5) +
                geom_line(aes(group = factor(result), color = factor(result),
                              y = predicted)) +
                scale_color_manual(values = c(blue, red)) +
                facet_wrap(~champion, ncol = 4, scales = "free") +
                theme_minimal() +
                theme(axis.text = element_text(size = 8)) +
                labs(y = ifelse(i == 13, "dmgtochampsperminute", "dmgtochamps"))
            
            ggsave(paste0(savefolder, i, "/Diagnostics/prediction_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)
            
            matchdata %>% filter(position == pos) %>%
                ggplot(aes(x = goldspent, y = dmgtochampsperminute)) +
                geom_point(aes(shape = factor(result), color = factor(result)), 
                           alpha = 0.5) +
                geom_line(aes(group = factor(result), color = factor(result),
                              y = predicted)) +
                scale_color_manual(values = c(blue, red)) +
                facet_wrap(~champion, scales="free") +
                theme_minimal() +
                theme(axis.text = element_text(size = 8)) +
                labs(y = ifelse(i == 13, "dmgtochampsperminute", "dmgtochamps"))
            
            ggsave(paste0(savefolder, i, "/Diagnostics/gold_prediction_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)
            
            
            
            ggplot(matchdata %>% filter(position == pos), 
                   aes(x = goldspent, y = resid)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                geom_point(aes(shape = factor(result),
                               color = factor(result)), alpha = 0.7) + 
                scale_color_manual(values = c(blue, red)) +
                geom_hline(aes(yintercept = 0), color = "gray") +
                theme_minimal() + 
                theme(axis.text = element_text(size = 8))
            
            ggsave(paste0(savefolder, i, "/Diagnostics/residual_vs_gold_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)  
            
            # ggplot(matchdata %>% filter(position == pos), 
            #        aes(x = gamelength, y = resid)) +
            #   facet_wrap(~champion, scales = "free") +
            #   geom_point(aes(shape = factor(result),
            #                  color = factor(result)), alpha = 0.7) + 
            #   scale_color_manual(values = c(blue, red)) +
            #   geom_hline(aes(yintercept = 0), color = "gray") +
            #   theme_minimal() + 
            #   theme(axis.text = element_text(size = 8))
            # 
            # ggsave(paste0(savefolder, i, "/Diagnostics/residual_vs_time_graph_", pos, ".png"),
            #        height = ceiling(n_champs/4)*1.5, width = 8.5)  
            
        } else if (i == 14) {
            matchdata %>% filter(position == pos) %>%
                ggplot(aes(x = gamelength, y = dmgtochamps)) +
                geom_point(aes(shape = factor(result), color = factor(result)), 
                           alpha = 0.5) +
                geom_line(aes(group = factor(result), color = factor(result),
                              y = predicted)) +
                scale_color_manual(values = c(blue, red)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                theme_minimal() +
                theme(axis.text = element_text(size = 8)) +
                labs(y = "dmgtochamps")
            
            ggsave(paste0(savefolder, i, "/Diagnostics/prediction_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)
            
            matchdata %>% filter(position == pos) %>%
                ggplot(aes(x = goldspent, y = dmgtochamps)) +
                geom_point(aes(shape = factor(result), color = factor(result)), 
                           alpha = 0.5) +
                geom_line(aes(group = factor(result), color = factor(result),
                              y = predicted)) +
                scale_color_manual(values = c(blue, red)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                theme_minimal() +
                theme(axis.text = element_text(size = 8)) +
                labs(y = "dmgtochamps")
            
            ggsave(paste0(savefolder, i, "/Diagnostics/gold_prediction_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)
            
            
            ggplot(matchdata %>% filter(position == pos), 
                   aes(x = dmgtochamps, y = resid)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                geom_point(aes(shape = factor(result),
                               color = factor(result)), alpha = 0.7) + 
                scale_color_manual(values = c(blue, red)) +
                geom_hline(aes(yintercept = 0), color = "gray") +
                theme_minimal() + 
                theme(axis.text = element_text(size = 8)) +
                labs(x =  "dmgtochampsperminute")
            
            ggsave(paste0(savefolder, i, "/Diagnostics/residual_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)  
            
            
            ggplot(matchdata %>% filter(position == pos), 
                   aes(x = goldspent, y = resid)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                geom_point(aes(shape = factor(result),
                               color = factor(result)), alpha = 0.7) + 
                scale_color_manual(values = c(blue, red)) +
                geom_hline(aes(yintercept = 0), color = "gray") +
                theme_minimal() + 
                theme(axis.text = element_text(size = 8)) +
                labs(x =  "dmgtochampsperminute")
            
            ggsave(paste0(savefolder, i, "/Diagnostics/residual_vs_gold_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)  
            
            
        } else {
            matchdata %>% filter(position == pos) %>%
                ggplot(aes(x = gamelength, y = dmgtochamps)) +
                geom_point(alpha = 0.5) +
                geom_line(data = predictions %>% filter(position == pos)) +
                scale_color_manual(values = c(blue, red)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                geom_ribbon(aes(ymin = lwr, ymax = upr),
                            fill = yellow,
                            data = predictions %>% filter(position == pos),
                            alpha = 0.7
                ) +
                theme_minimal() +
                theme(axis.text = element_text(size = 8)) +
                labs(title = "OKS set to median")
            ggsave(paste0(savefolder, i, "/Diagnostics/prediction_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)
            
            
            matchdata %>% filter(position == pos) %>%
                ggplot(aes(x = goldspent, y = dmgtochamps)) +
                geom_point(alpha = 0.5) +
                geom_line(data = predictions %>% filter(position == pos)) +
                scale_color_manual(values = c(blue, red)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                geom_ribbon(aes(ymin = lwr, ymax = upr),
                            fill = yellow,
                            data = predictions %>% filter(position == pos),
                            alpha = 0.7
                ) +
                theme_minimal() +
                theme(axis.text = element_text(size = 8)) +
                labs(title = "OKS set to median")
            ggsave(paste0(savefolder, i, "/Diagnostics/gold_prediction_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)
            
            ggplot(predictions %>% filter(position == pos), 
                   aes(x = sqrt(dmgtochamps), y = resid)) +
                facet_wrap(~champion, scales = "free", ncol = 4) +
                geom_point(aes(shape = factor(result),
                               color = factor(result)), alpha = 0.7) + 
                scale_color_manual(values = c(blue, red)) +
                geom_hline(aes(yintercept = 0), color = "gray") +
                theme_minimal() + 
                theme(axis.text = element_text(size = 8)) +
                labs(x="sqrt(predicted dmgtochamps)")
            
            ggsave(paste0(savefolder, i, "/Diagnostics/residual_graph_", pos, ".png"),
                   height = ceiling(n_champs/4)*1.5, width = 8.5)  
        }
        
        
    }, fit)
    
}

output_diagnostics(4, matchdata, r_models)
output_diagnostics(11, matchdata, r_models)
output_diagnostics(12, matchdata, r_models)
output_diagnostics(13, matchdata, r_models)
output_diagnostics(14, matchdata, r_models)



#----------------------more sanity checks

output_rankings <- function(i, matchdata, r_models){
    purple <- "#85016E"
    red <- "#FF020A"
    blue <- "#0980B2"
    yellow <- "#C4A20A"
    green <- "#006112"
    
    fit <- lm(r_models[[i]], matchdata)
    if (i  == 13) {
        fit$residuals <- (matchdata$dmgtochampsperminute - fit$fitted.values) /
            fit$fitted.values
        matchdata$predicted <- fit$fitted.values
    } else {
        fit$residuals <- (matchdata$dmgtochamps - fit$fitted.values^2) /
            fit$fitted.values^2
        matchdata$predicted <- fit$fitted.values^2
    }
    
    matchdata$resid <- fit$residuals
    #remove players who role-swapped
    adj_data <- matchdata %>% 
        left_join(matchdata %>%
                      group_by(player, position) %>%
                      summarise(n_games = n()) )
    
    walk(unique(adj_data$league), function(lg){
        a <- adj_data %>% filter(league  == lg) 
        a %>%
            select(league, team, player, champion, dmgtochamps, predicted, goldspent) %>%
            write_csv(paste0(savefolder, i, "/Rankings/", lg, "_dmg_vals.csv"))
        a %>%
            group_by(position, team, player) %>%
            summarise(dmg_performance = round(mean(resid)*100, digits = 2)) %>%
            arrange(position, desc(dmg_performance)) %>%
            write_csv(paste0(savefolder, i, "/Rankings/", lg, "_dmg_ratings.csv"))
        
        c <- a %>% filter(n_games >=  8) %>%
            group_by(player, n_games, position) %>%
            summarise(
                total_predicted = sum(predicted),
                avg_predicted = mean(predicted),
                mean_r = mean(resid)*100,
                median_r = median(resid)*100,
                raw_avg_dmg = mean(dmgtochamps)
            ) %>% 
            mutate(#diff = ifelse(median_r > mean_r, sprintf('\u2191'), sprintf('\u2193')),
                #sign = ifelse(mean_r > 0, "+", "-"),
                avg_dmg = (total_predicted*mean_r + total_predicted)/n_games)
        c <- c %>%
            left_join(c %>% group_by(position) %>%
                          summarise(max_predicted = max(avg_predicted)))
        c %>%
            ggplot(aes(x = mean_r, y = median_r)) +
            labs(y = "median % resid", size = "% of highest prediction/position",
                 x = "mean % resid") +
            geom_point(aes(size = avg_predicted/max_predicted), alpha = 0.7) +
            geom_abline(aes(slope = 1, intercept = 0), color = "gray", linetype = "dotdash") +
            geom_vline(aes(xintercept = 0), color = "gray") +
            geom_hline(aes(yintercept = 0), color = "gray") +
            geom_text_repel(aes(label = player), force = 3,
                            size = 2, box.padding = unit(0.75, "lines")) +
            facet_wrap(~position) +
            theme_minimal() +
            theme(legend.position = "bottom")
        ggsave(paste0(savefolder, i, "/Rankings/", lg, "_meanvsmedian_graph.png"),
               width = 10, height = 7.5)
        
        color_vals <- rep(c(blue, yellow, red), 6)
        shape_vals <- rep(1:6, each = 3)
        alpha_vals <- rep(seq(1, 0.5, by = -0.1), 3)
        walk(unique(a$position), function(pos){
            n_champs <- unique(a$champion[a$position  == pos]) %>% length
            p <- a %>% filter(n_games >= 8, position  == pos)
            max_y <- (max(p$resid) + 0.1) %>% round(digits = 1)
            a %>% filter(n_games >=  8, position  == pos) %>%
                ggplot(aes(x = predicted, y = resid, color = champion, shape = champion)) +
                geom_point(aes(alpha = champion), stroke = 0.8) +
                scale_colour_manual(name = "Champion",
                                    values = color_vals[1:n_champs]) +
                scale_shape_manual(name = "Champion",
                                   values = shape_vals[1:n_champs]) +
                scale_alpha_manual(name = "Champion",
                                   values = alpha_vals[1:n_champs]) +
                facet_wrap(~player, nrow = 3) +
                theme_minimal() +
                labs(y = "% diff") +
                geom_hline(aes(yintercept = 0), color = "gray") +
                geom_abline(aes(slope = 1, intercept = 0), color = "gray") +
                scale_y_continuous(breaks = seq(-1, max_y, by = 0.5),
                                   minor_breaks = seq(-1, max_y, by = 0.1))
            
            ncols <- ceiling(n_champs/3)
            
            ggsave(paste0(savefolder, i, "/Rankings/", lg, 
                          "_breakdown_", pos, ".png"),
                   width = ncols*2 + 1, height = 6)
        })
        
        
        
        
        
        c %>%
            ggplot(aes(x = avg_predicted, y = mean_r)) +
            geom_point(alpha = 0.7, size = 2) +
            geom_hline(aes(yintercept = 0), alpha = 0.7) +
            geom_text_repel(aes(label = player), size = 3) +
            facet_wrap(~position, scales = "free_x") +
            theme_minimal() +
            theme(legend.position = "bottom") +
            labs(x = "average predicted damage", y = "mean % difference from prediction")
        ggsave(paste0(savefolder, i, "/Rankings/", lg, "_dmgvsresid_graph.png"),
               width = 10, height = 7.5)
        
        ranks <- a %>% filter(n_games >=  8) %>%
            mutate(resid = 100*resid,
                   plabel = paste0(" (", n_games, ") ", player)) %>%
            left_join(c %>% select(player, position, mean_r)) %>% 
            arrange(desc(mean_r)) %>%
            mutate(
                plabel = fct_inorder(plabel)
            ) %>%
            ggplot(aes(x = plabel, y = resid)) +
            labs(y = "% diff from expected", fill = NULL, x = NULL) +
            geom_violin(aes(fill = position %>% 
                                fct_relevel(c("Top", "Jungle", "Middle", "ADC", "Support"))), 
                        alpha = 0.7, draw_quantiles = 0.5) +
            geom_point(aes(y = mean_r), shape = 8, size = 0.8) +
            scale_fill_manual(values = c(yellow, green, blue, red, purple)) +
            #   coord_flip() +
            theme_minimal() +
            geom_hline(aes(yintercept = 0), color = "gray", linetype = "dashed") +
            theme(axis.text.x = element_text(angle = 20,hjust = 1,size = 12)) 
        
        ranks
        ggsave(paste0(savefolder, i, "/Rankings/", lg, "_graph_all.png"),
               width = 40, height = 5)
        
        ranks + 
            facet_wrap(~position, scales = "free_x") +
            theme(axis.text.x = element_text(angle = 20,hjust = 1,size = 8)) 
        
        ggsave(paste0(savefolder, i, "/Rankings/", lg, "_graph_byposition.png"),
               width = 15, height = 8)
    })
    
    
    
}

walk(c(4,11,12,13,14), function(i){
    output_rankings(i, matchdata, r_models)
})








base <- ggplot(matchdata, aes(y = resid)) +
    facet_grid(. ~ position, scales = "free", space = "free")

base + geom_boxplot((aes(x = league))) + 
    ylim(-2,1) +
    theme(
        axis.text.x = element_text(size = 6)
    )

base + geom_boxplot(aes(x = champion)) + coord_flip() +
    facet_grid(position ~ ., scales = "free", space = "free")

base + geom_boxplot(aes(x = factor(result)))


base + geom_boxplot(aes(x = factor(firedrakes)))

boxplot(fit$residuals ~ factor(dataset$firedrakes), main = "Resid vs firedrakes")
# boxplot(fit$residuals ~ factor(dataset$league), main = "Resid vs league")
# boxplot(fit$residuals ~ factor(dataset$side), main = "Resid vs side")

plot(fit$residuals ~ log(dataset$gamelength), main = "Resid vs log(gamelength)")
abline(h = 0)
plot(fit$residuals ~ dataset$goldspent, main = "Resid vs goldspent")
abline(h = 0)
plot(fit$residuals ~ log(dataset$otherkills + 0.1), main = "Resid vs log(otherkills)")
abline(h = 0)
plot(fit$residuals ~ log(dataset$teamkills + 0.1), main = "Resid vs log(teamkills)")
abline(h = 0)
plot(fit$residuals ~ dataset$killshare, main = "Resid vs killshare")
abline(h = 0)
summary(fit)



#------------------Squared Relative Error, Newton-raphson------------------#

source("LPRE.R")

wo_influential <- matchdata[matchdata$dmgtochamps < 25000,]
xmatrix <- model.matrix(dmgtochamps ~ champion*goldspent + position*goldspent, 
                        data = wo_influential)

beta0 = lm(log(wo_influential$dmgtochamps)~xmatrix-1)$coeff
temp = findroot(wo_influential$dmgtochamps,xmatrix,beta0)

fitted <- exp(xmatrix %*% temp$beta.hat)
residuals <- (wo_influential$dmgtochamps - fitted)

(residuals/fitted)^2 %>% mean

xpredmatrix <- model.matrix(dmgtochamps ~ champion*goldspent + position*(goldspent), 
                            data = matchdata)
beta0 = lm(log(matchdata$dmgtochamps)~xpredmatrix-1)$coeff
temp = findroot(matchdata$dmgtochamps,xpredmatrix,beta0)

matchdata$predicted <- exp(xpredmatrix %*% temp$beta.hat)
matchdata$resid <- (matchdata$dmgtochamps - matchdata$predicted)/matchdata$predicted
fitted <- exp(xpredmatrix %*% temp$beta.hat)
residuals <- (matchdata$dmgtochamps - fitted)
(residuals/fitted)^2 %>% mean

plot(residuals/fitted ~ log(fitted))
abline(h = 0)
plot(residuals/matchdata$dmgtochamps ~ log(fitted))



xpredmatrix <- model.matrix(dmgtochamps ~ champion*log(goldspent) + position*log(goldspent), 
                            data = matchdata)
beta0 = lm(log(matchdata$dmgtochamps)~xpredmatrix-1)$coeff
temp = findroot(matchdata$dmgtochamps,xpredmatrix,beta0)

matchdata$predicted <- exp(xpredmatrix %*% temp$beta.hat)
matchdata$resid <- (matchdata$dmgtochamps - matchdata$predicted)/matchdata$predicted
fitted <- exp(xpredmatrix %*% temp$beta.hat)
residuals <- (matchdata$dmgtochamps - fitted)
(residuals/fitted)^2 %>% mean

plot(residuals/fitted ~ log(fitted))
abline(h = 0)
plot(residuals/matchdata$dmgtochamps ~ log(fitted))



ggplot(matchdata, aes(x = league, y = resid)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    theme_minimal() +
    facet_wrap(~position)


purple <- "#85016E"
red <- "#FF020A"
blue <- "#0980B2"
yellow <- "#C4A20A"
green <- "#006112"

pdf(paste0(savefolder, "LPRE/Diagnostics/diagnostics.pdf"))
par(mfrow = c(1,1))
hist(residuals/fitted.values)

plot(residuals/fitted.values ~ matchdata$otherkillshare, main = "Other kill share")
abline(h = 0)

boxplot(residuals/fitted.values ~ matchdata$champion, main = "% Resid vs champion")

boxplot(residuals/fitted.values ~ factor(matchdata$result), main = "% Resid vs result") 

boxplot(residuals/fitted.values ~ factor(matchdata$firedrakes), main = "% Resid vs firedrakes")
# boxplot(residuals/fitted.values ~ factor(matchdata$league), main = "Resid vs league")
# boxplot(residuals/fitted.values ~ factor(matchdata$side), main = "Resid vs side")

plot(residuals/fitted.values ~ log(matchdata$gamelength), main = "% Resid vs log(gamelength)",
     ylim = c(-2,2))
abline(h = 0)

plot(residuals/fitted.values ~ matchdata$goldspent, main = "% Resid vs goldspent")
abline(h = 0)
plot(actual_r ~ matchdata$goldspent, main = "Resid vs goldspent")
abline(h = 0)

plot(residuals/fitted.values ~ log(matchdata$otherkills + 0.1), main = "% Resid vs log(otherkills)")
abline(h = 0)

plot(residuals/fitted.values ~ log(matchdata$teamkills + 0.1), main = "% Resid vs log(teamkills)")
abline(h = 0)

plot(residuals/fitted.values ~ matchdata$killshare, main = "% Resid vs killshare")
abline(h = 0)

dev.off()


r_gold <- range(matchdata$goldspent)
champ_pos <- unique(matchdata[,c("champion", "position")])
goldseq <- seq(r_gold[1], r_gold[2], by = 500)
len_g <- length(goldseq)

predictions <- map2_df(champ_pos$champion, champ_pos$position, function(champ, pos){
    r_gold <- matchdata %>% filter(champion == champ, position == pos) %>%
        `$`(goldspent) %>% range()
    goldseq <- seq(r_gold[1], r_gold[2], by = 500)
    len_g <- length(goldseq)
    tibble(
        goldspent = rep(goldseq, 2),
        result = rep(c(0,1), each = len_g),
        predicted = NA,
        resid = NA,
        champion = champ,
        position = pos
    )  })

xpredmatrix <- model.matrix(~ champion*log(goldspent) + position*log(goldspent), 
                            data = predictions)
predictions$predicted <- exp(xpredmatrix %*% temp$beta.hat)
predictions$resid <- (predictions$predicted - predictions$predicted)/predictions$predicted


matchdata$resid <- as.vector(matchdata$resid)

walk(c("Top", "Jungle", "Middle", "ADC", "Support"), function(pos){
    
    n_champs <- length(unique(matchdata$champion[matchdata$position == pos])) 
    
    matchdata %>% filter(position == pos) %>%
        ggplot(aes(x = goldspent, y = dmgtochamps)) +
        geom_point(aes(color = factor(result)), alpha = 0.3) +
        geom_line(aes(y = predicted),
                  data=predictions %>% filter(position == pos)) +
        scale_color_manual(values = c(blue, red)) +
        facet_wrap(~champion) +
        theme_minimal() +
        theme(axis.text = element_text(size = 8)) 
    
    ggsave(paste0(savefolder, "LPRE/Diagnostics/gold_prediction_graph_", pos, ".png"),
           height = ceiling(n_champs/4)*1.5, width = 10)  
    
    ggplot(matchdata %>% filter(position == pos), 
           aes(x = log(predicted), y = resid)) +
        facet_wrap(~champion, scales = "free", ncol = 4) +
        geom_point(aes(shape = factor(result),
                       color = factor(result)), alpha = 0.7) + 
        scale_color_manual(values = c(blue, red)) +
        geom_hline(aes(yintercept = 0), color = "gray") +
        theme_minimal() + 
        theme(axis.text = element_text(size = 8))
    
    ggsave(paste0(savefolder, "LPRE/Diagnostics/residual_graph_", pos, ".png"),
           height = ceiling(n_champs/4)*1.5, width = 8.5)  
    
}


)



adj_data <- matchdata %>% 
    left_join(matchdata %>%
                  group_by(player, position) %>%
                  summarise(n_games = n()) ,
              by = c("player", "position"))

walk(unique(adj_data$league), function(lg){
    a <- adj_data %>% filter(league  == lg) 
    a %>%
        select(league, team, player, champion, dmgtochamps, predicted, goldspent) %>%
        write_csv(paste0(savefolder, "LPRE/Rankings/", lg, "_dmg_vals.csv"))
    a %>%
        group_by(position, team, player) %>%
        summarise(dmg_performance = round(mean(resid)*100, digits = 2)) %>%
        arrange(position, desc(dmg_performance)) %>%
        write_csv(paste0(savefolder, "LPRE/Rankings/", lg, "_dmg_ratings.csv"))
    
    cd <- a %>% filter(n_games >=  8) %>%
        group_by(player, n_games, position) %>%
        summarise(
            total_predicted = sum(predicted),
            avg_predicted = mean(predicted),
            median_predicted = median(predicted),
            mean_r = mean(resid)*100,
            median_r = median(resid)*100,
            raw_avg_dmg = mean(dmgtochamps)
        ) %>% 
        mutate(#diff = ifelse(median_r > mean_r, sprintf('\u2191'), sprintf('\u2193')),
            #sign = ifelse(mean_r > 0, "+", "-"),
            avg_dmg = (total_predicted*mean_r + total_predicted)/n_games)
    cd <- cd %>%
        left_join(cd %>% group_by(position) %>%
                      summarise(max_predicted = max(avg_predicted)),
                  by = "position")
    cd %>%
        ggplot(aes(x = mean_r, y = median_r)) +
        labs(y = "median % resid", size = "% of highest prediction/position",
             x = "mean % resid") +
        geom_point(aes(size = avg_predicted/max_predicted), alpha = 0.7) +
        geom_abline(aes(slope = 1, intercept = 0), color = "gray", linetype = "dotdash") +
        geom_vline(aes(xintercept = 0), color = "gray") +
        geom_hline(aes(yintercept = 0), color = "gray") +
        geom_text_repel(aes(label = player), force = 3,
                        size = 2, box.padding = unit(0.75, "lines")) +
        facet_wrap(~position) +
        theme_minimal() +
        theme(legend.position = "bottom")
    ggsave(paste0(savefolder, "LPRE/Rankings/", lg, "_meanvsmedian_graph.png"),
           width = 10, height = 7.5)
    
    
    cd %>%
        ggplot(aes(x = median_predicted, y = median_r)) +
        geom_point(alpha = 0.7) +
        # geom_abline(aes(slope = 1, intercept = 0), color = "gray", linetype = "dotdash") +
        # geom_vline(aes(xintercept = 0), color = "gray") +
        geom_hline(aes(yintercept = 0), color = "gray") +
        geom_text_repel(aes(label = player), 
                        size = 2) +
        facet_wrap(~position, scales = "free_x") +
        theme_minimal() +
        theme(legend.position = "bottom")
    ggsave(paste0(savefolder, "LPRE/Rankings/", lg, "graph_byposition.png"),
           width = 10, height = 7.5)
    
    
    color_vals <- rep(c(blue, yellow, red), 6)
    shape_vals <- rep(1:6, each = 3)
    alpha_vals <- rep(seq(1, 0.5, by = -0.1), 3)
    walk(unique(a$position), function(pos){
        n_champs <- unique(a$champion[a$position  == pos]) %>% length
        p <- a %>% filter(n_games >=  8, position  == pos)
        max_y <- (max(p$resid) + 0.1) %>% round(digits = 1)
        a %>% filter(n_games >=  8, position  == pos) %>%
            ggplot(aes(x = goldspent, y = resid, color = champion, shape = champion)) +
            geom_point(aes(alpha = champion), stroke = 0.8) +
            scale_colour_manual(name = "Champion",
                                values = color_vals[1:n_champs]) +
            scale_shape_manual(name = "Champion",
                               values = shape_vals[1:n_champs]) +
            scale_alpha_manual(name = "Champion",
                               values = alpha_vals[1:n_champs]) +
            facet_wrap(~player, nrow = 3) +
            theme_minimal() +
            labs(y = "% diff") +
            geom_hline(aes(yintercept = 0), color = "gray") +
            geom_abline(aes(slope = 1, intercept = 0), color = "gray") +
            scale_y_continuous(breaks = seq(-1, max_y, by = 0.5),
                               minor_breaks = seq(-1, max_y, by = 0.1))
        
        ncols <- ceiling(n_champs/3)
        
        ggsave(paste0(savefolder, "LPRE/Rankings/", lg, 
                      "_breakdown_", pos, ".png"),
               width = ncols * 2 + 1, height = 6)
        
    })
})

