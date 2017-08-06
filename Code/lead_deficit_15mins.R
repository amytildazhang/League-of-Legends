#Riot will occasionally compare the leading team's gold lead at 20 with their 
#average gold difference at 20. Why is this dumb? Because gold difference at 20
#is likely to be bimodal, therefore an average doesn't give you any sense of 
#whether a gold lead is typical for the team or not. It DOES give you a sense if
#they are ahead or behind more often, and by how much...but that isn't the context
#that they are presented in. So. Less dumb stats.

library(here)
library(tidyverse)
library(readxl)
library(stringr)
library(forcats)
library(ggrepel)

#---------------------------------------------------Read in data
summerfile <- here("Data") %>% list.files %>%
  `[`(str_detect(., "^2017-summer"))
teamgold15 <- read_xlsx(paste0("Data/", summerfile) %>% here,
                       col_types=c("text", rep("guess", 90))) %>%
  filter(player=="Team") %>%
  select(gameid, league, team, gdat15, result)

savefolder <- here("Output/lead_deficit_15mins/")

#---------------------------------------------------Quick plot of distributions
#find leagues with max # games per team > 20
teamgold15 %>% group_by(league, team) %>%
  summarise(n_games=n()) %>% 
  group_by(league) %>%
  summarise(max_games=max(n_games))

include_leagues <- c("EULCS", "LCK", "LMS", "NALCS", "TCL")


teamgold15 <- teamgold15 %>% filter(league %in% include_leagues)

bwidth = 300

walk(teamgold15$league %>% unique, function(lg){
  n_teams <- teamgold15$team[teamgold15$league == lg] %>% unique %>% length
  p <- teamgold15 %>% filter(league==lg) %>%
    ggplot(aes(x=gdat15)) +
    geom_histogram(binwidth=bwidth, center=bwidth/2) +
    facet_wrap(~team, ncol=n_teams/2) +
    geom_vline(aes(xintercept=0), color="red")
  ggsave(here(paste0("Output/lead_deficit_15mins/histogram_", lg, ".png")),
         width=8, height=2, units="in")
    
})


#---------------------------------------------------Add annotations to graphs
avg_lines <- teamgold15 %>% group_by(league, team) %>%
  summarise(
    avg=mean(gdat15),
    lead_avg=ifelse(gdat15 > 0, gdat15, NA) %>% mean(na.rm=TRUE),
    deficit_avg=ifelse(gdat15 < 0, gdat15, NA) %>% mean(na.rm=TRUE),
    abs_avg=abs(gdat15) %>% mean(na.rm=TRUE)
  ) %>%
  gather("averages", "avg_values", avg:abs_avg)

avg_lines$averages <- avg_lines$averages %>% 
  fct_collapse(lead_deficit=c("lead_avg", "deficit_avg"))
avg_lines$lty <- avg_lines$averages %>%
  fct_recode("NA"="abs_avg", dashed="avg", solid="lead_deficit") %>%
  as.character


walk(teamgold15$league %>% unique, function(lg){
  n_teams <- teamgold15$team[teamgold15$league == lg] %>% unique %>% length
  p <- teamgold15 %>% filter(league==lg) %>%
    ggplot(aes(x=gdat15)) +
    geom_histogram(binwidth=bwidth, center=bwidth/2) +
    facet_wrap(~team, ncol=n_teams/2) +
    geom_vline(aes(xintercept=0), color="red") +
    geom_vline(aes(xintercept=avg_values, linetype=lty),
               data=filter(avg_lines,league==lg, averages != "abs_avg"),
               color="gray") +
    scale_linetype_manual(values=c("dashed", "solid"), 
                          labels=c("pure average", "lead/deficit average")) +
    theme_minimal() +
    theme(axis.text=element_text(size=6),
          legend.key.size = unit(0.45, "cm"),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          legend.text=element_text(size=8),
          legend.position="bottom") +
    labs(linetype=NULL, x=NULL, y=NULL)
  p
  ggsave(paste0(savefolder, "histogram_", lg, ".png"),
         width=8, height=2.5, units="in")
  
})


#---------------------------------------------------CSV of summary statistics

team_summaries <- teamgold15 %>% 
  mutate(ahead=ifelse(gdat15 > 0, 1, -1),
         result=ifelse(result > 0, 1, -1)) %>%
  group_by(league, team) %>%
  summarise(
    avg=mean(gdat15),
    lead_avg=ifelse(gdat15 > 0, gdat15, NA) %>% mean(na.rm=TRUE),
    deficit_avg=ifelse(gdat15 < 0, gdat15, NA) %>% mean(na.rm=TRUE),
    abs_avg=abs(gdat15) %>% mean(na.rm=TRUE),
    games_ahead=sum(gdat15>0),
    games_behind=sum(gdat15<0),
    games_won=sum(result > 0),
    games_lost=sum(result < 0),
    predictive=(sum(ahead==result)/sum(abs(result))*100) %>% round,
    winahead=(sum(ahead==1 & result==1)/sum(ahead==1)*100) %>% round,
    losebehind=(sum(ahead==-1 & result==-1)/sum(ahead==-1)*100) %>% round
  ) %>%
  mutate(
    "%ahead"=(games_ahead/(games_ahead+games_behind)*100) %>% round(digits=0),
    "%won"=(games_won/(games_won+games_lost)*100) %>% round(digits=0)
    )


write_csv(team_summaries, paste0(savefolder, "team_summary.csv"))




team_summaries %>%
  ggplot(aes(x=losebehind, y=winahead)) +
  geom_point() +
  theme_minimal() +
  geom_text_repel(aes(label=paste0(team, " ", sprintf('\u2191'), games_ahead, 
                                   " ", sprintf('\u2193'), games_behind)), 
                  size=1.5) +
  facet_wrap(~league) +
  geom_hline(aes(yintercept=50), color="gray") +
  geom_vline(aes(xintercept=50), color="gray") +
  xlim(0,100) + ylim(0,100) +
  labs(x="% games lost when behind at 15", y="% games won when ahead at 15")

ggsave(paste0(savefolder, "comparison_winloss_updown.png"),
       width=8, height=4.5)




team_summaries %>%
  ggplot(aes(x=abs(deficit_avg), y=lead_avg)) +
  geom_point(aes(color=lead_avg/abs(deficit_avg), 
                 size=lead_avg/abs(deficit_avg)), alpha=0.8) +
  theme_minimal() +
  geom_text_repel(aes(label=paste0(team, " ", sprintf('\u2191'), games_ahead, 
                                   " ", sprintf('\u2193'), games_behind)), 
                  size=1.5) +
  facet_wrap(~league) +
  labs(x=expression("avg deficit"), 
       y=expression("avg lead")) +
  geom_abline(aes(slope=1, intercept=0), color="gray", linetype="dashed") +
  labs(size="lead:deficit", color="lead:deficit")
  
ggsave(paste0(savefolder, "comparison_lead_deficit.png"),
       width=8, height=4.5)
