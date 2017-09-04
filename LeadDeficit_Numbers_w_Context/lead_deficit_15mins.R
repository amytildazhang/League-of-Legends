library(here)
library(tidyverse)
library(readxl)
library(stringr)
library(forcats)
library(ggrepel)
library(cowplot)

#---------------------------------------------------Read in data
summerfile <- here("Data") %>% list.files %>%
  `[`(str_detect(., "^2017-summer"))
teamgold15 <- read_xlsx(paste0("Data/", summerfile) %>% here,
                       col_types=c("text", rep("guess", 90))) %>%
  filter(player=="Team") %>%
  select(gameid, league, team, gdat15, result)

savefolder <- here("LeadDeficit_Numbers_w_Context/Output/")

#---------------------------------------------------Quick plot of distributions
#find leagues with max # games per team > 20
teamgold15 %>% group_by(league, team) %>%
  summarise(n_games=n()) %>% 
  group_by(league) %>%
  summarise(max_games=max(n_games))

include_leagues <- c("EULCS", "LCK", "LMS", "NALCS", "TCL")


teamgold15 <- teamgold15 %>% filter(league %in% include_leagues)

bwidth = 300


# save quick plots of all distributions
# commented out bc they aren't necessary anymore
# walk(teamgold15$league %>% unique, function(lg){
#   n_teams <- teamgold15$team[teamgold15$league == lg] %>% unique %>% length
#   teamgold15 %>% filter(league==lg) %>%
#     ggplot(aes(x=gdat15)) +
#     geom_histogram(binwidth=bwidth, center=bwidth/2) +
#     facet_wrap(~team, ncol=n_teams/2) +
#     geom_vline(aes(xintercept=0), color="red") +
#     theme_minimal()+
#     theme(axis.text = element_text(size=6))
#   ggsave(here(paste0("Output/lead_deficit_15mins/histogram_", lg, ".png")),
#          width=8, height=2, units="in")
#     
# })
# 

#---------------------------------------------------Add annotations to graphs
#team name, average lines
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


#plot and save graphs with annotations
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
         width=8, height=2, units="in")
  
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
    "%won"=(games_won/(games_won+games_lost)*100) %>% round(digits=0),
    game_record = games_won/(games_won + games_lost)
    ) %>%
  arrange(league, -game_record) %>%
  group_by(league) %>%
  mutate(rank = row_number())


write_csv(team_summaries, paste0(savefolder, "team_summary.csv"))



#------------------------------------------New question:
#do teams tend to win when ahead and lose when behind? by how much?

yellow <- "#C4A20A"
red <- "#FF020A"

winloss_updown <- team_summaries %>%
  ggplot(aes(x=100-losebehind, y=winahead)) +
  geom_point(aes(color=(games_ahead > 7 & games_behind > 7))) +
  scale_color_manual(values=c(red, "black"), 
                     breaks=list(FALSE),
                     labels=c("<7 games ahead/behind", NULL)) +
  theme_minimal() +
  geom_text_repel(aes(label=paste0("(", rank, ") ",
                                   team, " ", sprintf('\u2191'), games_ahead, 
                                   " ", sprintf('\u2193'), games_behind)), 
                  size=2, force=4) +
 # facet_wrap(~league, ncol=1) +
  facet_wrap(~league, ncol = 2) +
  geom_hline(aes(yintercept=50), color="gray") +
  geom_vline(aes(xintercept=50), color="gray") +
  # xlim(0,100) + ylim(0,100) +
  scale_x_continuous(minor_breaks = seq(0 , 100, 10), 
                     breaks = seq(0, 100, 20), 
                     limits=c(0,100)) +
  scale_y_continuous(minor_breaks = seq(0 , 100, 10), 
                     breaks = seq(0, 100, 20), 
                     limits=c(0,100)) +
  labs(x="when behind at 15", 
       y="when ahead at 15",
       color=NULL,
       title="% games won") +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.box.margin = margin(t=0, r=0, b=-5, l=-12,unit="pt"),
        legend.box.spacing = unit(0.5, "pt"),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        plot.title = element_text(margin=margin(b=-7, unit="pt")),
        panel.spacing = unit(2, "lines"))
winloss_updown

ggsave(paste0(savefolder, "comparison_winloss_updown.png"),
       width=6, height=8)


#a graph that I was trying out and decided not to use
team_summaries %>%
  ggplot(aes(x=(lead_avg)/(lead_avg-deficit_avg), 
             # y=(games_ahead)/(games_ahead + games_behind))) +
  y=game_record)) +
  geom_point(aes(color=(abs_avg + lead_avg + deficit_avg)/3, 
                 size=(abs_avg + lead_avg + deficit_avg)/3), 
                 alpha=0.8) +
  theme_minimal() +
  geom_text_repel(aes(label=paste0("(", rank, ") ",
                                   team, " ", sprintf('\u2191'), games_ahead, 
                                   " ", sprintf('\u2193'), games_behind)), 
                  size=1.5) +
  facet_wrap(~league, ncol = 2) +
  labs(x="average lead %/% (average lead + average deficit)",
       y="% games ahead") +
  geom_vline(aes(xintercept=0.5), color="gray") +
  geom_hline(aes(yintercept=0.5), color="gray") +
  labs(size="absolute average", color="absolute average") +
  theme(legend.position = "bottom",
        legend.title=element_text(size=8),
        legend.text = element_text(size=6)) +
  scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), 
                     breaks = seq(0, 1, 0.2), 
                     limits=c(0.2,0.8)) +
  scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), 
                     breaks = seq(0, 1, 0.2), 
                     limits=c(0.2,0.8))
  

#what is average lead/deficit size? Does this contribute to team's behavior for win/loss?
comparison_lead_deficit <- team_summaries %>%
  ggplot(aes(x=-deficit_avg,
             y=lead_avg)) +
  geom_point(aes(color=game_record), 
             alpha=0.8) +
 # scale_color_continuous(high="#132B43", low="#56B1F7") +
  theme_minimal() +
  geom_text_repel(aes(label=paste0("(", rank, ") ",
                                   team, " ", sprintf('\u2191'), games_ahead, 
                                   " ", sprintf('\u2193'), games_behind)), 
                  size=2, force=4) +
  facet_wrap(~league, scales="free", ncol = 2) +
  geom_abline(aes(slope=1, intercept=0), color="gray") +
  labs(size="|average|", color="game win %",
       title="Average leads vs deficits",
       x="Average deficit", y="Average lead") +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.box.margin = margin(t=2, r=0, b=-5, l=-6,unit="pt"),
        legend.box.spacing = unit(0.5, "pt"),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        plot.title = element_text(margin=margin(b=-7, unit="pt")),
        legend.key.size = unit(5, "pt"),
        legend.key.width = unit(10, "pt"),
        panel.spacing = unit(2, "lines")) #+
comparison_lead_deficit
ggsave(paste0(savefolder, "comparison_lead_deficit.png"),
       width=6, height=8)

library(cowplot)
#plot win/loss graph and average lead/deficit graph next to each other
plot_grid(winloss_updown + facet_wrap(~league, ncol=1, scales="free") +

            theme(axis.text.y = element_text(hjust=0)),
          comparison_lead_deficit + facet_wrap(~league, scales="free", ncol=1),
          align="h") +
  ggsave(paste0(savefolder, "size_vs_percentage.png"),
                width=7, height=14)


