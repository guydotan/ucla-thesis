## exploratory data analysis ##

setwd("~/Documents/UCLA MAS/Thesis/repo-local/uclathesis/")
library("ggplot2")
library("dplyr")
library("ggimage")
library("egg")
library("reshape2")
library("ggpmisc")


nba <- read.csv("data/nba_adv_complete.csv", stringsAsFactors = F, na.strings = "")
champs <-  read.csv("data/id-mapping/nba-champs.csv", stringsAsFactors = F, na.strings = "")

nba <- nba[nba$typeSeason == "Regular Season",]
names(champs)[5] <- "idTeam"

nba$dateGame <- as.Date(nba$dateGame)

# compare pace by seasons
season_avgs <- nba %>% 
  group_by(slugSeason) %>% 
  summarise(
    possessions = mean(possessions.own, na.rm = T),
    pace = mean(pace, na.rm = T),
    fg3a = mean(fg3aTeam.own, na.rm = T),
    fg2a = mean(fg2aTeam.own),
    fga = mean(fgaTeam.own, na.rm = T),
    ppg = mean(ptsTeam.own, na.rm = T),
    reb = mean(trebTeam.own, na.rm = T),
    ortg = sum(ptsTeam.own, na.rm = T)/sum(possessions.own, na.rm = T)*100
  )

melted_season <- melt(season_avgs, id = "slugSeason")



# scatter plot - pace
g1 <- ggplot(nba[nba$locationGame=='H',], aes(x = dateGame, y = pace)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  ggtitle("Pace per Game (2007-2020) - Scatterplot") + 
  xlab("") +
  ylab("Pace") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size=12))

# box plots - pace
g2 <- ggplot(nba[nba$locationGame=='H',], aes(x=slugSeason, y=pace)) + 
  geom_boxplot() +
  xlab("") +
  ggtitle("Pace per Game (2007-2020) - Box Plot") + 
  ylab("Pace")+ 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size=12))
#geom_image(data = c, aes(image = logo), size = 0.05, by = "height",asp = 1)

ggarrange(g1, g2)

melted_season$variable <- ordered(melted_season$variable, levels = c("ppg","fga","reb","fg3a","possessions","pace","ortg"))

# line plots
lp <- ggplot(filter(melted_season, variable == "fg3a" | variable == "ppg" | variable == "fga" | variable == "reb")
             ,  aes(x=slugSeason, y=value, group=variable)) +
        geom_line(aes(color=variable), alpha = 0.8, size = 1.5) +
        geom_point(aes(color=variable), alpha = 0.8, size = 4) +
        theme(legend.title=element_blank()) +
        scale_color_manual(values = c("#8BB8E8","#003B5C", "#FFD100","#808080"),
                           labels = c("Pts", "FGA", "Reb", "3PA")) +
        xlab("") +
        ylab("Team Average per Game") +
        ggtitle("League per Game Averages for Traditional Box Score Stats by Season (2007-2020)") + 
        theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12))
lp


# 
# # box plots - 3pa
# ggplot(team_avgs , aes(x=slugSeason, y=fg3a)) + 
#   geom_boxplot() +
#   xlab("") +
#   ggtitle("3pa per Game (2007-2020) - Boxplot") + 
#   ylab("Pace")+ 
#   theme(plot.title = element_text(hjust = 0.5))
# #geom_image(data = c, aes(image = logo), size = 0.05, by = "height",asp = 1)
# 


## 
# comparing PPG season to season
nba$fga_per100 <- nba$fgaTeam.own/nba$possessions.own

d1 <- ggplot(data=filter(nba, slugSeason == "2007-08" | slugSeason == "2019-20"), 
             aes(x=ptsTeam.own, group=slugSeason, fill=slugSeason)) +
  geom_density(adjust=1.5, alpha=.5) +
  theme(legend.position = "none") +
  xlab("") +
  ggtitle("Distribution of Team Single Game Scoring") + 
  theme(plot.title = element_text(hjust = 0.6)) +
  scale_fill_ucla("main")
  
d1

# comparing ORtg season to season
d2 <- ggplot(data=filter(nba, slugSeason == "2007-08" | slugSeason == "2019-20"), 
             aes(x=offRtg, group=slugSeason, fill=slugSeason)) +
  geom_density(adjust=1.5, alpha=.5) +
  theme(legend.position = "none") +
  xlab("") +
  ggtitle("Distribution of Points per 100 Possessions") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_ucla("main")
d2

# comparing ORtg season to season
d3 <- ggplot(data=filter(nba, slugSeason == "2007-08" | slugSeason == "2019-20"), 
             aes(x=fgaTeam.own, group=slugSeason, fill=slugSeason)) +
  geom_density(adjust=1.5, alpha=.5) +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab("") +
  ggtitle("Distribution of Field Goal Attempts") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12)) +
  scale_fill_ucla("main")
d3

# comparing ORtg season to season
d4 <- ggplot(data=filter(nba, slugSeason == "2007-08" | slugSeason == "2019-20"), 
             aes(x=fga_per100, group=slugSeason, fill=slugSeason)) +
  geom_density(adjust=1.5, alpha=.5) +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab("") +
  ggtitle("Distribution of Field Goal Attempts per 100 Possessions") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12)) +
  scale_fill_ucla("main")
d4

ggarrange(d1, d2, d3, d4, byrow = F)

# comparing possesions season to season
# d3 <- ggplot(data=filter(nba, slugSeason == "2007-08" | slugSeason == "2019-20"), 
#              aes(x=possessions.own, group=slugSeason, fill=slugSeason)) +
#   geom_density(adjust=1.5, alpha=0.5) +
#   theme(legend.title=element_blank()) +
#   xlab("") +
#   ggtitle("Distribution of Team Single Game Possesions") + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_ucla("main")
# d3



# ortg with champs

team_avgs <- nba %>% 
  group_by(slugSeason, idTeam, slugTeam) %>% 
  summarise(
    possessions = mean(possessions.own, na.rm = T),
    pace = mean(pace, na.rm = T),
    ortg = sum(ptsTeam.own)/sum(possessions.own)*100,
    drtg = sum(ptsTeam.opp)/sum(possessions.opp)*100,
    nrtg = sum(ptsTeam.own)/sum(possessions.own)*100 - sum(ptsTeam.opp)/sum(possessions.opp)*100,
    ppg = mean(ptsTeam.own),
    fg3a = mean(fg3aTeam.own),
    logo = max(urlTeamSeasonLogo.own),
    wins = sum(isWin),
    winpct = sum(isWin)/n()
  )

team_avgs <- left_join(team_avgs , champs[,c(1,5,4)] , by = c("idTeam" , "slugSeason"))
team_avgs$Champion <- ifelse(is.na(team_avgs$Champion), 0, 1)

team_avgs$slugSeason <- as.factor(team_avgs$slugSeason)

c <- team_avgs[team_avgs$Champion == 1,]

gg_ortg <- ggplot(team_avgs, aes(x = slugSeason, y = ortg)) +
  geom_point(aes(x=slugSeason, color = winpct), size = 4) +
  geom_image(data = c, aes(image = logo), size = 0.05, by = "height",asp = 1) +
  scale_color_continuous(name="Team Win %")+
  xlab("") +
  ylab("Offensive Rating") +
  ggtitle("Team Offensive Rating by Season") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# drtg with champs
gg_drtg <- ggplot(team_avgs, aes(x = slugSeason, y = drtg)) +
  geom_point(aes(x=slugSeason, color = winpct), size = 4) +
  geom_image(data = c, aes(image = logo), size = 0.05, by = "height",asp = 1) +
  scale_color_continuous(name="Team Win %")+
  xlab("") +
  ylab("Defensive Rating") +
  ggtitle("Team Defensive Rating by Season") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# nrtg with champs
gg_nrtg <- ggplot(team_avgs, aes(x = slugSeason, y = nrtg)) +
  geom_point(aes(x=slugSeason, color = winpct), size = 4) +
  geom_image(data = c, aes(image = logo), size = 0.05, by = "height",asp = 1) +
  scale_color_continuous(name="Team Win %")+
  xlab("") +
  ylab("Net Rating") +
  ggtitle("Team Net Rating by Season") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggarrange(gg_ortg, gg_drtg, gg_ntrg, ncol = 1, byrow = F)

#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#'
ucla_colors <- c(
  #`red`        = "#d11141",
  `green`      =  "#00FF87",
  `blue`        = "#2774AE",
  #`orange`     = "#f37735",
  `yellow`     =  "#FFD100",
  `light grey` = "#808080",
  `dark blue`  = "#003B5C"
  )

ucla_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ucla_colors)
  
  ucla_colors[cols]
}

ucla_palettes <- list(
  `main`  = ucla_cols("blue", "yellow"),#, "light grey")
  `three` = ucla_cols("blue","yellow","light grey"),
  `four` = ucla_cols("blue","yellow","dark blue","light grey")
)

ucla_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ucla_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_ucla <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ucla_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ucla_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


scale_fill_ucla <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ucla_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ucla_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


### correlation matrix

#install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(nba[,c(23:29,31:45,91,93)], use = "complete.obs"), 1)
ggcorrplot(corr, type = 'lower', show.diag = T, method = "circle",#hc.order = T, 
              #title = "NBA Box Score Metrics Correlations",
              colors = c("#2774AE" ,"#808080", "#FFD100"))  + 
              theme(plot.title = element_text(hjust = 0.5, size = 14),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))


# historam matrix
melted_nba <- melt(nba[,c(23:29,31:45,91,93)])
#Then you can facet your ggplot by variable and automatically create separate histograms for each dimension.

library(tidyr)
library(ggplot2)

nba_v <- nba[,c(21,23:29,31:45,91,93)] %>% gather(-outcomeGame , key = 'var', value = 'value')#%>% head()

nba_v <- nba_v[complete.cases(nba_v$outcomeGame),]

nba_v %>%
  ggplot(aes(x = value, group=outcomeGame, fill=outcomeGame)) +
    geom_density(alpha = 0.5) + 
    facet_wrap(~var, scales = 'free') +
     scale_fill_ucla("main")


# EDA
# View(nba_ytd[nba_ytd$teamGameNumNoPost==82,])
# nba$recVsML <- ifelse(nba$teamWinProb < .5 & nba$isWin == T , 1 , 0)
# nba19 <- nba[nba$yearSeason == 2019,]
# 
# nba19 <- nba19 %>%
#   group_by(idTeam) %>%
#   mutate(group_sum = sum(recVsML))
# 
# View(nba19[,c(15,99)])
