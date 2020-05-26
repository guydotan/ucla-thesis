library(ggplot2)
library(dplyr)
library(scales)

df <- data.frame( matrix(c("Table Games",
                           "Slots",
                           "Sportsbooks"	,
                           4127214,
                           7988409,395411),nrow = 3,2,byrow = F),stringsAsFactors = F)
names(df) <- c("Game","Revenue")
df$Revenue <- as.numeric(df$Revenue)

data <- df
# Compute the position of labels

data <- data %>% 
  arrange(desc(Game)) %>%
  mutate(prop = Revenue / sum(data$Revenue) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

mycols <- hue_pal()(4)#c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data, aes(x = 1.75, y = prop, fill = Game)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = ypos+1.25, label = paste0(format(round(prop,1),nsmall=1),'%')), 
            color = "white", size = 5, fontface = 'bold') +
  #scale_fill_manual(values = alpha(mycols ,1.00) ) +
  #scale_fill_hue() + 
  scale_alpha(0.85)+
  scale_fill_ucla("three")+
  ggtitle("Nevada Revenue Breakdown by Gaming Source") +
  theme_void() + 
  theme(legend.title= element_blank(), legend.position = "bottom", 
        legend.text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  xlim(.2,2.5)



### Break even points
df <- c()
probW <- seq(.475,.575,0.005)
df$probW <- probW
df <- data.frame(df)
df$EV <- 100*df$probW - 110*(1-probW)
df$ROI <- df$EV/110

scaleFUN <- function(x) sprintf("%.1f%%", x*100)

ggplot(data=df, aes(x=probW, y=ROI, group=1)) +
  #geom_point(color="#2774AE") +
  geom_vline(xintercept=0.524, linetype="dashed", 
             color = "#FFD100", size = 1.05) +
  geom_hline(yintercept=0,  
             color = "dark grey") +
  geom_line(color="#2774AE", size = 1.05)+
  geom_point(aes(x=0.5238, y=0), color="#2774AE", size = 2) +
  scale_x_continuous(labels = scaleFUN, breaks=seq(.48,.57,.01)) +
  xlab("Win Percentage") +
  ylab("ROI") +
  ggtitle("Break Even Point on -110 Moneyline Odds") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title =  element_text(size=14),
        axis.text = element_text(size=14)
        )
  
