healthcheckScores <- tribble(~Date,~Squad,~Player,~Area,~Score,
        "2018-05-24","Blue",1,"Easy to release",1,
        "2018-05-24","Blue",2,"Easy to release",2,
        "2018-05-24","Blue",3,"Easy to release",1,

        "2018-05-17","Blue",1,"Easy to release",2,
        "2018-05-17","Blue",2,"Easy to release",3,
        "2018-05-17","Blue",3,"Easy to release",1,

        "2018-05-24","Red",1,"Easy to release",2,
        "2018-05-24","Red",2,"Easy to release",2,
        "2018-05-24","Red",3,"Easy to release",2,

        "2018-05-17","Red",1,"Easy to release",2,
        "2018-05-17","Red",2,"Easy to release",1,
        "2018-05-17","Red",3,"Easy to release",2,

        "2018-05-24","Blue",1,"Value",3,
        "2018-05-24","Blue",2,"Value",3,
        "2018-05-24","Blue",3,"Value",3,

        "2018-05-17","Blue",1,"Value",3,
        "2018-05-17","Blue",2,"Value",2,
        "2018-05-17","Blue",3,"Value",3,

        "2018-05-24","Red",1,"Value",1,
        "2018-05-24","Red",2,"Value",2,
        "2018-05-24","Red",3,"Value",3,

        "2018-05-17","Red",1,"Value",1,
        "2018-05-17","Red",2,"Value",2,
        "2018-05-17","Red",3,"Value",3,

        "2018-05-24","Green",1,"Value",3,
        "2018-05-24","Green",2,"Value",2,
        "2018-05-24","Green",3,"Value",3,

        "2018-05-24","Green",1,"Teamwork",2,
        "2018-05-24","Green",2,"Teamwork",2,
        "2018-05-24","Green",3,"Teamwork",1



) %>% mutate(Date = as.Date(Date))

healthcheckDF <- healthcheckScores %>% group_by(Squad, Area, Date) %>% summarise(
  Spread = sd(Score),
  upper = max(Score),
  lower = min(Score),
  Score = mean(Score)
)
healthcheckDFTrend <- healthcheckDF %>% mutate(prevScore = lag(Score)) %>%
  group_by(Squad, Area, Score, Spread, Date) %>% summarise(
    Trend = sign(Score - prevScore ),
  #  Trend = scales::rescale(x=(Score - prevScore), from=c(-3,3), to=c(-1,1))
    )

ggplot(healthcheckDF, aes(y=Area, x=Squad)) +
  geom_point(data=(healthcheckDF %>% ungroup() %>% filter(Date == max(Date))), size=15, aes(color=Score, alpha=Spread)) +
  scale_color_distiller(type="div", palette = "RdYlGn", direction = 1,limits=c(1,3)) + scale_alpha_continuous(range=c(0.5,1))


ggplot(healthcheckDFTrend %>% ungroup() %>% filter(Date == max(Date)), aes(y=Area, x=Squad)) +
  geom_spoke(aes(angle=Trend*(pi/3)), radius=0.4, size = 2, lineend="round", arrow=arrow(length=unit(0.5, "cm"), angle=40, type="open")) +
  geom_point(size=17, color="black") +
  geom_point(size=15, aes(color=Score)) +
  scale_color_distiller(type="div", palette = "RdBu", direction = 1,limits=c(1,3), guide="colorbar") +
  scale_alpha_continuous(range=c(0.3,1), guide=F) +
  coord_equal() +
  theme_dark() + scale_x_discrete(position="top") +
  theme(plot.background = element_rect(fill="grey40"),
        panel.background = element_rect(fill="grey40"),
        panel.grid.major.x = element_blank(),
        text=element_text(color="white", size = 15),
        axis.text = element_text(color="white"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dotted", size=1),
        axis.ticks = element_blank(),
        legend.position="bottom",
        legend.background = element_blank(),
        legend.margin = margin(-18,0,4,0),
        legend.title = element_text(size = rel(0.8)),
        legend.title.align = 0,
        legend.text = element_text(size = rel(0.6)),
        legend.justification = c(0.95,0.95))
ggsave("~/Desktop/squadHealthCheck.png", width=2.2 + (length(healthcheckDF$Squad %>% unique())*1.1), height=1.6+(length(healthcheckDF$Area %>% unique())*1.08), dpi=300)

filterSquad <- "Blue"
ggplot(healthcheckDFTrend %>% mutate(highlight = Squad == filterSquad), aes(x=Date, y=Score, group=Squad, color=highlight)) + geom_line() + facet_grid(Area ~ .) +
  scale_y_continuous(limits=c(0,3)) + scale_x_date() +
  theme_dark() + scale_color_manual(values = c("TRUE" = "#ef8a62", "FALSE" = "grey30"), guide=F) +
  theme(plot.background = element_rect(fill="grey40"),
        panel.background = element_rect(fill="grey40", color = "black"),
        panel.grid.major.x = element_blank(),
        text=element_text(color="white", size = 15),
        axis.text = element_text(color="white"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dotted", size=1),
        #panel.grid.major.x = element_line(colour = "grey50", linetype = "dotted", size=1),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(0.8, "cm")) +
    geom_jitter(data=healthcheckScores %>% mutate(highlight = Squad == filterSquad), width=0.4, height=0.1) +
    geom_errorbar(data=healthcheckDF %>% filter(Squad == filterSquad) %>% mutate(highlight = Squad == filterSquad), aes(ymax = upper, ymin = lower), width=0.8) + ggtitle(paste0(filterSquad, " Squad Healthcheck over time"))
