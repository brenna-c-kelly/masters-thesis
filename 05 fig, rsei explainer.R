

rsei_eg <- read.csv("data/rsei scores example.csv")

rsei_eg <- rsei_eg %>%
  filter(County %in% c("MONTGOMERY, VA", "CUYAHOGA, OH",
                       "WAYNE, MI", "SCHUYLKILL, PA"))


rsei_eg$RSEI.Score <- as.numeric(gsub(",", "", rsei_eg$RSEI.Score))
rsei_eg$RSEI.Score <- as.numeric(rsei_eg$RSEI.Score)
rsei_eg$RSEI.Score_log <- log(rsei_eg$RSEI.Score)

rsei_eg <- rsei_eg[order(rsei_eg$RSEI.Score, decreasing = TRUE), ]

ggplot(rsei_eg, mapping = aes(x = Submission.Year, y = RSEI.Score, fill = County)) +
  geom_bar(position="dodge", stat = "identity") +
  #scale_y_continuous(name ="RSEI Score (log)",
  #                   breaks=c(0, 1, 5, 10, 15),
  #                   labels=c("0" = "1", "1" = "2.7", "5" = "148",
  #                            "10" = "22,026", "15" = "3,269,017")) +
  scale_x_continuous(name ="Year",
                     breaks = c(2011, 2014, 2017, 2020)) +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_bw()






