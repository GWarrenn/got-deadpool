## Author: August Warren
## Description: Game of Thrones Deadpool -- Kings Landing
## Date: 5/14/2019
## Status: Published
## Specs: R version 3.3.2 (2016-10-31)

library(tidyverse)
library(googlesheets)
library(curl)
library(reshape2)
library(ggbeeswarm)
library(scales)
library(viridis)
library(gridExtra)
library(grid)

gs_ls()

got_doc <- gs_title("GoT Battle of Westeros Deadpool (Responses)")

gs_ws_ls(got_doc)

deadpool_data <- gs_read(ss = got_doc)

columns <- colnames(deadpool_data)

columns <- sub(x=columns,pattern = "What will become of the following characters\\? \\[(.*)\\]",
               replacement = "\\1")

colnames(deadpool_data) <- columns

characters <- c("Email Address","Jon Snow","Sansa Stark","Arya Stark","Dany Targaryen","Tyrion Lannister","Jamie Lannister",
                "Bran Stark","Ser Brienne of Tarth","Podrick Payne","Tormund Giantsbane","Davos Seaworth","Greyworm",
                "Sam Tarly","Gilly","Sandor Clegane -- The Hound","Gendry",
                "Lord Varys","Drogon","Cersei Lannister","Gregor Clegane -- The Mountain",
                "Qyburn","Euron Greyjoy","Bronn","Yara Greyjoy")

predictions <- deadpool_data %>%
  select(characters) %>%
  rename(email = `Email Address`)

predictions_long <- melt(predictions,id.vars = "email")

predictions_long$value <- gsub(x=predictions_long$value,pattern = "(Live|Die).*",replacement = "\\1")

live_die_pct <- predictions_long %>%
  group_by(email,value) %>%
  summarise(n=n()) %>%
  mutate(percent = n/(length(characters)-1))

live_die_pct_w <- dcast(data = live_die_pct,formula = email ~ value,value.var = c("percent"))

rip <- c("Qyburn","Euron Greyjoy","Lord Varys","Sandor Clegane -- The Hound","Cersei Lannister","Gregor Clegane -- The Mountain","Jamie Lannister")

predictions_long <- predictions_long %>%
  mutate(actual = ifelse(variable %in% rip,"Die","Live"),
         outcome = ifelse(actual == value,"Correct","Incorrect"))

predictions_long$sup_email <- predictions_long$email
predictions_long$sup_email <- gsub(x = predictions_long$sup_email,pattern = "@.*",replacement = "")

substr(predictions_long$sup_email,6,10) <- "~    "

predictions_long$sup_email <- gsub(x = predictions_long$sup_email,pattern = "    ",replacement = "")

export_predictions_long <- predictions_long %>%
  select(variable,value,actual,outcome,sup_email)

write.csv(export_predictions_long,file = "got-deadpool\\data\\predictions_westeros.csv")

outcomes <- predictions_long %>%
  group_by(email,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/(length(characters)-1))

outcomes_w <- dcast(data = outcomes,formula = email ~ outcome,value.var = c("percent"))

outcomes_w$sup_email <- outcomes_w$email
outcomes_w$sup_email <- gsub(x = outcomes_w$sup_email,pattern = "@.*",replacement = "")

substr(outcomes_w$sup_email,6,10) <- "~    "

outcomes_w$sup_email <- gsub(x = outcomes_w$sup_email,pattern = "    ",replacement = "")

export_outcomes <- outcomes_w %>%
  select(sup_email,Correct) %>%
  mutate(rank = rank(-Correct, ties.method ="min"))

write.csv(export_outcomes,file = "got-deadpool\\data\\outcome_table_westoros.csv")

character_outcomes <- predictions_long %>%
  group_by(variable,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/44)

character_outcomes_w <- dcast(data = character_outcomes,formula = variable ~ outcome,value.var = c("percent"))

character_outcomes_w <- character_outcomes_w %>%
  mutate(actual = ifelse(variable %in% rip,"Die","Live"))

character_outcomes_plot <- ggplot(character_outcomes_w,aes(x=reorder(variable,-Correct),y=Correct,fill=actual)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(reorder(variable,-Correct),y=Correct,label=percent(round(Correct,2))),hjust=-.25) +
  theme(axis.title.y = element_blank(),legend.position = "bottom") +
  scale_fill_manual(values = c("#ff9197","#90f98e")) +
  scale_y_continuous(labels = scales::percent,expand = c(0,0),limits = c(0,1.05)) +
  labs(y="% Fate Correctly Predicted",fill="Actual Fate",
       title="GoT Battle of Kings Landing Deadpool: % Correctly Predicted",
       caption="Data from 44 Game of Thrones Deadpool Predictions | Graph by August Warren") +
  coord_flip() 

ggsave(plot = character_outcomes_plot, "got-deadpool\\results\\character_outcomes_westeros.png", w = 10.67, h = 8,type = "cairo-png")

################################################################
##
## comparison of predicted % dead vs. % correct  
##
################################################################

actual_dead <- round(length(rip)/(length(characters)-1),digits = 2)

outcomes_w_live_die <- merge(outcomes_w,live_die_pct_w,by="email")

model <- lm(data = outcomes_w_live_die,formula = Correct ~ Die)

outcomes_w_live_die_plot <- ggplot(outcomes_w_live_die,aes(x=Die,y=Correct)) +
  ##geom_point(size=4,alpha=.3) +
  geom_beeswarm(size=4,cex=1,groupOnX=FALSE,alpha=.75,color="#DC143C") +
  geom_smooth(method = "lm",size=2) +
  geom_vline(xintercept = actual_dead) +
  scale_x_continuous(labels=scales::percent,limits = c(0,1)) +
  scale_y_continuous(labels=scales::percent,limits = c(0,1)) +
  geom_text(aes(.75,0,label = paste("R-Squared: ",as.character(round(summary(model)$r.squared,3))), hjust = 0)) +
  geom_text(aes(actual_dead,0,label = paste("Actual % Dead: ",as.character(percent(actual_dead))), hjust = 0)) +
  labs(y="Percent of Fates Correctly Predicted",
       x="Percent of Characters Predicted to Die")
       ##title="GoT Deadpool: % of Characters Predicted to Die vs. % Correctly Predicted",
       ##caption="Data from Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren")

################################################################
##
## comparison of predicted % dead vs. actual  
##
################################################################

dead_vs_actual_hist_plot <- ggplot(outcomes_w_live_die,aes(x=Die)) +
  geom_histogram(binwidth = .05,color="black",fill="#DC143C") +
  geom_vline(xintercept = actual_dead) +
  geom_vline(xintercept = mean(outcomes_w_live_die$Die)) +
  geom_text(aes(mean(outcomes_w_live_die$Die),11,label = paste("Avg. Predicted \n% Dead: ",as.character(percent(round(mean(outcomes_w_live_die$Die),2)))), hjust = 0)) +
  geom_text(aes(actual_dead,11,label = paste("Actual \n% Dead: ",as.character(percent(actual_dead))), hjust = 1)) +
  scale_x_continuous(labels=scales::percent) +
  labs(x="Percent of Character's Predicted to Die",
       y="Frequency")
       ##title="GoT Deadpool: % of Characters Predicted to Die vs. % Correctly Predicted",
       ##caption="Data from Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren")

fates_plots <- grid.arrange(outcomes_w_live_die_plot, dead_vs_actual_hist_plot, nrow = 1,
                            top="GoT Battle of Kings Landing Deadpool: % of Characters Predicted to Die vs. % Correctly Predicted",  
                            bottom = textGrob(
                              "Data from 44 Game of Thrones Deadpool Predictions | Graph by August Warren",
                              gp = gpar(fontface = 3, fontsize = 9),
                              hjust = 1,
                              x = 1
                            )
                          )

ggsave(plot = fates_plots, "got-deadpool\\results\\fates_plots_westeros.png", w = 12, h = 8,type = "cairo-png")

################################################################
##
## % correct by demographics
##
################################################################

demos <- deadpool_data %>%
  select(`Email Address`,`To which gender do you most closely identify?`,`Have you read any of the books (Song of Ice and Fire)?`) %>%
  rename(gender = `To which gender do you most closely identify?`,
         books = `Have you read any of the books (Song of Ice and Fire)?`,
         email = `Email Address`)

demos$gender <- ifelse(is.na(demos$gender),"Female",demos$gender)
demos$books <- ifelse(is.na(demos$books),"Yes",demos$books)

demos_predictions <- merge(predictions_long,demos,by="email")

gender <- demos_predictions %>%
  group_by(gender,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/sum(n)) %>%
  filter(gender!= "Two of us: one male one female") %>% ##goddamnit mika and chris 
  rename(subgroup = gender)

gender$demo <- "Gender"

books <- demos_predictions %>%
  group_by(books,outcome) %>%
  summarise(n=n()) %>%
  mutate(percent=n/sum(n)) %>%
  rename(subgroup = books)

books$demo <- "Read the Books"

combined_demos <- rbind(gender,books)

combined_demos$subgroup <- factor(combined_demos$subgroup,levels = c("Yes","No","Male","Female"))

demo_plot <- ggplot(combined_demos,aes(x=subgroup,y=percent,fill=outcome)) +
  geom_bar(position=position_dodge(),stat="identity",color="black") +
  geom_text(aes(label = percent(round(percent,2))),position = position_dodge(width = 1),vjust=-.5) +
  facet_wrap(~demo,scales = "free") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("#90f98e","#ff9197")) +
  theme(axis.title.x = element_blank(),legend.position = "bottom") +
  labs(y="Percent of Fates Correctly Predicted",
       title="GoT Deadpool: % of Fates Predicted by Demographics",
       fill="Prediction",
       caption="Data from 34 Game of Thrones Battle of Winterfell Deadpool Predictions | Graph by August Warren")

ggsave(plot = demo_plot, "got-deadpool\\results\\prev_week_comparison.png", w = 10.67, h = 8,type = "cairo-png")

################################################################
##
## Bring in previous week's data to compare
##
################################################################

prev_week <- read.csv("got-deadpool\\data\\outcome_table.csv")

merged <- merge(export_outcomes,prev_week,by = "sup_email")

merged$diff_ranking <- merged$rank.y - merged$rank.x

prev_week_comparison <- ggplot(merged,aes(x=Correct.y,y=Correct.x)) +
  geom_beeswarm(size=7,cex=1,groupOnX=FALSE,alpha=.75,aes(fill=diff_ranking),colour="black",pch=21) +
  geom_abline() +
  scale_x_continuous(limits = c(0,1),labels=scales::percent) +
  scale_y_continuous(limits = c(0,1),labels=scales::percent) +
  scale_fill_gradient2(high = "#00E51A", mid = "#FFFE2E", low = "#F5000F") +
  theme(legend.position="bottom") +
  labs(x="Battle of Winterfell % Correct",
       y="Battle of Kings Landing % Correct",
       fill="Change in Ranking",
      title="GoT Deadpools % Correct: Winterfell vs. Kings Landing",
      caption="Data from 23 Game of Thrones Battle of Winterfell & Kings Landing Deadpool Predictions | Graph by August Warren")

ggsave(plot = prev_week_comparison, "got-deadpool\\results\\prev_week_comparison.png", w = 10.67, h = 8,type = "cairo-png")
