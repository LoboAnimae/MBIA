library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
load(file = "clean.Rdata")
glimpse(data)

one_million <- 1000000

honey_tendency_national <- data %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total_produced_millions = sum(totalprod) / one_million)

honey_tendency_state <- data %>%
  group_by(state, year) %>%
  arrange(year) %>%
  summarize(total_produced_millions = sum(totalprod) / one_million)


ggplot(honey_tendency_national, aes(x = year, y = total_produced_millions, group = 1)) +
  geom_line() +
  expand_limits(y = 0) +
  ggtitle("National Honey Production in Millions of Pounds") +
  xlab("Year") +
  ylab("Millions of Pounds of Honey Produced")


ggplot(honey_tendency_state, aes(x = year, y = total_produced_millions, color = state)) +
  geom_line() +
  expand_limits(y = 0) +
  ggtitle("Statal Honey Production in Millions of Pounds") +
  xlab("Year") +
  ylab("Millions of Pounds of Honey Produced") +
  geom_label_repel(aes(label = state),
    nudge_x = 1,
  )
