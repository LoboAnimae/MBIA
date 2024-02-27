library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
load(file = "clean.Rdata")
glimpse(data)
file_path <- "./plots"
one_million <- 1000000

save <- function(name) {
  ggsave(name,
    path = file_path,
    width = 4.5,
    height = 4.5,
    dpi = 300,
    units = "in",
    device = "png"
  )
}

honey_tendency_national <- data %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total_produced_millions = sum(totalprod) / one_million)

honey_tendency_state <- data %>%
  group_by(state, year) %>%
  arrange(year) %>%
  summarize(total_produced_millions = sum(totalprod) / one_million)

honey_tendency_north_dakota <- data %>%
  group_by(state, year) %>%
  filter(state == "ND") %>%
  arrange(year) %>%
  summarize(total_produced_millions = sum(totalprod) / one_million)

honey_tendency_california <- data %>%
  group_by(state, year) %>%
  filter(state == "CA") %>%
  arrange(year) %>%
  summarize(total_produced_millions = sum(totalprod) / one_million)


ggplot(honey_tendency_national, aes(x = year, y = total_produced_millions, group = 1)) +
  geom_line() +
  geom_smooth(method = lm) +
  expand_limits(y = 0) +
  ggtitle("National Honey Production in Millions of Pounds") +
  xlab("Year") +
  ylab("Millions of Pounds of Honey Produced") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save("national_tendency.png")

ggplot(honey_tendency_state, aes(x = year, y = total_produced_millions, color = state, group = state)) +
  geom_line(show.legend = FALSE) +
  expand_limits(y = 0) +
  ggtitle("Statal Honey Production in Millions of Pounds") +
  xlab("Year") +
  ylab("Millions of Pounds of Honey Produced")

save("states_tendency.png")



ggplot(honey_tendency_north_dakota, aes(x = year, y = total_produced_millions, color = state, group = state)) +
  geom_line(color = "#005100") +
  expand_limits(y = 0) +
  ggtitle("North Dakota Honey Production in Millions of Pounds") +
  xlab("Year") +
  ylab("Millions of Pounds of Honey Produced") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save("states_tendency_north_dakota.png")


ggplot(honey_tendency_california, aes(x = year, y = total_produced_millions, color = state, group = state)) +
  geom_line(color = "#960606") +
  expand_limits(y = 0) +
  ggtitle("California Honey Production in Millions of Pounds") +
  xlab("Year") +
  ylab("Millions of Pounds of Honey Produced") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save("states_tendency_california.png")

year_1998 <- data %>%
  filter(year == 1998) %>%
  group_by(state) %>%
  arrange(desc(totalprod)) %>%
  summarize(total_revenue_millions = sum(totalprod) / one_million)

ggplot(year_1998, aes(x = reorder(state, total_revenue_millions), y = total_revenue_millions, fill = state)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  expand_limits(y = 0) +
  ggtitle("Total Revenue for Honey in 1998 per State") +
  xlab("State") +
  ylab("Value of Honey Produced in Millions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

save("states_revenue_1998.png")
