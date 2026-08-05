#Vergelijking BTM en thematic analysis

#probleem dat thematic analysis meerdere topics kan hebben terwijl BTM maar eentje kan hebben
#We gaan de quotes die meerdere thematic topics hebben, vermenigvuldigen met het aantal keer dat bij een topic past
#Op die manier kunnen we een Sankey diagram maken waarbij een duidelijk begin en einde is. 

data <- read.csv("Vergelijking_BTM_thematic.csv", sep = ";")
View(data)

library(dplyr)
library(tidyr)

data_long <- data %>%
  pivot_longer(
    cols = c(
      Urban.traffic.dynamics,
      Public.space.and.maintenance,
      Public.transport.accessibility,
      Neighbourhoods.and.social.dynamics,
      Nothing
    ),
    names_to = "topic_name",
    values_to = "is_true"
  ) %>%
  filter(is_true) %>%
  mutate(
    Topic_nr_thematic = case_when(
      topic_name == "Urban.traffic.dynamics" ~ 1,
      topic_name == "Public.space.and.maintenance" ~ 2,
      topic_name == "Public.transport.accessibility" ~ 3,
      topic_name == "Neighbourhoods.and.social.dynamics" ~ 4,
      topic_name == "Nothing" ~ 0
    )
  ) %>%
  select(-is_true, -topic_name)


write.csv(data_long, "Vergelijking_BTM_thematic_with_numbers.csv", row.names = FALSE)


#Sankey diagram
library(dplyr)

data_long <- data_long %>%
  mutate(assigned_topic_BTM = ifelse(is.na(assigned_topic_BTM), 0, assigned_topic_BTM))

flows <- data_long %>%
  group_by(assigned_topic_BTM, Topic_nr_thematic) %>%
  summarise(n = n(), .groups = "drop")

flows

flows <- flows %>%
  rename(
    source = assigned_topic_BTM,
    target = Topic_nr_thematic,
    value = n
  )

#install.packages("networkD3")
library(networkD3)

# nodes maken
nodes <- data.frame(
  name = unique(c(flows$source, flows$target))
)

nodes <- nodes %>%
  mutate(NodeGroup = rep(0:4, length.out = n()))

nodes <- nodes %>%
  mutate(NodeID = rep(0:9, length.out = n()))

# index mapping
flows$source_id <- match(flows$source, nodes$name) - 1
flows$target_id <- match(flows$target, nodes$name) - 1

flows$source <- paste("BTM", flows$source)
flows$target <- paste("Thematic analysis", flows$target)

flows$target_id <-
    flows$target_id + 5

#kleurtjes
library(htmlwidgets)

colour_scale <- JS(
  'd3.scaleOrdinal()
    .domain(["0", "1", "2", "3", "4"])
    .range(["#999999", "#E97132", "#196B24", "#7570b3", "#0F9ED5"])'
)


flows$LinkGroup <- as.character(flows$source_id)

nodes$label <- c(
  "BTM No changes",
  "BTM Urban traffic dynamics",
  "BTM Public space and maintenance",
  "BTM Public transport accessibility",
  "BTM Neighbourhoods and social dynamics",
  "Thematic analysis No changes",
  "Thematic analysis Urban traffic dynamics",
  "Thematic analysis Public space and maintenance",
  "Thematic analysis Public transport accessibility",
  "Thematic analysis Neighbourhoods and social dynamics"
  
)

sankeyNetwork(
  Links = flows,
  Nodes = nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "label",
  fontSize = 22,
  fontFamily = "Arial",
  NodeGroup = "NodeGroup",
  colourScale = colour_scale,
  nodeWidth = 30,
  LinkGroup = "LinkGroup"
)

write.csv(flows, "Flows_Sankey.csv", row.names = FALSE)
