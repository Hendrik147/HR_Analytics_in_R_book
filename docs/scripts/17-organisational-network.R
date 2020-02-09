## ----organisational-network, include=FALSE------------------------------------
chap <- 18
lc <- 0
rq <- 0
# **`r paste0("(LC", chap, ".", (lc <- lc + 1), ")")`**
# **`r paste0("(RQ", chap, ".", (rq <- rq + 1), ")")`**

knitr::opts_chunk$set(
  tidy = FALSE, 
  out.width = '\\textwidth', 
  fig.height = 4,
  warning = FALSE
  )

options(scipen = 99, digits = 3)

# Set random number generator see value for replicable pseudorandomness. Why 76?
# https://www.youtube.com/watch?v=xjJ7FheCkCU
set.seed(76)


## ----include=FALSE------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidygraph)) install.packages("tidygraph")
if(!require(ggraph)) install.packages("ggraph")
if(!require(igraph)) install.packages("igraph")
if(!require(visNetwork)) install.packages("visNetwork")


## -----------------------------------------------------------------------------
library(tidyverse) # tidy data analysis
library(tidygraph) # tidy graph analysis
library(ggraph)    # for plotting
library(igraph)    # for plotting
library(visNetwork) # for visualising graph


## ----eval=FALSE---------------------------------------------------------------
## cooc_all_edges <- read_csv("https://hranalytics.netlify.com/data/asoiaf-all-edges.csv")


## ----read_data_ona, echo=FALSE, warning=FALSE, message=FALSE------------------
cooc_all_edges <- read_csv("data/asoiaf-all-edges.csv")


## -----------------------------------------------------------------------------
main_ch <- cooc_all_edges %>%
  select(-Type) %>%
  gather(x, name, Source:Target) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(weight)) %>%
  ungroup()

main_ch_l <- main_ch %>%
  arrange(desc(sum_weight)) %>%
  top_n(50, sum_weight)
main_ch_l


## -----------------------------------------------------------------------------
cooc_all_f <- cooc_all_edges %>%
  filter(Source %in% main_ch_l$name & Target %in% main_ch_l$name)


## -----------------------------------------------------------------------------
as_tbl_graph(cooc_all_f, directed = FALSE)

as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  activate(edges) %>%
  filter(!edge_is_multiple())


## -----------------------------------------------------------------------------
as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(n_rank_trv = node_rank_traveller()) %>%
  arrange(n_rank_trv)


## -----------------------------------------------------------------------------
#Centrality

as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(neighbors = centrality_degree()) %>%
  arrange(-neighbors)


## -----------------------------------------------------------------------------
#Grouping and clustering

as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(group = group_infomap()) %>%
  arrange(-group)


## -----------------------------------------------------------------------------
#Querying node types

as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(center = node_is_center(),
         keyplayer = node_is_keyplayer(k = 10))


## -----------------------------------------------------------------------------
#Node pairs

as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(dist_to_center = node_distance_to(node_is_center()))


## -----------------------------------------------------------------------------
#Edge betweenness

as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  activate(edges) %>% 
  mutate(centrality_e = centrality_edge_betweenness())

#The complete code

cooc_all_f_graph <- as_tbl_graph(cooc_all_f, directed = FALSE) %>%
  mutate(n_rank_trv = node_rank_traveller(),
         neighbors = centrality_degree(),
         group = group_infomap(),
         center = node_is_center(),
         dist_to_center = node_distance_to(node_is_center()),
         keyplayer = node_is_keyplayer(k = 10)) %>%
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>%
  mutate(centrality_e = centrality_edge_betweenness())

cooc_all_f_graph %>%
  activate(nodes) %>% # %N>%
  as_tibble()

cooc_all_f_graph %>%
  activate(edges) %>% # %E>%
  as_tibble()


## -----------------------------------------------------------------------------
#Plotting

layout <- create_layout(cooc_all_f_graph, 
                        layout = "fr")


## -----------------------------------------------------------------------------
ggraph(layout) + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(group)), size = 5) +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_graph(base_family="sans") +
  labs(title = "A Song of Ice and Fire character network",
       subtitle = "Nodes are colored by group")

ggsave("plotshiringroup.pdf", width = 21, height = 29.7, units = "cm")

cols <- RColorBrewer::brewer.pal(3, "Set1")

ggraph(layout) + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(center), size = dist_to_center)) +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  scale_colour_manual(values = c(cols[2], cols[1])) +
  theme_graph(base_family="sans") +
  labs(title = "A Song of Ice and Fire character network",
       subtitle = "Nodes are colored by centeredness")
        

ggsave("plotshirin.pdf", width = 21, height = 29.7, units = "cm")


## -----------------------------------------------------------------------------

nodes2 <- data.frame(id = (unique(c(cooc_all_f$Source, cooc_all_f$Target))), group = layout$group)
                               
edges2 <- data.frame(from = cooc_all_f$Source,
                    to = (cooc_all_f$Target))

visNetwork(nodes2, edges2, height = "1000px", width = "100%") %>%
  visLayout(randomSeed = 12) %>% # to have always the same network 
  visGroups(groupname = "1", color = "red") %>% 
  visGroups(groupname = "2", color = "blue") %>%
  visGroups(groupname = "3", color = "green") %>% 
  visGroups(groupname = "4", color = "purple") %>%
  visGroups(groupname = "5", color = "orange") %>% 
  visIgraphLayout() %>%
  visOptions(highlightNearest = TRUE) %>%
  visNodes(size = 15)

visNetwork(nodes2, edges2, height = "1000px", width = "100%") %>%
  visLayout(randomSeed = 12) %>% # to have always the same network 
  visGroups(groupname = "1", color = "red") %>% 
  visGroups(groupname = "2", color = "blue") %>%
  visGroups(groupname = "3", color = "green") %>% 
  visGroups(groupname = "4", color = "purple") %>%
  visGroups(groupname = "5", color = "orange") %>% 
  visIgraphLayout() %>%
  visOptions(highlightNearest = TRUE) %>%
  visNodes(size = 15) %>%
  visEdges(arrows = "to", arrowStrikethrough = F) %>%
visSave(file = "transfers2.html", selfcontained = T)


