
# Load the libraries
library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)

setwd("D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris")

simdt = fread("similarity_mat_all_rolling.csv")

sim_mat = as.matrix(simdt %>% select(-c(1:3)))

rownames(sim_mat) = simdt$CODE_ACTIVITE

# Set diagonal to 0 (a type has 0 similarity with itself in this context)
diag(sim_mat) <- 0

sim_long = simdt %>% pivot_longer(., cols = c(4:ncol(simdt)), names_to = "target") %>%
  select(-c(2:3)) %>% rename(source = CODE_ACTIVITE, weight = value)



TCIdt = fread("TCI_all_rolling.csv")

sim_nodes <- simdt %>% select(1:3) %>% rename(name = CODE_ACTIVITE) %>% 
  left_join(.,TCIdt %>% group_by(id) %>% summarise(Complexity = mean(Complexity, na.rm = T)) %>% ungroup()%>% rename(name = id))

network <- graph_from_data_frame(d = sim_long, vertices = sim_nodes, directed = T) 

graph <- as_tbl_graph(network, directed = TRUE)

library(leidenAlg)



res = 1
# We mutate the graph to add a 'community' attribute to each node.
graph <- graph %>%
  activate(nodes) %>%
  mutate(community = as.factor(find_partition(graph, edge_weights = graph%>% activate(edges) %>% pull(weight), resolution = res)))

#part = find_partition(graph, edge_weights = graph%>% activate(edges) %>% pull(weight), resolution = res)

# Check the node data to see the new 'community' column
# Use `print(n=...` to see more rows)
graph %>% activate(nodes) %>% as_tibble()

# --- Important pre-processing for visualization ---
# A graph with 200 nodes can be a "hairball" if all edges are plotted.
# It's common practice to filter out the weakest edges to reveal the core structure.
# Let's keep only edges with a weight above a certain percentile or value.
# This makes the final plot much cleaner.

# Let's see the distribution of weights
summary(graph %>% activate(edges) %>% pull(weight))

# Let's filter to keep, for example, the top 20% of edges
# Or you can use a hard threshold, e.g., weight > 0.01
threshold <- quantile(graph %>% activate(edges) %>% pull(weight), 0.9)

graph_filtered <- graph %>%
  activate(edges) %>%
  filter(weight > 0) %>%
  # Also, remove any nodes that are now isolated after filtering edges
  activate(nodes) %>%
  filter(!node_is_isolated())

# Now, create the layout using the filtered graph
# The layout is calculated once and then used for plotting.
# This is much more efficient.
set.seed(123) # for a reproducible layout
layout <- create_layout(graph_filtered, layout = 'fr', weights = weight, circular = F)




# Create the plot
ggraph(layout) +
  # 1. Add the edges
  # We use geom_edge_fan for nice curved lines.
  # Alpha is mapped to weight, so strong ties are more visible.
  #geom_edge_fan(aes(alpha = weight), show.legend = FALSE) +
  
  # 2. Add the nodes
  # Color is mapped to the 'community' attribute.
  # We add a small stroke for better visibility.
  geom_node_point(aes(color = Complexity),size = 3) +
  
  #guides(color = guide_legend(override.aes = list(size=5), title="Amenity Group"))+
  
  # 3. Use a nice, clean theme
  theme_graph(base_family = 'sans') 
  
  # 4. Add titles
  # labs(title = "Network of Commercial Amenity Similarities in Paris",
  #      subtitle = "Nodes are amenity types, colored by community. Edges show co-location frequency.",
  #      caption = "Data Source: Your commercial amenity data") +
  
  # 5. Optional: Improve the legend
  



# Activate the node data from our original (unfiltered) graph
node_data <- graph %>%
  activate(nodes) %>%
  as_tibble()

# Group by community and list the members
community_summary <- node_data %>%
  group_by(community) %>%
  # Summarise by pasting all the type names together
  summarise(
    size = n(),
    members = paste(name, collapse = ", ")
  ) %>%
  # Arrange by size for easier inspection
  arrange(desc(size))

# Print the summary table
# This is your key to labeling the groups in your paper!
print(community_summary, width = 120)

# Example: To see just the members of community '1'
# community_summary %>% filter(community == '1') %>% pull(members)

