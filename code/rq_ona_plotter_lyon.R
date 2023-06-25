library(tidyverse)

make.ona.plot <- function(set, 
                          plot_name, 
                          flip_x = FALSE
) {
  mean <- plot(set, title = "Mean ONA plot") %>%
    edges(
      weights = set$line.weights,
      edge_size_multiplier = 4,
      edge_color = c("red")) %>%
    nodes(
      node_size_multiplier = 2,
      self_connection_color = c("red")) %>%
    units(
      points=set$points,
      points_color = c("red"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(mean)
  
  mean_points <- plot(set, title = "Mean ONA Scores") %>%
    units (
      points = set$points[set$points$classroom == "CE2",],
      points_color = c("green"),
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE
    ) %>%
    units (
      points = set$points[set$points$classroom == "CM1",],
      points_color = c("blue"),
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE
    ) %>%
    units (
      points = set$points[set$points$classroom == "CM2",],
      points_color = c("purple"),
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE
    ) %>%
    units(
      points = set$points[set$points$classroom == "5",],
      points_color = c("orange"),
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE
    )
  print(mean_points)
  
  class_CE2 <- plot(set, title = "Classroom CE2 plot") %>%
    edges(
      weights = set$line.weights[set$line.weights$classroom == "CE2",],
      edge_size_multiplier = 4,
      edge_color = c("green")) %>%
    nodes(
      node_size_multiplier = 2,
      self_connection_color = c("green")) %>%
    units(
      points=set$points[set$points$classroom == "CE2",],
      points_color = c("green") ,
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(class_CE2)
  
  class_CM1 <- plot(set, title = "Classroom CM1 plot") %>%
    edges(
      weights = set$line.weights[set$line.weights$classroom == "CM1",],
      edge_size_multiplier = 4,
      edge_color = c("blue")) %>%
    nodes(
      node_size_multiplier = 2,
      self_connection_color = c("blue")) %>%
    units(
      points=set$points[set$points$classroom == "CM1",],
      points_color = c("blue") ,
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(class_CM1)
  
  class_CM2 <- plot(set, title = "Classroom CM2 plot") %>%
    edges(
      weights = set$line.weights[set$line.weights$classroom == "CM2",],
      edge_size_multiplier = 4,
      edge_color = c("purple")) %>%
    nodes(
      node_size_multiplier = 2,
      self_connection_color = c("purple")) %>%
    units(
      points=set$points[set$points$classroom == "CM2",],
      points_color = c("purple") ,
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(class_CM2)
  
  class_5 <- plot(set, title = "Classroom 5 plot") %>%
    edges(
      weights = set$line.weights[set$line.weights$classroom == "5",],
      edge_size_multiplier = 4,
      edge_color = c("orange")) %>%
    nodes(
      node_size_multiplier = 2,
      self_connection_color = c("orange")) %>%
    units(
      points=set$points[set$points$classroom == "5",],
      points_color = c("orange") ,
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(class_5)
  
  # Difference networks 
  
  CE2_v_CM1 <- plot(set, title = "Classroom CE2 v. CM1") %>%
    edges(
      weights = (colMeans(set$line.weights[set$line.weights$classroom == "CE2",]) - colMeans(set$line.weights[set$line.weights$classroom == "CM1",])) * 8,
      edge_size_multiplier = 3, 
      edge_arrow_saturation_multiplier = 2,
      edge_color = c("green", "blue")
    ) %>%
    nodes(
      node_size_multiplier = 1, 
      self_connection_color = c("green", "blue")
    ) %>% 
    units(
      points=set$points[set$points$classroom == "CE2",],
      points_color = c("green") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    units(
      points=set$points[set$points$classroom == "CM1",],
      points_color = c("blue") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) 
  print(CE2_v_CM1)
  
  CM1_v_CM2 <- plot(set, title = "Classroom CM1 v. CM2") %>%
    edges(
      weights = (colMeans(set$line.weights[set$line.weights$classroom == "CM1",]) - colMeans(set$line.weights[set$line.weights$classroom == "CM2",])) * 8,
      edge_size_multiplier = 3, 
      edge_arrow_saturation_multiplier = 2,
      edge_color = c("blue", "purple")
    ) %>%
    nodes(
      node_size_multiplier = 1, 
      self_connection_color = c("blue", "purple")
    ) %>% 
    units(
      points=set$points[set$points$classroom == "CM1",],
      points_color = c("blue") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    units(
      points=set$points[set$points$classroom == "CM2",],
      points_color = c("purple") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE)
  print(CM1_v_CM2)
  
  CM2_v_5 <- plot(set, title = "Classroom CM2 v. 5") %>%
    edges(
      weights = (colMeans(set$line.weights[set$line.weights$classroom == "CM2",]) - colMeans(set$line.weights[set$line.weights$classroom == "5",])) * 8,
      edge_size_multiplier = 3, 
      edge_arrow_saturation_multiplier = 2,
      edge_color = c("purple", "orange")
    ) %>%
    nodes(
      node_size_multiplier = 1, 
      self_connection_color = c("purple", "orange")
    ) %>% 
    units(
      points=set$points[set$points$classroom == "CM2",],
      points_color = c("purple") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    units(
      points=set$points[set$points$classroom == "5",],
      points_color = c("orange") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE)
  print(CM2_v_5)
  
  # Try most distinct age groups 
  CE2_v_5 <- plot(set, title = "Classroom CE2 v. 5") %>%
    edges(
      weights = (colMeans(set$line.weights[set$line.weights$classroom == "CE2",]) - colMeans(set$line.weights[set$line.weights$classroom == "5",])) * 8,
      edge_size_multiplier = 3,
      edge_arrow_saturation_multiplier = 2,
      edge_color = c("green", "orange")
    ) %>%
    nodes(
      node_size_multiplier = 1, 
      self_connection_color = c("green", "orange")
    ) %>% 
    units(
      points=set$points[set$points$classroom == "CE2",],
      points_color = c("green") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    units(
      points=set$points[set$points$classroom == "5",],
      points_color = c("orange") ,
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE)
  print(CE2_v_5)
}