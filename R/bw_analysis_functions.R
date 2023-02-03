get_average_abundance <- function(dataset, grouping_vars) {
  dataset %>%
    group_by(!!! syms(grouping_vars)) %>% 
    summarise(n = n(),
              total_count = sum(litter_count),
              mean_count = mean(litter_count),
              median_count = median(litter_count))
}

get_top_item <- function(dataset, grouping_vars) {
  
  # blocking
  stretch_stats <- dataset %>% 
    group_by(!!! syms(c("uuid", grouping_vars))) %>% 
    summarise(litter_count = sum(litter_count, na.rm = TRUE))
  
  # stats
  stretch_stats %>% 
    group_by(!!! syms(grouping_vars)) %>% 
    summarise(total_count = sum(litter_count),
              mean_count = mean(litter_count),
              median_count = median(litter_count),
              presence = sum(litter_count > 0)/n()*100) %>% 
    mutate(percent_of_total = total_count/sum(total_count)*100) %>% 
    mutate(rank = dense_rank(desc(mean_count))) %>%  
    relocate(rank) %>%
    relocate(percent_of_total, .after = total_count) %>% 
    mutate(across(3:7, round, 1))
}

get_top_item_new <- function(dataset, grouping_vars) {
  
  # blocking
  stretch_stats <- dataset %>% 
    group_by(!!! syms(c("stretch_id", grouping_vars))) %>% 
    summarise(litter_count = sum(litter_count, na.rm = TRUE))
  
  # stats
  stretch_stats %>% 
    group_by(!!! syms(grouping_vars)) %>% 
    summarise(total_count = sum(litter_count),
              mean_count = mean(litter_count),
              median_count = median(litter_count),
              presence = sum(litter_count > 0)/n()*100) %>% 
    mutate(percent_of_total = total_count/sum(total_count)*100) %>% 
    mutate(rank = dense_rank(desc(mean_count))) %>%  
    relocate(rank) %>%
    relocate(percent_of_total, .after = total_count) %>% 
    mutate(across(3:7, round, 1))
}

get_average_item_count <- function(dataset, grouping_vars) {
  dataset %>%
    group_by(!!! syms(c(grouping_vars))) %>%
    summarise(total_count = sum(litter_count),
              mean_count = mean(litter_count),
              median_count = median(litter_count),
              presence = sum(litter_count > 0)/n()*100) %>% 
    mutate(percent_of_total = total_count/sum(total_count)*100) %>% 
    mutate(rank = dense_rank(desc(mean_count))) %>%  
    relocate(rank) %>%
    relocate(percent_of_total, .after = total_count) %>% 
    mutate(across(3:7, round, 1))
}

get_category_count <- function(dataset, grouping_vars, category) {
  
  stretch_stats <- dataset %>% 
    group_by(!!! syms(c("uuid", grouping_vars, category))) %>% 
    mutate(litter_count = sum(litter_count, na.rm = TRUE))
  
  stats <- stretch_stats %>% 
    get_average_item_count(c(grouping_vars, category))

}

get_material_count <- function(dataset, grouping_vars) {
  
  stretch_stats <- dataset %>% 
    group_by(!!! syms(c("uuid", grouping_vars))) %>% 
    mutate(litter_count = sum(litter_count, na.rm = TRUE))
  
  material_stats <- stretch_stats %>% 
    get_average_item_count(grouping_vars)
  
  .GlobalEnv$material_stats <- material_stats
}

get_count <- function(dataset, grouping_vars, category) {
  
  stretch_stats <- dataset %>% 
    group_by(!!! syms(c("uuid", grouping_vars, category))) %>% 
    summarise(litter_count = sum(litter_count, na.rm = TRUE))
  
  material_stats <- stretch_stats %>% 
    mutate(stretch_country = "UK") %>% 
    get_average_item_count(c(grouping_vars, category))
  
  material_stats <- stretch_stats %>% 
    get_average_item_count(c(grouping_vars, category)) %>% 
    bind_rows(material_stats)
  
  return(material_stats)
}