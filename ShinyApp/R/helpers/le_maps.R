le_map = function(data, measure, sex_input, yrs) { # title
  
  if(measure == "Life Expectancy at Birth") {
    dta <- data %>% 
      filter(sex == sex_input, years == yrs) %>% 
      mutate(
        geoid_str = as.character(GEOID),
        le_str = paste0(round(med,2), " (", round(lci,2), ", ", round(uci,2), ")")) %>%
      st_as_sf()
    }
  else if (measure == "Change in LE at Birth") {
    dta <- data %>% 
      filter(sex == sex_input) %>% 
      mutate(
        geoid_str = as.character(GEOID),
        le_str = paste0(round(med,2), " (", round(lci,2), ", ", round(uci,2), ")")) %>%
      st_as_sf()
  }
  else if (measure == "Posterior Probability of a Decrease in LE") {
    dta <- data %>%
      filter(sex == sex_input) %>%
      mutate(
        geoid_str = as.character(GEOID),
        le_str = round(prob_decr, 2)) %>%
      st_as_sf()
  }
  
  legend_title <- if (measure == "Life Expectancy at Birth") {
    "LE at Birth (Years)"
  } else if (measure == "Change in LE at Birth") {
    "Change in LE at Birth (Years)"
  } else if (measure == "Posterior Probability of a Decrease in LE") {
    "Posterior Probability (%)"
  } else {
    NULL
  }
  
  color_pal <- if(measure == "Life Expectancy at Birth") {
    brewer.pal(n = 5, name = "Blues")
  } else if (measure == "Change in LE at Birth") {
    colorRampPalette(c("red", "white", "green"))(5)
  } else if (measure == "Posterior Probability of a Decrease in LE") {
    brewer.pal(7, "RdBu")[7:1]
  } else {
    NULL
  }
  
  fill_val <- if(measure == "Life Expectancy at Birth") {
    "med"
  } else if (measure == "Change in LE at Birth") {
    "med"
  } else if (measure == "Posterior Probability of a Decrease in LE") {
    "prob_decr"
  } else {
    NULL
  }
  
  popup <- if(measure == "Life Expectancy at Birth") {
    c("GEOID" = "geoid_str","LE" = "le_str")
  } else if (measure == "Change in LE at Birth") {
    c("GEOID" = "geoid_str","Change in LE" = "le_str")
  } else if (measure == "Posterior Probability of a Decrease in LE") {
    c("GEOID" = "geoid_str","Posterior Probability" = "le_str")
  } else {
    NULL
  }
  
  # Map
  tm_shape(dta) + 
    tm_borders(lwd = 0.5) +
    tm_polygons(
      fill = fill_val, 
      fill.scale = tm_scale_intervals(style = "jenks", 
                                      values = color_pal,
                                      label.na = "NA"), 
      fill.legend = tm_legend(title = legend_title),
      # id = "GEOID",
      popup.vars = popup) +
    tm_title(paste0("Life Expectancy at Birth - ", 
                    ifelse(sex_input == "Male", "Men - ", "Women - "),
                    yrs)) +
    tm_layout(
      title.position = c('center', 'top'),
      inner.margins = c(0,0,0,0),
      legend.position = c('right', 'bottom'),
      legend.title.size = 1.2, 
      legend.text.size = 1.2) + 
    tmap_options(
      component.autoscale = FALSE
    )
}