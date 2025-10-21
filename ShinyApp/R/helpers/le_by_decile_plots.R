decile_plots = function(le_by_decile, statistic, age, yrs){
  
  # Change data to use gender language
  le_by_decile <- le_by_decile %>%
    mutate(gen = case_when(
      Sex == "Male" ~ "Men",
      Sex == "Female" ~ "Women"
    ) %>% as.factor())
  
  # Filter data
  dta_filtered <- filter(le_by_decile, Age == age, years == yrs)
  
  if(statistic == "Mean") {
    
    dta <- dta_filtered$le
    lci <- dta_filtered$le_lci
    uci <- dta_filtered$le_uci
    title_tmp <- "Mean LE "
    tooltip <- paste0(title_tmp, "of ", round(dta, 2),
                      " (95% CI: ", round(lci, 2), ", ", 
                      round(uci, 2),") \nyears in decile ",
                      dta_filtered$decile_income)
    dta_filtered$tooltip <- tooltip
    dta_filtered$dta <- dta
    dta_filtered$lci <- lci
    dta_filtered$uci <- uci
    
  } else if(statistic == "Standard Deviation") {
    
    dta <- dta_filtered$sd
    lci <- dta_filtered$sd_lci
    uci <- dta_filtered$sd_uci
    title_tmp <- "Standard deviation of LE "
    tooltip <- paste0(title_tmp, "of ", round(dta, 2),
                      " (95% CI: ", round(lci, 2), ", ", 
                      round(uci, 2),") \nyears in decile ",
                      dta_filtered$decile_income)
    dta_filtered$tooltip <- tooltip
    dta_filtered$dta <- dta
    dta_filtered$lci <- lci
    dta_filtered$uci <- uci
    
  }
  
  # Title text
  titleBase <- paste0(title_tmp, "by Median Household Income Decile for Philadelphia")
  
  # Line plot
  le_by_decile_plot <- ggplot(dta_filtered, aes(x = decile_income,
                                   y = dta
                                   )) +
    geom_ribbon_interactive(alpha = 0.3, aes(ymin = lci,
                                             ymax = uci,
                                             group = gen,
                                             color = gen,
                                             fill = gen
                                             )) +
    geom_line_interactive(
      aes(group = gen, color = gen)
    ) +
    geom_point_interactive(
      aes(color = gen, tooltip = tooltip, data_id = paste0(gen, "_", decile_income))
    ) +
    labs(
      x = "Decile of Median Household Income",
      y = title_tmp,
      fill = "Gender"
    ) +
    scale_x_continuous(limits = c(-0.2, 11), breaks = seq(0, 10, by = 2)) +
    theme_bw() +
    guides(col = "none") +
    ggtitle(titleBase) +
    theme(
      plot.title = element_text(color = "black", face = "bold", size = 15, hjust = 0.5),
      axis.text = element_text(color = "black", size = 12),
      axis.title.x = element_text(color = "black", face = "bold", size = 12),
      axis.title.y = element_text(color = "black", face = "bold", size = 12),
      strip.text = element_text(color = "black", face = "bold", size = 12),
      strip.background = element_blank()
    )
  
  # Make ggplot interactive with girafe
  gg <- girafe(
    ggobj = le_by_decile_plot,
    width_svg = 8, height_svg = 6,
    options = list(
      opts_tooltip(
        css = "
          background:white;
          padding:5px;
          border: 1px solid black;
          border-radius:6px;
          font-size:12px;",
        opacity = 0.95,
        offx = 10, offy = 10,
        use_cursor_pos = TRUE
      ),
      opts_hover(css = "stroke-width:2px;stroke:black;"),
      opts_toolbar(saveaspng = TRUE)
    )
  )
  
}
