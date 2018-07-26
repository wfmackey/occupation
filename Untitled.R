####3.1: occ.chart ####
occ.chart <- function(age = "all", 
                      uniform = TRUE, 
                      level = "bach", 
                      pair = c("m.mar", "f"),
                      chartfields = fields,
                      display = TRUE) {
  
  pair <- sort(pair)
  
  pairlabel <- c("", "")
  for (x in 1:2) {
    if (pair[x] == "m") {
      pairlabel[x] <- c("Male")
    }
    if (pair[x] == "f") {
      pairlabel[x] <- c("Female")
    }
    if (pair[x] == "f.nochild") {
      pairlabel[x] <- c("Female, no child")
    }
    if (pair[x] == "f.haschild") {
      pairlabel[x] <- c("Female, has child")
    }
    if (pair[x] == "m.mar") {
      pairlabel[x] <- c("Male, married")
    }
    if (pair[x] == "f.mar") {
      pairlabel[x] <- c("Female, married")
    }
    if (pair[x] == "f.nochild.mar") {
      pairlabel[x] <- c("Female married no child")
    }
    if (pair[x] == "f.haschild.mar") {
      pairlabel[x] <- c("Female married has child")
    }
    if (pair[x] == "m.unmar") {
      pairlabel[x] <- c("Male, unmarried")
    }
    if (pair[x] == "f.unmar") {
      pairlabel[x] <- c("Female, unmarried")
    }
    if (pair[x] == "f.nochild.unmar") {
      pairlabel[x] <- c("Female unmarried no child")
    }
    if (pair[x] == "f.haschild.unmar") {
      pairlabel[x] <- c("Female unmarried has child")
    }
  }
  
  
  if(age == "2534") {
    if(uniform == TRUE) {
      usedata <- occ.combined %>% filter(grepl('occ.2534.uniform', group))
      print("using occ.2534.uniform")
      title <- paste0("Occupations of of (", pairlabel[1],") & (", pairlabel[2], "), 25-34 year olds (uniform age-weighted)")
    } 
    else {
      usedata <- occ.combined %>% filter(grepl('occ.2534', group),
                                         !grepl('occ.2534.uniform', group))
      print("using occ.2534")
      title <- paste0("Occupations of of (", pairlabel[1],") & (", pairlabel[2], "), 25-34 year olds")
    }
  } else {
    if(uniform == TRUE) {
      usedata <- occ.combined %>% filter(grepl('occ.anyage.uniform', group))
      print("using occ.anyage.uniform")
      title <- paste0("Occupations of of [", pairlabel[1],"] and [", pairlabel[2], "], 18-65 year olds (uniform age-weighted)")
    } else {
      usedata <- occ.combined %>% filter(grepl('occ.anyage', group),
                                         !grepl('occ.anyage.uniform', group))
      print("using occ.anyage")
      title <- paste0("Occupations of of [", pairlabel[1],"] and [", pairlabel[2], "], 18-65 year olds")
    }
  }
  
  # If both dip and bach (will be overridden if either dip or bach)
  subtitle <- "B. = bachelor;  D. = diploma"
  facetcol <- ceiling(length(fields))/2
  
  if(level == "dip") {
    usedata <- filter(usedata, !grepl('B.', qualfield ))
    # usedata <- filter(usedata, str_detect(qualfield, 'D. '))
    subtitle <- "D.  = diploma"
    facetcol <- ceiling(length(fields))/2 + 1
    
  }
  
  if(level == "bach") {
    usedata <- filter(usedata, !grepl('D.', qualfield ))
    subtitle <- "B.  = bachelor"
    facetcol <- ceiling(length(fields))/2 + 1
  }
  
  
  
  occ.current.chart <- usedata %>% 
    filter(code == pair[1] | code == pair[2]) %>% 
    arrange(code) %>% 
    
    filter(qualfield %in% chartfields) %>% 
    ggplot(aes(x = year, color=occ, group=interaction(occ, code))) + 
    geom_line(aes(y = pc, linetype = code), stat="identity") + 
    geom_point(aes(y = pc), stat="identity") + 
    theme_minimal() +
    labs(title = title,
         subtitle = subtitle,
         x = "",
         y = ""
    ) +
    theme(plot.title    = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=8),
          strip.text.x = element_text(size = 10, colour = "black"),
          axis.text.x=element_text(size=8, angle = 90),
          panel.grid.major.x=element_blank()
    ) +
    scale_y_continuous(breaks = seq(0, 1, .1),
                       minor_breaks = seq(0 , 1, .05)) +
    scale_color_manual(labels = occ.labels, 
                       values =  occ.colors) +
    scale_linetype_manual(labels = pairlabel,
                          values = c("solid", "dotted")) + 
    facet_wrap(. ~ qualfield, ncol= facetcol,
               labeller = label_wrap_gen(width = 10)) +
    
    NULL
  
  if (display == TRUE) plot(occ.current.chart)
  
  current.chart.name <- paste0("occ_",age,"age_",level,"level_",pair[1],"_",pair[2],".pdf")
  
  ggsave(current.chart.name, plot = last_plot(), device = "pdf", path = "plots",
         scale = 1, width = 297, height = 210, units = "mm",
         dpi = 320, limitsize = TRUE)
  
  
}

