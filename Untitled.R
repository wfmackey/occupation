
flex.chart.tester <- function(type = lfs,
                       age = "all", 
                       uniform = TRUE, 
                       level = "bach", 
                       pair = c("m.mar", "f.mar"),
                       chartfields = fields,
                       display = TRUE,
                       save = TRUE) {
  
  typename <- deparse(substitute(type))
  
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
  
  
  # Set labels and colours for each TYPE (this is called via TYPE within the ggplot function)
  lfs.labels = c( "Employed FT", 
                  "Employed PT",
                  "Employed AWAY",
                  "NILF",
                  "Unemployed")
  lfs.colors = c( gred,
                  gorange,
                  gblack,
                  ggrey,
                  gyellow)
  
  occ.labels = c( "Manager/prof.",
                  "Tech/trade",
                  "Sales/admin/service",
                  "Labourer/machine",
                  "Not working")
  occ.colors = c( gorange,
                  gdark,
                  gred,
                  gdarkest,
                  ggrey)     
  
  
  # Get data
  usedata <- get(paste0(typename,".combined"))
  
  
  # Filter data according to settings:
  # Uniform age-weighted method or not
  if(uniform == TRUE) {
    usedata <- usedata %>% filter(grepl('.uniform', group))
    uniform.name <- ", (uniform age-weighted)"
  } else {
    usedata <- usedata %>% filter(!grepl('.uniform', group))
    uniform.name <- ""
  }
  
  # Bach or diploma level, or both
  # If both dip and bach (will be overridden if either dip or bach)
  level.name <- "B. = bachelor;  D. = diploma"
  facetcol <- ceiling(length(fields))/2
  
  if(level == "dip") {
    usedata <- filter(usedata, !grepl('B.', qualfield ))
    facetcol <- ceiling(length(fields))/2 + 1
    level.name <- "D. = diploma"
    
  }
  
  if(level == "bach") {
    usedata <- filter(usedata, !grepl('D.', qualfield ))
    facetcol <- ceiling(length(fields))/2 + 1
    level.name <- "B. = bachelor"
  }
  
  
  ## Age
  if(age == "all"){
    usedata <- usedata %>% filter(is.na(agegroup))
    age.name <- "18-64 year old"
  } else {
    usedata <- usedata %>% filter(agegroup == age)
    age.name <- paste0(substr(age, 1, 2),"-",substr(age, 3, 4)," year old")
  }
  
  ##Create name
  if(typename == "lfs"){
    title <- paste0("Labour force status of ", age.name," [",pairlabel[1],"] & [", pairlabel[2],"]")
  } else {
    title <- paste0("Occupations of ", age.name," [",pairlabel[1],"] & [", pairlabel[2],"]")
  }
  
  subtitle <- paste0(level.name, uniform.name)
  
  ##Check number of rows:
  print(nrow(usedata))
  
  ##Enquote type to allow for use as a name in ggplot function
  type <- enquo(type)
  
  testingdata <<- usedata
  
  current.chart <- usedata %>% 
    filter(code == pair[1] | code == pair[2]) %>% 
    arrange(code) %>% 
    filter(qualfield %in% chartfields) %>% 
    ggplot(aes(x = year, color=!!type, group=interaction(!!type, code))) + 
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
    scale_color_manual(labels = get(paste0(typename,".labels")),
                       values =  get(paste0(typename,".colors"))) +
    scale_linetype_manual(labels = pairlabel,
                          values = c("solid", "dotted")) +
    facet_wrap(. ~ qualfield, ncol= facetcol,
               labeller = label_both) +
    
    NULL
  
  if (display == TRUE) plot(current.chart)
  
  if (save == TRUE) {
    chart.name <- paste0(typename, "_",age,"age_",level,"level_",pair[1],"_",pair[2],".pdf")
    ggsave(chart.name, plot = last_plot(), device = "pdf", path = "plots",
           scale = 1, width = 297, height = 210, units = "mm",
           dpi = 320, limitsize = TRUE)
  }
  
  
}




flex.chart.tester()








  usedata <- testingdata %>% mutate(child = if_else(grepl("haschild", code) == TRUE, TRUE,
                                            if_else(grepl("nochild", code) == TRUE, FALSE,
                                            NA_integer_)
                                            ))

  typename = "lfs"
  usedata %>% 
  filter(code == "f.haschild.mar" | code == grepl("f.nochild.mar", code)) %>% 
  arrange(code) %>% 
  filter(qualfield %in% chartfields) %>% 
  ggplot(aes(x = year, color=lfs, group=interaction(lfs, gender))) + 
  geom_line(aes(y = pc, linetype = gender), stat="identity") + 
  geom_point(aes(y = pc), stat="identity") + 
  theme_minimal() +
  labs(title = title,
       subtitle = subtitle,
       x = "",
       y = ""
  ) +
  facet_wrap(married ~ qualfield, ncol= facetcol,
             labeller = label_both) +
  # facet_wrap(. ~ vars(qualfield, married), ncol= facetcol,
  #            labeller = label_both) +
  
  NULL

  
  









