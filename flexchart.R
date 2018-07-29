



####3 Functions: Defining functions to create LFS and OCC charts####
####3.0: General charts ####
flexchart <- function(data,
                      type,
                      pair = c("m.mar", "f"),
                      title = "No title",
                      subtitle = "No subtitle",
                      save = FALSE,
                      savename = "tempsave",
                      chartfields = fields,
                      display = TRUE,
                      code = code,
                      pc = pc,
                      year = year) {

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
  
  type <- enquo(type)
  code <- enquo(code)
  
  current.chart <- data %>% 
    filter(code == pair[1] | code == pair[2]) %>% 
    arrange(code) %>% 
    
    filter(qualfield %in% chartfields) %>% 
    
    ggplot(aes(x = !!year, color=!!type, group=interaction(!!type, !!code))) + 
    geom_line(aes(y = !!pc, linetype = !!code), stat="identity") + 
    geom_point(aes(y = !!pc), stat="identity") + 
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
    scale_color_manual(labels =  get(paste0(!!type,".labels")), 
                       values =  get(paste0(!!type,".colors"))) +
    scale_linetype_manual(labels = pairlabel,
                          values = c("solid", "dotted")) + 
    facet_wrap(. ~ qualfield, ncol= facetcol,
               labeller = label_wrap_gen(width = 10)) +
    
    NULL
  
  if (display == TRUE) plot(current.chart)
  
  if (save == TRUE) {
  ggsave(savename, plot = last_plot(), device = "pdf", path = "plots",
         scale = 1, width = 297, height = 210, units = "mm",
         dpi = 320, limitsize = TRUE)
  }
  
  
}



# flexchart <- function(data,
#                       type = "lfs",
#                       pair = c("m.mar", "f"),
#                       title = "No title",
#                       subtitle = "No subtitle",
#                       save = FALSE,
#                       savename = "tempsave",
#                       chartfields = fields,
#                       display = TRUE) {
  
  
flexchart(usedata,
          type = lfs,
          pair = c("m.mar", "m.unmar"),
          title = "Hope this title works!",
          subtitle = "please work!",
          save = FALSE)  
  



usedata %>% 
ggplot() + 
  geom_point(aes(pc, year))

test <- function(data, x) {
  x <- enquo(x)
  data %>% ggplot() + geom_point(aes(!!x, year))
}

test(usedata, pc)



func <- function(data, type) {
  type <- enquo(type)
  
  # data %>% ggplot() + geom_point(aes(!!x, year))
  data %>% 
    ggplot(aes(year, color=!!type, group=interaction(!!type, code))) + 
    geom_line(aes(y = pc, linetype = code), stat="identity") + 
    geom_point(aes(y = pc), stat="identity")
}

func(data = usedata, x = lfs)


  
flex <- function(data,
                      type,
                      pair = c("m.mar", "f"),
                      title = "No title",
                      subtitle = "No subtitle",
                      save = FALSE,
                      savename = "tempsave",
                      chartfields = fields,
                      display = TRUE) {
  
  lfs.current.chart <- data %>% 
    filter(code == pair[1] | code == pair[2]) %>% 
    arrange(code) %>% 
    
    filter(qualfield %in% chartfields) %>% 
    
    ggplot(aes_string(x = year, color=!!type, group=interaction(!!type, code))) + 
    geom_line(aes(y = pc, linetype = code), stat="identity") + 
    geom_point(aes(y = pc), stat="identity")
  
  }







