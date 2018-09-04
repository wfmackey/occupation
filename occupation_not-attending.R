## Census data:

library(gridExtra)
library(tidyverse)
library(zoo)


# Define some Grattan-y colours:
glightyellow <- "#FFE07F"
gyellow <- "#FFC35A"
gorange <- "#F68B33"
gdark <- "#D4582A"
gred <- "#A02226"
gdarkest <- "#621214"

ggrey <- "#AEAEAE"
gblack <- "#141413"

## Define groups
married.def <- c("Married in a registered marriage", "Married in a de facto marriage")
foe.table <- read_csv("data/foe_table.csv") %>% select(-X3)




# Fields, default (noting that these can be changed within lfs.chart options)
fields <- c("Y12",
            "B. Science (excl maths)", 
            "B. IT",
            "B. Engineering", 
            "B. Other health", 
            "B. Medicine", 
            "B. Nursing", 
            "B. Education",
            "B. Commerce", 
            "B. Humanities", 
            "B. Law",
            "B. Performing Arts",
            "D. Science (excl maths)",
            "D. IT",
            "D. Engineering",
            "D. Other health",
            "D. Nursing",
            "D. Education",
            "D. Commerce",
            "D. Humanities",
            "D. Law",
            "D. Performing Arts",
            "B. all",
            "D. all")



#1: Load in data####

##1.1: LFS data####

##Read in 2006 field data
x <- "06"

lfs06 <- read_csv(paste0("data/",x,"_long_bach_lfs_notattending.csv"), skip = 11) %>% 
  
         rename(
                      qual = "X1",
                      age = "X2",
                      marriage = "X3",
                      foe = "X4",
                      lfs = "TISP Number of Children Ever Born",  # Not an error; dealing with the odd way ABS structures their csv exports
                      f.nochild = "None",
                      f.child1 = "One",
                      f.child2 = "Two",
                      f.child3 = "Three",
                      f.child4 = "Four",
                      f.child5 = "Five",
                      f.child6 = "Six or more",
                      m = "Not applicable",
                      total = "Total"
                ) %>% 
  
        slice(-1) %>% select(-X15) %>%  filter(!is.na(lfs)) %>% #tidy
  
        do(na.locf(.)) %>%   # fill in label columns

        mutate(
                     age = as.numeric(gsub("(.*)\\syears?", "\\1", age)),  
                     f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
                     f = ifelse(total - m < 0, 0, total - m),
                     married = (marriage == married.def[1] | marriage == married.def[2]),
                     is.2534 = (age >= 25 & age <= 34),
                     lfs = replace(lfs, lfs=="Unemployed, looking for full-time work", "Unemployed"),
                     lfs = replace(lfs, lfs=="Unemployed, looking for part-time work", "Unemployed"),
                     qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
                     qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
                     year = paste0("20",x)
                ) %>% 
  
        select(
                    -starts_with("f.child"), 
                    -marriage, 
                    -total
              ) %>% 
  
        left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  
        rename(
                    field = broad, 
                    foe4digit = foe
              ) %>% 
  
        filter(
                    lfs != "Not applicable" & lfs != "Not stated", 
                    field != "Not applicable" & field != "Not stated",
                    qual != "Total"
              )



            
            ## 2006 Year 12 data
            ### note that this is the same data-source as occupation2.R
            lfs06.y12 <- read_csv(paste0("data/",x,"_long_y12_lfs.csv")) %>% 
              rename(age = "AGEP Age",
                     marriage = "MDCP Social Marital Status",
                     lfs = "LFSP Labour Force Status",
                     f.nochild = "None",
                     f.child1 = "One",
                     f.child2 = "Two",
                     f.child3 = "Three",
                     f.child4 = "Four",
                     f.child5 = "Five",
                     f.child6 = "Six or more",
                     m = "Not applicable",
                     total = "Total") %>%
              mutate(qual = "Y12",
                     field = "Y12",
                     foe4digit = "Y12",
                     f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
                     f = ifelse(total - m < 0, 0, total - m),
                     married = (marriage == married.def[1] | marriage == married.def[2]),
                     is.2534 = (age >= 25 & age <= 34),
                     lfs = replace(lfs, lfs=="Unemployed, looking for full-time work", "Unemployed"),
                     lfs = replace(lfs, lfs=="Unemployed, looking for part-time work", "Unemployed"),
                     qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
                     qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
                     year = paste0("20",x)) %>% 
              select(-starts_with("f.child"),
                     -marriage,
                     -total) %>% 
              filter(lfs != "Not applicable" & lfs != "Not stated")
            
            lfs06 <- bind_rows(lfs06, lfs06.y12)



##Read in 2011 data
x <- 11



lfs11 <- read_csv(paste0("data/",x,"_long_bach_lfs_notattending.csv"), skip = 11) %>% 
  
  rename(
    qual = "X1",
    age = "X2",
    marriage = "X3",
    foe = "X4",
    lfs = "TISP Number of Children Ever Born",  # Not an error; dealing with the odd way ABS structures their csv exports
    f.nochild = "No children",
    f.child1 = "1 child",
    f.child2 = "2 children",
    f.child3 = "3 children",
    f.child4 = "4 children",
    f.child5 = "5 children",
    f.child6 = "6 children",
    f.child7 = "7 children",
    f.child8 = "8 children",
    m = "Not applicable",
    total = "Total"
  ) %>% 
  
  slice(-1) %>% select(-X17) %>% filter(!is.na(lfs)) %>%  #tidy
  
  do(na.locf(.)) %>%   # fill in label columns

  mutate(
             age = as.numeric(gsub("(.*)\\syears?", "\\1", age)),  
             f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
             f = ifelse(total - m < 0, 0, total - m),
             married = (marriage == married.def[1] | marriage == married.def[2]),
             is.2534 = (age >= 25 & age <= 34),
             lfs = replace(lfs, lfs=="Unemployed, looking for full-time work", "Unemployed"),
             lfs = replace(lfs, lfs=="Unemployed, looking for part-time work", "Unemployed"),
             qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
             qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
             year = paste0("20",x)
             
         ) %>% 
  
  select(-starts_with("f.child"),
         -marriage,
         -total) %>% 
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  rename(field = broad, 
         foe4digit = foe) %>% 
  filter(lfs != "Not applicable" & lfs != "Not stated", 
         field != "Not applicable" & field != "Not stated",
         qual != "Total")


  

        # 2011 Year 12 data 
        lfs11.y12 <- read_csv(paste0("data/",x,"_long_y12_lfs.csv")) %>% 
          rename(age = "AGEP Age",
                 marriage = "MDCP Social Marital Status",
                 lfs = "LFSP Labour Force Status",
                 f.nochild = "No children",
                 f.child1 = "1 child",
                 f.child2 = "2 children",
                 f.child3 = "3 children",
                 f.child4 = "4 children",
                 f.child5 = "5 children",
                 f.child6 = "6 children",
                 f.child7 = "7 children",
                 f.child8 = "8 children",
                 m = "Not applicable",
                 total = "Total") %>%
          mutate(qual = "Y12",
                 field = "Y12",
                 foe4digit = "Y12",
                 f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
                 f = ifelse(total - m < 0, 0, total - m),
                 married = (marriage == married.def[1] | marriage == married.def[2]),
                 is.2534 = (age >= 25 & age <= 34),
                 lfs = replace(lfs, lfs=="Unemployed, looking for full-time work", "Unemployed"),
                 lfs = replace(lfs, lfs=="Unemployed, looking for part-time work", "Unemployed"),
                 qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
                 qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
                 year = paste0("20",x)) %>% 
          select(-starts_with("f.child"),
                 -marriage,
                 -total) %>% 
          filter(lfs != "Not applicable" & lfs != "Not stated")
        
  lfs11 <- bind_rows(lfs11, lfs11.y12)



## Read in 2016 data
x <- 16

# Field
lfs16 <- read_csv(paste0("data/",x,"_long_bach_lfs_notattending.csv"), skip = 11) %>% 
  
  rename(
            qual = "X1",
            age = "X2",
            marriage = "X3",
            foe = "X4",
            lfs = "TISRP Number of Children Ever Born (ranges)",  # Not an error; dealing with the odd way ABS structures their csv exports
            f.nochild = "No children",
            f.child1 = "One child",
            f.child2 = "Two children",
            f.child3 = "Three children",
            f.child4 = "Four children",
            f.child5 = "Five children",
            f.child6 = "Six children",
            f.child7 = "Seven children",
            f.child8 = "Eight or more children",
            m = "Not applicable",
            total = "Total"
  ) %>% 
  
  slice(-1) %>% select(-X18) %>%  filter(!is.na(lfs)) %>% #tidy
  
  do(na.locf(.)) %>%   # fill in label columns

  mutate(
             age = as.numeric(age),
             f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
             f = ifelse(total - m < 0, 0, total - m),
             married = (marriage == married.def[1] | marriage == married.def[2]),
             is.2534 = (age >= 25 & age <= 34),
             lfs = replace(lfs, lfs=="Unemployed, looking for full-time work", "Unemployed"),
             lfs = replace(lfs, lfs=="Unemployed, looking for part-time work", "Unemployed"),
             qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
             qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
             year = paste0("20",x)
             
         ) %>% 
  left_join(foe.table, by = "foe") %>% 
  ## add Grattan fields 
  rename(field = "broad",
         foe4digit = "foe") %>%
  filter(lfs != "Not applicable" & lfs != "Not stated",
         field != "Not applicable" & field != "Not stated",
         qual != "Total") %>%
  select(-starts_with("f.child"),
         -marriage,
         -'Not stated',
         -total)

filter(lfs != "Not applicable" & lfs != "Not stated", 
       field != "Not applicable" & field != "Not stated",
       qual != "Total")
        # 2016 Year 12 data  
        lfs16.y12 <- read_csv(paste0("data/",x,"_long_y12_lfs.csv")) %>% 
          rename(age = "AGEP Age",
                 marriage = "MDCP Social Marital Status",
                 lfs = "LFSP Labour Force Status",
                 f.nochild = "No children",
                 f.child1 = "One child",
                 f.child2 = "Two children",
                 f.child3 = "Three children",
                 f.child4 = "Four children",
                 f.child5 = "Five children",
                 f.child6 = "Six children",
                 f.child7 = "Seven children",
                 f.child8 = "Eight or more children",
                 m = "Not applicable",
                 drop = "Not stated",
                 total = "Total") %>%
          mutate(qual = "Y12",
                 field = "Y12",
                 foe4digit = "Y12",
                 f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
                 f = ifelse(total - m < 0, 0, total - m),
                 married = (marriage == married.def[1] | marriage == married.def[2]),
                 is.2534 = (age >= 25 & age <= 34),
                 lfs = replace(lfs, lfs=="Unemployed, looking for full-time work", "Unemployed"),
                 lfs = replace(lfs, lfs=="Unemployed, looking for part-time work", "Unemployed"),
                 qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
                 qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
                 year = paste0("20",x)) %>% 
          select(-starts_with("f.child"),
                 -marriage,
                 -drop,
                 -total) %>% 
          filter(lfs != "Not applicable" & lfs != "Not stated")

lfs16 <- bind_rows(lfs16, lfs16.y12) 

# Append LFS data 2006-2016
lfs.semiwide <- bind_rows(lfs06, lfs11, lfs16) 

# We want to have the dataset LONG, and edit fields to reflect degree level (ie B. Science, D. Education)
lfs <-  gather(lfs.semiwide,
               gender ,
               count  ,
               f.nochild:f ,
               factor_key = FALSE
) %>% 
  mutate(qualfield = ifelse(qual=="Bachelor",      paste0("B. ",field),
                            ifelse(qual=="Diploma & AdDip", paste0("D. ",field),
                                   "Y12")))



# And change factor levels of vars to something more logical
lfs.order =  c("Employed, worked full-time",
               "Employed, worked part-time",
               "Employed, away from work",
               "Not in the labour force",
               "Unemployed")


lfs <- lfs %>% mutate(lfs = factor(lfs, levels = lfs.order))













##1.2: OCC data####    

##Read in 2006 field data
x <- "06"

occ06 <- read_csv(paste0("data/",x,"_long_bach_occ_notattending.csv"), skip = 11) %>% 
  rename(qual = "X1",
         age = "X2",
         marriage = "X3",
         foe = "X4",
         occ = "TISP Number of Children Ever Born",
         f.nochild = "None",
         f.child1 = "One",
         f.child2 = "Two",
         f.child3 = "Three",
         f.child4 = "Four",
         f.child5 = "Five",
         f.child6 = "Six or more",
         m = "Not applicable",
         total = "Total") %>%
  
  slice(-1) %>% select(-X15) %>%  filter(!is.na(occ)) %>% #tidy
  
  do(na.locf(.)) %>%   # fill in label columns
  
  mutate(
         age = as.numeric(gsub("(.*)\\syears?", "\\1", age)),  
         f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
         f = ifelse(total - m < 0, 0, total - m),
         married = (marriage == married.def[1] | marriage == married.def[2]),
         is.2534 = (age >= 25 & age <= 34),
         occ = replace(occ, occ=="Not applicable", "Not working"),
         occ = replace(occ, occ=="Machinery Operators And Drivers", "Machinery Operators and Drivers"),
         qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
         qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
         year = paste0("20",x)
         ) %>% 
  select(-starts_with("f.child"),
         -marriage,
         -total) %>% 
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  rename(field = broad, 
         foe4digit = foe) %>% 
  filter(occ != "Inadequately described" & occ != "Not stated", 
         field != "Not applicable" & field != "Not stated",
         qual != "Total")



          # 2006 Year 12 data  
          occ06.y12 <- read_csv(paste0("data/",x,"_long_y12_occ.csv"))%>% 
            rename(age = "AGEP Age",
                   marriage = "MDCP Social Marital Status",
                   occ = "OCC06P Occupation 06 (ANZSCO)",
                   f.nochild = "None",
                   f.child1 = "One",
                   f.child2 = "Two",
                   f.child3 = "Three",
                   f.child4 = "Four",
                   f.child5 = "Five",
                   f.child6 = "Six or more",
                   m = "Not applicable",
                   total = "Total") %>%
            mutate(qual = "Y12",
                   field = "Y12",
                   foe4digit = "Y12",
                   f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
                   f = ifelse(total - m < 0, 0, total - m),
                   married = (marriage == married.def[1] | marriage == married.def[2]),
                   is.2534 = (age >= 25 & age <= 34),
                   occ = replace(occ, occ=="Not applicable", "Not working"),
                   occ = replace(occ, occ=="Machinery Operators And Drivers", "Machinery Operators and Drivers"),
                   qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
                   qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
                   year = paste0("20",x)) %>% 
            select(-starts_with("f.child"),
                   -marriage,
                   -total) %>% 
            filter(occ != "Inadequately described" & occ != "Not stated")
          
          occ06 <- bind_rows(occ06, occ06.y12)


##Read in 2011 occupation field data
x <- "11"

occ11 <- read_csv(paste0("data/",x,"_long_bach_occ_notattending.csv"), skip = 11) %>% 
  rename(qual = "X1",
         age = "X2",
         marriage = "X3",
         foe = "X4",
         occ = "TISP Number of Children Ever Born",  # not an error!
         f.nochild = "No children",
         f.child1 = "1 child",
         f.child2 = "2 children",
         f.child3 = "3 children",
         f.child4 = "4 children",
         f.child5 = "5 children",
         f.child6 = "6 children",
         f.child7 = "7 children",
         f.child8 = "8 children",
         m = "Not applicable",
         total = "Total") %>% 
  
  slice(-1) %>% select(-X17) %>%  filter(!is.na(occ)) %>% #tidy
  
  do(na.locf(.)) %>%   # fill in label columns
  
  mutate(
         age = as.numeric(gsub("(.*)\\syears?", "\\1", age)),  
         f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
         f = ifelse(total - m < 0, 0, total - m),
         married = (marriage == married.def[1] | marriage == married.def[2]),
         is.2534 = (age >= 25 & age <= 34),
         occ = replace(occ, occ=="Not applicable", "Not working"),
         occ = replace(occ, occ=="Machinery Operators And Drivers", "Machinery Operators and Drivers"),
         qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
         qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
         year = paste0("20",x)
         ) %>% 
  
  select(-starts_with("f.child"),
         -marriage,
         -total) %>% 
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
                foe4digit = foe) %>% 
  filter(occ != "Inadequately described" & occ != "Not stated", 
         field != "Not applicable" & field != "Not stated",
         qual != "Total")



      # 2011 occupation Year 12 data  
      occ11.y12 <- read_csv(paste0("data/",x,"_long_y12_occ.csv")) %>%  
        rename(age = "AGEP Age in Single Years",
               marriage = "MDCP Social Marital Status",
               occ = "OCCP Occupation",
               f.nochild = "No children",
               f.child1 = "1 child",
               f.child2 = "2 children",
               f.child3 = "3 children",
               f.child4 = "4 children",
               f.child5 = "5 children",
               f.child6 = "6 children",
               f.child7 = "7 children",
               f.child8 = "8 children",
               m = "Not applicable",
               total = "Total") %>%
        mutate(qual = "Y12",
               field = "Y12",
               foe4digit = "Y12",
               f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
               f = ifelse(total - m < 0, 0, total - m),
               married = (marriage == married.def[1] | marriage == married.def[2]),
               is.2534 = (age >= 25 & age <= 34),
               occ = replace(occ, occ=="Not applicable", "Not working"),
               occ = replace(occ, occ=="Machinery Operators And Drivers", "Machinery Operators and Drivers"),
               qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
               qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
               year = paste0("20",x)) %>% 
        select(-starts_with("f.child"),
               -marriage,
               -total) %>% 
        filter(occ != "Inadequately described" & occ != "Not stated")
      
      occ11 <- bind_rows(occ11, occ11.y12)


##Read in 2016 occupation field data
x <- "16"

occ16 <- read_csv(paste0("data/",x,"_long_bach_occ_notattending.csv"), skip = 11) %>% 
  rename(qual = "X1",
         age = "X2",
         marriage = "X3",
         foe = "X4",
         occ = "TISRP Number of Children Ever Born (ranges)",  # Not an error; dealing with the odd way ABS structures their csv exports
         f.nochild = "No children",
         f.child1 = "One child",
         f.child2 = "Two children",
         f.child3 = "Three children",
         f.child4 = "Four children",
         f.child5 = "Five children",
         f.child6 = "Six children",
         f.child7 = "Seven children",
         f.child8 = "Eight or more children",
         m = "Not applicable",
         total = "Total") %>%
  
  slice(-1) %>% select(-X18) %>%  filter(!is.na(occ)) %>% #tidy
  
  do(na.locf(.)) %>%   # fill in label columns
  
  mutate(
         age = as.numeric(age),
         f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
         f = ifelse(total - m < 0, 0, total - m),
         married = (marriage == married.def[1] | marriage == married.def[2]),
         is.2534 = (age >= 25 & age <= 34),
         occ = replace(occ, occ=="Not applicable", "Not working"),
         occ = replace(occ, occ=="Machinery Operators And Drivers", "Machinery Operators and Drivers"),
         qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
         qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
         year = paste0("20",x)
         ) %>% 
  
  select(-starts_with("f.child"),
         -marriage,
         -total,
         -'Not stated') %>% 
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
                foe4digit = foe) %>% 
  filter(occ != "Inadequately described" & occ != "Not stated", 
         field != "Not applicable" & field != "Not stated",
         qual != "Total")



      # 2016 occupation Year 12 data  
      occ16.y12 <- read_csv(paste0("data/",x,"_long_y12_occ.csv")) %>%  
        rename(age = "AGEP Age",
               marriage = "MDCP Social Marital Status",
               occ = "OCC06P Occupation 06 (ANZSCO)",
               f.nochild = "No children",
               f.child1 = "One child",
               f.child2 = "Two children",
               f.child3 = "Three children",
               f.child4 = "Four children",
               f.child5 = "Five children",
               f.child6 = "Six children",
               f.child7 = "Seven children",
               f.child8 = "Eight or more children",
               m = "Not applicable",
               total = "Total") %>%
        mutate(qual = "Y12",
               field = "Y12",
               foe4digit = "Y12",
               f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
               f = ifelse(total - m < 0, 0, total - m),
               married = (marriage == married.def[1] | marriage == married.def[2]),
               is.2534 = (age >= 25 & age <= 34),
               occ = replace(occ, occ=="Not applicable", "Not working"),
               occ = replace(occ, occ=="Machinery Operators And Drivers", "Machinery Operators and Drivers"),
               qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
               qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
               year = paste0("20",x)) %>% 
        select(-starts_with("f.child"),
               -marriage,
               -total) %>% 
        filter(occ != "Inadequately described" & occ != "Not stated")

occ16 <- bind_rows(occ16, occ16.y12)   



# Append occ data 2006-2016
occ.semiwide <- bind_rows(occ06, occ11, occ16) 

# We want to have the dataset LONG, and edit fields to reflect degree level (ie B. Science, D. Education)
occ <-  gather(occ.semiwide,
               gender ,
               count  ,
               f.nochild:f ,
               factor_key = FALSE) %>% 
  mutate(qualfield = ifelse(qual=="Bachelor", paste0("B. ",field),
                            ifelse(qual=="Diploma & AdDip", paste0("D. ",field),
                                   "Y12")))

# We also want to create a more compressed occupation group: 
occlist <-  c("Managers", 
              "Professionals", 
              "Technicians and Trades Workers", 
              "Sales Workers", 
              "Clerical and Administrative Workers", 
              "Community and Personal Service Workers", 
              "Labourers", 
              "Machinery Operators and Drivers", 
              "Not working")

occgrouplist <- c("Manager/prof.",
                  "Manager/prof.",
                  "Tech/trade",
                  "Sales/admin/service",
                  "Sales/admin/service",
                  "Sales/admin/service",
                  "Labourer/machine",
                  "Labourer/machine",
                  "Not working")

occgroup <- as_tibble(data.frame("occ" = occlist, "occgroup" = occgrouplist))

# Then merge into occ data, and rename for ease of use
occ <- left_join(occ, occgroup, by = "occ") %>% 
  rename(occ.detail = occ,
         occ = occgroup)

# And change factor levels of occ to something more logical
occ.order = c(  "Manager/prof.",
                "Tech/trade",
                "Sales/admin/service",
                "Labourer/machine",
                "Not working")

occ <- occ %>% mutate(occ = factor(occ, levels = occ.order))






#2: Creating summary version####

##2.1: LFS summaries####
### 2.1.1 All ages: 18-65 years old ####

# Looking at all ages 18-65, ignoring marriage
lfs.anyage <- lfs %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qualfield, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count))

# Add total bach + dip totals
add <- lfs %>% filter(qual != "Y12") %>% 
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

lfs.anyage <- bind_rows(lfs.anyage, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, gender, lfs)


# Looking at working age, ignoring marriage -- UNIFORM AGE WEIGHTING
lfs.anyage.uniform <- lfs %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qualfield, age, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qualfield, gender, lfs) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE))

add <- lfs %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, age, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qual, gender, lfs) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

lfs.anyage.uniform <- bind_rows(lfs.anyage.uniform, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, gender, lfs)




# Looking at all ages, by marriage
lfs.anyage.mar <- lfs %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qualfield, married, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count))

# Add total bach + dip totals
add <- lfs %>% filter(qual != "Y12") %>% 
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, married, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

lfs.anyage.mar <- bind_rows(lfs.anyage.mar, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, married, gender, lfs)




# Looking at working age, by marriage -- UNIFORM AGE WEIGHTING
lfs.anyage.uniform.mar <- lfs %>%
  filter(age >= 18 & age <= 65) %>%  
  group_by(year, qualfield, married, age, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qualfield, married, gender, lfs) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE))

add <- lfs %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, married, age, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qual, married, gender, lfs) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

lfs.anyage.uniform.mar <- bind_rows(lfs.anyage.uniform.mar, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, married, gender, lfs)


# Add grouping tag to anyage files:
lfs.anyage <- mutate(lfs.anyage, group = "lfs.anyage")
lfs.anyage.uniform <- mutate(lfs.anyage.uniform, group = "lfs.anyage.uniform")
lfs.anyage.mar <- mutate(lfs.anyage.mar, group = "lfs.anyage.mar")
lfs.anyage.uniform.mar <- mutate(lfs.anyage.uniform.mar, group = "lfs.anyage.uniform.mar")


##Create half-finished final dataset (to be completed with ages in the loop to follow)        
lfs.combined <- bind_rows(lfs.anyage, lfs.anyage.uniform,
                          lfs.anyage.mar, lfs.anyage.uniform.mar)




##2.1.2: By age group: 25-34, 35-44, ..., 55-64 year olds####

for(agel in seq(from=15, to=55, by=10)){
  ageu = agel + 9
  print(paste0("Preparing LFS file for ", agel, " to ", ageu, " year olds"))
  # Ignoring marriage; no uniform age weighting
  a <- lfs %>% 
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count)) 
  # Add total bach + dip totals
  b <- lfs %>% filter(qual != "Y12") %>% 
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("lfs.",agel,ageu),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, gender, lfs) %>% 
           mutate(group = paste0("lfs.",agel,ageu),
                  agegroup = paste0(agel,ageu))
  )
  
  # Ignoring marriage -- UNIFORM AGE WEIGHTING
  a <- lfs %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, age, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qualfield, gender, lfs) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE))
  # Add total bach + dip totals
  b <- lfs %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, age, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qual, gender, lfs) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("lfs.",agel,ageu,".uniform"),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, gender, lfs) %>% 
           mutate(group = paste0("lfs.",agel,ageu,".uniform"),
                  agegroup = paste0(agel,ageu)) 
  )
  
  
  # By marriage -- no uniform age weighting
  a <- lfs %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, married, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count))    
  # Add total bach + dip totals
  b <- lfs %>% filter(qual != "Y12") %>% 
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, married, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("lfs.",agel,ageu,".mar"),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, married, gender, lfs) %>% 
           mutate(group = paste0("lfs.",agel,ageu,".mar"),
                  agegroup = paste0(agel,ageu))
  )
  
  # By marriage -- UNIFORM AGE WEIGHTING
  a <- lfs %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, married, age, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qualfield, married, gender, lfs) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE))
  
  b <- lfs %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, married, age, gender, lfs) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qual, married, gender, lfs) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("lfs.",agel,ageu,".uniform.mar"),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, married, gender, lfs) %>% 
           mutate(group = paste0("lfs.",agel,ageu,".uniform.mar"),
                  agegroup = paste0(agel,ageu))
  )
  
  
  ## Incrementally add all age group files to combined dataset
  lfs.combined <- bind_rows(lfs.combined, 
                            get(paste0("lfs.",agel,ageu)),
                            get(paste0("lfs.",agel,ageu,".uniform")),
                            get(paste0("lfs.",agel,ageu,".mar")),
                            get(paste0("lfs.",agel,ageu,".uniform.mar")))
}


# single-year-old analysis
for (x in 23:65) {

a <- lfs %>%
  filter(age == x) %>%
  group_by(year, qualfield, age, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qualfield, gender, lfs) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE))
# Add total bach + dip totals
b <- lfs %>%
  filter(age == x) %>%
  group_by(year, qual, age, gender, lfs) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qual, gender, lfs) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
#Combine rows
assign(paste0("lfs.",x,".uniform"),
       bind_rows(a, b) %>% 
         ungroup() %>% 
         select(-qual) %>%                   
         group_by(year, qualfield, gender, lfs) %>% 
         mutate(group = paste0("lfs.",x,".uniform"),
                agegroup = paste0(x)))



lfs.combined <- bind_rows(lfs.combined, 
                          get(paste0("lfs.",x, ".uniform")))
}




##Create grouping and add code
lfs.combined <- lfs.combined %>% mutate(code = if_else(is.na(married), gender, 
                                                       if_else(married == TRUE, paste0(gender,".mar"),
                                                               paste0(gender,".unmar"))))



## Restructure to variable format
lfs.combined <- lfs.combined %>% 
  dplyr::rename(gender_detail = gender) %>% 
  mutate(gender     = if_else(grepl("f", gender_detail),"f","m"),
         childgroup = if_else(grepl("child", gender_detail), TRUE, FALSE),
         child   = if_else(grepl("haschild", gender_detail), TRUE, 
                           if_else(childgroup == TRUE, FALSE, NA))) %>% 
  
  ## And create groupings for various charts
  mutate(mfc = if_else(gender == "m" & is.na(married), "Male",
                       if_else(gender == "f" & is.na(married) & child == TRUE, "Female, with child",
                               if_else(gender == "f" & is.na(married) & child == FALSE, "Female, no child", 
                                       NA_character_))),
         bach = if_else(grepl("B.", qualfield) | qualfield == "Y12", TRUE, FALSE),
         uniform = grepl("uniform", group),
         field = if_else(qualfield == "Y12" , "Y12" ,substr(qualfield, 4, nchar(qualfield)))
  )


# Finally, generate 2-digit field codes for variable assigning
shortfield <- data.frame(
  "field" = substr(unique(lfs.combined$qualfield), 4, nchar(unique(lfs.combined$qualfield))),
  "shortfield" = c("ag",
                   "com",
                   "den",
                   "edu",
                   "eng",
                   "hum",
                   "it",
                   "law",
                   "mth",
                   "med",
                   "nur",
                   "oca",
                   "ohe",
                   "per",
                   "sci",
                   "y12",
                   "all"
  ))

# Add shortfield to lfs.combined 
lfs.combined <- left_join(lfs.combined, shortfield) %>% mutate(field = if_else(shortfield == "y12", "Y12", field))


# And set order for fields
qualfields.order <- c( "Y12", 
                       "B. all",
                       "B. Engineering", 
                       "B. Law",
                       "B. Medicine", 
                       "B. Dentistry", 
                       "B. Commerce", 
                       "B. IT",
                       "B. Education",
                       "B. Agriculture",
                       "B. Maths",
                       "B. Other health", 
                       "B. Science (excl maths)", 
                       "B. Nursing", 
                       "B. Humanities", 
                       "B. Other CA",
                       "B. Performing Arts",
                       "D. all",
                       "D. Engineering", 
                       "D. Law",
                       "D. Medicine", 
                       "D. Dentistry", 
                       "D. Commerce", 
                       "D. IT",
                       "D. Education",
                       "D. Agriculture",
                       "D. Maths",
                       "D. Other health", 
                       "D. Science (excl maths)", 
                       "D. Nursing", 
                       "D. Humanities", 
                       "D. Other CA",
                       "D. Performing Arts")

# Order levels
lfs.combined <- lfs.combined %>%mutate(qualfield = factor(qualfield, levels = qualfields.order))




##2.2: OCC summaries####

###2.2.1 All ges (18-65) ####

# Looking at all ages 18-65, ignoring marriage
occ.anyage <- occ %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qualfield, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count))

# Add total bach + dip totals
add <- occ %>% filter(qual != "Y12") %>% 
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

occ.anyage <- bind_rows(occ.anyage, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, gender, occ)


# Looking at working age, ignoring marriage -- UNIFORM AGE WEIGHTING
occ.anyage.uniform <- occ %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qualfield, age, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qualfield, gender, occ) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE))

add <- occ %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, age, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qual, gender, occ) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

occ.anyage.uniform <- bind_rows(occ.anyage.uniform, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, gender, occ)



# Looking at all ages, by marriage
occ.anyage.mar <- occ %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qualfield, married, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count))

# Add total bach + dip totals
add <- occ %>% filter(qual != "Y12") %>% 
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, married, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(pc = count / sum(count)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

occ.anyage.mar <- bind_rows(occ.anyage.mar, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, married, gender, occ)




# Looking at working age, by marriage -- UNIFORM AGE WEIGHTING
occ.anyage.uniform.mar <- occ %>%
  filter(age >= 18 & age <= 65) %>%  
  group_by(year, qualfield, married, age, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qualfield, married, gender, occ) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE))

add <- occ %>%
  filter(age >= 18 & age <= 65) %>% 
  group_by(year, qual, married, age, gender, occ) %>% 
  summarise(count = sum(count)) %>% 
  mutate(temp.pc = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(year, qual, married, gender, occ) %>% 
  summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                            ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))

occ.anyage.uniform.mar <- bind_rows(occ.anyage.uniform.mar, add) %>% 
  ungroup() %>% 
  select(-qual) %>%                   
  group_by(year, qualfield, married, gender, occ)



## Combine into single dataset to enable plotting of any combination
occ.anyage <- mutate(occ.anyage, group = "occ.anyage")
occ.anyage.uniform <- mutate(occ.anyage.uniform, group = "occ.anyage.uniform")
occ.anyage.mar <- mutate(occ.anyage.mar, group = "occ.anyage.mar")
occ.anyage.uniform.mar <- mutate(occ.anyage.uniform.mar, group = "occ.anyage.uniform.mar")



##Create half-finished final dataset (to be completed with ages in the loop to follow)        
occ.combined <- bind_rows(occ.anyage, occ.anyage.uniform,
                          occ.anyage.mar, occ.anyage.uniform.mar)


##Create grouping and add code
occ.combined <- occ.combined %>% mutate(code = if_else(is.na(married), gender, 
                                                       if_else(married == TRUE, paste0(gender,".mar"),
                                                               paste0(gender,".unmar"))))




###2.2.2 By age group: 25-34, 35-44, ..., 55-64 year olds####

for(agel in seq(from=15, to=55, by=10)){
  ageu = agel + 9
  print(paste0("Preparing OCC file for ", agel, " to ", ageu, " year olds"))
  # Ignoring marriage; no uniform age weighting
  a <- occ %>% 
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count)) 
  # Add total bach + dip totals
  b <- occ %>% filter(qual != "Y12") %>% 
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("occ.",agel,ageu),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, gender, occ) %>% 
           mutate(group = paste0("occ.",agel,ageu),
                  agegroup = paste0(agel,ageu))
  )
  
  # Ignoring marriage -- UNIFORM AGE WEIGHTING
  a <- occ %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, age, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qualfield, gender, occ) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE))
  # Add total bach + dip totals
  b <- occ %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, age, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qual, gender, occ) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("occ.",agel,ageu,".uniform"),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, gender, occ) %>% 
           mutate(group = paste0("occ.",agel,ageu,".uniform"),
                  agegroup = paste0(agel,ageu)) 
  )
  
  
  # By marriage -- no uniform age weighting
  a <- occ %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, married, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count))    
  # Add total bach + dip totals
  b <- occ %>% filter(qual != "Y12") %>% 
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, married, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(pc = count / sum(count)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("occ.",agel,ageu,".mar"),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, married, gender, occ) %>% 
           mutate(group = paste0("occ.",agel,ageu,".mar"),
                  agegroup = paste0(agel,ageu))
  )
  
  # By marriage -- UNIFORM AGE WEIGHTING
  a <- occ %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qualfield, married, age, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qualfield, married, gender, occ) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE))
  
  b <- occ %>%
    filter(age >= agel & age <= ageu) %>% 
    group_by(year, qual, married, age, gender, occ) %>% 
    summarise(count = sum(count)) %>% 
    mutate(temp.pc = count / sum(count)) %>% 
    ungroup() %>% 
    group_by(year, qual, married, gender, occ) %>% 
    summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
    mutate(qualfield = ifelse(qual == "Bachelor", "B. all", 
                              ifelse(qual == "Diploma & AdDip", "D. all", "Y12")))
  #Combine rows
  assign(paste0("occ.",agel,ageu,".uniform.mar"),
         bind_rows(a, b) %>% 
           ungroup() %>% 
           select(-qual) %>%                   
           group_by(year, qualfield, married, gender, occ) %>% 
           mutate(group = paste0("occ.",agel,ageu,".uniform.mar"),
                  agegroup = paste0(agel,ageu))
  )
  
  
  ## Incrementally add all age group files to combined dataset
  occ.combined <- bind_rows(occ.combined, 
                            get(paste0("occ.",agel,ageu)),
                            get(paste0("occ.",agel,ageu,".uniform")),
                            get(paste0("occ.",agel,ageu,".mar")),
                            get(paste0("occ.",agel,ageu,".uniform.mar")))
}


##Create grouping and add code, child
occ.combined <- occ.combined %>% 
  mutate(code =  if_else(is.na(married), gender, 
                         if_else(married == TRUE, paste0(gender,".mar"),
                                 paste0(gender,".unmar"))))


## Restructure to variable format
occ.combined <- occ.combined %>% 
  dplyr::rename(gender_detail = gender) %>% 
  mutate(gender     = if_else(grepl("f", gender_detail),"f","m"),
         childgroup = if_else(grepl("child", gender_detail), TRUE, FALSE),
         child   = if_else(grepl("haschild", gender_detail), TRUE, 
                           if_else(childgroup == TRUE, FALSE, NA))) %>% 
  
  ## And create groupings for various charts
  mutate(mfc = if_else(gender == "m" & is.na(married), "Male",
                       if_else(gender == "f" & is.na(married) & child == TRUE, "Female, with child",
                               if_else(gender == "f" & is.na(married) & child == FALSE, "Female, no child", 
                                       NA_character_))),
         bach = if_else(grepl("B.", qualfield) | qualfield == "Y12", TRUE, FALSE),
         uniform = grepl("uniform", group),
         field = if_else(qualfield == "Y12" , "Y12" ,substr(qualfield, 4, nchar(qualfield)))
  )


# Add shortfield to lfs.combined 
occ.combined <- left_join(occ.combined, shortfield) %>% mutate(field = if_else(shortfield == "y12", "Y12", field))

# Set field order
occ.combined <- occ.combined %>% mutate(qualfield = factor(qualfield, levels = qualfields.order))










#3: Functions: Defining functions to create LFS and OCC charts####

##flex.chart ####
flex.chart <- function(type = lfs,
                       age = "all", 
                       uniform = TRUE, 
                       level = "bach", 
                       pair = c("m.mar", "f.haschild"),
                       chartfields = fields,
                       display = TRUE,
                       save = TRUE,
                       object = FALSE,
                       exportdata = FALSE) {
  
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
  
  ##Enquote type and facet to allow for use as a name in ggplot function
  type <- enquo(type)
  
  
  ## Create chart
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
               labeller = label_wrap_gen(width = 10)) +
    
    NULL
  
  
  chart.name <- paste0(typename, "_",age,"age_",level,"level_",pair[1],"_",pair[2])
  
  
  if (object == TRUE) {
    assign(chart.name,
           current.chart,
           .GlobalEnv) 
    print(paste0("Object ",chart.name," is now saved."))
  }
  
  if (exportdata == TRUE) {
    write_csv(current.chart$data,
              path = paste0("output/",chart.name,".csv"))
  }
  
  if (display == TRUE) plot(current.chart)
  
  if (save == TRUE) {
    ggsave(paste0("atlas/",chart.name,".pdf"), 
           plot = last_plot(), device = "pdf",
           scale = 1, width = 297, height = 210, units = "mm",
           dpi = 320, limitsize = TRUE)
  }
  
  
}





#4: Adhoc Boomerang charts ####

flex.chart(type = lfs,
           age = "2534",
           pair = (c("f", "m")),
           uniform = TRUE,
           display = TRUE,
           object = FALSE,
           save = FALSE,
           exportdata = TRUE
)

flex.chart(type = occ,
           age = "2534",
           pair = (c("f", "m")),
           uniform = TRUE,
           display = TRUE,
           object = FALSE,
           save = FALSE,
           exportdata = TRUE
)


flex.chart(type = lfs,
           age = "22",
           pair = (c("f", "m")),
           uniform = TRUE,
           display = TRUE,
           object = FALSE,
           save = FALSE,
           exportdata = TRUE
)



flex.chart(type = lfs,
           age = "24",
           pair = (c("f", "m")),
           uniform = TRUE,
           display = TRUE,
           object = FALSE,
           save = FALSE,
           exportdata = TRUE
)






#5: Animation(?) ####
library(gganimate)
library(tweenr)


library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Prep animate
  ## Set labels and colours for each TYPE (this is called via TYPE within the ggplot function)
lfs.labels = c( "Unemployed",
                "NILF",
                "Employed AWAY",
                "Employed PT",
                "Employed FT")
lfs.colors = c( gred,
                ggrey,
                gdark,
                gorange,
                gyellow
                )


# Prep data
  animate <- lfs.combined %>% 
             mutate(single = grepl("^[0-9]{2}$", agegroup)) %>% 
             filter(single == TRUE,
                    qualfield == "Y12" | qualfield == "B. all",
                    uniform == TRUE) %>% 
             mutate(age = as.integer(agegroup)) %>% 
             group_by(year, qualfield, gender_detail, age, lfs) %>% 
             summarise(pc = mean(pc)) %>% ungroup() %>% 
             mutate(gender_detail = case_when(gender_detail == "f" ~ "Female",
                                              gender_detail == "f.nochild" ~ "Female, no child",
                                              gender_detail == "f.haschild" ~ "Female, has child",
                                              gender_detail == "m" ~ "Male"),
                    qualfield = case_when(qualfield == "Y12" ~ "Year 12 only",
                                          qualfield == "B. all" ~ "Bachelor graduate"))
  
  
  animate %>% 
    filter(gender_detail == "Male" |  gender_detail == "Female") %>% 
    # filter(age == 30) %>%  ##
    ggplot(aes(y = pc, x = year, fill = lfs, order = as.numeric(lfs))) + 
      geom_bar(stat="identity") +
      theme_minimal() +
      labs(title = "Labour-force status",
           x = "",
           y = ""
      ) +
      theme(plot.title    = element_text(size = 12, hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
            legend.position="bottom",
            legend.title = element_blank(),
            legend.text = element_text(size=10),
            strip.text.x = element_text(size = 12, colour = "black", angle = 0, hjust = 1, vjust = 1),
            axis.text.x=element_text(size=12, angle = 0),
            panel.grid.major.x=element_blank()
      ) +
      scale_fill_manual(values = c("Not in the labour force" = ggrey,
                                   "Unemployed" = gred,
                                   "Employed, away from work" = gdark,
                                   "Employed, worked part-time" = gorange,
                                   "Employed, worked full-time" = gyellow),
                        breaks = c("Not in the labour force",
                                   "Unemployed",
                                   "Employed, away from work",
                                   "Employed, worked part-time",
                                   "Employed, worked full-time"),
                        labels = c("NILF",
                                   "Unemployed",
                                   "Employed AWAY",
                                   "Employed PT",
                                   "Employed FT")) +
     facet_grid(. ~ qualfield + gender_detail) +
      # ## gganimate
      labs(title = 'Age: {frame_time}', x = 'Year', y = '') +
      transition_time(age) +
      enter_grow() +
      exit_shrink() +
      ease_aes('sine-in-out') +
      NULL
    