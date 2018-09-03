## Census data:

library(tidyverse)
library(gridExtra)
setwd("~/Dropbox (Grattan Institute)/Mapping Australian higher ed 2018/Mapping 2018 data/census/occupation")

# Define some Grattan-y colours:
glightyellow <- "#FFE07F"
gyellow <- "#FFC35A"
gorange <- "#F68B33"
gdark <- "#D4582A"
gred <- "#A02226"
gdarkest <- "#621214"

ggrey <- "#828282"
gblack <- "#141413"

## Define groups
married.def <- c("Married in a registered marriage", "Married in a de facto marriage")
foe.table <- read_csv("foe_table.csv") %>% select(-X3)




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

lfs06 <- read_csv(paste0("sheets/",x,"_long_bach_lfs.csv"), n_max = 478848) %>% 
  dplyr::rename(qual = "QALLP - 1 Digit Level",
         age = "AGEP Age",
         marriage = "MDCP Social Marital Status",
         foe = "QALFP - 4 Digit Level",
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
  mutate(f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
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
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
         foe4digit = foe) %>% 
  filter(lfs != "Not applicable" & lfs != "Not stated", 
         field != "Not applicable" & field != "Not stated")




# 2006 Year 12 data  
lfs06.y12 <- read_csv(paste0("sheets/",x,"_long_y12_lfs.csv")) %>% 
  dplyr::rename(age = "AGEP Age",
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

lfs11 <- read_csv(paste0("sheets/",x,"_long_bach_lfs.csv")) %>% 
  dplyr::rename(qual = "QALLP Non-School Qualification: Level of Education",
         age = "AGEP Age in Single Years",
         marriage = "MDCP Social Marital Status",
         foe = "QALFP Non-School Qualification: Field of Study",
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
  mutate(f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
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
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
         foe4digit = foe) %>% 
  filter(lfs != "Not applicable" & lfs != "Not stated", 
         field != "Not applicable" & field != "Not stated")


# 2011 Year 12 data 
lfs11.y12 <- read_csv(paste0("sheets/",x,"_long_y12_lfs.csv")) %>% 
  dplyr::rename(age = "AGEP Age",
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
lfs16 <- read_csv(paste0("sheets/",x,"_long_bach_lfs.csv")) %>% 
  dplyr::rename(qual = "QALLP - 1 Digit Level",
         age = "AGEP Age",
         marriage = "MDCP Social Marital Status",
         foe = "QALFP - 4 Digit Level",
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
  mutate(f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
         f = ifelse(total - m < 0, 0, total - m),
         married = (marriage == married.def[1] | marriage == married.def[2]),
         is.2534 = (age >= 25 & age <= 34),
         lfs = replace(lfs, lfs=="Unemployed, looking for full-time work", "Unemployed"),
         lfs = replace(lfs, lfs=="Unemployed, looking for part-time work", "Unemployed"),
         qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
         qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
         year = paste0("20",x)) %>% 
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
         foe4digit = foe) %>% 
  filter(lfs != "Not applicable" & lfs != "Not stated", 
         field != "Not applicable" & field != "Not stated") %>%
  select(-starts_with("f.child"),
         -marriage,
         -drop,
         -total) 


# 2016 Year 12 data  
lfs16.y12 <- read_csv(paste0("sheets/",x,"_long_y12_lfs.csv")) %>% 
  dplyr::rename(age = "AGEP Age",
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

occ06 <- read_csv(paste0("sheets/",x,"_long_bach_occ.csv"), n_max = 658416) %>% 
  dplyr::rename(qual = "QALLP - 1 Digit Level",
         age = "AGEP Age",
         marriage = "MDCP Social Marital Status",
         foe = "QALFP - 4 Digit Level",
         occ = "OCCP - 1 Digit Level",
         f.nochild = "None",
         f.child1 = "One",
         f.child2 = "Two",
         f.child3 = "Three",
         f.child4 = "Four",
         f.child5 = "Five",
         f.child6 = "Six or more",
         m = "Not applicable",
         total = "Total") %>%
  mutate(f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
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
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
         foe4digit = foe) %>% 
  filter(occ != "Inadequately described" & occ != "Not stated", 
         field != "Not applicable" & field != "Not stated")



# 2006 Year 12 data  
occ06.y12 <- read_csv(paste0("sheets/",x,"_long_y12_occ.csv"))%>% 
  dplyr::rename(age = "AGEP Age",
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

occ11 <- read_csv(paste0("sheets/",x,"_long_bach_occ.csv"), n_max = 658416) %>% 
  dplyr::rename(qual = "QALLP - 1 Digit Level",
         age = "AGEP Age",
         marriage = "MDCP Social Marital Status",
         foe = "QALFP - 4 Digit Level",
         occ = "OCCP - 1 Digit Level",
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
  mutate(f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
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
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
         foe4digit = foe) %>% 
  filter(occ != "Inadequately described" & occ != "Not stated", 
         field != "Not applicable" & field != "Not stated")



# 2011 occupation Year 12 data  
occ11.y12 <- read_csv(paste0("sheets/",x,"_long_y12_occ.csv")) %>%  
  dplyr::rename(age = "AGEP Age in Single Years",
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

occ16 <- read_csv(paste0("sheets/",x,"_long_bach_occ.csv"), n_max = 658416) %>% 
  dplyr::rename(qual = "QALLP - 1 Digit Level",
         age = "AGEP Age",
         marriage = "MDCP Social Marital Status",
         foe = "QALFP - 4 Digit Level",
         occ = "OCCP - 1 Digit Level",
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
  mutate(f.haschild = ifelse(total - m - f.nochild < 0, 0, total - m - f.nochild),
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
  left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
  dplyr::rename(field = broad, 
         foe4digit = foe) %>% 
  filter(occ != "Inadequately described" & occ != "Not stated", 
         field != "Not applicable" & field != "Not stated")



# 2016 occupation Year 12 data  
occ16.y12 <- read_csv(paste0("sheets/",x,"_long_y12_occ.csv")) %>%  
  dplyr::rename(age = "AGEP Age",
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
  dplyr::rename(occ.detail = occ,
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
##3.1: lfs.chart ####
lfs.chart <- function(age = "all", 
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
      usedata <- lfs.combined %>% filter(grepl('lfs.2534.uniform', group))
      print("using lfs.2534.uniform")
      title <- paste0("Labour force status of (", pairlabel[1],") & (", pairlabel[2], "), 25-34 year olds (uniform age-weighted)")
    } 
    else {
      usedata <- lfs.combined %>% filter(grepl('lfs.2534', group),
                                         !grepl('lfs.2534.uniform', group))
      print("using lfs.2534")
      title <- paste0("Labour force status of (", pairlabel[1],") & (", pairlabel[2], "), 25-34 year olds")
    }
  } else {
    if(uniform == TRUE) {
      usedata <- lfs.combined %>% filter(grepl('lfs.anyage.uniform', group))
      print("using lfs.anyage.uniform")
      title <- paste0("Labour force status of [", pairlabel[1],"] and [", pairlabel[2], "], 18-65 year olds (uniform age-weighted)")
    } else {
      usedata <- lfs.combined %>% filter(grepl('lfs.anyage', group),
                                         !grepl('lfs.anyage.uniform', group))
      print("using lfs.anyage")
      title <- paste0("Labour force status of [", pairlabel[1],"] and [", pairlabel[2], "], 18-65 year olds")
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
  
  
  
  lfs.current.chart <- usedata %>% 
    filter(code == pair[1] | code == pair[2]) %>% 
    arrange(code) %>% 
    
    filter(qualfield %in% chartfields) %>% 
    ggplot(aes(x = year, color=lfs, group=interaction(lfs, code))) + 
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
    scale_color_manual(labels = lfs.labels, 
                       values =  lfs.colors) +
    scale_linetype_manual(labels = pairlabel,
                          values = c("solid", "dotted")) + 
    facet_wrap(. ~ qualfield, ncol= facetcol,
               labeller = label_wrap_gen(width = 10)) +
    
    NULL
  
  if (display == TRUE) plot(lfs.current.chart)
  
  current.chart.name <- paste0("lfs_",age,"age_",level,"level_",pair[1],"_",pair[2],".pdf")
  
  ggsave(current.chart.name, plot = last_plot(), device = "pdf", path = "plots",
         scale = 1, width = 297, height = 210, units = "mm",
         dpi = 320, limitsize = TRUE)
  
  
}


##3.2: occ.chart ####
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



##3.3: Flexible chart
##3.3: flex.chart ####
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
            path = paste0("chartdata/",chart.name,".csv"))
  }
  
  if (display == TRUE) plot(current.chart)
  
  if (save == TRUE) {
    ggsave(paste0(chart.name,".pdf"), 
           plot = last_plot(), device = "pdf", path = "plots",
           scale = 1, width = 297, height = 210, units = "mm",
           dpi = 320, limitsize = TRUE)
  }
  
  
}



#4: Plotting####
runallcharts <- FALSE


## 4.1: LFS Charts ####    

#Settings
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

# reminder: lfs.chart syntax, noting that uniform can only be used on 25-34
# lfs.chart(age = "all", uniform = FALSE, level = "bach", pair = c("m.", "f."))

# All pairs of codes:
allpairs <- combn(unique(lfs.combined$code),2)

# Create all possible permutations:
if (runallcharts == TRUE) {
  for (age in c("all", "2534")) {
    for (level in c("bach", "dip", "all")) {
      for (x in 1:ncol(allpairs)) {
        lfs.chart(age = age, 
                  level = level, 
                  pair = c(allpairs[1,x], allpairs[2,x]),
                  display = FALSE)              
      }
    }
  }
}



## 4.2: OCC Charts ####
#Settings
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

# Generate all pairs
allpairs <- combn(unique(occ.combined$code),2)

# Create all possible permutations:
if (runallcharts == TRUE) {
  for (age in c("all", "2534")) {
    for (level in c("bach", "dip", "all")) {
      for (x in 1:ncol(allpairs)) {
        occ.chart(age = age, 
                  level = level, 
                  pair = c(allpairs[1,x], allpairs[2,x]),
                  display = FALSE)              
      }
    }
  }
}







#5: Adhoc Boomerang charts ####

#5.1 LF participation (45-44, 55-64) bachelor (male, female) by married v unmarried
for(a in c("4554","5564")) {
  for(g in c("m","f")) {
    flex.chart(type = lfs,
               age = a,
               pair = c(paste0(g,".mar"),paste0(g,".unmar")),
               level = "bach",
               display = TRUE
    )
    
  }
}

#5.2 LF participation (35-44, 45-54) bachelor (male, female) by married v unmarried
for(a in c("3544","4554")) {
  flex.chart(type = lfs,
             age = a,
             pair = c("f.haschild.mar","f.haschild.unmar"),
             level = "bach",
             display = TRUE
  )
}


#5.3 LF participation/occ (45-44) bachelor (female) by all AND by child/no child
for(a in c("4554")) {
  flex.chart(type = lfs,
             age = a,
             pair = c("f.haschild","f.nochild"),
             level = "bach",
             display = TRUE
  )
}


for(a in c("4554")) {
  flex.chart(type = lfs,
             age = a,
             pair = c("f","m"),
             level = "bach",
             display = TRUE
  )
}


for(a in c("4554")) {
  flex.chart(type = occ,
             age = a,
             pair = c("f.haschild","f.nochild"),
             level = "bach",
             display = TRUE
  )
}


for(a in c("4554")) {
  flex.chart(type = occ,
             age = a,
             pair = c("f","m"),
             level = "bach",
             display = TRUE
  )
}







flex.chart(type = lfs,
           age = "2534",
           pair = (c("f.nochild", "m")),
           uniform = TRUE,
           display = FALSE,
           object = TRUE,
           exportdata = TRUE
           )

flex.chart(type = lfs,
           age = "2534",
           pair = (c("f.haschild", "f.nochild")),
           uniform = TRUE,
           display = TRUE,
           object = FALSE,
           exportdata = TRUE
)

flex.chart(type = occ,
           age = "2534",
           pair = (c("f.haschild", "f.nochild")),
           uniform = TRUE,
           display = TRUE,
           object = FALSE,
           exportdata = TRUE
)


flex.chart(type = lfs,
           age = "2534",
           pair = (c("f", "m")),
           uniform = TRUE,
           display = FALSE,
           object = TRUE,
           exportdata = TRUE
)



flex.chart(type = occ,
           age = "2534",
           pair = (c("f", "m")),
           uniform = TRUE,
           display = FALSE,
           object = TRUE,
           exportdata = TRUE
)







#6: Additional charts ####

##6.1: marriage/child facet split
typename <- "lfs"
title <- "Chart title"
subtitle <- "Chart subtitle"
chartfields1 <- c("Y12", 
                  "all",
                  "Engineering",
                  "Law",
                  "Medicine",
                  "Commerce",
                  "IT",
                  "Education")


chartfields2 <- c("Other health",
                  "Science (excl maths)",
                  "Nursing",
                  "Maths",
                  "Humanities",
                  "Other CA",
                  "Dentistry",
                  "Performing Arts",
                  "Agriculture")


for (x in 1:2) {
  title <-    c("Labour force status of 25-34 year olds, by field of bachelor degree")
  subtitle <- c(paste0("Page ",x," of 2"))
  fieldgroup <- get(paste0("chartfields",x))
  
  
  assign(paste0("chart",x),
         
         lfs.combined %>% 
           filter(!is.na(mfc),
                  uniform == TRUE,
                  bach == TRUE,
                  agegroup == "2534") %>% 
           filter(field %in% fieldgroup) %>%
           ggplot(aes(x = year, color=lfs, group=lfs)) + 
           geom_line(aes(y = pc), stat="identity") + 
           geom_point(aes(y = pc), stat="identity") + 
           theme_minimal() +
           labs(title = title,
                subtitle = subtitle,
                x = "",
                y = ""
           ) +
           theme(plot.title     = element_text(size = 12, hjust = 0.5),
                 plot.subtitle  = element_text(size = 10, hjust = 0.5, face = "italic"),
                 legend.position="bottom",
                 legend.title   = element_blank(),
                 legend.text    = element_text(size=8),
                 strip.text.x   = element_text(size = 10, colour = "black"),
                 axis.text.x=element_text(size=8, angle = 90),
                 panel.grid.major.x=element_blank()
           ) +
           scale_y_continuous(breaks = seq(0, 1, .1),
                              minor_breaks = seq(0 , 1, .05)) +
           scale_color_manual(labels = get(paste0(typename,".labels")),
                              values =  get(paste0(typename,".colors"))) +
           # scale_linetype_manual(labels = pairlabel,
           #                       values = c("solid", "dotted")) +
           facet_wrap(. ~ field + mfc,
                      ncol= 12,
                      labeller = label_wrap_gen(width = 10)
           ) +
           
           NULL
  )
  
}  

  
  
#   
  