## Census data: 

library(tidyverse)
setwd("~/Dropbox (Grattan Institute)/Mapping Australian higher ed 2018/Mapping 2018 data/census/occupation")

# Define some Grattan-y colours:
glightyellow <- "#FFE07F"
gyellow <- "#FFC35A"
gorange <- "#F68B33"
gdark <- "#D4582A"
gred <- "#A02226"
gdarkest <- "#621214"

ggrey <- "#828282"

## Define groups
married.def <- c("Married in a registered marriage", "Married in a de facto marriage")
foe.table <- read_csv("foe_table.csv") %>% select(-X3)


######Labour force status#######

####1: Load in data####

####1.1: LFS data####

##Read in 2006 field data
  x <- "06"

  lfs06 <- read_csv(paste0("sheets/",x,"_long_bach_lfs.csv"), n_max = 478848) %>% 
    rename(qual = "QALLP - 1 Digit Level",
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
    rename(field = broad, 
           foe4digit = foe) %>% 
    filter(lfs != "Not applicable" & lfs != "Not stated", 
           field != "Not applicable" & field != "Not stated")
  

  
  
  # 2006 Year 12 data  
      lfs06.y12 <- read_csv(paste0("sheets/",x,"_long_y12_lfs.csv")) %>% 
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
  
  lfs11 <- read_csv(paste0("sheets/",x,"_long_bach_lfs.csv")) %>% 
    rename(qual = "QALLP Non-School Qualification: Level of Education",
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
    rename(field = broad, 
           foe4digit = foe) %>% 
    filter(lfs != "Not applicable" & lfs != "Not stated", 
           field != "Not applicable" & field != "Not stated")

  
  # 2011 Year 12 data 
  lfs11.y12 <- read_csv(paste0("sheets/",x,"_long_y12_lfs.csv")) %>% 
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
    lfs16 <- read_csv(paste0("sheets/",x,"_long_bach_lfs.csv")) %>% 
                rename(qual = "QALLP - 1 Digit Level",
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
                rename(field = broad, 
                       foe4digit = foe) %>% 
                filter(lfs != "Not applicable" & lfs != "Not stated", 
                       field != "Not applicable" & field != "Not stated") %>%
                select(-starts_with("f.child"),
                       -marriage,
                       -drop,
                       -total) 
  
      
    # 2016 Year 12 data  
    lfs16.y12 <- read_csv(paste0("sheets/",x,"_long_y12_lfs.csv")) %>% 
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

    
    

    
    
    
####1.2: OCC data####    

    ##Read in 2006 field data
    x <- "06"
    
    occ06 <- read_csv(paste0("sheets/",x,"_long_bach_occ.csv")) %>% 
      rename(qual = "QALLP - 1 Digit Level",
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
             qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
             qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
             year = paste0("20",x)) %>% 
      select(-starts_with("f.child"),
             -marriage,
             -total) %>% 
      left_join(., foe.table, by = "foe") %>%  ## add Grattan fields 
      rename(field = broad, 
             foe4digit = foe) %>% 
      filter(occ != "Inadequately described" & occ != "Not stated", 
             field != "Not applicable" & field != "Not stated")
    
    
    
    
    !!!! not done Y12 yet !!!!
      
      # 2006 Year 12 data  
      occ06.y12 <- read_csv(paste0("sheets/",x,"_long_y12_occ.csv")) %>% 
      rename(age = "AGEP Age",
             marriage = "MDCP Social Marital Status",
             occ = "LFSP Labour Force Status",
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
             occ = replace(occ, occ=="Unemployed, looking for full-time work", "Unemployed"),
             occ = replace(occ, occ=="Unemployed, looking for part-time work", "Unemployed"),
             qual = replace(qual, qual=="Bachelor Degree Level", "Bachelor"),
             qual = replace(qual, qual=="Advanced Diploma and Diploma Level", "Diploma & AdDip"),
             year = paste0("20",x)) %>% 
      select(-starts_with("f.child"),
             -marriage,
             -total) %>% 
      filter(occ != "Not applicable" & occ != "Not stated")
    
    occ06 <- bind_rows(occ06, occ06.y12)
    
    
    
    
        
####LFS 2: Creating summary version####
    
    ## ALL working age: 18-65 years old
    
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
                  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
        
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
            mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
          
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
                  mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
          
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
            mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
          
          lfs.anyage.uniform.mar <- bind_rows(lfs.anyage.uniform.mar, add) %>% 
            ungroup() %>% 
            select(-qual) %>%                   
            group_by(year, qualfield, married, gender, lfs)
          
          
          
          
          
          
    ##Young group: 25-34 year olds##      

    # Looking at 25-34 year olds, ignoring marriage -- no uniform age weighting
    lfs.2534 <- lfs %>%
                filter(is.2534 == TRUE) %>% 
                group_by(year, qualfield, gender, lfs) %>% 
                summarise(count = sum(count)) %>% 
                mutate(pc = count / sum(count))    
    
          # Add total bach + dip totals
          add <- lfs %>% filter(qual != "Y12") %>% 
            filter(is.2534 == TRUE) %>% 
            group_by(year, qual, gender, lfs) %>% 
            summarise(count = sum(count)) %>% 
            mutate(pc = count / sum(count)) %>% 
          mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
          
          lfs.2534 <- bind_rows(lfs.2534, add) %>% 
            ungroup() %>% 
            select(-qual) %>%                   
            group_by(year, qualfield, gender, lfs)
          
          
          # Looking at 25-34 year olds, ignoring marriage -- UNIFORM AGE WEIGHTING
          lfs.2534.uniform <- lfs %>%
            filter(is.2534 == TRUE) %>% 
            group_by(year, qualfield, age, gender, lfs) %>% 
            summarise(count = sum(count)) %>% 
            mutate(temp.pc = count / sum(count)) %>% 
            ungroup() %>% 
            group_by(year, qualfield, gender, lfs) %>% 
            summarise(pc = mean(temp.pc, na.rm = TRUE))
          
          add <- lfs %>%
            filter(is.2534 == TRUE) %>% 
            group_by(year, qual, age, gender, lfs) %>% 
            summarise(count = sum(count)) %>% 
            mutate(temp.pc = count / sum(count)) %>% 
            ungroup() %>% 
            group_by(year, qual, gender, lfs) %>% 
            summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
            mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
          
          lfs.2534.uniform <- bind_rows(lfs.2534.uniform, add) %>% 
            ungroup() %>% 
            select(-qual) %>%                   
            group_by(year, qualfield, gender, lfs)
    
                
    
    # Looking at 25-34 year olds, by marriage -- no uniform age weighting
    lfs.2534.mar <- lfs %>%
                    filter(is.2534 == TRUE) %>% 
                    group_by(year, qualfield, married, gender, lfs) %>% 
                    summarise(count = sum(count)) %>% 
                    mutate(pc = count / sum(count))    
    
          # Add total bach + dip totals
          add <- lfs %>% filter(qual != "Y12") %>% 
            filter(is.2534 == TRUE) %>% 
            group_by(year, qual, married, gender, lfs) %>% 
            summarise(count = sum(count)) %>% 
            mutate(pc = count / sum(count)) %>% 
          mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
          
          lfs.2534.mar <- bind_rows(lfs.2534.mar, add) %>% 
            ungroup() %>% 
            select(-qual) %>%                   
            group_by(year, qualfield, married, gender, lfs)
          
    
          
          # Looking at 25-34 year olds, by marriage -- UNIFORM AGE WEIGHTING
          lfs.2534.uniform.mar <- lfs %>%
            filter(is.2534 == TRUE) %>% 
            group_by(year, qualfield, married, age, gender, lfs) %>% 
            summarise(count = sum(count)) %>% 
            mutate(temp.pc = count / sum(count)) %>% 
            ungroup() %>% 
            group_by(year, qualfield, married, gender, lfs) %>% 
            summarise(pc = mean(temp.pc, na.rm = TRUE))
          
          add <- lfs %>%
            filter(is.2534 == TRUE) %>% 
            group_by(year, qual, married, age, gender, lfs) %>% 
            summarise(count = sum(count)) %>% 
            mutate(temp.pc = count / sum(count)) %>% 
            ungroup() %>% 
            group_by(year, qual, married, gender, lfs) %>% 
            summarise(pc = mean(temp.pc, na.rm = TRUE)) %>% 
            mutate(qualfield = ifelse(qual == "Bachelor", "B. all", "D. all"))
          
          lfs.2534.uniform.mar <- bind_rows(lfs.2534.uniform.mar, add) %>% 
            ungroup() %>% 
            select(-qual) %>%                   
            group_by(year, qualfield, married, gender, lfs)
          
    ## Combine into single dataset to enable plotting of any combination
        lfs.2534 <- mutate(lfs.2534, group = "lfs.2534")
        lfs.2534.uniform <- mutate(lfs.2534.uniform, group = "lfs.2534.uniform")
        lfs.2534.mar <- mutate(lfs.2534.mar, group = "lfs.2534.mar")
        lfs.2534.uniform.mar <- mutate(lfs.2534.uniform.mar, group = "lfs.2534.uniform.mar")
        lfs.anyage <- mutate(lfs.anyage, group = "lfs.anyage")
        lfs.anyage.uniform <- mutate(lfs.anyage.uniform, group = "lfs.anyage.uniform")
        lfs.anyage.mar <- mutate(lfs.anyage.mar, group = "lfs.anyage.mar")
        lfs.anyage.uniform.mar <- mutate(lfs.anyage.uniform.mar, group = "lfs.anyage.uniform.mar")

        ##Create final product:        
        lfs.combined <- bind_rows(lfs.2534, lfs.2534.uniform,
                                  lfs.2534.mar, lfs.2534.uniform.mar,
                                  lfs.anyage, lfs.anyage.uniform,
                                  lfs.anyage.mar, lfs.anyage.uniform.mar) %>% 
                        select(-count)
        ##Create grouping
        lfs.combined <- lfs.combined %>% mutate(code = if_else(is.na(married), gender, 
                                          if_else(married == TRUE, paste0(gender,".mar"),
                                          paste0(gender,".unmar"))))
        
        # lfs.combined <- mutate(code = if_else((gender=="m." & ))
        # = "m.",
        # = "f.",
        # = "f.nochild",
        # = "f.haschild",
        # = "m.mar",
        # = "f.mar",
        # = "f.nochild.mar",
        # = "f.haschild.mar",
        # = "m.unmar",
        # = "f.unmar",
        # = "f.nochild.unmar",
        # = "f.haschild.unmar",
        
        # 
        # age = "all"
        # uniform = TRUE
        # level = "bach" 
        # pair = c("m.mar", "f")
        # chartfields = fields    
        # 
        #    
          
#### Functions: Defining functions to create LFS and OCC charts####
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
          
          
####LFS 3: Plotting####

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
                  
    
###Labour force status###
  
        #Settings
        lfs.labels = c("Employed AWAY", 
                        "Employed FT", 
                        "Employed PT",
                        "NILF",
                        "Unemployed")
        lfs.colors = c( gyellow,
                        gorange,
                        gdark,
                        gred,
                        gdarkest)

#### LFS Charts ####
        # reminder: lfs.chart syntax, noting that uniform can only be used on 25-34
        # lfs.chart(age = "all", uniform = FALSE, level = "bach", pair = c("m.", "f."))
        
        # All pairs of codes:
        allpairs <- combn(unique(lfs.combined$code),2)
        
          #lfs.1.1: all ages, m/f
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
        
        
          
    
    ####Bin
            
names(lfs)        
unique(data2016$lfs)
              