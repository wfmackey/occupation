

##Create half-finished final dataset (to be completed with ages in the loop to follow)        
occ.combined <- bind_rows(occ.anyage, occ.anyage.uniform,
                          occ.anyage.mar, occ.anyage.uniform.mar)




##By age group: 25-34, 35-44, ..., 55-64 year olds##

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


##Create grouping and add code
occ.combined <- occ.combined %>% mutate(code = if_else(is.na(married), gender, 
                                                       if_else(married == TRUE, paste0(gender,".mar"),
                                                               paste0(gender,".unmar"))))



