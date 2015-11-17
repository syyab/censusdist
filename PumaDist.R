##########################
# DESCRIPTION
##########################
# File1: State - Census - Puma: https://www.census.gov/geo/maps-data/data/centract_rel.html
# File2: Census - Pop: https://www.census.gov/geo/maps-data/data/tract_rel_download.html
#             headers: https://www.census.gov/geo/maps-data/data/tract_rel_layout.html
# File3: State Name - State FIPS - County FIPS - County Name: https://www.census.gov/geo/reference/codes/cou.html
# File4: State - puma00 - puma10 - proportion of puma00 in puma10: http://mcdc.missouri.edu/data/corrlst/puma2k_puma2010.csv
#       !!remove second row for File4 (the detailed descriptions) for script to work!!
#
# We get pumas associated with a county by getting all unique puma ids matching the fips state code in File1
# Each census tract is in just one PUMA
# So to get the population of a PUMA in a county we sum the population of each census tract for that PUMA in that county
#
# Now we have to distribute the rows of our data
# We take the sex, race splitups and distribute them by the proportion of population of that PUMA in that county
#
# NOTE
# All PUMA ids are relative to a specific state
# All census tracts ids are relative to a specific county and state
# Sex and race are assumed distributed uniformally across counties
##########################
# FUNCTIONS
##########################
# translate_county(state, county) - translates county fips code in statefp to english name
# translate_statefp(state) - translates statefp code to abbrevation (e.g. 33 -> NH)
# translate_state(state) - translates state abbreviation to statefp
# translate_puma10(state, puma) - returns list containing puma00 codes matching to puma10 and their proportion
# get_old_pumas(state, pumas) - takes puma10 codes and returns all puma00 codes matching
# get_pop(state, county, tract) - gets population of census tract in countyfp, statefp
# get_state_dist(state) - gets relative distribution of pumas in each county for scaling ACS PUMA data
# get_acs_df(state) - returns the df with final sex/race rows for puma/counties in statefp
# generate_sheet(state) - takes statefp or state abbrev. and writes the appropriate excel sheet
##########################
library(xlsx)

tract_map <- read.csv("2010_Census_Tract_to_2010_PUMA.txt")
tract_pops <- read.csv("us2010trf.txt", header = FALSE)
fips_names <- read.csv("national_county.txt", header = FALSE)

names(fips_names) <- c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "CLASSFP")
fips_names <- fips_names[,c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME")]

names(tract_pops) <- unlist(strsplit("STATE00,COUNTY00,TRACT00,GEOID00,POP00,HU00,PART00,AREA00,AREALAND00,STATE10,COUNTY10,TRACT10,GEOID10,POP10,HU10,PART10,AREA10,AREALAND10,AREAPT,AREALANDPT,AREAPCT00PT,ARELANDPCT00PT,AREAPCT10PT,AREALANDPCT10PT,POP10PT,POPPCT00,POPPCT10,HU10PT,HUPCT00,HUPCT10", split = ","))
tract_pops <- tract_pops[,c("STATE10", "COUNTY10", "TRACT10", "POP10")]

puma_map <- read.csv("puma2k_puma2010.csv")
puma_map <- puma_map[,c("state","puma2k","puma12","AFACT2")]

translate_county <- function(state, county){
  fips_names$COUNTYNAME[which(fips_names$STATEFP == state & fips_names$COUNTYFP == county)]
}

translate_statefp <- function(state){
  fips_names$STATE[which(fips_names$STATEFP == state)][[1]]
}

translate_state <- function(state){
  fips_names$STATEFP[which(fips_names$STATE == state)][[1]]
}

translate_puma10 <- function(state, puma){
  subs <- puma_map[which(puma_map$state == state & puma_map$puma12 == puma),]
  t <- subs$AFACT2
  names(t) <- subs$puma2k
  t
}

get_old_pumas <- function(state, pumas){
  t <- NULL
  for(puma in pumas){
    t <- c(t, names(translate_puma10(state, puma)))
  }
  sort(unique(as.numeric(as.character(t))))
}

get_pop <- function(state, county, tract){
  subset(tract_pops, tract_pops$STATE10 == state & tract_pops$COUNTY10 == county & tract_pops$TRACT10 == tract)$POP10[1]
}

get_state_dist <- function(state){
  state_tracts <- subset(tract_map, STATEFP == state)
  
  call_get_pop <- function(x) get_pop(x[["STATEFP"]], x[["COUNTYFP"]], x[["TRACTCE"]])
  get_puma_pop <- function(y) lapply(split(y, y$PUMA5CE), function(x) apply(x, 1, call_get_pop))
  
  t <- sapply(split(state_tracts, state_tracts$COUNTYFP), get_puma_pop)
  
  # (puma, pop) pairs for each county
  county_puma_pops <- sapply(t, function(x) sapply(x, sum))   
  # (puma, pop) pairs for the state
  puma_pops <- sapply(get_puma_pop(state_tracts), sum)
  # Now we calculate (puma, pop percentage) pairs for each county
  county_puma_dist <- county_puma_pops
  
  for(i in seq_along(county_puma_pops)){
    county <- t[[i]]
    for(j in seq_along(county)){
      county_puma_dist[[i]][[j]] <- county_puma_dist[[i]][[j]] / puma_pops[[as.character(names(county)[[j]])]]
    }
  }
  county_puma_dist
}

get_acs_df <- function(state, data){
  state_dist <- get_state_dist(state)
  new_data <- subset(data, PUMA10 != -9)[,c("PUMA10", "PWGTP", "SEX", "RAC1P")]
  old_data <- subset(data, PUMA00 != -9)[,c("PUMA00", "PWGTP", "SEX", "RAC1P")]
  
  # Calculate column and row names to create a zero'd initial dataframe
  col_names <- NULL
  for(i in seq_along(state_dist)){
    county <- names(state_dist)[i]
    county <- translate_county(state, county)
    county_dist <- state_dist[[i]]
    pumas <- names(county_dist)
    
    for(puma in pumas){
      col_names <- c(col_names, paste0(county, "|PUMA10:", puma))
    }
    # Get column names for the 2000 pumas
    old_pumas <- get_old_pumas(state, pumas)
    for(old_puma in old_pumas){
      col_names <- c(col_names, paste0(county, "|PUMA00:", old_puma))
    }
  }
  
  row_names <- NULL
  for(race in 1:9) for(sex in 1:2) row_names <- c(row_names, paste0("SEX:",sex, "|RACE:",race))
  
  df <- as.data.frame(matrix(0, ncol = length(col_names), nrow = 18))
  colnames(df) <- col_names
  rownames(df) <- row_names
  
  # Now we insert the proportion of the puma data in that county into the right slot in the dataframe
  # We first aggregate and insert for puma10 codes
  aggdata <- aggregate(new_data, by = list(new_data$PUMA10, new_data$SEX, new_data$RAC1P), FUN = sum)
  
  for(i in 1:nrow(aggdata)){
    row <- aggdata[i,]
    
    puma <- as.character(row[[1]])
    sex <- row[[2]]
    race <- row[[3]]
    rows <- row$PWGTP[1]
    
    for(j in seq_along(state_dist)){
      if(puma %in% names(state_dist[[j]])){
        county <- names(state_dist)[j]
        county <- translate_county(state, county)
        insert_at_col <- which(colnames(df) == paste0(county, "|PUMA10:", puma))
        insert_at_row <- which(rownames(df) == paste0("SEX:", sex, "|RACE:", race))
        puma10_rows <- round(rows * state_dist[[j]][[puma]])
        
        df[insert_at_row,insert_at_col] <- puma10_rows
      }
    }
  }
  
  
  # We now aggregate by puma00 codes. Since puma00codes are based off puma10 distributions we use same method
  # to get the puma10 values and break those down into their puma00 distributions
  aggdata <- aggregate(old_data, by = list(old_data$PUMA00, old_data$SEX, old_data$RAC1P), FUN = sum)
  
  for(i in 1:nrow(aggdata)){
    row <- aggdata[i,]
    
    puma <- as.character(row[[1]])
    sex <- row[[2]]
    race <- row[[3]]
    rows <- row$PWGTP[1]
    
    for(j in seq_along(state_dist)){
      if(puma %in% names(state_dist[[j]])){
        county <- names(state_dist)[j]
        county <- translate_county(state, county)
        insert_at_row <- which(rownames(df) == paste0("SEX:", sex, "|RACE:", race))
        puma10_rows <- round(rows * state_dist[[j]][[puma]])
        
        old_pumas <- translate_puma10(state, puma)
        for(k in seq_along(old_pumas)){
          puma00_rows <- round(puma10_rows * old_pumas[k])
          
          insert_at_col <- which(colnames(df) == paste0(county, "|PUMA00:", names(old_pumas)[k]))
          
          df[insert_at_row, insert_at_col] <- puma00_rows + df[insert_at_row, insert_at_col]
        }
      }
    }
  }
  
  df
}

generate_sheet <- function(state){
  state_name <- toupper(state)
  state_fp <- as.integer(state)
  
  if(state_name %in% unique(fips_names$STATE)){
    state_fp <- translate_state(state_name)
  }
  else if(state %in% unique(fips_names$STATEFP)){
    state_name <- translate_statefp(state_fp)
  }
  else{
    message("Please enter correct state fips code or state abbreviation")
    return(NA)
  }
  
  res <- try(data <- read.csv(paste0("ss13p", tolower(state_name), ".csv")), silent = TRUE)
  if(inherits(res, 'try-error')){
    message(paste0("You don't have the csv ss13p",tolower(state_name), " in working dir"))
    return(NA)
  }
  
  data <- data[,c("PUMA00","PUMA10", "PWGTP", "SEX", "RAC1P")]
  
  df <- get_acs_df(state_fp, data)
  write.xlsx(df, paste0(state_name,"_final.xlsx"))
}

answer <- ""
while(answer != "exit"){
  answer <- readline("Please enter a state abbrevation or state fips code: ")
  generate_sheet(answer)
}
