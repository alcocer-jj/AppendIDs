#' @title Append Monadic Country-Year Identifiers
#' @description Standardizes a dataframe with monadic country-year observations by creating a "country" column based on Gleditsch-Ward (GW) country-year observations.
#' A master dataframe of Gleditch-Ward country-year observations is used to standardize country-year identifiers across observations, allowing for merging of datasets from sources using different country name coding schemes.
#' In addition to GW country names and country codes (gwno), adds additional columns with other potential country-year identifiers including Correlates of War (COW) and International Finanical Statistics (IFS) codes.
#' Supplied dataframe must include columns designating each observation's country identifier and the year of observation.
#' Dataframe must include single observations for each country-year without duplicated observations.
#'
#' @param df A dataframe object with monadic country data which includes a column designating country names or identifiers for each observation and a column designating years for each observation.
#' Supplied country identifiers can be provided as a column containing country names, or Gleditch-Ward or Correlates of War abbreviations or identifiers. Function is not case-sensitive for country column names, as long as column name includes "country".
#' @param breaks TRUE or FALSE, if TRUE ID joining process will be interrupted at instances where GWNO codes cannot be identified for a given country-year observation. Returns a vector of missing GWNO codes.
#' @return A dataframe object with monadic country data, updated to standardize current country names based on Gleditsch-Ward number and year observations (Gleditsch and War 1999).
#' Country names will be returned as the column "country" and years as "year", Previous identifiers returned as "countryname_raw".
#' Dataframe will also include the following additional country-year identifiers: Gleditsch-Ward Number [gwno], Gleditsch-Ward Abbreviation [gwabbrev], Correlates of War Country Code [ccode], International Finanical Statistics (IFS) number [ifscode], and IFS abbreviation [ifs].
#' @examples
#' data_cy <- cy_append_ids(data, breaks=T)
#' @export
cy_append_ids <- function(df, breaks=T) {

  #Load in the country IDs file
  urlfile <- "https://raw.githubusercontent.com/alcocer-jj/Rdata/main/sysdata.rds"
  ids <- readRDS(url(urlfile))

  # prevents false positives for "notfound"
  ids$minyear2 <- ifelse(ids$minyear2 == -9999, 9999, ids$minyear2)

  ############################################
  ## Format Data                            ##
  ## (based on original append IDS function) #
  ############################################

  yrexists = 0
  ctryexists = 0
  for (i in 1:length(names(df))) {
    #Find the country variable and name it country
    if (names(df)[i]=="Country" | names(df)[i]=="country" |
        names(df)[i]=="CountryName" | names(df)[i]=="countryname" |
        names(df)[i]=="Countryname" | names(df)[i]=="COUNTRY") {
      names(df)[i]="country"
      df$country = as.character(df$country)
      ctryexists=1
      #R will sometimes read the country name as a factor. We'll make
      #  sure it's just a string
      df$country = as.character(df$country)
    }
    #Check to see if the year variable exists
    if (names(df)[i]=="Year" | names(df)[i]=="year" | names(df)[i]=="YEAR") {
      names(df)[i] = "year"
      yrexists = 1
    }
    #Check to see if a gwno code already exists
    if (names(df)[i] == "GWNo" | names(df)[i]=="gwno" |
        names(df)[i]=="GWNO") {
      names(df)[i]="gwno_raw"
    }

    #Check to see if a cow code already exists
    if (names(df)[i]=="COW" | names(df)[i]=="cow" |
        names(df)[i]=="ccode" | names(df)[i]=="cowcode") {
      names(df)[i] = "ccode_raw"
    }
  }

  if(ctryexists == 0) {
    stop("The data must have a variable containing the country names
         before the append ids function is called")
  }

  if (yrexists == 0) {
    warning("NO YEAR VARIABLE PROVIDED. If this is not intended to be a cross-sectional dataset, please ensure that the year variable is correctly named.
            GWNOs for cross-sectional datasets will be assigned based on 2015 country names.")

    #We're going to pretend that the year of every observation is 2015
    df$year = rep(2015,nrow(df))
  }

  # check if there are already duplicate country-years in the data
  n_occur <- data.frame(table(df$country, df$year))
  if(sum(n_occur$Freq > 1) > 0){
    warning("STOP: Duplicated country-years in data. Check and remove duplicates before running append_ids(). See list of duplicates above.")
    print(n_occur[n_occur$Freq > 1,])
  }
  rm(n_occur)


  #############################
  ## Preprocess Country Names #
  #############################

  df$countryname_raw <- df$country
  df$country <- tolower(df$country)
  df$country <- gsub('[[:punct:]]', '', df$country)
  df$country <- gsub('\\s', '', df$country)

  ########################
  ## Append Country IDs #
  ########################

  # keep track of countries without gwnos
  df$notfound <- NA

  # merge in ids
  df <- merge(df, ids, by = "country", all.x = T)

  # note country names not found, or outside of min/max years
  df$notfound <- ifelse(!is.na(df$gwno),
                        !((df$minyear1<=df$year & df$maxyear1>=df$year) | (df$minyear2<=df$year & df$maxyear2>=df$year)),TRUE)
  notfound <- df$countryname_raw[df$notfound]
  df <- df[!(df$notfound),]

  #print(df)

  df[,c("minyear1", "maxyear1", "minyear2", "maxyear2","notfound")] <- NULL


  #############################
  # Display and check countries
  #  that did not get a gwno
  #############################
  notfound <- unique(notfound)
  notfound <- notfound[!(notfound %in% df$countryname_raw)]

  print("The following countries were not given gwno codes: ")
  for (i in 1:length(notfound)) {
    print(notfound[i])
  }

  if(breaks) {
    print("**Check missing GWNO values, then type 'c' when done**")
    browser()
  }

  ###############################
  # Clean up and return dataframe
  ###############################

  # make gwno name be the country name
  df$country <- NULL
  names(df)[names(df)=="gwname"] = "country"

  # put the id variables first
  nonids <- names(df)[!(names(df) %in% c('country','gwno','year','ccode','ifs','ifscode','gwabbrev'))]
  df <- df[,c('country','gwno','year','ccode','ifs','ifscode','gwabbrev',nonids)]

  # Sort the data by gwno and year
  df = df[order(df$gwno,df$year),]


  #If the year variable was created and not originally part of the data, drop it here
  if (yrexists==0) {
    #Find theyear variable
    yrloc = -1
    for (q in 1:length(names(df))) {
      if (names(df)[q]=="year") {
        yrloc = q
        break
      }
    }
    df = df[,-yrloc]
  }

  #unload ids file
  rm(ids)

  # warn about duplicates added by append_ids
  if (yrexists == 1) {
    n_occur <- data.frame(table(df$country, df$year))
    if(sum(n_occur$Freq > 1) > 0){
      warning("Duplicate country-years introduced by append_ids. Check and remove duplicates as appropriate. See list of duplicates above.")
      print(n_occur[n_occur$Freq > 1,])
    }
    rm(n_occur)
  }

  #return the data frame
  return(df)
}


#' @title Append Dyadic Country-Year Identifiers
#' @description Standardizes a dataframe with dyadic country-year observations by creating "repcountry" and "parcountry" columns based on Gleditsch-Ward (GW) country-year observations.
#' A master dataframe of Gleditch-Ward country-year observations is used to standardize country-year identifiers across observations, allowing for merging of datasets from sources using different country name coding schemes.
#' In addition to GW country names and country codes (gwno), adds additional columns with other potential country-year identifiers including Correlates of War (COW) and International Finanical Statistics (IFS) codes.
#' Supplied dataframe must include columns designating observations for each dyad member's country identifier and the year of observation.
#' Dataframe must include single observations for each repcountry-parcountry-year without duplicated observations.
#'
#' @param df A dataframe object with dyadic country data which includes columns designating country names or identifiers for each country in the dyad and a column designating years for each observation.
#' Supplied country identifiers can be provided as a column containing country names, or Gleditch-Ward or Correlates of War abbreviations or identifiers.
#' Dyadic country identifiers can be differentiated numerically ("Country1" and "Country2") or alphabetically ("CountryA" and "CountryB").
#' Function is not case-sensitive for country column names, as long as column name includes "country".
#' @param breaks TRUE or FALSE, if TRUE ID joining process will be interrupted at instances where GWNO codes cannot be identified for a given country-year observation. Returns a vector of missing GWNO codes.
#' @return A dataframe object with dyadic country data, updated to standardize current country names based on Gleditsch-Ward number and year observations (Gleditsch and War 1999).
#' Country names will be returned as the columns "repcountry" and "parcountry" and years as "year", Previous identifiers returned as "repcountryname_raw" and "parcountryname_raw".
#' Dataframe will also include the following additional country-year identifiers: Gleditsch-Ward Number [gwno], Gleditsch-Ward Abbreviation [gwabbrev], Correlates of War Country Code [ccode], International Finanical Statistics (IFS) number [ifscode], and IFS abbreviation [ifs].
#' @examples
#' data_dyad <- dyad_append_ids(data, breaks = F)
#' @export
dyad_append_ids <- function(df, breaks=T) {

  #Load in the country IDs file
  urlfile <- "https://raw.githubusercontent.com/alcocer-jj/Rdata/main/sysdata.rds"
  ids <- readRDS(url(urlfile))

  # prevents false positives for "notfound"
  ids$minyear2 <- ifelse(ids$minyear2 == -9999, 9999, ids$minyear2)

  ############################################
  ## Format Data                            ##
  ## (based on original append IDS function) #
  ############################################

  yrexists = 0
  ctryexists1 = 0
  ctryexists2 = 0
  for (i in 1:length(names(df))) {
    #Find the first country variable and name it repcountry
    if (names(df)[i]=="Country1" | names(df)[i]=="country1" |
        names(df)[i]=="CountryName1" | names(df)[i]=="countryname1" |
        names(df)[i]=="Countryname1" | names(df)[i]=="Country 1" |
        names(df)[i]=="country 1" | names(df)[i]=="CountryName 1" |
        names(df)[i]=="countryname 1" | names(df)[i]=="Countryname 1"|
        names(df)[i]=="countryname A" | names(df)[i]=="Countryname A"|
        names(df)[i]=="countryA" | names(df)[i]=="CountryA"|
        names(df)[i]=="countrynameA" | names(df)[i]=="CountrynameA"|
        names(df)[i]=="countryNameA" | names(df)[i]=="CountryNameA"|
        names(df)[i]=="repcountry") {
      names(df)[i]="repcountry"
      df$repcountry = as.character(df$repcountry)
      ctryexists1=1
    }

    #Find the second country variable and name it parcountry
    if (names(df)[i]=="Country2" | names(df)[i]=="country2" |
        names(df)[i]=="CountryName2" | names(df)[i]=="countryname2" |
        names(df)[i]=="Countryname2" | names(df)[i]=="Country 2" |
        names(df)[i]=="country 2" | names(df)[i]=="CountryName 2" |
        names(df)[i]=="countryname 2" | names(df)[i]=="Countryname 2" |
        names(df)[i]=="countryname B" | names(df)[i]=="Countryname B"|
        names(df)[i]=="countryB" | names(df)[i]=="CountryB"|
        names(df)[i]=="countrynameB" | names(df)[i]=="CountrynameB"|
        names(df)[i]=="countryNameB" | names(df)[i]=="CountryNameB"|
        names(df)[i]=="parcountry") {
      names(df)[i]="parcountry"
      df$parcountry = as.character(df$parcountry)
      ctryexists2=1
    }

    #Check to see if the year variable exists
    if (names(df)[i]=="Year" | names(df)[i]=="year" | names(df)[i]=="YEAR") {
      names(df)[i] = "year"
      df$year <- as.numeric(df$year)
      yrexists = 1
    }

    #Check to see if gwno codes already exist
    if (names(df)[i] == "GWNo1" | names(df)[i]=="gwno1" |
        names(df)[i]=="GWNO1" | names(df)[i] == "GWNo 1"
        | names(df)[i]=="gwno 1" | names(df)[i]=="GWNO 1") {
      names(df)[i]="repgwno_raw"
    }

    if (names(df)[i] == "GWNo2" | names(df)[i]=="gwno2" |
        names(df)[i]=="GWNO2" | names(df)[i] == "GWNo 2"
        | names(df)[i]=="gwno 2" | names(df)[i]=="GWNO 2") {
      names(df)[i]="pargwno_raw"
    }

    #Check to see if cow codes already exist
    if (names(df)[i]=="COW1" | names(df)[i]=="cow1" |
        names(df)[i]=="ccode1" | names(df)[i]=="cowcode1" |
        names(df)[i]=="COW 1" | names(df)[i]=="cow 1" |
        names(df)[i]=="ccode 1" | names(df)[i]=="cowcode 1") {
      names(df)[i] = "repccode_raw"
    }

    if (names(df)[i]=="COW2" | names(df)[i]=="cow2" |
        names(df)[i]=="ccode2" | names(df)[i]=="cowcode2") {
      names(df)[i] = "parccode_raw"
    }
  }

  if(ctryexists1 == 0 |ctryexists2 == 0 ) {
    stop("The data must have variables containing both country names
         before the append ids function is called")
  }

  if (yrexists == 0) {
    # We're going to pretend that the year of every observation is 2015
    df$year = rep(2015,nrow(df))
  }

  #############################
  ## Preprocess Country Names #
  #############################

  df$repcountryname_raw <- df$repcountry
  df$repcountry <- tolower(df$repcountry)
  df$repcountry <- gsub('[[:punct:]]', '', df$repcountry)
  df$repcountry <- gsub('\\s', '', df$repcountry)

  df$parcountryname_raw <- df$parcountry
  df$parcountry <- tolower(df$parcountry)
  df$parcountry <- gsub('[[:punct:]]', '', df$parcountry)
  df$parcountry <- gsub('\\s', '', df$parcountry)

  ########################
  ## Append Country IDs #
  ########################

  # keep track of countries without gwnos
  df$repnotfound <- NA
  df$parnotfound <- NA

  # merge in ids for repcountry
  df <- merge(df, ids, by.x = "repcountry", by.y = "country", all.x = T)

  # note country names not found, or outside of min/max years
  df$repnotfound <- ifelse(!is.na(df$gwno),
                           !((df$minyear1<=df$year & df$maxyear1>=df$year) | (df$minyear2<=df$year & df$maxyear2>=df$year)),TRUE)
  notfound <- df$repcountryname_raw[df$repnotfound]
  df <- df[!(df$repnotfound),]

  #print(df)

  df[,c("minyear1", "maxyear1", "minyear2", "maxyear2","repnotfound")] <- NULL

  names(df)[names(df)=="gwno"] <- "repgwno"
  names(df)[names(df)=="ccode"] <- "repccode"
  names(df)[names(df)=="ifscode"] <- "repifscode"
  names(df)[names(df)=="ifs"] <- "repifs"
  names(df)[names(df)=="gwabbrev"] <- "repgwabbrev"
  names(df)[names(df)=="gwname"] <- "repcountry.stand"

  # merge in ids for parcountry
  df <- merge(df, ids, by.x = "parcountry", by.y = "country", all.x = T)

  # note country names not found, or outside of min/max years
  df$parnotfound <- ifelse(!is.na(df$gwno),
                           !((df$minyear1<=df$year & df$maxyear1>=df$year) | (df$minyear2<=df$year & df$maxyear2>=df$year)), TRUE)
  notfound <- c(df$parcountryname_raw[df$parnotfound], notfound)
  df <- df[!(df$parnotfound),]

  #print(df)

  df[,c("minyear1", "maxyear1", "minyear2", "maxyear2", "parnotfound")] <- NULL

  names(df)[names(df)=="gwno"] <- "pargwno"
  names(df)[names(df)=="ccode"] <- "parccode"
  names(df)[names(df)=="ifscode"] <- "parifscode"
  names(df)[names(df)=="ifs"] <- "parifs"
  names(df)[names(df)=="gwabbrev"] <- "pargwabbrev"
  names(df)[names(df)=="gwname"] <- "parcountry.stand"


  #############################
  # Display and check countries
  #  that did not get a gwno
  #############################
  notfound <- unique(notfound)
  notfound <- notfound[!(notfound %in% df$repcountryname_raw | notfound %in% df$parcountryname_raw)]

  print("The following countries were not given gwno codes: ")
  for (i in 1:length(notfound)) {
    print(notfound[i])
  }

  if(breaks) {
    print("**Check missing GWNO values, then type 'c' when done**")
    browser()
  }

  ###############################
  # Clean up and return dataframe
  ###############################

  # make gwno name be the country name
  df$repcountry <- NULL
  df$parcountry <- NULL
  names(df)[names(df)=="repcountry.stand"] = "repcountry"
  names(df)[names(df)=="parcountry.stand"] = "parcountry"

  # put the id variables first
  nonids <- names(df)[!(names(df) %in% c('repcountry','repgwno','parcountry','pargwno','year','repccode','parccode','repifs','parifs',
                                         'repifscode','parifscode','repgwabbrev','pargwabbrev'))]
  df <- df[,c('repcountry','repgwno','parcountry','pargwno','year','repccode','parccode','repifs','parifs',
              'repifscode','parifscode','repgwabbrev','pargwabbrev',nonids)]

  # Sort the data by gwno and year
  df = df[order(df$repgwno,df$pargwno,df$year),]


  #If the year variable was created and not originally part of the data, drop it here
  if (yrexists==0) {
    #Find theyear variable
    yrloc = -1
    for (q in 1:length(names(df))) {
      if (names(df)[q]=="year") {
        yrloc = q
        break
      }
    }
    df = df[,-yrloc]
  }

  #return the data frame
  return(df)
}


#' @title Append Suffixes (Monadic Country-Year)
#' @description Appends a designated character string ("suffix") to all variables in a given dataframe except for identifying column variables. Identifying column variables include country names, identifiers, and abbreviations, and year.
#' This function is designed to allow researchers to append suffixes to differentiate between variables in a combined dataset based on their dataset of origin.
#' For example, "COW" could be appended as a suffix to variables from the Correlates of War dataset.
#' @param df A monadic country-year dataframe.
#' @param suffix A character string to append as an suffix to all non-identifying variable columns, such as "COW" for Correlates of War.
#' @return A monadic country-year dataframe with variable column names modified with the provided suffix.
#' @examples
#' cy_append_suffix("IR of Afghanistan", "Afghanistan")
#' @export
cy_append_suffix <- function(df,suffix) {
  ids.names = c("country","year","gwno","ccode","ifscode","ifs","gwabbrev")
  for (i in 1:length(names(df))) {
    if ((names(df)[i] %in% ids.names)==F) {
      names(df)[i] = paste(names(df)[i],suffix,sep="_")
    }
  }
  return(df)
}


#' @title Append Suffixes (Dyadic Country-Year)
#' @description Appends a designated character string ("suffix") to all variables in a given dataframe except for identifying column variables. Identifying column variables include country names, identifiers, and abbreviations, and year.
#' This function is designed to allow researchers to append suffixes to differentiate between variables in a combined dataset based on their dataset of origin.
#' For example, "COW" could be appended as a suffix to variables from the Correlates of War dataset.
#' @param df A dyadic country-year dataframe.
#' @param suffix A character string to append as an suffix to all non-identifying variable columns, such as "COW" for Correlates of War.
#' @return A dyadic country-year dataframe with variable column names modified with the provided suffix.
#' @examples
#' dyad_append_suffix("IR of Afghanistan", "Afghanistan")
#' @export
dyad_append_suffix <- function(df,suffix) {
  ids.names = c("repcountry","parcountry","year","repgwno","repccode","repifscode","repifs","repgwabbrev",
                "pargwno","parccode","parifscode","parifs","pargwabbrev")
  for (i in 1:length(names(df))) {
    if ((names(df)[i] %in% ids.names)==F) {
      names(df)[i] = paste(names(df)[i],suffix,sep="_")
    }
  }
  return(df)
}

#' @title Append Country-Year Identifiers
#' @description Standardizes a dataframe with country-year observations by creating new country identifer columns based on Gleditsch-Ward (GW) country-year observations.
#' A master dataframe of Gleditch-Ward country-year observations is used to standardize country-year identifiers across observations, allowing for merging of datasets from sources using different country name coding schemes.
#' In addition to GW country names and country codes (gwno), adds additional columns with other potential country-year identifiers including Correlates of War (COW) and International Finanical Statistics (IFS) codes.
#' Supplied dataframe must include columns designating observations' country identifier and the year of observation.
#' For dyadic data the dataframe should include columns with country identifiers for each country in the dyad.
#' Dataframe must include single observations for each country-year observation or repcountry-parcountry-year without duplicated observations.
#'
#' @param df A dataframe object with country data which includes columns designating country names or identifiers for each country and a column designating years for each observation.
#' Supplied dataframe can contain either monadic country data, with a column for country identifiers and year, or dyadic country data, with columns for each paired country and year.
#' Supplied country identifiers can be provided as a column containing country names, or Gleditch-Ward or Correlates of War abbreviations or identifiers.
#' Dyadic country identifiers can be differentiated numerically ("Country1" and "Country2") or alphabetically ("CountryA" and "CountryB").
#' Function is not case-sensitive for country column names, as long as column name includes "country".
#'
#' @param breaks TRUE or FALSE, if TRUE ID joining process will be interrupted at instances where GWNO codes cannot be identified for a given country-year observation. Returns a vector of missing GWNO codes.
#'
#' @return A dataframe object with either monadic or dyadic country-year data based on the prior dataframe, updated to standardize current country names based on Gleditsch-Ward number and year observations (Gleditsch and War 1999).
#' Country names will be returned as the column "country" in monadic data, or as "repcountry" and "parcountry" in dyadic data, and years as "year".
#' Previous identifiers are returned as "countryname_raw" in monadic data and as "repcountryname_raw" and "parcountryname_raw" in dyadic data.
#' Dataframe will also include the following additional country-year identifiers: Gleditsch-Ward Number [gwno], Gleditsch-Ward Abbreviation [gwabbrev], Correlates of War Country Code [ccode], International Finanical Statistics (IFS) number [ifscode], and IFS abbreviation [ifs].
#' @examples
#' new_data <- append_ids(data, breaks = F)
#' @export
append_ids <- function(df, dyad = FALSE, breaks = TRUE){
  if(dyad){
    print("Adding ID variables to dyadic data...")
    dat <- dyad_append_ids(df, breaks)
    return(dat)
  }else{
    print("Adding ID variables to country-year data... Don't forget to set dyad = TRUE if you are working with dyadic data.")
    dat <- cy_append_ids(df, breaks)
    return(dat)
  }
}


#' @title Append Suffixes
#' @description Appends a designated character string ("suffix") to all variables in a given dataframe except for identifying column variables. Identifying column variables include country names, identifiers, and abbreviations, and year.
#' This function is designed to allow researchers to append suffixes to differentiate between variables in a combined dataset based on their dataset of origin.
#' For example, "COW" could be appended as a suffix to variables from the Correlates of War dataset.
#'
#' @param df A country-year dataframe with either monadic or dyadic country-year observations.
#' @param suffix A character string to append as an suffix to all non-identifying variable columns, such as "COW" for Correlates of War.
#' @return A monadic or dyadic country-year dataframe with variable column names modified with the provided suffix.
#' @examples
#' data_suffix <- append_suffix(data, " ")
#' @export
append_suffix <- function(df, suffix, dyad = FALSE){
  if(dyad){
    print("Adding variable suffixes to dyadic data...")
    dat <- dyad_append_suffix(df, suffix)
    return(dat)
  }else{
    print("Adding variable suffixes to country-year data... Don't forget to set dyad = TRUE if you are working with dyadic data.")
    dat <- cy_append_suffix(df, suffix)
    return(dat)
  }
}
