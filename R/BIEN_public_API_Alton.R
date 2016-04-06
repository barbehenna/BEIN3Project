#ALL CODE WRITTEN BY BRIAN MAITNER 2015. Alton Barbehenn restructured the comments so that we can make a package.

#' Lat/Long from Species Names(s)
#'
#' @author Brian Maitner
#'
#' @keywords GIS
#'
#' @param species species here can be either a single species or a vector of species
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.
#'
#' @export
#'
#' @examples BIEN.gis.species("Abies amabilis")
#' species_vector<-c("Abies amabilis", "Acer nigrum")
#' BIEN.gis.species(species_vector)
BIEN.gis.species<-function(species,cultivated=FALSE,only.new.world=TRUE){

  require("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'

  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query

  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }


  # set the query
  occQuery <- paste("SELECT scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")


  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}


#' Species list from country/countries
#'
#' @author Brian Maitner
#'
#' @param country Accepts single countries or vectors
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.
#'
#' @export
#'
#' @examples BIEN.list.country("Canada")
#' country_vector<-c("Canada","United States")
#' BIEN.list.country(country_vector)
BIEN.list.country<-function(country,cultivated=FALSE,only.new.world=TRUE){

  require("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query

  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }






  # set the query
  occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")

  #occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND (is_cultivated = 0 OR is_cultivated IS NULL) AND is_new_world = 1 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial LIMIT 2;") #Limit for testing only



  query = occQuery

  #print(query)

  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}






#' Species list from state(s)
#'
#' @author Brian Maitner
#'
#' @param Accepts single countries or vectors
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.
#'
#' @export
#'
#' @examples BIEN.list.state("Michigan")
#' state_vector<-c("Michigan","Arizona")
#' BIEN.list.state(state_vector)
BIEN.list.state<-function(state,cultivated=FALSE,only.new.world=TRUE){
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  require("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }



  # set the query
  occQuery <- paste("SELECT DISTINCT state_province, scrubbed_species_binomial",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ")" ,paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY state_province,scrubbed_species_binomial;")

  #occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND (is_cultivated = 0 OR is_cultivated IS NULL) AND is_new_world = 1 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial LIMIT 2;") #Limit for testing only



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}


#' Species list from a given county (and state)
#'
#' @author Brian Maitner
#'
#' @note Requires both state and county be supplied
#'
#' @param state Accepts single states or vectors
#' @param county Accepts single counties or vectors
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.
#'
#' @export
#'
#' @examples BIEN.list.county("Michigan","Kent")
#' BIEN.list.county(state="Michigan", county="Kent")
#' county_vector<-c("Kent","Kalamazoo")
#' BIEN.list.county(state="Michigan",county=county_vector)
BIEN.list.county<-function(state,county,cultivated=FALSE,only.new.world=TRUE){
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  require("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }



  # set the query
  occQuery <- paste("SELECT DISTINCT state_province, county, scrubbed_species_binomial",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") AND  county in (", paste(shQuote(county, type = "sh"),collapse = ', '), ") " ,paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY state_province,county,scrubbed_species_binomial;")

  #occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND (is_cultivated = 0 OR is_cultivated IS NULL) AND is_new_world = 1 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial LIMIT 2;") #Limit for testing only



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}



#' List species
#'
#' @return Lists all available species in the BIEN database
#'
#' @author Brian Maitner
#'
#' @export
BIEN.list.all<-function(){

  require("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'


  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  occQuery <- paste("SELECT DISTINCT scrubbed_species_binomial FROM view_full_occurrence_individual ORDER BY scrubbed_species_binomial;")

  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);

  dbDisconnect(con)
  return(df)

}




#' Occurrences from Genus
#'
#' @author Brian Maitner
#'
#' @param genus Accepts a single Genus or a vector of Genera
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.
#'
#' @export
#'
#' @examples BIEN.gis.genus("Abutilon")
#' genus_vector<-c("Abutilon","Abronia")
#' BIEN.gis.genus(genus_vector)
#' BIEN.gis.genus(genus = "Abutilon",cultivated = TRUE,only.new.world = FALSE)#returns all records for the genus Abutilon, including cultivates and Old World records.
BIEN.gis.genus<-function(genus,cultivated=FALSE,only.new.world=TRUE){

  require("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }






  # set the query
  occQuery <- paste("SELECT scrubbed_genus, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")




  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}



#' Occurrences from Family
#'
#' @author Brian Maitner
#'
#' @param family Accepts a single Family or a vector of Families
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.
#'
#' @export
#'
#' @examples BIEN.gis.family("Theaceae")
#' family_vector<-c("Theaceae","Ericaceae")
#' BIEN.gis.family(family_vector)
BIEN.gis.family<-function(family,cultivated=FALSE,only.new.world=TRUE){

  require("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'

  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }






  # set the query
  occQuery <- paste("SELECT scrubbed_family, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")




  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}




#' Occurrences from State/Province
#'
#' @author Brian Maitner
#'
#' @param state Accepts a single State or a vector
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.  Probably not very useful to change at the moment.
#'
#' @note Insanely slow
#'
#' @export
#'
#' @examples BIEN.gis.state("Rhode Island")
#' state_vector<-c("Rhode Island","Maryland")
#' BIEN.gis.state(state_vector)
BIEN.gis.state<-function(state,cultivated=FALSE,only.new.world=TRUE){

  require("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
    # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }


  # set the query
  occQuery <- paste("SELECT state_province, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")




  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}



#' Occurrences from Country
#'
#' @author Brian Maitner
#'
#' @param country Accepts a single Country or a vector
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is true.  Probably not very useful to change at the moment.
#'
#' @note Insanely slow
#'
#' @export
#'
#' @examples BIEN.gis.state("Mexico")
#' state_vector<-c("Mexico","Canada")
#' BIEN.gis.state(state_vector)
BIEN.gis.country<-function(country,cultivated=FALSE,only.new.world=TRUE){

  require("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
    # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }


  # set the query
  occQuery <- paste("SELECT country, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY country,scrubbed_species_binomial;")




  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}





#' Occurrence records from from lat/long Bounding box
#'
#' @author Brian Maitner
#'
#' Will only do one box at a time, specified by min/max lat and long
#'
#' @param min.lat A number -90 to 90
#' @param max.lat A number -90 to 90 that is greater than min.lat
#' @param min.long A number -180 to 180
#' @param max.long A number -180 to 180 that is greater than min.long
#' @param cultivated cultivated = Return cultivated records as well?  Default is FALSE.
#' @param only.new.world only.new.world = Return only records from the New World?  Default is TRUE.
#'
#' @export
#'
#' @examples output_test<-BIEN.gis.box(min.lat = 32,max.lat = 33,min.long = -114,max.long = -113,cultivated = TRUE, only.new.world = FALSE)
BIEN.gis.box<-function(min.lat,max.lat,min.long,max.long,cultivated=FALSE,only.new.world=TRUE){

  require("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)

  #set conditions for query

  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }

  if(cultivated==TRUE){
    cultivated_query<-""
    cultivated_select<-",is_cultivated"
  }

  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }

  if(only.new.world==FALSE){
    newworld_query<-""
    newworld_select<-",is_new_world"

  }


  # set the query
  occQuery <- paste("SELECT scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE latitude between " , paste(shQuote(min.lat, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.lat, type = "sh"),collapse = ', '),"AND longitude between ", paste(shQuote(min.long, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.long, type = "sh"),collapse = ', '), paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")


  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}





#' BIEN ranges function
#'
#' @author Brian Maitner
#'
#' @param species species as either a single species or a vector of species
#' @param directory the directory where you want the range shapefiles saved. Default is your current working directory
#' @param matched returns information on whether range maps were found for each species submitted
#' @param match_names_only Do you want to check whether the maps you're looking for exist without downloading them?
#'
#' @export
#'
#' @note The code marked as not run is only marked that way because the directories are not initialized.
#'
#' @examples species_vector<-c("Abies_lasiocarpa","Abies_amabilis")
#' testwd<-"/Users/Alton/Documents/GitHub/KerkhoffPackage" #Set a working directory,Obviously change this to suit your needs
#' BIEN.ranges.species(species_vector)
#' BIEN.ranges.species(species_vector,test_wd)#saves ranges to a specified working directory
#' BIEN.ranges.species("Abies_lasiocarpa")
#' BIEN.ranges.species("Abies_lasiocarpa","/Users/Alton/Documents/GitHub/KerkhoffPackage")
BIEN.ranges.species<-function(species,directory=NULL,matched=TRUE,match_names_only=FALSE){
  require(RPostgreSQL)
  require(rgeos)
  require(rgdal)
  require(maptools)
  host='vegbiendev.nceas.ucsb.edu'
  #user='public_bien'
  #password='bien_public'
  #Specify the dbname for range maps
  #dbname='public_vegbien'

  #I (Alton) changed the dbname, user, and password parameters to match the original function
  #There was an issue where the shape files weren't scaled properly, however this
  #databse change seems to remedy the situation.
  dbname = 'geombien'
  user = 'bien_read'
  password = 'T0d0B!en'

  #record original working directory,change to specified directory if given
  wd<-getwd()
  if(is.null(directory)==FALSE){
    setwd(directory)
  }


  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  #make sure there are no spaces in the species names
  species<-gsub(" ","_",species)

  if(match_names_only==FALSE){
    # set the query
    rangeQuery <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    #rangeQuery <- paste("SELECT * FROM ranges LIMIT 10;")
    #rangeQuery <- paste("SELECT species FROM ranges LIMIT 100000;")




    query = rangeQuery
    #print(query)
    # create query to retrieve
    df <- dbGetQuery(con, statement = query);

    dbDisconnect(con)

    if(length(df)==0){
      message("No species matched")
    }else{

      for(l in 1:length(df$species)){
        Species<-df$species[l]
        #sp_range<-readWKT(df$st_astext[l])
        sp_range<-readWKT(df$st_astext[l],p4s="+init=epsg:3857")
        #proj4string(sp_range) <- CRS("+init=epsg:3857")
        sp_range<-spTransform(sp_range,CRS("+init=epsg:4326"))
        #assign(paste(species),sp_range,envir=.GlobalEnv)

        #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
        spdf<-as.data.frame(Species)
        spdf<-SpatialPolygonsDataFrame(sp_range,spdf)
        class(spdf)
        writePolyShape(x=spdf,fn = Species)
        #save output

      }#for species in df loop
    }#else

    setwd(wd) #return wd to original

    #list matched species
    if(matched==TRUE){
      found<-as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found)<-c("Species","Range_map_downloaded?")
      found$`Range_map_downloaded?`<-as.character(found$`Range_map_downloaded?`)
      found$`Range_map_downloaded?`[which(species%in%df$species)]<-"Yes"
      return(found)
    }#matched = true
  }#match names only if statement

  if(match_names_only==TRUE){

    rangeQuery <- paste("SELECT species FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    query = rangeQuery
    #print(query)
    # create query to retrieve
    df <- dbGetQuery(con, statement = query);

    dbDisconnect(con)

    if(length(df)==0){
      message("No species matched")
    }else{
      found<-as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found)<-c("Species","Range_map_available?")
      found$`Range_map_available?`<-as.character(found$`Range_map_available?`)
      found$`Range_map_available?`[which(species%in%df$species)]<-"Yes"
      return(found)

    }

  } #matched_names_only ==TRUE



}




#' Traits from species
#'
#' @author Brian Maitner
#'
#' @param species Accepts a single species or a vector
#'
#' @export
#'
#' @examples BIEN.trait.species("Poa annua")
#' species_vector<-c("Poa annua","Juncus trifidus")
#' BIEN.trait.species(species_vector)
BIEN.trait.species<-function(species){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'

  require("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE taxon in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY taxon;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}




#' Traits from trait name
#'
#' @author Brian Maitner
#'
#' @param trait Accepts a single trait or a vector
#'
#' @export
#'
#' @examples BIEN.trait.trait("Height")
#' trait_vector<-c("Height", "Leaf dry mass")=
#' BIEN.trait.trait(trait_vector)
BIEN.trait.trait<-function(trait){
  require("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'

  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") ORDER BY taxon;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}




#' Specific traits from species
#'
#' @author Brian Maitner
#'
#' This function downloads trait data for given species
#'
#' @param trait Accepts a single trait or a vector
#' @param species Accepts a single species or a vector
#'
#' @export
#'
#' @examples BIEN.trait.traitbyspecies(trait = "Height", species = "Carex capitata")
#' trait_vector<-c("Height", "Leaf dry mass")
#' species_vector<-c("Carex capitata","Betula nana")
#' BIEN.trait.traitbyspecies(trait=trait_vector,species=species_vector)
BIEN.trait.traitbyspecies<-function(trait,species){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  require("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND taxon in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY taxon,trait_name;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}



#' Specific traits from genus/genera
#'
#' @author Brian Maitner
#'
#' This function downloads specific trait data for given genus/genera
#'
#' @export
#'
#' @param trait Accepts a single trait or a vector
#' @param genus Accepts a single genus or a vector
#'
#' @examples #see BIEN.trait.traitbyspecies
BIEN.trait.traitbygenus<-function(trait,genus){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  require("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ORDER BY genus,taxon,trait_name;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}




#' Specific traits from family/families
#'
#' @author Brian Maitner
#'
#' @return This function downloads specific trait data for given family/families
#'
#' @param trait Accepts a single trait or a vector
#' @param genus Accepts a single family or a vector
#'
#' @export
#'
#' @examples #see BIEN.trait.traitbyspecies
BIEN.trait.traitbyfamily<-function(trait,family){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  require("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") ORDER BY family,taxon,trait_name;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}




#' Traits from species
#'
#' @author Brian Maitner
#'
#' @param genus Accepts a single genus or a vector
#'
#' @export
#'
#' @examples #see BIEN.trait.species
BIEN.trait.genus<-function(genus){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  require("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ORDER BY genus;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}




#' Traits from family/families
#'
#' @author Brian Maitner
#'
#' @param genus Accepts a single family or a vector
#'
#' @export
#'
#' @examples #see BIEN.trait.species
BIEN.trait.family<-function(family){
  require("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") ORDER BY family,taxon;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}







#' List all traits
#'
#' @author Brian Maitner
#'
#' @export
#'
#' @return This function lists all currently available types of trait data. This is especially useful if you want to figure out what your trait of interest is titled in this database.
BIEN.trait.list<-function(){
  require("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT DISTINCT trait_name FROM agg_traits ORDER BY trait_name;")



  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);




  dbDisconnect(con)
  return(df)


}
