####
# CALCULATING TSM FOR NORMAL, +1.5°C & +2°C SCENARIOS. 
# FOR FULL EXPOSURE TO SUN (tsmsun) & FULL SHADE (tsmshd)
####

### INITIAL SETUP----
## Setting Working Directory
fdir = "C:\\Users\\Bryan\\Google Drive\\TSMVisualization\\"
#fdir= "/Volumes/GoogleDrive/My\ Drive/TSMVisualization/"
setwd(fdir)

## Packages needed for TSM
pkgst <- c('raster','rgdal','plotly','reshape2','taxize', 'magrittr', 'TrenchR')
#install.packages(pkgst)
lapply(pkgst, library, character.only = TRUE)

## Dataframe containing perrtaining information for Class Lepidosauria
lizardsdf <- readRDS("Data\\Lepidosauria.Rda")
#lizardsdf <- readRDS("Data/Lepidosauria.Rda")

attach(lizardsdf)
Binomial
Synonym


 Tb_lizard=function(T_a, T_g, u, svl, m, psi, rho_S, elev, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5){

  # stopifnot(u>=0, svl>=0, m>=0, rho_S>=0, rho_S<=1, elev>=0, doy>0, doy<367, sun %in% c(TRUE, FALSE), surface %in% c(TRUE, FALSE), alpha_S>=0, alpha_S<=1, alpha_L>=0, alpha_L<=1, epsilon_s>=0, epsilon_s<=1, F_d>=0, F_d<=1, F_r>=0, F_r<=1, F_a>=0, F_a<=1, F_g>=0, F_g<=1)

  psi= psi*pi/180 #convert zenith angle to radians

  # constants
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol C (p.279) Parentheses all from Campbell & Norman 1998

  tau=0.65 # atmospheric transmisivity
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p.159)

  # Calculate radiation
  # view angles, parameterize for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g
  h=svl/1000 # length of svl in m

  A=0.121*m^0.688   # total lizard area, Roughgarden (1981)
  A_p= (-1.1756810^-4*psi^2-9.2594*10^-2*psi+26.2409)*A/100      # projected area
  F_p=A_p/A

  # radiation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos (psi))  # (11.12) optical air mass
  m_a[(psi>(80*pi/180))]=5.66

  # Flux densities
  epsilon_ac= 9.2*10^-6*(T_a+273)^2 # (10.11) clear sky emissivity
  L_a=sigma*(T_a+273)^4  # (10.7) long wave flux densities from atmosphere
  L_g=sigma*(T_g+273)^4  # (10.7) long wave flux densities from ground

  S_d=0.3*(1-tau^m_a)* S_p0 * cos(psi)  # (11.13) diffuse radiation
  S_p = S_p0 * tau^m_a
  S_b = S_p * cos(psi)
  S_t = S_b + S_d
  S_r= rho_S*S_t # (11.10) reflected radiation

  #__________________________________________________
  # conductance

  dim=svl/1000 # characteristic dimension in meters
  g_r= 4*epsilon_s*sigma*(T_a+273)^3/c_p # (12.7) radiative conductance

  g_Ha=1.4*0.135*sqrt(u/dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)

  #__________________________________________________
  # operative environmental temperature

  #calculate with both surface and air temp (on ground and in tree)

  sprop=1 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  Te=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature
  Te_surf= T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))

  # calculate in shade, no direct radiation
  sprop=0 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  TeS=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature
  TeS_surf=T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))

  #Select Te to return
  if(sun==TRUE & surface==TRUE) Te= Te_surf
  if(sun==TRUE & surface==FALSE) Te= Te
  if(sun==FALSE & surface==TRUE) Te= TeS_surf
  if(sun==FALSE & surface==FALSE) Te= TeS

  return(Te)
}


#Uploading shapefile
shp <- list.files("Data\\Updated\\REPTILES", pattern="\\.shp$")
shp <- gsub(".shp", "", shp) #Comparing to list of shapefiles for all reptiles

rho_S <- 0.7 #Specified in the same function (DOUBLECHECK)
doy <- list(15,45,75,105,135,165,195,225,255,285,315,345) #Day of year for each month (DOUBLE CHECK)


complete_df <- function(org) {
  
  begin = proc.time()
  setwd("C:\\Users\\Bryan\\Google Drive\\TSMVisualization\\")
  
  cat("Starting \"Exposed\"\n\n")
  
  exposed <- therm_safety_margin(org, 0, begin)
  
  cat("Starting \"Covered\". Exposed took", timetaken(started.at = begin), "\n\n" )
  covered <- therm_safety_margin(org, 100, begin)
  combined <- rbind(exposed, covered)

  setwd("C:\\Users\\lbuckley\\My Documents\\TSMviz")
  
  filename <- paste("Data\\",sub(" ","_",org),"_combined.rds",sep="")
  saveRDS(combined,file=filename)
  
  #filenamedf <- paste("TSMdfs\\",sub(" ","_",org),"_combined.csv",sep="")
  #write.csv(combined,filenamedf)
  #fwrite(combined, filenamedf)
  #saveRDS(combined, filenamedf)
  done <- cat("Process complete. \nOverall:", timetaken(started.at = begin))
  return (done)
}  

therm_safety_margin = function(org, shade, begin, substrate="Soil", depth=0, month=1:12){

  # shade = 0
  # substrate = "soil"
  # depth = 0
  # month = 1:12
  name <- ifelse(org%in%shp==T,org, #Using the correct name to upload the shapefile
                  ifelse(lizardsdf$Synonym[lizardsdf$Binomial==org]%in%shp==T,as.character(lizardsdf$Synonym[lizardsdf$Binomial==org]),
                         ifelse(lizardsdf$Accepted[lizardsdf$Binomial==org]%in%shp==T,as.character(lizardsdf$Accepted[lizardsdf$Binomial==org],NA))))
  
  cat("Now, reading shapefile.\n\n")
  
  shape <- readOGR(dsn="Data\\Updated\\REPTILES", name)
  crs(shape) <- crs("+init=epsg:4326") #Changing projection to better match other spatial objects
  
  cat("Done reading shape file. So far, ",timetaken(started.at = begin), "\n\nNow reading files for variables\n\n")
  
  #Microclimate data
  crp <- function(x)crop(x,extent(shape)) #Function for cropping & masking
  mas <- function(x)mask(x,shape) #Using them individually because lists make things more complicated
  
  ncfiletg <- unlist(paste("Data\\Microclim\\",shade,"_","shade","\\","D",depth,"cm","_",substrate,"_",
                           shade,"_",month,".nc",sep="",collapse=NULL)) #Ground Temperature
  ncfileta <- unlist(paste("Data\\Microclim\\",shade,"_","shade","\\","TA1cm","_",substrate,"_",
                           shade,"_",month,".nc",sep="",collapse=NULL)) #Air Temperature
  ncfilews <- unlist(paste("Data\\Microclim\\wind_speed_ms_1cm\\V1cm_",month,".nc",sep="")) #Wind Speed
  ncfileza <- unlist(paste("Data\\Microclim\\zenith_angle\\ZEN_",month,".nc",sep="")) #Zenith Amgle
  
  # takes a character of a filename and returns the cropped and masked version 
  crop_mask <- function(object) {
    return(object  %>% lapply(brick) %>%
             lapply(crp) %>%
             lapply(mas))
  }
  
  Tg <- crop_mask(ncfiletg)
  Ta <- crop_mask(ncfileta)
  u <- crop_mask(ncfilews)
  psi <- crop_mask(ncfileza)
  
  #Calculating information relevant to the lizard only
  ctmax <- lizardsdf$Tmax[lizardsdf$Binomial==org] #Critical Thermal Maximum
  
  svl <- as.numeric(lizardsdf$SVL[lizardsdf$Binomial==org]) #SVL in mm
  
  mass <- lizardsdf$Mass[lizardsdf$Binomial==org] #Mass in g
  
  
  elevation <- raster("Data\\Elevation\\ETOPO1_Ice_g_gmt4.grd") #Obtained elevation from NOAA
  elevation <- resample(elevation, Ta[[1]], method='bilinear') #Making rasters match more closely
  elevation <- replicate(12,elevation) %>% #The function does not work without a list (not sure 100% why....)
    lapply(crp) %>% lapply(mas)
  
  cat("Read all files for variables. So far,", timetaken(started.at = begin), "\n\nNow calculating TSM\n\n")
  
  #NORMAL CONDITIONS
  if(shade == 0) {
    sun = TRUE
  } else {
    sun = FALSE
  }
  
  TeN <- mapply(Tb_lizard,Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=sun, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  
  #TSM +1.5 °C SCENARIO
  Ta1 <- lapply(Ta,FUN=function(x){x+1.5}) #Adding the +1.5°C to air and substrate data
  Tg1 <- lapply(Tg,FUN=function(x){x+1.5})
  Te1 <- mapply(Tb_lizard,Ta1, Tg1, u, svl, mass, psi, rho_S, elevation, doy, sun=sun, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  
  #TSM +2°C SCENARIO
  Ta2 <- lapply(Ta,FUN=function(x){x+2}) #Adding the +2°C to air and substrate data
  Tg2 <- lapply(Tg,FUN=function(x){x+2})
  Te2 <- mapply(Tb_lizard,Ta2, Tg2, u, svl, mass, psi, rho_S, elevation, doy, sun=sun, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  
  cat("Calculated TSM for all scenarios. So far,", timetaken(started.at = begin), "\n\n")
  
  hours <- c("x","y", "12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM") 
  month <- c(1:12)
  names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  
  #Rasters the input list to data frames, renames columns for future use, switch to long format 
  
  process <- function(list, condition) {
    output <- list %>% lapply(FUN=function(x){ctmax-x}) %>% lapply(as.data.frame, xy=TRUE,na.rm=TRUE)
    for (i in c(1:12)) {
      colnames(output[[i]]) <- hours
      output[[i]] <- melt(output[[i]], id=c("x", "y"))
      output[[i]]$Month <- names(month)[i]  #add month and scenarios info
      output[[i]]$Scenario <- condition
    }
    output <- do.call(rbind, output)   #Combining the individual scenarios dataframes in the list into a single one
    return (output)
  }
  
  TsmNdf <- process(TeN, "Normal")
  Tsm1df <- process(Te1, "+1.5 °C")
  Tsm2df <- process(Te2, "+2 °C")
  
  Tsmdf <- rbind(TsmNdf,Tsm1df,Tsm2df) #Now combining all 3 scenarios into 1
  colnames(Tsmdf) <- c("x","y","Hour","Tsm","Month","Scenario") #Renaming all necessary columns
  
  # require(plyr)
  neworder <- c("Normal","+1.5 °C","+2 °C")
  Tsmdf <- arrange(transform(Tsmdf, Scenario=factor(Scenario,levels=neworder)),Scenario) #Rearraging orders of scenarios for plotting
  
  neworder2 <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
  Tsmdf <- arrange(transform(Tsmdf, Hour=factor(Hour,levels=neworder2)),Hour) #Order of hours
   
  Tsmdf <- arrange(transform(Tsmdf, Month=factor(Month,levels=names(month))),Month) #And order of months
  
  
  # filename <- paste("TSMdfs\\",sub(" ","_",org),"_",shade,".Rda",sep="")
  # save(Tsmdf,file=filename)
  
  # filenamedf <- paste("TSMdfs\\",sub(" ","_",org),"_",shade,".csv",sep="")
  # fwrite(Tsmdf, filenamedf)
  # write.csv(Tsmdf,filenamedf)
  
  if (shade == 0) {
    Tsmdf$Shade <- "Exposed"
  } else {
    Tsmdf$Shade <- "Covered"
  }
  return(Tsmdf)
}


org <- c("Takydromus sexlineatus","Coleonyx brevis", "Psammodromus hispanicus", "Holbrookia maculata","Lepidophyma flavimaculatum", "Psammodromus algirus", "Sceloporus undulatus", "Cophosaurus texanus", "Petrosaurus mearnsi","Platysaurus intermedius","Psammodromus hispanicus","Sceloporus magister","Tiliqua rugosa","Urosaurus ornatus")

# All the organisms that have necessary data for TPC
#  "Xantusia riversiana" doesn't work     small
                   
#   "Eulamprus tympanum"  "Sceloporus occidentalis"  "Sceloporus graciosus"  "Sphaerodactylus macrolepis"  meidum

# "Lacerta agilis" too big 


org <- c("Coleonyx brevis", "Takydromus sexlineatus", "Psammodromus hispanicus", "Psammodromus algirus", "Ctenotus regius")


getwd()
org <- c("Podarcis muralis", "Anolis carolinensis", "Uta stansburiana", "Sceloporus variabilis", "Takydromus septentrionalis", "Pseudemoia entrecasteauxii")
mapply(complete_df, org)
complete_df("Coleonyx brevis")

