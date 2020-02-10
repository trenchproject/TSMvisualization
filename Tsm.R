####
# CALCULATING TSM FOR NORMAL, +1.5°C & +2°C SCENARIOS. 
# FOR FULL EXPOSURE TO SUN (tsmsun) & FULL SHADE (tsmshd)
####

### INITIAL SETUP----
## Setting Working Directory
fdir = "/Volumes/GoogleDrive/Shared drives/TrEnCh/TSMVisualization/"
setwd(fdir)

## Packages needed for TSM
pkgst <- c('raster','rgdal','plotly','reshape2','taxize')
install.packages(pkgst)
lapply(pkgst, library, character.only = TRUE)

## Dataframe containing perrtaining information for Class Lepidosauria
lizardsdf <- readRDS("Data/Lepidosauria.Rda")
attach(lizardsdf)

### To calculate TSM ----
 
##Full exposure to sun (0% shade)
tsmsun = function(org,shade,substrate="Soil",depth=0,month=1:12){
  #Adding this function here to save time?
  Tb_lizard=function(Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5){
    
    psi= psi*pi/180 #convert zenith angle to radians
    
    # constants
    sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
    c_p=29.3 # specific heat of air, J/mol degrees K or C
    
    tau=0.65 # atmospheric transmisivity
    S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)
    
    # Calculate radiation
    # view angles, parameterize for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g
    h=svl/1000 # length of cylinder in m
    theta = psi # angle between solar beam and a normal to the plane in radians, = psi for horizontal surfaces
    
    # F_p=(cos (theta)+(4*h*sin (theta))/(pi*d))/(2+4*h/d)  # beam view angle, Fig 11.6
    A=0.121*mass^0.688   # total lizard area, roughgarden 1981 from Norris (1965) and Porter and James (1979)
    A_p= (-1.1756810^-4*psi^2-9.2594*10^-2*psi+26.2409)*A/100      # projected area
    F_p=A_p/A
    
    # radiation
    p_a=101.3* exp (-elevation/8200)  # atmospheric pressure
    m_a=p_a/(101.3*cos (psi))  # (11.12) optical air mass
    m_a[(psi>(80*pi/180))]=5.66
    
    # Flux densities
    epsilon_ac= 9.2*10^-6*(Ta+273)^2 # (10.11) clear sky emissivity
    L_a=epsilon_ac*sigma*(Ta+273)^4  # (10.7) long wave flux densities from atmosphere 
    L_g=epsilon_s*sigma*(Tg+273)^4  # (10.7) long wave flux densities from ground
    
    #S_p=S_p0*tau^m_a # (11.11) direct irradience , W/m^2
    dd2= 1+2*0.1675*cos(2*pi*doy/365)
    S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
    
    S_d=0.3*(1-tau^m_a)* S_p  # (11.13) diffuse radiation
    #S_t=S_p*cos (psi)+S_d # solar irradience 
    S_r= rho_S*S_p # (11.10) reflected radiation
    
    #__________________________________________________
    # conductance
    
    dim=svl/1000 # characteristic dimension in meters (Table 9.5)
    g_r= 4*sigma*(Ta+273)^3/c_p # (12.7) radiative conductance
    
    g_Ha=1.4*0.135*sqrt(u/dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
    
    #__________________________________________________
    # operative environmental temperature
    
    #calculate with both surface and air temp (on ground and in tree)
    
    sprop=1 #proportion of radiation that is direct, Sears and Angilletta 2012
    R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
    Te=Ta+(R_abs-epsilon_s*sigma*(Ta+273)^4)/(c_p*(g_r+g_Ha))                       
    Te_surf= Tg+(R_abs-epsilon_s*sigma*(Tg+273)^4)/(c_p*(g_r+g_Ha))        
    
    # calculate in shade, no direct radiation
    sprop=0 #proportion of radiation that is direct, Sears and Angilletta 2012
    R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
    TeS=Ta+(R_abs-epsilon_s*sigma*(Ta+273)^4)/(c_p*(g_r+g_Ha))                       
    TeS_surf=Tg+(R_abs-epsilon_s*sigma*(Tg+273)^4)/(c_p*(g_r+g_Ha))  
    
    #Select Te to return
    if(sun==TRUE & surface==TRUE) Te= Te_surf
    if(sun==TRUE & surface==FALSE) Te= Te
    if(sun==FALSE & surface==TRUE) Te= TeS_surf
    if(sun==FALSE & surface==FALSE) Te= TeS
    
    return(Te) 
  }
  
 
  #Uploading shapefile
  shp <- list.files("Data/Ranges/REPTILES/Files", pattern="/.shp$")
  shp <- gsub(".shp", "", shp) #Comparing to list of shapefiles for all reptiles
    name <<- ifelse(org%in%shp==T,org, #Using the correct name to upload the shapefile
                  ifelse(lizardsdf$Synonym[lizardsdf$Binomial==org]%in%shp==T,as.character(lizardsdf$Synonym[lizardsdf$Binomial==org]),
                         ifelse(lizardsdf$Accepted[lizardsdf$Binomial==org]%in%shp==T,as.character(lizardsdf$Accepted[lizardsdf$Binomial==org],NA))))
  shape <<- readOGR(dsn="Data/Ranges/REPTILES/Files", name)
  crs(shape) <<- crs("+init=epsg:4326") #Changing projection to better match other spatial objects
  
  
  #Microclimate data
  crp <- function(x)crop(x,extent(shape)) #Function for cropping & masking
  mas <- function(x)mask(x,shape) #Using them individually because lists make things more complicated
  
  ncfiletg <- unlist(paste("Data/Microclim/",shade,"_","shade","/","D",depth,"cm","_",substrate,"_",
                           shade,"_",month,".nc",sep="",collapse=NULL)) #Ground Temperature
  Tg <- lapply(ncfiletg,brick)
  Tg <- lapply(Tg, crp)
  Tg <- lapply(Tg, mas)
  
  ncfileta <- unlist(paste("Data/Microclim/",shade,"_","shade","/","TA1cm","_",substrate,"_",
                           shade,"_",month,".nc",sep="",collapse=NULL)) #Air Temperature
  Ta <<- lapply(ncfileta,brick)
  Ta <<- lapply(Ta, crp)
  Ta <<- lapply(Ta, mas)
  
  ncfilews <- unlist(paste("Data/Microclim/wind_speed_ms_1cm/V1cm_",month,".nc",sep="")) #Wind Speed
  u <- lapply(ncfilews,brick)
  u <- lapply(u, crp)
  u <- lapply(u, mas)
  
  ncfileza <- unlist(paste("Data/Microclim/zenith_angle/ZEN_",month,".nc",sep="")) #Zenith Amgle
  psi <- lapply(ncfileza,brick)
  psi <- lapply(psi, crp)
  psi <- lapply(psi,mas)
  
  
  
  #Calculating information relevant to the lizard only
  ctmax <<- lizardsdf$Tmax[lizardsdf$Binomial==org] #Critical Thermal Maximum
  
  svl <<- as.numeric(lizardsdf$SVL[lizardsdf$Binomial==org]) #SVL in mm
  
  mass <<- lizardsdf$Mass[lizardsdf$Binomial==org] #Mass in g
  
  elevation <- raster("Data\\Elevation\\ETOPO1_Ice_g_gmt4.grd") #Obtained elevation from NOAA
  elevation <- resample(elevation, Ta[[1]], method='bilinear') #Making rasters match more closely
  elevation <- replicate(12,elevation) #The function does not work without a list (not sure 100% why....)
  elevation <- lapply(elevation, crp)
  elevation <- lapply(elevation, mas)
  
  rho_S <- 0.7 #Specified in the same function (DOUBLECHECK)
  
  doy <- list(15,45,75,105,135,165,195,225,255,285,315,345) #Day of year for each month (DOUBLE CHECK)

  #NORMAL CONDITIONS
  TeN <- mapply(Tb_lizard,Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  TsmN <<- lapply(TeN,FUN=function(x){ctmax-x}) #Final Substraction

  #TSM +1.5° C SCENARIO
  Ta1 <- lapply(Ta,FUN=function(x){x+1.5}) #Adding the +1.5°C to air and substrate data
  Tg1 <- lapply(Tg,FUN=function(x){x+1.5})
  Te1 <- mapply(Tb_lizard,Ta1, Tg1, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  Tsm1 <<- lapply(Te1,FUN=function(x){ctmax-x}) #Final Substraction

  #TSM +2°C SCENARIO
  Ta2 <- lapply(Ta,FUN=function(x){x+2}) #Adding the +2°C to air and substrate data
  Tg2 <- lapply(Tg,FUN=function(x){x+2})
  Te2 <- mapply(Tb_lizard,Ta2, Tg2, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  Tsm2 <<- lapply(Te2,FUN=function(x){ctmax-x}) #Final Substraction

  TsmNdf <<- lapply(TsmN,as.data.frame, xy=TRUE,na.rm=TRUE) #First rasters to data frames
  Tsm1df <<- lapply(Tsm1,as.data.frame, xy=TRUE,na.rm=TRUE)
  Tsm2df <<- lapply(Tsm2,as.data.frame, xy=TRUE,na.rm=TRUE)
  
  colnames <- c("x","y", "12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM") 
  TsmNdf <<- lapply(TsmNdf, setNames,colnames) #Renaming columns for future use
  Tsm1df <<- lapply(Tsm1df, setNames,colnames)
  Tsm2df <<- lapply(Tsm2df, setNames,colnames)
  
  TsmNdf <<- lapply(TsmNdf, melt,id=c("x","y")) #switching to long format
  Tsm1df <<- lapply(Tsm1df, melt,id=c("x","y"))
  Tsm2df <<- lapply(Tsm2df, melt,id=c("x","y"))
  
  month <- c(1:12)
  names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  lapply(1:12,function(x)(TsmNdf[[x]]$Month <<- names(month)[x])) #Now adding month info to the dataframe
  lapply(1:12,function(x)(Tsm1df[[x]]$Month <<- names(month)[x]))
  lapply(1:12,function(x)(Tsm2df[[x]]$Month <<- names(month)[x]))
  

  lapply(1:12,function(x)(TsmNdf[[x]]$Scenario <<- "Normal")) #Adding column with scenarios
  lapply(1:12,function(x)(Tsm1df[[x]]$Scenario <<- "+1.5° C"))
  lapply(1:12,function(x)(Tsm2df[[x]]$Scenario <<- "+2° C"))
  
  
  TsmNdf <<- do.call(rbind, TsmNdf) #Combining the individual scenarios dataframes in the list into a single one
  Tsm1df <<- do.call(rbind, Tsm1df)
  Tsm2df <<- do.call(rbind, Tsm2df)
  
  Tsmdf <<- rbind(TsmNdf,Tsm1df,Tsm2df) #Now combining all 3 scenarios into 1
  colnames(Tsmdf) <<- c("x","y","Hour","Tsm","Month","Scenario") #Renaming all necessary columns
  
  require(plyr)
  neworder <- c("Normal","+1.5° C","+2° C")
  Tsmdf <<- arrange(transform(Tsmdf, Scenario=factor(Scenario,levels=neworder)),Scenario) #Rearraging orders of scenarios for plotting
  
  neworder2 <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
  Tsmdf <<- arrange(transform(Tsmdf, Hour=factor(Hour,levels=neworder2)),Hour) #Order of hours
  
  Tsmdf <<- arrange(transform(Tsmdf, Month=factor(Month,levels=names(month))),Month) #And order of months
  
  filename <- paste("TSMdfs/",sub(" ","_",org),"_",shade,".Rda",sep="")
  save(Tsmdf,file=filename)
  
  return()
}

##Fully covered (100% shade)
tsmshd = function(org,shade,substrate="Soil",depth=0,month=1:12){
  #Adding this function here to save time?
  Tb_lizard=function(Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=FALSE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5){
    
    psi= psi*pi/180 #convert zenith angle to radians
    
    # constants
    sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
    c_p=29.3 # specific heat of air, J/mol degrees K or C
    
    tau=0.65 # atmospheric transmisivity
    S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)
    
    # Calculate radiation
    # view angles, parameterize for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g
    h=svl/1000 # length of cylinder in m
    theta = psi # angle between solar beam and a normal to the plane in radians, = psi for horizontal surfaces
    
    # F_p=(cos (theta)+(4*h*sin (theta))/(pi*d))/(2+4*h/d)  # beam view angle, Fig 11.6
    A=0.121*mass^0.688   # total lizard area, roughgarden 1981 from Norris (1965) and Porter and James (1979)
    A_p= (-1.1756810^-4*psi^2-9.2594*10^-2*psi+26.2409)*A/100      # projected area
    F_p=A_p/A
    
    # radiation
    p_a=101.3* exp (-elevation/8200)  # atmospheric pressure
    m_a=p_a/(101.3*cos (psi))  # (11.12) optical air mass
    m_a[(psi>(80*pi/180))]=5.66
    
    # Flux densities
    epsilon_ac= 9.2*10^-6*(Ta+273)^2 # (10.11) clear sky emissivity
    L_a=epsilon_ac*sigma*(Ta+273)^4  # (10.7) long wave flux densities from atmosphere 
    L_g=epsilon_s*sigma*(Tg+273)^4  # (10.7) long wave flux densities from ground
    
    #S_p=S_p0*tau^m_a # (11.11) direct irradience , W/m^2
    dd2= 1+2*0.1675*cos(2*pi*doy/365)
    S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
    
    S_d=0.3*(1-tau^m_a)* S_p  # (11.13) diffuse radiation
    #S_t=S_p*cos (psi)+S_d # solar irradience 
    S_r= rho_S*S_p # (11.10) reflected radiation
    
    #__________________________________________________
    # conductance
    
    dim=svl/1000 # characteristic dimension in meters (Table 9.5)
    g_r= 4*sigma*(Ta+273)^3/c_p # (12.7) radiative conductance
    
    g_Ha=1.4*0.135*sqrt(u/dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
    
    #__________________________________________________
    # operative environmental temperature
    
    #calculate with both surface and air temp (on ground and in tree)
    
    sprop=1 #proportion of radiation that is direct, Sears and Angilletta 2012
    R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
    Te=Ta+(R_abs-epsilon_s*sigma*(Ta+273)^4)/(c_p*(g_r+g_Ha))                       
    Te_surf= Tg+(R_abs-epsilon_s*sigma*(Tg+273)^4)/(c_p*(g_r+g_Ha))        
    
    # calculate in shade, no direct radiation
    sprop=0 #proportion of radiation that is direct, Sears and Angilletta 2012
    R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
    TeS=Ta+(R_abs-epsilon_s*sigma*(Ta+273)^4)/(c_p*(g_r+g_Ha))                       
    TeS_surf=Tg+(R_abs-epsilon_s*sigma*(Tg+273)^4)/(c_p*(g_r+g_Ha))  
    
    #Select Te to return
    if(sun==TRUE & surface==TRUE) Te= Te_surf
    if(sun==TRUE & surface==FALSE) Te= Te
    if(sun==FALSE & surface==TRUE) Te= TeS_surf
    if(sun==FALSE & surface==FALSE) Te= TeS
    
    return(Te) 
  }
  
  
  #Uploading shapefile
  shp <- list.files("Data/Ranges/REPTILES/Files", pattern="/.shp$")
  shp <- gsub(".shp", "", shp) #Comparing to list of shapefiles for all reptiles
  name <<- ifelse(org%in%shp==T,org, #Using the correct name to upload the shapefile
                  ifelse(lizardsdf$Synonym[lizardsdf$Binomial==org]%in%shp==T,as.character(lizardsdf$Synonym[lizardsdf$Binomial==org]),
                         ifelse(lizardsdf$Accepted[lizardsdf$Binomial==org]%in%shp==T,as.character(lizardsdf$Accepted[lizardsdf$Binomial==org],NA))))
  shape <<- readOGR(dsn="Data/Ranges/REPTILES/Files", name)
  crs(shape) <<- crs("+init=epsg:4326") #Changing projection to better match other spatial objects
  
  
  #Microclimate data
  crp <- function(x)crop(x,extent(shape)) #Function for cropping & masking
  mas <- function(x)mask(x,shape) #Using them individually because lists make things more complicated
  
  ncfiletg <- unlist(paste("Data/Microclim/",shade,"_","shade","/","D",depth,"cm","_",substrate,"_",
                           shade,"_",month,".nc",sep="",collapse=NULL)) #Ground Temperature
  Tg <- lapply(ncfiletg,brick)
  Tg <- lapply(Tg, crp)
  Tg <- lapply(Tg, mas)
  
  ncfileta <- unlist(paste("Data/Microclim/",shade,"_","shade","/","TA1cm","_",substrate,"_",
                           shade,"_",month,".nc",sep="",collapse=NULL)) #Air Temperature
  Ta <<- lapply(ncfileta,brick)
  Ta <<- lapply(Ta, crp)
  Ta <<- lapply(Ta, mas)
  
  ncfilews <- unlist(paste("Data/Microclim/wind_speed_ms_1cm/V1cm_",month,".nc",sep="")) #Wind Speed
  u <- lapply(ncfilews,brick)
  u <- lapply(u, crp)
  u <- lapply(u, mas)
  
  ncfileza <- unlist(paste("Data/Microclim/zenith_angle/ZEN_",month,".nc",sep="")) #Zenith Amgle
  psi <- lapply(ncfileza,brick)
  psi <- lapply(psi, crp)
  psi <- lapply(psi,mas)
  
  
  
  #Calculating information relevant to the lizard only
  ctmax <- lizardsdf$Tmax[lizardsdf$Binomial==org] #Critical Thermal Maximum
  
  svl <- as.numeric(lizardsdf$SVL[lizardsdf$Binomial==org]) #SVL in mm
  
  mass <- lizardsdf$Mass[lizardsdf$Binomial==org] #Mass in g
  
  elevation <- raster("Data/Elevation/ETOPO1_Ice_g_gmt4.grd") #Obtained elevation from NOAA
  elevation <- resample(elevation, Ta[[1]], method='bilinear') #Making rasters match more closely
  elevation <- replicate(12,elevation) #The function does not work without a list (not sure 100% why....)
  elevation <- lapply(elevation, crp)
  elevation <- lapply(elevation, mas)
  
  rho_S <- 0.7 #Specified in the same function (DOUBLECHECK)
  
  doy <- list(15,45,75,105,135,165,195,225,255,285,315,345) #Day of year for each month (DOUBLE CHECK)
  
  #NORMAL CONDITIONS
  TeN <- mapply(Tb_lizard,Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=FALSE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  TsmN <<- lapply(TeN,FUN=function(x){ctmax-x}) #Final Substraction
  
  #TSM +1.5° C SCENARIO
  Ta1 <- lapply(Ta,FUN=function(x){x+1.5}) #Adding the +1.5°C to air and substrate data
  Tg1 <- lapply(Tg,FUN=function(x){x+1.5})
  Te1 <- mapply(Tb_lizard,Ta1, Tg1, u, svl, mass, psi, rho_S, elevation, doy, sun=FALSE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  Tsm1 <<- lapply(Te1,FUN=function(x){ctmax-x}) #Final Substraction
  
  #TSM +2°C SCENARIO
  Ta2 <- lapply(Ta,FUN=function(x){x+2}) #Adding the +2°C to air and substrate data
  Tg2 <- lapply(Tg,FUN=function(x){x+2})
  Te2 <- mapply(Tb_lizard,Ta2, Tg2, u, svl, mass, psi, rho_S, elevation, doy, sun=FALSE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) #Running the function
  Tsm2 <<- lapply(Te2,FUN=function(x){ctmax-x}) #Final Substraction
  
  TsmNdf <<- lapply(TsmN,as.data.frame, xy=TRUE,na.rm=TRUE) #First rasters to data frames
  Tsm1df <<- lapply(Tsm1,as.data.frame, xy=TRUE,na.rm=TRUE)
  Tsm2df <<- lapply(Tsm2,as.data.frame, xy=TRUE,na.rm=TRUE)
  
  colnames <- c("x","y", "12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM") 
  TsmNdf <<- lapply(TsmNdf, setNames,colnames) #Renaming columns for future use
  Tsm1df <<- lapply(Tsm1df, setNames,colnames)
  Tsm2df <<- lapply(Tsm2df, setNames,colnames)
  
  TsmNdf <<- lapply(TsmNdf, melt,id=c("x","y")) #switching to long format
  Tsm1df <<- lapply(Tsm1df, melt,id=c("x","y"))
  Tsm2df <<- lapply(Tsm2df, melt,id=c("x","y"))
  
  month <- c(1:12)
  names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  lapply(1:12,function(x)(TsmNdf[[x]]$Month <<- names(month)[x])) #Now adding month info to the dataframe
  lapply(1:12,function(x)(Tsm1df[[x]]$Month <<- names(month)[x]))
  lapply(1:12,function(x)(Tsm2df[[x]]$Month <<- names(month)[x]))
  
  
  lapply(1:12,function(x)(TsmNdf[[x]]$Scenario <<- "Normal")) #Adding column with scenarios
  lapply(1:12,function(x)(Tsm1df[[x]]$Scenario <<- "+1.5° C"))
  lapply(1:12,function(x)(Tsm2df[[x]]$Scenario <<- "+2° C"))
  
  
  TsmNdf <<- do.call(rbind, TsmNdf) #Combining the individual scenarios dataframes in the list into a single one
  Tsm1df <<- do.call(rbind, Tsm1df)
  Tsm2df <<- do.call(rbind, Tsm2df)
  
  Tsmdf <<- rbind(TsmNdf,Tsm1df,Tsm2df) #Now combining all 3 scenarios into 1
  colnames(Tsmdf) <<- c("x","y","Hour","Tsm","Month","Scenario") #Renaming all necessary columns
  
  require(plyr)
  neworder <- c("Normal","+1.5° C","+2° C")
  Tsmdf <<- arrange(transform(Tsmdf, Scenario=factor(Scenario,levels=neworder)),Scenario) #Rearraging orders of scenarios for plotting
  
  neworder2 <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
  Tsmdf <<- arrange(transform(Tsmdf, Hour=factor(Hour,levels=neworder2)),Hour) #Order of hours
  
  Tsmdf <<- arrange(transform(Tsmdf, Month=factor(Month,levels=names(month))),Month) #And order of months
 
  filename <- paste("TSMdfs/",sub(" ","_",org),"_",shade,".Rda",sep="")
  save(Tsmdf,file=filename)
  
  return()
}


### Calculating TSM ----

Binomial
#Can have any name
org <- c("Phrynosoma cornutum")

#This when only one organism
tsmsun(org,0) #Always had to be 0
tsmshd(org,100) #Always had to be 100

#This is for presentation only to be used as a list for both functions
org <- c("Platysaurus intermedius","Psammodromus hispanicus","Sceloporus magister")
shd <- c(0,100)
#This when multiple
mapply(tsmsun,org,shd) 



















































