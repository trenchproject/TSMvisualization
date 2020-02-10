###
#THIS ONE PLOTS WITH GGPLOT2
###

## Packages needed for TSM
pkgsp <- c('ggplot2','ggridges','plotly')
install.packages(pkgsp)
lapply(pkgsp, library, character.only = TRUE)

### INITIAL SETUP----
## Setting Working Directory
fdir = "/Volumes/GoogleDrive/Shared drives/TrEnCh/TSMVisualization/"
setwd(fdir)

##Selecting plot data ALL POSSIBLE DATA ARE SHOWN AS COMMENTS.
org <- c("Sceloporus magister") #Any name

#shade <- c("Exposed","Covered") 
shd <- c("Exposed")

#hour <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
hr <- c("03 PM")

#month <- c("January","February","March","April","May","June","July","August","September","October","November","December")
mnth <- c("June")

#category <- c("< 0","0-1","1-3","> 3")
cat <- c("1-3")

#scenario <- c("Normal","+1.5° C","+2° C")
scn <- c("Normal")


##Uploading previous data for both shades
spdata1 <- paste("TSMdfs/",gsub(" ","_",org),"_0.Rda", sep= "") #Starting with 0 shade
load(spdata1)
Tsm0 <- Tsmdf
Tsm0$Shade <- c("Exposed")

spdata2 <- paste("TSMdfs/",gsub(" ","_",org),"_100.Rda", sep= "")
load(spdata2)
Tsm100<- Tsmdf
Tsm100$Shade <- c("Covered")

#Making a bigger one? for better plotting
Tsmdf <- rbind(Tsm0,Tsm100)


### NOW PLOTTING----

## Density graphs

#1 Single month divided by shade and scenario
#HOURS: all for y axis, TSM: all for x axis, MONTH: only one, SCENARIO: fill rigdelines, SHADE: facet_wrap, CATEGORY: NA
ggplot(subset(Tsmdf,Month==mnth),aes(x=Tsm,y= Hour)) + geom_density_ridges2(aes(fill=Scenario),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(org,"|",mnth)) + scale_y_discrete(limits = rev(levels(Tsmdf$Hour)))  + theme_bw() + facet_wrap(~Shade) + scale_fill_manual(values = c("green","yellow","red"))

ggsave(paste(gsub(" ","_",org),"d1.png"))

#2 Single month divided by scenairo and shade
#HOURS: all for y axis, TSM: all for x axis, MONTH: only one, SCENARIO: facet_wrap, SHADE: fill, CATEGORY: NA
ggplot(subset(Tsmdf, Month==mnth),aes(x=Tsm,y= Hour)) + geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(org,"|",mnth)) + scale_y_discrete(limits = rev(levels(Tsmdf$Hour)))  + theme_bw() + facet_wrap(~Scenario) + scale_fill_manual(values = c("blue","red"))

ggsave(paste(gsub(" ","_",org),"d2.png"))
  
#3 All months in a single scenario divided by shade  
#HOURS: all for y axis, TSM: all for x axis, MONTH: all facet_wrap, SCENARIO: only one, SHADE: fill, CATEGORY: NA
ggplot(subset(Tsmdf, Scenario==scn),aes(x=Tsm,y= Hour)) + geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(org,"|",scn)) + scale_y_discrete(limits = rev(levels(Tsmdf$Hour)))  + theme_bw() + facet_wrap(~Month) + scale_fill_manual(values = c("blue","red"))

ggsave(paste(gsub(" ","_",org),"d3.png"))


## Maps

#1 Single hour, single month, divided by shade
ggplot() + 
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",mnth,"|",hr,"|",scn))  + 
  scale_fill_manual(values = c("green","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf,Hour==hr & Month==mnth & Scenario==scn), aes(x = x, y = y, fill = Category) , interpolate = T) + 
  facet_wrap(~Shade )  + 
  coord_quickmap(xlim = c(min(Tsmdf$x), max(Tsmdf$x)), ylim = c(min(Tsmdf$y), max(Tsmdf$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"1.png"))


#2 Similar
ggplot() + 
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",mnth,"|",hr,"|",shd))  + 
  scale_fill_manual(values = c("green","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf,Hour==hr & Month==mnth & Shade==shd), aes(x = x, y = y, fill = Category) , interpolate = T) + 
  facet_wrap(~Scenario )  + 
  coord_quickmap(xlim = c(min(Tsmdf$x), max(Tsmdf$x)), ylim = c(min(Tsmdf$y), max(Tsmdf$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"2.png"))

#3 A single hour in all months for both shades and a single scenario
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",hr,"|",shd))  + 
  scale_fill_manual(values = c("green","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf,Hour==hr & Shade==shd), aes(x = x, y = y, fill = Category) , interpolate = T) +
  facet_grid(rows=vars(Scenario),cols=vars(Month) )  + 
  coord_quickmap(xlim = c(min(Tsmdf$x), max(Tsmdf$x)), ylim = c(min(Tsmdf$y), max(Tsmdf$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"3.png"))

#4 A single hour in all months for both shades
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",hr,"|",scn))  + 
  scale_fill_manual(values = c("green","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf,Hour==hr & Scenario==scn), aes(x = x, y = y, fill = Category),interpolate = T) +
  facet_grid(rows=vars(Shade),cols=vars(Month) )  + 
  coord_quickmap(xlim = c(min(Tsmdf$x), max(Tsmdf$x)), ylim = c(min(Tsmdf$y), max(Tsmdf$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"4.png"))

#5 All months and all hours for a single scenario and a single shade
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",scn,"|",shd))  + 
  scale_fill_manual(values = c("green","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf,Scenario==scn & Shade==shd),aes(x = x, y = y, fill = Category),interpolate = T)+
  facet_grid(rows=vars(Month),cols=vars(Hour) )  + 
  coord_quickmap(xlim = c(min(Tsmdf$x), max(Tsmdf$x)), ylim = c(min(Tsmdf$y), max(Tsmdf$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"5.png"))

#6 #STILL WORKING ON THIS
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",hr,"|",shd))  + 
  scale_fill_manual(values = c("green","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf,Hour==hr & Shade==shd), aes(x = x, y = y, fill = Category) , interpolate = T) +
  facet_grid(rows=vars(Scenario),cols=vars(Month) )  + 
  coord_quickmap(xlim = c(min(Tsmdf$x), max(Tsmdf$x)), ylim = c(min(Tsmdf$y), max(Tsmdf$y)),expand = TRUE) + 
  theme_bw( ) +
  facet_wrap(~Shade)

ggsave(paste(gsub(" ","_",org),"6.png"))
















