###
#THIS ONE PLOTS WITH GGPLOT2
###

## Packages needed for TSM
pkgsp <- c('ggplot2','ggridges','plotly')
#install.packages(pkgsp)
lapply(pkgsp, library, character.only = TRUE)

### INITIAL SETUP----
## Setting Working Directory
fdir = "C:\\Users\\Bryan\\Google Drive\\TSMVisualization\\"
#fdir= "/Volumes/GoogleDrive/My\ Drive/TSMVisualization/"
setwd(fdir)
getwd()
##Selecting plot data ALL POSSIBLE DATA ARE SHOWN AS COMMENTS.
org <- c("Phrynosoma cornutum") #Any name

#shade <- c("Exposed","Covered") 
shd <- c("Exposed")

#hour <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
hr <- c("04 PM")

#mnth <- c("January","February","March","April","May","June","July","August","September","October","November","December")
mnth <- c("July")

#category <- c("< 0","0-1","1-3","> 3")
cat <- c("1-3")

#scenario <- c("Normal","+1.5 °C","+2 °C")
scn <- c("Normal")


##Uploading previous data for both shades
spdata1 <- paste("TSMdfs\\",gsub(" ","_",org),"_0.Rda", sep= "") #Starting with 0 shade
spdata1 <- "TSMdfs\\Chamaeleo_jacksonii_0.Rda"
load(spdata1)
Tsm0 <- Tsmdf
Tsm0$Shade <- c("Exposed")

spdata2 <- paste("TSMdfs\\",gsub(" ","_",org),"_100.Rda", sep= "")
#spdata2 <- paste("TSMdfs/",gsub(" ","_",org),"_100.Rda", sep= "")
spdata2 <- "TSMdfs\\Chamaeleo_jacksonii_100.Rda"
load(spdata2)
Tsm100<- Tsmdf
Tsm100$Shade <- c("Covered")

#Making a bigger one? for better plotting
Tsmdf_plot <- rbind(Tsm0,Tsm100)

levels(Tsmdf_plot$Hour)
combined <- fread(paste("TSMdfs\\",gsub(" ","_",org),"_combined.csv", sep= ""))

neworder2 <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

levels(combined$Hour) = neworder2

### NOW PLOTTING----

## Density graphs

#1 Single month divided by shade and scenario
#HOURS: all for y axis, TSM: all for x axis, MONTH: only one, SCENARIO: fill rigdelines, SHADE: facet_wrap, CATEGORY: NA
ggplot(subset(Tsmdf_plot,Month==mnth),aes(x=Tsm,y= Hour)) + geom_density_ridges2(aes(fill=Scenario),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(org,"|",mnth)) + scale_y_discrete(limits = rev(levels(Tsmdf_plot$Hour)))  + theme_bw() + facet_wrap(~Shade) 

ggsave(paste(gsub(" ","_",org),"d1.png"))



ggplot(data = subset(Tsmdf_plot,Month==mnth),aes(x=Tsm, y= Hour)) + geom_rect(data = rects, aes(ymin = 0, ymax = 20, xmin = xstart, xmax = xend, fill = "red"), alpha = 1) + 
  geom_density_ridges2(aes(fill=Scenario),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(org,"|",mnth)) 


ggplot() + geom_rect(aes(xmin = -40, xmax = 0, ymin = 0, ymax = 15), fill = "red", alpha = 0.5)

rects <- data.frame(xstart = 0, xend = 10, col = "red")


#2 Single month divided by scenairo and shade
#HOURS: all for y axis, TSM: all for x axis, MONTH: only one, SCENARIO: facet_wrap, SHADE: fill, CATEGORY: NA
ggplot(subset(Tsmdf_plot, Month==mnth),aes(x=Tsm,y= Hour)) + geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(org,"|",mnth)) + scale_y_discrete(limits = rev(levels(Tsmdf_plot$Hour)))  + theme_bw() + facet_wrap(~Scenario) + scale_fill_manual(values = c("blue","red")) 

ggsave(paste(gsub(" ","_",org),"d2.png"))

#3 All months in a single scenario divided by shade  
#HOURS: all for y axis, TSM: all for x axis, MONTH: all facet_wrap, SCENARIO: only one, SHADE: fill, CATEGORY: NA
ggplot(subset(Tsmdf_plot, Scenario==scn),aes(x=Tsm,y= Hour)) + geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(org,"|",scn)) + scale_y_discrete(limits = rev(levels(Tsmdf_plot$Hour)))  + theme_bw() + facet_wrap(~Month) + scale_fill_manual(values = c("blue","red"))

ggsave(paste(gsub(" ","_",org),"d3.png"))


## Maps

#1 Single hour, single month, divided by shade
ggplot() + 
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",mnth,"|",hr,"|",scn))  + 
  geom_raster(data=subset(Tsmdf_plot,Hour==hr & Month==mnth & Scenario==scn), aes(x = x, y = y, fill = Tsm) , interpolate = TRUE) + 
  facet_wrap(~Shade )  + 
  coord_quickmap(xlim = c(min(Tsmdf_plot$x), max(Tsmdf_plot$x)), ylim = c(min(Tsmdf_plot$y), max(Tsmdf_plot$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"_",mnth,"_",hr,"_",scn,"d.png"))



#1 using the non long formatted data
Tsmdf_plot <- fread(paste("TSMdfs\\",gsub(" ","_",org),"_combined_nonlong.csv", sep= ""))

Tsmdf_plot <- subset(Tsmdf_plot, Month==mnth & Scenario==scn)

ggplot() + 
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",mnth,"|",hr,"|",scn))  + 
  geom_raster(data=Tsmdf_plot, aes(x = x, y = y, fill = `04 PM`), interpolate = TRUE) + 
  facet_wrap(~Shade )  + 
  coord_quickmap(xlim = c(min(Tsmdf_plot$x), max(Tsmdf_plot$x)), ylim = c(min(Tsmdf_plot$y), max(Tsmdf_plot$y)),expand = TRUE) + 
  theme_bw( )








#2 Similar
ggplot() + 
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",mnth,"|",hr,"|",shd))  + 
 
  geom_raster(data=subset(Tsmdf_plot,Hour==hr & Month==mnth & Shade==shd), aes(x = x, y = y, fill = Tsm) , interpolate = TRUE) + 
  facet_wrap(~Scenario )  + 
  coord_quickmap(xlim = c(min(Tsmdf_plot$x), max(Tsmdf_plot$x)), ylim = c(min(Tsmdf_plot$y), max(Tsmdf_plot$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"2.png"))

#3 A single hour in all months for both shades and a single scenario
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",hr,"|",shd))  + 
  scale_fill_manual(values = c("green4","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf_plot,Hour==hr & Shade==shd), aes(x = x, y = y, fill = Category) , interpolate = T) +
  facet_grid(rows=vars(Scenario),cols=vars(Month) )  + 
  coord_quickmap(xlim = c(min(Tsmdf_plot$x), max(Tsmdf_plot$x)), ylim = c(min(Tsmdf_plot$y), max(Tsmdf_plot$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"3.png"))

#4 A single hour in all months for both shades
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",hr,"|",scn))  + 
  scale_fill_manual(values = c("green4","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf_plot,Hour==hr & Scenario==scn), aes(x = x, y = y, fill = Category),interpolate = T) +
  facet_grid(rows=vars(Shade),cols=vars(Month) )  + 
  coord_quickmap(xlim = c(min(Tsmdf_plot$x), max(Tsmdf_plot$x)), ylim = c(min(Tsmdf_plot$y), max(Tsmdf_plot$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"4.png"))

#5 All months and all hours for a single scenario and a single shade
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",scn,"|",shd))  + 
  scale_fill_manual(values = c("green4","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf_plot,Scenario==scn & Shade==shd),aes(x = x, y = y, fill = Category),interpolate = T)+
  facet_grid(rows=vars(Month),cols=vars(Hour) )  + 
  coord_quickmap(xlim = c(min(Tsmdf_plot$x), max(Tsmdf_plot$x)), ylim = c(min(Tsmdf_plot$y), max(Tsmdf_plot$y)),expand = TRUE) + 
  theme_bw( )

ggsave(paste(gsub(" ","_",org),"5.png"))

#6 #STILL WORKING ON THIS
ggplot()+  
  borders(fill="grey",colour="black") +  
  ggtitle(paste(org, "|",hr,"|",shd))  + 
  scale_fill_manual(values = c("green4","yellow","orange","red")) +
  geom_raster(data=subset(Tsmdf_plot,Hour==hr & Shade==shd), aes(x = x, y = y, fill = Category) , interpolate = T) +
  facet_grid(rows=vars(Scenario),cols=vars(Month) )  + 
  coord_quickmap(xlim = c(min(Tsmdf_plot$x), max(Tsmdf_plot$x)), ylim = c(min(Tsmdf_plot$y), max(Tsmdf_plot$y)),expand = TRUE) + 
  theme_bw( ) +
  facet_wrap(~Shade)

ggsave(paste(gsub(" ","_",org),"6.png"))