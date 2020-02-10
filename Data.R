####
# CREATING A DATA FRAME FOR LEPIDOSAURIA WITH AVAILABLE DATA
####

### INITIAL SETUP----
## Setting Working Directory
fdir = "/Volumes/GoogleDrive/Shared drives/TrEnCh/TSMVisualization/"
setwd(fdir)

## Packages needed for TSM
pkgsd <- c('pdftools','tidyverse','taxize')
install.packages(pkgsd)
lapply(pkgsd, library, character.only = TRUE)
library(dplyr)

### LIZARD DATA----
## Global dataset for species and CTmax/min
globtherm = read.csv("Data/Traits/GlobalTherm_upload_10_11_17.csv", header = TRUE, na.strings = "")
ectotherms = subset(globtherm, Class == "Lepidosauria", select = c(Genus, Species, Class, Tmax, REF_max)) # Subsetting for Lizards (Lepidosuria) only
ectotherms$Binomial = paste(ectotherms$Genus, ectotherms$Species) # Pasting together species and genus names to use for matching

##Adding synonyms & accepted names to ectotherms df
#synonyms <- synonyms(Binomial, db="itis")# THIS ONE TAKES FOREVER USING TAXIZE BUT WORKS GREAT! 3/19/2019
require(taxize)
synonyms <- readRDS(file = "Data/synonyms.rds") #Already looked them up so just uploading previous results
syn <- synonyms_df(synonyms) #Turning them into a data frame
match1 <- match(as.character(ectotherms$Binomial), syn$.id)
matched <- which(!is.na(match1))
ectotherms$Synonym[matched] <- syn$syn_name[match1[matched]] #From synonym database
ectotherms$Accepted[matched] <- syn$acc_name[match1[matched]] #From synonym database

##Adding Topt. Synonyms and accepted names do not yield results
Topt = read.csv("Data/Traits/rspb20081957supp01.csv", skip = 4, header = T)
Topt <- Topt[-c(71:101),]
Topt$Species <- gsub("_"," ",Topt$Species)
match1 <- match(as.character(ectotherms$Binomial), Topt$Species)
matched <- !is.na(match1)
ectotherms$Topt[matched] <- Topt$newTopt[match1[matched]] #From synonym database

##Confirming presence of shapefiles to be used
shp <- list.files("Data/Ranges/REPTILES/Files", pattern="/.shp$") 
shp <- gsub(".shp", "", shp) #Replacing name strings for synonyms
Shapefile1 <- ectotherms$Binomial%in%shp #Matching with directory files
Shapefile2 <- ectotherms$Synonym%in%shp
Shapefile3 <- ectotherms$Accepted%in%shp
Shapefile1[Shapefile1=="FALSE"] <- NA 
Shapefile2[Shapefile2=="FALSE"] <- NA
Shapefile3[Shapefile3=="FALSE"] <- NA
ectotherms$Shapefile <- ifelse(!is.na(Shapefile1),Shapefile1,ifelse(is.na(Shapefile2),Shapefile3,Shapefile2)) #New column indicating shapefile availability

## Masses for lizards in g
lizards <-read.csv("Data/Traits/jzo_696_sm_appendix-s1.csv",header = TRUE, na.strings = "")
#Adding first mass
match1 <- match(as.character(ectotherms$Binomial), lizards$Species)
matched1 <- which(!is.na(match1))
match2 <- match(as.character(ectotherms$Synonym), lizards$Species)
matched2 <- which(!is.na(match2))
match3 <- match(as.character(ectotherms$Accepted), lizards$Species)
matched3 <- which(!is.na(match3))
ectotherms$Mass[matched1] <- ifelse(!is.na(lizards$Weight..g.[match1[matched1]]),lizards$Weight..g.[match1[matched1]], ifelse(is.na(lizards$Weight..g.[match2[matched2]]), lizards$Weight..g.[match3[matched3]], lizards$Weight..g.[match2[matched2]]))

## SVL in mm. Source #1 (same as above)
#Adding first SVL
SVL1 <- rep(NA,length(ectotherms$Binomial))
SVL1[matched1] <- ifelse(!is.na(lizards$SVL..mm.[match1[matched1]]),lizards$SVL..mm.[match1[matched1]], ifelse(is.na(lizards$SVL..mm.[match2[matched2]]), lizards$SVL..mm.[match3[matched3]], lizards$SVL..mm.[match2[matched2]]))

## SVL in mm. Source #2
SVLdf <- pdf_text("Data/Traits/geb_414_sm_apps2.pdf") %>% readr::read_lines(skip=8) # First part not usable so subsetting from after 8 lines
SVLdf <- as.data.frame(SVLdf) %>% separate(SVLdf,c("Family", "Taxon","Name","SVL"),sep="/s+") # Separating strings. Only 4 and the rest are discarded
SVLdf[SVLdf==""]  <- NA # Empty cells to NA
SVLdf <- na.omit(SVLdf) # Deleting all empty lines
SVLdf$Name <- gsub("_", " ", SVLdf$Name) # Separating the names so they match with "Binomial"
match1 <- match(as.character(ectotherms$Binomial), SVLdf$Name)
matched1 <- which(!is.na(match1))
match2 <- match(as.character(ectotherms$Synonym),  SVLdf$Name)
matched2 <- which(!is.na(match2))
match3 <- match(as.character(ectotherms$Accepted),  SVLdf$Name)
matched3 <- which(!is.na(match3))
SVL2 <- rep(NA,length(ectotherms$Binomial))
SVL2[matched1] <- ifelse(!is.na(SVLdf$SVL[match1[matched1]]),SVLdf$SVL[match1[matched1]], ifelse(is.na(SVLdf$SVL[match2[matched2]]), SVLdf$SVL[match3[matched3]], SVLdf$SVL[match2[matched2]]))
ectotherms$SVL <- ifelse(!is.na(SVL1),SVL1,SVL2)

##One last subsetting for those with complete data
lizardsdf = subset(ectotherms, !is.na(ectotherms$Tmax) & !is.na(ectotherms$Mass) & !is.na(ectotherms$SVL) & !is.na(ectotherms$Shapefile)) # Subsetting 

##Saving lizards df
saveRDS(lizardsdf,"Data/Lepidosauria.Rda")
write.csv(lizardsdf,"Data/Lepidosauria.csv")















