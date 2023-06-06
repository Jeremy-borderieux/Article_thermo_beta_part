####  New NFI data presentation, period 2005 - 2022 ####

#### Spatial data

SER_sf<-read_sf(file.path("Data","Spatial_Data"),layer="ser_l93")

#### NFI data loading
NFI_ecologie<-data.table(read.table(unz(file.path("Data","Data_NFI_2005_2021","export_dataifn_2005_2021.zip"),"ECOLOGIE.csv"),sep=";",header=T))
NFI_plot_info<-data.table(read.table(unz(file.path("Data","Data_NFI_2005_2021","export_dataifn_2005_2021.zip"),"PLACETTE.csv"),sep=";",header=T))
NFI_arbres<-data.table(read.table(unz(file.path("Data","Data_NFI_2005_2021","export_dataifn_2005_2021.zip"),"ARBRE.csv"),sep=";",header=T))
NFI_cover<-data.table(read.table(unz(file.path("Data","Data_NFI_2005_2021","export_dataifn_2005_2021.zip"),"COUVERT.csv"),sep=";",header=T))
#NFI_habitat<-data.table(read.table(unz(file.path("Data","Data_NFI_2005_2021","export_dataifn_2005_2021.zip"),"HABITAT.csv"),sep=";",header=T))


colnames(NFI_plot_info)[1]<-"campaign"
#### loading the already harmonized flora surveys of the NFI
NFI_flora<-readRDS(file.path("Data","Data_NFI_2005_2021","harmonized_NFI_survey_2005_2021.RData"))
colnames(NFI_flora)<-c("idp","species_name","abund","tree")

#### loading climate data extracted from the digitalis v2b french model
NFI_climate<-readRDS(file.path("Data","Data_NFI_2005_2021","climatic_database_NFI_2021.RData"))
NFI_climate[,campagne:=NULL]
NFI_climate[,greco:=NULL]

## renaming the clnames
colnames(NFI_ecologie)<-tolower(colnames(NFI_ecologie));colnames(NFI_ecologie)[1]<-"campaign";NFI_ecologie[,x:=NULL]
colnames(NFI_plot_info)<-tolower(colnames(NFI_plot_info));colnames(NFI_plot_info)[1]<-"campaign";NFI_plot_info[,x:=NULL]
colnames(NFI_arbres)<-tolower(colnames(NFI_arbres));colnames(NFI_arbres)[1]<-"campaign";NFI_arbres[,x:=NULL]
colnames(NFI_cover)<-tolower(colnames(NFI_cover));colnames(NFI_cover)[1]<-"campaign";NFI_cover[,x:=NULL]
#colnames(NFI_habitat)<-tolower(colnames(NFI_habitat));colnames(NFI_habitat)[1]<-"campaign";NFI_habitat[,x:=NULL]
NFI_ecologie[,campaign:=NULL]

## we select only the first visit of plots (some are visided a second time 5 years later for some variable only)
NFI_plot_info<-NFI_plot_info[visite==1,]
NFI_plot_info[,campagne_2 := floor(idp/100000)+2005]

##loading precise elevation data
precise_elevation<-fread(file.path("Data","Data_NFI_2005_2021","altitudes_exactes.csv"))[idp!='NULL',]
precise_elevation[,idp:=as.numeric(idp)]
colnames(precise_elevation)<-c("idp","alti")

## we compute the canopy cover of a plot
canopy_cover<-NFI_cover[strate=="R",.(canopy_cover=sum(tcl),canopy_cover_abs=sum(tca)),by=idp]

#### management and basal area computation
NFI_plot_info[,gest_struct:=sfo]
NFI_plot_info[,tmp:=switchv(as.character(sver),"0",`NA`="0",`0`="0",`X`="0",`2`="1",`3`="4",`4`="2",`5`="3",`6`="1")]
NFI_plot_info$gest_struct[NFI_plot_info$campagne>2013]<-NFI_plot_info$tmp[NFI_plot_info$campagne>2013]
NFI_plot_info[,gest_struct:=switchv(as.character(gest_struct),`NA`=NA,`0`="debois",`1`="FR",`2`="FIR",`3`="TSF",`4`="T")]
NFI_plot_info[,gest_struct:=as.character(gest_struct)]
# Imputation aux arbres simplifi?s de la valeur moyenne des mesures des arbres de la m?me placette, essences, et classe de taille
NFI_arbres[, dimess := cut(c13, breaks = c(0, 70.5, 117.5, 164.5, 1000), labels = c("PB", "BM", "GB", "TGB"), right = FALSE)]
NFI_arbres[, htot := ifelse(is.na(htot), mean(htot, na.rm = TRUE), htot), by = c("idp", "espar", "dimess")]
NFI_arbres[, ir5 := ifelse(is.na(ir5), mean(ir5, na.rm = TRUE), ir5), by = c("idp", "espar", "dimess")]

### we keep the living and standing trees only
NFI_arbres<-NFI_arbres[veget=="0",]

NFI_arbres[,campaign_2 := floor(idp/100000)+2005]

### basal area tree level
NFI_arbres[,gFinal:=c13*c13*w/(4*pi)]
NFI_arbres[,gInitial:=pi/10000*(c13/(2*pi)-ir5/10)^2*w]

### basal area
NFI_dendro <- NFI_arbres[,.(basal_area=sum(gFinal), Ntot=sum(w)), by = idp]



## merge understory tree databases. Historically in the canopy cover dataset, now in the florisitic dataset
## -> everything the floristic dataset. 
NR_trees<-NFI_cover[strate=="NR",]
NR_trees[,strate:=NULL]
NR_trees[,tca:=NULL]
NR_trees[,tcl:=NULL]
NR_trees[,p1525:=NULL]
NR_trees[,p7ares:=NULL]
NR_trees[,tree:=1]

NFI_flora[,campaign:=2005+floor(idp/100000)]

espar_to_sp <- data.table(read.csv(file.path("Data","Data_NFI_2005_2021","espar-cdref13.csv"), sep=";",encoding = "UTF-8",stringsAsFactors = F))
colnames(espar_to_sp)[1]<-"espar_c"
espar_to_sp[,espar_c:=ifelse(nchar(espar_c)==1,paste0("0",espar_c),espar_c)]

espar_to_sp[,species_name:=word(lib_cdref,1,2)]
espar_to_sp[,species_name:=ifelse(word(species_name,2,2)=="x",word(lib_cdref,1,3),species_name)]

NR_trees<-merge(NR_trees,espar_to_sp[,c("espar_c","species_name")],by="espar_c")
NR_trees[,espar_c:=NULL]
NR_trees[,abund:=1]
NR_trees<-NR_trees[,c(2,4,5,3,1)]
NFI_flora<-rbind(NR_trees,NFI_flora)
rm(NR_trees)
## end of merging of canopy cover and understory trees

