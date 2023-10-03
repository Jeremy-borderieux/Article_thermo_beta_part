#### Import Packages ####

# You can run this line to import and install the latest version of the package used:
# (they won't be included in your global R installation, they will only be used in this project)
# For reproducibility, instead of running the below command, you can use the renv package to restore 
# all the packages used to their exact version (more info in line 30)

## basic loading and installation
  for(pack in c("data.table", # faster and different data.frames
                "broman", # all sort of utility functions
                "stringr", # string manipulation
                "ggplot2", # plotting and data representation
                "ggspatial",
                "sf", # handling spatial vectors
                "foreach", # for loop for parallel computation and aggregates the outputs
                "doParallel", # Parallel computation, with window
                "raster",
                "car",
                "scales",
                "showtext",
                "ggpubr",# create composite ggplots
                "cowplot", # create composite ggplots
                "lubridate")){ # easier date - time manipulation
    if(!pack %in%installed.packages()[,"Package"] )install.packages(pack)
    library(pack,character.only=TRUE)} ;rm(pack)




## For maximum reproducibility, run renv::restore() to get all the packages with the version used during the analysis
# running renv::restore() can take some time, up to 20 minutes
# Some problems may occurs, as the R Version used is not retrieved nor updated (RTools version coand vary too). 
#This analysis was done with R.4.2.2 and windows 10

# renv::restore()

# you can now load the packages
library(data.table) # faster and different data.frames
library(broman) # all sort of utility functions
library(stringr)# string manipulation
library(ggplot2)# plotting and data representation
library(ggspatial)
library(sf)# handling spatial vectors
library(foreach) # for loop for parallel computation and aggregates the outputs
library(doParallel) # Parallel computation, with window
library(raster)
library(car)
library(scales)
library(showtext)
library(ggpubr) # create composite ggplots
library(cowplot)# create composite ggplots 
library(lubridate) # easier date - time manipulation



#### Import functions ####

## the analysis is summarized in functions, a description of each functions is written along in the "functions.R" script
source(file.path("Scripts","Functions.R"))

#### Import Datasets ####

## lexical detail
## idp : NFI plot identifier
## Ser : Sylvoecoregions; forest ecoregions simplified to ecoregions in the main text
## Tmoy : MAT, mean annua temperature 

source(file.path("Scripts","Read_2005_2021_NFI.R"))

NFI_plot_info[,ident:=as.character(idp)]
NFI_plot_info[,ser:=as.character(ser)]

# we start by merging various datasets with the data.table 'NFI_plot_info' containing all NFI plots ID (idp) and
# basic informations such as coordinates 
NFI_plot_info<-merge(NFI_plot_info,NFI_climate,all.x=T,by="idp")
NFI_plot_info<-merge(NFI_plot_info,NFI_dendro,all.x=T,by="idp")
NFI_plot_info<-merge(NFI_plot_info,canopy_cover,all.x=T,by="idp")
NFI_plot_info<-merge(NFI_plot_info,precise_elevation,all.x=T,by="idp")

NFI_plot_info[,elevation_blurred:=elevation]
NFI_plot_info[,elevation:=ifelse(is.na(alti),elevation,alti)]
NFI_plot_info[,alti:=NULL]

## the NFI_ecologie allows us to know the date when the floristic survey was performed
NFI_plot_info<-merge(NFI_plot_info,NFI_ecologie,all.x=T,by="idp")
NFI_plot_info[,dateeco:=as.character(dateeco)]
NFI_plot_info[,dateeco:=ymd(dateeco)]
## we create a boolean variable: the survey was done during the vegetation season ?
NFI_plot_info[,survey_during_vege_season:=ifelse(month(dateeco)%in%c(4:9),1,0)]

ser_lsm<-fread(file.path("Data","SER_landscapemetrics.csv"))


## thermal, soil optimum data and habitat preferences

sp_indicator_value<-fread(file.path("Data","climplant_names_trait_V1.2.csv"))
colnames(sp_indicator_value)[colnames(sp_indicator_value)=="YearMeanMean"]<-"topt_climplant"


sp_indicator_value[,niche_breadth:=YearMean95-YearMean05]
sp_indicator_value[,niche_breadth_n:=(YearMean95-YearMean05)/topt_climplant]


climplant_full<-fread(file.path("Data","MeanTempYearClimPlantV1_2.csv"))
sp_tmp<-climplant_full$V1
climplant_full<-data.table(t(climplant_full[,2:ncol(climplant_full)]))
colnames(climplant_full)<-sp_tmp
melt_climplant_full<-melt(climplant_full)
colnames(melt_climplant_full)<-c("species_name","topt_climplant")
rm(sp_tmp)

NFI_flora_traits<-NFI_flora[idp%in%NFI_plot_info$idp & !is.na(species_name),]

NFI_flora_traits<-merge(NFI_flora_traits,sp_indicator_value[,c("lb_nom_final","topt_climplant","YearMeanMedian","YearMean95","YearMean05","YearMaxMean",  
                                                    "niche_breadth","niche_breadth_n",
                                                    "Area","indFor_Freq","indFor_Chytry","N_ellenberg","R_ellenberg","L_Ellenberg" ,
                                                    "vi_pH" ,"vi_CN", "azote","topt_picq" , "topt" , "pHopt" , "CNopt"  , "Nopt" , "STopt","Li")],by.x="species_name",by.y="lb_nom_final",all.x=T)

# aggregating the survey to have a Community Inferred Temperature at the plot level (community)
cit_climplant<-NFI_flora_traits[tree!=1 ,## removal of the tree species 
                            .(cit_climplant=mean(topt_climplant,na.rm=T),
                              median_climplant=mean(YearMeanMedian,na.rm=T),
                              cit_tmax_climplant=mean(YearMaxMean,na.rm=T),
                              cit_climplant_05=mean(YearMean05,na.rm=T),
                              cit_climplant_95=mean(YearMean95,na.rm=T),
                              cit_ecoplant=mean(topt,na.rm=T),
                              cit_ecoplant_picq=mean(topt_picq,na.rm=T),
                              n_sp_climplant=sum(!is.na(topt_climplant)),
                              mean_area=mean(Area,na.rm=T),
                              mean_niche_breadth=mean(niche_breadth,na.rm=T),
                              mean_niche_breadth_n=mean(niche_breadth_n,na.rm=T),
                              freq_for_mean=mean(indFor_Freq,na.rm=T),
                              indfor_Chytry=mean(indFor_Chytry,na.rm=T),
                              mean_azote=mean(azote,na.rm=T),
                              mean_N=mean(Nopt,na.rm=T),
                              mean_R=mean(R_ellenberg,na.rm=T),
                              mean_pH=mean(pHopt,na.rm=T),
                              mean_CN=mean(vi_CN,na.rm=T),
                              mean_L=mean(Li,na.rm=T)),
                            by=idp]


rm(NFI_flora_traits)

## include the bio indicated data, survey scale
NFI_plot_info<-merge(NFI_plot_info,cit_climplant,by="idp",all.x=T)
head(NFI_flora)


## set the time period
NFI_plot_info[,cycle:=ifelse(campaign%in%c(2005,2006,2007,2008,2009),"first",ifelse(campaign%in%c(2010,2011,2012,2013,2014),"first_half","second"))]
NFI_plot_info[,period:=ifelse(campaign%in%c(2005,2006,2007,2008,2009,2010,2011),"past",ifelse(campaign%in%c(2012,2013,2014),NA,"recent"))]


NFI_plot_info<-NFI_plot_info[!is.na(cit_climplant),]# some plots lacks any species with a thermal optimum
NFI_plot_info<-NFI_plot_info[gest_struct!="debois",]## the nfi classify deforested plots, we remove them
NFI_plot_info<-NFI_plot_info[n_sp_climplant>=5,]

NFI_plot_info_sf<-st_as_sf(NFI_plot_info,coords=c("xl","yl"),crs=st_crs(2154))


#### Geographical pairing and plot selection ####

col_of_interest<-c("campaign","elevation","cit_climplant","cit_ecoplant","cit_ecoplant_picq",
                   "tmoy_v2b_9015_13","mean_L","mean_pH","mean_pH","mean_N",
                   "canopy_cover","basal_area","Ntot","xl","yl")


NFI_plot_info_sf$idp<-as.numeric(NFI_plot_info_sf$idp)
NFI_plot_info_sf$xl<-st_coordinates(NFI_plot_info_sf)[,1]
NFI_plot_info_sf$yl<-st_coordinates(NFI_plot_info_sf)[,2]

clust_pairs<-makeCluster(3) # approx computation time with 3 cores = 6 minutes
registerDoParallel(clust_pairs)
pairs_ser<-foreach(ser_current=unique(NFI_plot_info$ser),.combine = rbind,.multicombine = T,.packages = c("data.table","foreach","sf"))%dopar%{
  
  # within each ecoregions we define the matrix of distance between each plots
  idp_in_ser<-NFI_plot_info[ser==ser_current,idp]
  print(ser_current)
  
  pair_distances<-get_distances(NFI_plot_info_sf[NFI_plot_info_sf$ser==ser_current,],NFI_plot_info_sf[NFI_plot_info_sf$ser==ser_current,]$period,"recent","past",5)
  pair_distances[,idp1:=as.numeric(idp1)]
  pair_distances[,idp2:=as.numeric(idp2)]
  
  # then we select pairs of plots optimally, we maximize the number of pairs with these contains:
  # the plots should be less than 1.9km apart, and have been surveyed 9 or 10 years appart 
  # the plots should not have an elevation difference of more than 100m
  
  pairs<-get_pair_optimal(NFI_plot_info_sf[,],pair_distances[idp1 %in%idp_in_ser & idp2 %in% idp_in_ser ,],distance_tresh=1.91,
                          differences_to_compute=col_of_interest,
                          differences_tresh_min = c(9,-100,rep(0,13)),
                          differences_tresh_max = c(11,100,rep(0,13)))
  pairs[,ser:=ser_current]
  pairs
}


stopCluster(clust_pairs)



pairs_ser[,pair_id_ser:=paste0(ser,"_",pair_id)]
pairs_ser[,greco:=str_extract(ser,"[:alpha:]")] # large ecoregions GRECO
summary(pairs_ser)

NFI_pairs_info<-NFI_plot_info[idp%in%c(pairs_ser$idp1,pairs_ser$idp2),]
NFI_pairs_info_sf<-st_as_sf(NFI_pairs_info,coords = c("xl","yl"),crs=st_crs(2154))


## we compute the mean variable of the NFI plots selected by the procedure to have 
## mean values for climate, canopy and dendrometric variable, topography, amount of survey done in the vegetation season
ser_variable_summary<-NFI_pairs_info[,.(n_plot=.N,n_pair=.N/2,
                                        #mean_pland=mean(c(pland_1000m.recent,pland_1000m.past),na.rm=T),
                                        mean_tmoy=mean(tmoy_v2b_9015_13,na.rm=T),
                                        q05_tmoy=quantile(tmoy_v2b_9015_13,0.05,na.rm = T),
                                        q95_tmoy=quantile(tmoy_v2b_9015_13,0.95,na.rm = T),
                                        mean_canopy_cover=mean(canopy_cover,na.rm=T),
                                        mean_basal_area=mean(basal_area,na.rm=T),
                                        mean_pp=mean(prec_v2b_9015_4_9,na.rm=T),
                                        mean_elevation=mean(elevation,na.rm=T),
                                        q05_elevation=quantile(elevation,0.05,na.rm = T),
                                        q95_elevation=quantile(elevation,0.95,na.rm = T),
                                        vege_season_past=mean(survey_during_vege_season[period=="past"]),
                                        vege_season_recent=mean(survey_during_vege_season[period=="recent"]),
                                        mean_delta_temp=mean(tmoy_v2b_9015_13-tmoy_v2b_6186_13,na.rm=T)),by=ser][order(ser),]

## we also include landscapemetrics computed at the ecoregion scale, such as forest cover, aggregation
ser_variable_summary<-merge(ser_variable_summary,ser_lsm,by.x="ser",by.y="codeser")
ser_variable_summary[,range_tmoy:=q95_tmoy-q05_tmoy]
ser_variable_summary[,range_alti:=q95_elevation-q05_elevation]
ser_variable_summary[,vege_season_dif:=vege_season_recent-vege_season_past]


head(ser_variable_summary[order(n_plot),]) # take a look at the ecoregions with the less pairs (n_plot/2)

# we remove two ecoregion with low numbers of plot pairs : J24 (4) and B44 (9)
# The F13 ecoregion didn't have a single eligible pair 
# We also removed the 3 ecoregion of corsica, K11, K12 and K13 to focus on metropolitan France and the continental ecoregions

ser_to_remove<-c("J24","B44","K11","K12","K13")

pairs_ser<-pairs_ser[!ser%in%ser_to_remove,]
ser_variable_summary<-ser_variable_summary[!ser%in%ser_to_remove,]
NFI_pairs_info<-NFI_pairs_info[!ser%in%ser_to_remove,]
NFI_pairs_info_sf<-st_as_sf(NFI_pairs_info,coords = c("xl","yl"),crs=st_crs(2154))


#### Floristic survey selection ####


NFI_flora_selected_plot<-NFI_flora[idp%in% c(pairs_ser$idp1,pairs_ser$idp2),]

flora_of_selected_plots<-NFI_flora_selected_plot[tree!=1,]## removal of tree species
flora_of_selected_plots_trees<-NFI_flora_selected_plot## no removal
flora_of_selected_plots_no_infl_sp<-NFI_flora_selected_plot[!species_name%in%c("Agrostis capillaris","Rubus ulmifolius","Brachypodium pinnatum"),]## removal of several influancial species

## creation of [site_ID , species] matrixes
table_flora<-create_table_sp(flora_of_selected_plots,"idp")
table_flora_trees<-create_table_sp(flora_of_selected_plots_trees,"idp")
table_flora_no_infl_species<-create_table_sp(flora_of_selected_plots_no_infl_sp,"idp")

## the taxonomic homogenization produced some surveys with the same species (with the new name) recorded twice as they were seperate species in a previous taxref version
## we remove these errors
table_flora[table_flora>1]<-1 ; table_flora_trees[table_flora_trees>1]<-1 ; table_flora_no_infl_species[table_flora_no_infl_species>1]<-1 

occ_past<-apply(table_flora[as.character(pairs_ser$idp2),],2,sum)
occ_recent<-apply(table_flora[as.character(pairs_ser$idp1),],2,sum)

remove_very_rare_species<-occ_past<5 | occ_recent<5

table_flora<-table_flora[,!remove_very_rare_species]
table_flora[table_flora==2]

#### Main analysis: Thermophilization and delta beta partition ####

## the computation of every variable of interest (from thermophilization, delta beta diversity and their component ) is done in this part
## we use the function get_contrib_one_ser() to perform the computations, it is described in the script "function.R"
## get_contrib_one_ser() returns a list of two objects :
### - a data.table of one row with the computed evolution of the variables, as well as their componenet for one ecoregions
### - a data.table with the table of pst and recent occurrences of every species used for the computation of thermophilization

## the computations us the foreach() loop structure, in order to parralelize the computations as well as binding the individual results of a loop more efficiently
## you can define the number of core you want to use to parrallelize with n_cores_to_use
## the loops are quite fast even without parrallel computing (you can replace %dopar% with %do%), however the later bootstrapped computations are much slower
## results of the bootstraps are provided in order to avoid running them again



## main analysis
list_contrib<-foreach(sertest=unique(pairs_ser$ser),
                       .combine = rbind_list_list,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr"))%do%{
                         res<-get_contrib_one_ser(sertest,table_flora,4,"topt_climplant")
                         cat(paste0(sertest," -> "))
                         return(res)}



contrib_therm_beta_ser<-list_contrib[[1]]


## we add the result the descriptive variable of the ecoregions
contrib_therm_beta_ser<-merge(contrib_therm_beta_ser,ser_variable_summary[,-"greco"],by="ser")

## list of the intermediary computation at the species and ecoregion scale
list_sp_contrib<-list_contrib[[2]]


## let's take a look at the ecoregion dataset
head(contrib_therm_beta_ser,10)

## let's take a look at the flora list
head(contrib_therm_beta_ser,10)

## supplementary analysis : with a different thermal optimum database Ecoplant Gégout et al, 2005  EcoPlant: A forest site database linking floristic data with soil and climate variables
list_contrib_ecoplant<-foreach(sertest=unique(pairs_ser$ser),
                                            .combine = rbind_list_list,.multicombine = T,.errorhandling = "remove")%do%{
                                              res<-get_contrib_one_ser(sertest,table_flora,4,"topt")
                                              cat(paste0(sertest," -> "))
                                              return(res)}
contrib_therm_beta_ser_ecoplant<-list_contrib_ecoplant[[1]]

## newer revised version of the EcoPlant database
list_contrib_ecoplant_picq<-foreach(sertest=unique(pairs_ser$ser),
                                                 .combine = rbind_list_list,.multicombine = T,.errorhandling = "remove")%do%{
                                                   res<-get_contrib_one_ser(sertest,table_flora,4,"topt_picq")
                                                   cat(paste0(sertest," -> "))
                                                   return(res)}

contrib_therm_beta_ser_ecoplant_picq<-list_contrib_ecoplant_picq[[1]]


#stopCluster(clust_contrib)


list_contrib_8_comp<-foreach(sertest=unique(pairs_ser$ser),
                             .combine = list,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr"))%do%{
                               res<-get_contrib_one_ser(sertest,table_flora,8,"topt_climplant")[[1]]
                               cat(paste0(sertest," -> "))
                               return(res)}

contrib_therm_beta_ser_8<-rbindlist(list_contrib_8_comp,fill=T)

# ecoregions don't display any rare species because of their low number of plots, we set the contributino to 0
contrib_therm_beta_ser_8[,contrib_topt_colder_col_rare:=ifelse(is.na(contrib_topt_colder_col_rare),0,contrib_topt_colder_col_rare)]
contrib_therm_beta_ser_8[,contrib_beta_colder_col_rare :=ifelse(is.na(contrib_beta_colder_col_rare ),0,contrib_beta_colder_col_rare )]
contrib_therm_beta_ser_8[,contrib_topt_warmer_col_rare :=ifelse(is.na(contrib_topt_warmer_col_rare ),0,contrib_topt_warmer_col_rare )]
contrib_therm_beta_ser_8[,contrib_beta_warmer_col_rare:=ifelse(is.na(contrib_beta_warmer_col_rare),0,contrib_beta_warmer_col_rare)]
contrib_therm_beta_ser_8[,contrib_beta_warmer_ext_rare:=ifelse(is.na(contrib_beta_warmer_ext_rare),0,contrib_beta_warmer_ext_rare)]
contrib_therm_beta_ser_8[,contrib_topt_warmer_ext_rare:=ifelse(is.na(contrib_topt_warmer_ext_rare),0,contrib_topt_warmer_ext_rare)]
contrib_therm_beta_ser_8[,contrib_beta_colder_col_rare:=ifelse(is.na(contrib_beta_colder_col_rare),0,contrib_beta_colder_col_rare)]
contrib_therm_beta_ser_8[,contrib_topt_colder_col_rare:=ifelse(is.na(contrib_topt_colder_col_rare),0,contrib_topt_colder_col_rare)]
contrib_therm_beta_ser_8[,contrib_topt_colder_ext_rare:=ifelse(is.na(contrib_topt_colder_ext_rare),0,contrib_topt_colder_ext_rare)]
contrib_therm_beta_ser_8[,contrib_beta_colder_ext_rare:=ifelse(is.na(contrib_beta_colder_ext_rare),0,contrib_beta_colder_ext_rare)]


## the following foreach loop are long computation (> 1 hours with 4 cores)
# their results are saved as .RData to rapidly go through the next parts of the script
bootstrap_random_topt<-readRDS(file.path("Saved_computation","bootstrap_random_topt.RData"))
bootstrap_same_n_occurrence<-readRDS(file.path("Saved_computation","bootstrap_same_n_occurrence.RData"))
bootstrap_random_topt_full<-readRDS(file.path("Saved_computation","bootstrap_random_topt_full.RData"))


## Parallel setup , you can shut down the cluster at anytime with stopCluster(clust_contrib)
n_cores_to_use<-floor(detectCores()*0.75)
clust_contrib<-makeCluster(n_cores_to_use)
registerDoParallel(clust_contrib)

## supplementary analysis : bootstrap with random thermal optima assigned to each species
bootstrap_random_topt<-foreach(sertest=unique(pairs_ser$ser),
                               .combine = rbind,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr","foreach"))%dopar%{
                                 res<-run_contrib_multiple_time(200,sertest,table_flora,8,"topt_climplant",random_topt=T,average = F)
                                 cat(paste0(sertest," -> "))
                                 return(res)}

stopCluster(clust_contrib)


## supplementary analysis : bootstrap with rarefied occurrences for the time span of the ecoregions with the most occurrences
clust_contrib<-makeCluster(n_cores_to_use)
registerDoParallel(clust_contrib)
bootstrap_same_n_occurrence<-foreach(sertest=unique(pairs_ser$ser),
                                    .combine = rbind,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr","foreach"))%dopar%{
                                      res<-run_contrib_multiple_time(200,sertest,table_flora,8,"topt_climplant",random_topt=F,rarefy=T)
                                      cat(paste0(sertest," -> "))
                                      return(res)}

stopCluster(clust_contrib)



#### Descriptive statistics  ####

## M & M 
# % of occ with a known thermal optimum
sum(flora_of_selected_plots[,species_name%in%sp_indicator_value[!is.na(topt_climplant),lb_nom_final]])/nrow(flora_of_selected_plots)

# N of pairs
nrow(NFI_pairs_info)/2

summary(contrib_therm_beta_ser$n_pair)

# N of occurrences
sum(list_sp_contrib$occurrence_past)
sum(list_sp_contrib$occurrence_recent)

# N of unique species wit ha known thermal optimum
length(unique(list_sp_contrib[,species_name]))

# Mean specific richness
sum(list_sp_contrib$occurrence_past)/(nrow(NFI_pairs_info)/2)
sum(list_sp_contrib$occurrence_recent)/(nrow(NFI_pairs_info)/2)

# number of recorded species whether the survey was done during the growing season or not
NFI_pairs_info[,sum(n_sp_climplant)/.N,by=.(period,survey_during_vege_season )]

# mean time difference between the two survey
pairs_ser[,mean(campaign_dif)]

# mean distance between the two survey
summary(pairs_ser$dist_pair)
summary(pairs_ser$xl_dif)
summary(pairs_ser$yl_dif)

# W. mean of the thermal optimum of the species
list_sp_contrib[,weighted.mean(topt_climplant,occurrence_past),]
list_sp_contrib[,weighted.mean(topt_climplant,occurrence_recent),]

# mean Beta-div
mean(contrib_therm_beta_ser[, Beta1])
sd(contrib_therm_beta_ser[, Beta1])

# Thermophilization descriptor 
mean(contrib_therm_beta_ser[, sp_thermo])
sd(contrib_therm_beta_ser[, sp_thermo])
sum(contrib_therm_beta_ser[, sp_thermo>0])

contrib_therm_beta_ser[,weighted.mean(sp_thermo,n_pair)]
contrib_therm_beta_ser[,weighted.mean(topt_ext,n_pair)]

contrib_therm_beta_ser[,weighted.mean(sp_delta_beta,n_pair)]
contrib_therm_beta_ser[,weighted.mean(beta_ext,n_pair)]
contrib_therm_beta_ser[,weighted.mean(beta_col,n_pair)]
contrib_therm_beta_ser[,weighted.mean(beta_ext,n_pair)]



mean(contrib_therm_beta_ser[, topt_ext])
sd(contrib_therm_beta_ser[, topt_ext])

# Homogenization descriptor 
mean(contrib_therm_beta_ser[, sp_delta_beta])
sd(contrib_therm_beta_ser[, sp_delta_beta])
sum(contrib_therm_beta_ser[, sp_delta_beta<0])

mean(contrib_therm_beta_ser[, beta_ext])
sd(contrib_therm_beta_ser[, beta_ext])

mean(contrib_therm_beta_ser[, beta_col])
sd(contrib_therm_beta_ser[, beta_col])

mean(contrib_therm_beta_ser[, contrib_beta_colder_ext])
sd(contrib_therm_beta_ser[, contrib_beta_colder_ext])

mean(contrib_therm_beta_ser[, contrib_beta_colder_col])
sd(contrib_therm_beta_ser[, contrib_beta_colder_col])


mean(contrib_therm_beta_ser[, Beta1])
sd(contrib_therm_beta_ser[, Beta1])

mean(contrib_therm_beta_ser[, Beta2])
sd(contrib_therm_beta_ser[, Beta2])

# proportion of surveys during the vege season for the two period
NFI_pairs_info[,mean(survey_during_vege_season),by=period]

# mean canopy cover of the two period
NFI_pairs_info[,mean(canopy_cover,na.rm=T),by=period]

# climate range of the ecoregions
ser_variable_summary[,greco:=str_extract(ser,"[:alpha:]")] # large ecoregions GRECO
ser_variable_summary[,ecoregion:= ifelse(greco%in% c("J"),"Medit",ifelse(greco%in% c("D","G","E","H","I"),"Mountain","Lowland")),]
ser_variable_summary[,.(MAT=range(mean_tmoy),Prec=range(mean_pp)),by=ecoregion]

##national scale homogenization 

summary(contrib_therm_beta_ser$n_pair)

large_ser<-foreach(grec=unique(contrib_therm_beta_ser$greco),.combine = rbind)%do%{
  print(grec)
  contrib_grec<-get_contrib_one_ser(contrib_therm_beta_ser[greco%in%grec,]$ser,table_flora,4,"topt_climplant")
  
return(contrib_grec[[1]])
  
  
}

summary(large_ser$Beta1)
summary(large_ser$Gamma1)


summary(contrib_therm_beta_ser$Beta1)
summary(contrib_therm_beta_ser$Gamma1)

#### Statistical tests #### 


test_signif_random<-function(variable_to_test,test,
                             data_true=contrib_therm_beta_ser,
                             data_sim=bootstrap_random_topt){
  obs<-data_true[,get(variable_to_test)]
  simu<-data_sim[,get(variable_to_test)]
  print(test(obs,simu))
  return(mean(obs)-mean(simu))
}

## difference between the thermophilization components and the null thermophilization model
# the other tests are performed during the creation of the main figure
test_signif_random("sp_thermo",wilcox.test,data_sim = bootstrap_random_topt)

## difference between the delta beta  components and 0
# the other tests are performed during the creation of the main figure
wilcox.test(contrib_therm_beta_ser$sp_delta_beta)

## difference between the delta beta components of the dataset and the delta beta components of the rarefaction model
test_signif_random("sp_delta_beta",wilcox.test,data_sim = bootstrap_same_n_occurrence)
test_signif_random("beta_col",wilcox.test,data_sim = bootstrap_same_n_occurrence)
test_signif_random("beta_ext",wilcox.test,data_sim = bootstrap_same_n_occurrence)
test_signif_random("contrib_beta_colder_ext",wilcox.test,data_sim = bootstrap_same_n_occurrence)
test_signif_random("contrib_beta_warmer_ext",wilcox.test,data_sim = bootstrap_same_n_occurrence)
test_signif_random("contrib_beta_colder_col",wilcox.test,data_sim = bootstrap_same_n_occurrence)
test_signif_random("contrib_beta_warmer_col",wilcox.test,data_sim = bootstrap_same_n_occurrence)

## area
sum(st_area(ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),]))
sum(st_area(ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),])*ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),]$p_forest   )

## linear models to asses the relationship between MAT and the components
# of delta beta
test_signif<-lm(beta_ext~mean_tmoy,data=contrib_therm_beta_ser)
test_signif<-lm(beta_col~mean_tmoy,data=contrib_therm_beta_ser)
test_signif<-lm(sp_delta_beta~mean_tmoy,data=contrib_therm_beta_ser)

# of thermophilization
test_signif<-lm(topt_ext~mean_tmoy,data=contrib_therm_beta_ser)
test_signif<-lm(topt_col~mean_tmoy,data=contrib_therm_beta_ser)
test_signif<-lm(sp_thermo~mean_tmoy,data=contrib_therm_beta_ser)

summary(test_signif)



test_signif<-lm(topt_ext~mean_tmoy+st_coordinates(coord_ser)[,1]+st_coordinates(coord_ser)[,2],data=contrib_therm_beta_ser)
test_signif<-lm(beta_col~mean_tmoy+st_coordinates(coord_ser)[,1]+st_coordinates(coord_ser)[,2],data=contrib_therm_beta_ser)


#### spatial autocorrelation

ggplot(contrib_therm_beta_ser,aes(x=mean_tmoy,y=residuals(test_signif)))+geom_point()+geom_smooth()

ggplot(contrib_therm_beta_ser,aes(x=st_coordinates(coord_ser)[,2],y=residuals(test_signif)))+geom_point()+geom_smooth()


ggplot(ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),],aes(fill=  residuals(test_signif)))+
  geom_sf()+
  scale_fill_gradient2()+
  theme_bw()


ggplot(ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),],aes(fill= inc.lag))+
  geom_sf()+
  scale_fill_gradient2(midpoint = 0,low="cadetblue",high="tomato")+
  theme_bw()

library(spdep)

nb <- poly2nb(ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),], queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)


lnb_beta<-foreach(nb=as.list(lw$neighbours),.combine = rbind)%do%{
  print(contrib_therm_beta_ser[nb,]$ser)
  contrib_grec<-get_contrib_one_ser(contrib_therm_beta_ser[nb,]$ser,table_flora,4,"topt_climplant")
  
  return(contrib_grec[[1]])
  
  
}

summary(large_ser$Beta1/large_ser$sp_tot_past)
summary(large_ser$Gamma1)


summary(contrib_therm_beta_ser$Beta1/large_ser$sp_tot_past)
summary(contrib_therm_beta_ser$Gamma1)




 moran(ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),]$topt_ext, lw, length(nb), Szero(lw))[1]

inc.lag <- lag.listw(lw, ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),]$topt_ext)

inc.lag


moran.test(ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),]$topt_ext,lw, alternative="greater")


moran.test(residuals(test_signif),lw, alternative="greater")



test_signif<-lm(beta_ext~mean_tmoy,data=contrib_therm_beta_ser)
moran.test(residuals(test_signif),lw, alternative="greater")
test_signif<-lm(beta_col~mean_tmoy,data=contrib_therm_beta_ser)
moran.test(residuals(test_signif),lw, alternative="greater")
test_signif<-lm(sp_delta_beta~mean_tmoy,data=contrib_therm_beta_ser)
moran.test(residuals(test_signif),lw, alternative="greater")
test_signif<-lm(topt_ext~mean_tmoy,data=contrib_therm_beta_ser)
moran.test(residuals(test_signif),lw, alternative="greater")
test_signif<-lm(topt_col~mean_tmoy,data=contrib_therm_beta_ser)
moran.test(residuals(test_signif),lw, alternative="greater")
test_signif<-lm(sp_thermo~mean_tmoy,data=contrib_therm_beta_ser)
moran.test(residuals(test_signif),lw, alternative="greater")

inc.lag <- lag.listw(lw, ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),]$topt_ext)
test_signif<-lm(topt_ext~mean_tmoy+inc.lag,data=contrib_therm_beta_ser)
summary(test_signif)

inc.lag <- lag.listw(lw, ser_value_sf_no_corsica[!is.na(ser_value_sf_no_corsica$sp_thermo),]$topt_ext)
test_signif<-lm(topt_ext~mean_tmoy+inc.lag,data=contrib_therm_beta_ser)
summary(test_signif)
#### Creation of tables for the extended and supplementary materials ####

## theses tables will contains the mean, standard deviation and 
## Wilcoxon test two sided  p-value with H0= mean =0
create_dt_result<-function(dt_partition_result,var_name=T){
  
  col_to_return<-c("sp_thermo","topt_ext","topt_col",
                   "contrib_topt_colder_ext", "contrib_topt_warmer_ext", "contrib_topt_colder_col" ,"contrib_topt_warmer_col",
                   "sp_delta_beta","beta_ext","beta_col",
                   "contrib_beta_colder_ext" ,"contrib_beta_warmer_ext", "contrib_beta_colder_col", "contrib_beta_warmer_col")
  
  sub_dt<-dt_partition_result[,..col_to_return]
  sub_dt<-melt(sub_dt, measure.vars=col_to_return)
  sub_dt<-sub_dt[,.(mean_value=signif(mean(value),3),
                    sd=signif(sd(value),2),
                    p_value_num=signif(wilcox.test(value)$p.value,3)),by=variable]
  
  sub_dt[,p_value:=as.character(p_value_num)]
  sub_dt[,p_value:=ifelse(p_value_num<0.0001,"<0.001",p_value)]
  sub_dt[,p_value_num:=NULL]
  
  if(var_name==F)sub_dt[,variable:=NULL]
  sub_dt
  return(sub_dt)
}

## table of the results wit hthe originial dataset and the null thermophilization model 
result_to_export<-cbind(create_dt_result(contrib_therm_beta_ser),create_dt_result(bootstrap_random_topt,F))

## table of the rarefaction null model and and the ClimPlant thermal optimum bootstrapping 
result_to_export_simulation<-cbind(create_dt_result(bootstrap_same_n_occurrence),create_dt_result(bootstrap_uncertain_topt,F))

## table of two other thermal optimum databases
result_to_export_different_topt<-cbind(create_dt_result(contrib_therm_beta_ser_ecoplant_picq),create_dt_result(contrib_therm_beta_ser_ecoplant,F))


write.table(result_to_export,file=file.path("Figures_results","sup_tab_results.csv"),sep=";",dec=",",row.names = F)
write.table(result_to_export_simulation,file=file.path("Figures_results","sup_tab_simulations.csv"),sep=";",dec=",",row.names = F)
write.table(result_to_export_different_topt,file=file.path("Figures_results","sup_tab_other_topt.csv"),sep=";",dec=",",row.names = F)

## Ecoregion mean table for Extended data
contrib_therm_beta_ser[,ecoregion:= ifelse(greco%in% c("J"),"Medit",ifelse(greco%in% c("D","G","E","H","I"),"Mountain","Lowland")),]

mean_round<-function(x,by=3)round(mean(x),by)

res_contrib_ecoreg<-contrib_therm_beta_ser[,.(
                          Thermophilization=mean_round(sp_thermo),
                          Extinction=mean_round(topt_ext),
                          Colonization=mean_round(topt_col),
                          sp_delta_beta=mean_round(sp_delta_beta),
                          Extinction=mean_round(beta_ext),
                          Colonization=mean_round(beta_col),
                          Ecoreg_n=.N,
                          Pair_n=sum(n_pair)),by=.(ecoregion)]

write.table(res_contrib_ecoreg,file=file.path("Figures_results","Ecoregion_scale_component.csv"),sep=";",dec=",",row.names = F)

#### Figure 1: theoretical background ####

example_dt<-data.table(topt=c(6,7,8,9,10,11),
                       occ=c(2,2,5,5,3,1),
                       occ_recent=c(0,3,3,6,2,3),
                       relative_topt=c(rep("Colder species",3),rep("Warmer species",3)))

example_dt<-data.table(topt=c(7,8,9,10),
                       occ=c(1,4,3,2),
                       occ_recent=c(0,4,4,2),
                       relative_topt=c(rep("Colder species",2),rep("Warmer species",2)))



change_in_occ<-example_dt$occ_recent-example_dt$occ

example_dt_colo<-data.table(topt=example_dt$topt[change_in_occ>0],
                       occ=change_in_occ[change_in_occ>0],
                       relative_topt=c("1col Warmer species"))
example_dt_colo<-rbind(example_dt,example_dt_colo,fill=T)

example_dt_ext<-data.table(topt=example_dt$topt[change_in_occ<0],
                            occ=abs(change_in_occ[change_in_occ<0]),
                            relative_topt=c("1ext Colder species"))
example_dt_colo<-rbind(example_dt_colo,example_dt_ext,fill=T)
example_dt_colo[,occ_ext:=occ]
example_dt_colo[1,occ_ext:= occ_ext -1]


colorRampPalette(c("cadetblue","lightcyan4"))(3)[2]
colorRampPalette(c("tomato","indianred3"))(3)[2]

hist_past<-ggplot(example_dt,aes(x=topt,y=occ,fill=relative_topt))+
  theme_classic2()+
  geom_col(width=0.55,color="grey5",linewidth=0.75,alpha=0.6,show.legend = F)+
  scale_y_continuous(expand = c(0.02,0.02),limits = c(0,5))+ # ,sec.axis = sec_axis(name="Effect on \U0394\U03B2-diversity",trans = function(x)-x+1)
  labs(y="Species occurrences",x="Thermal optimum")+
  scale_x_continuous(breaks = c(1:11))+
  geom_hline(yintercept = 1.2,lty=2,linewidth=0.75,color="grey25")+
  geom_vline(xintercept = 8.5,lty=2,linewidth=0.75)+
  scale_fill_manual(values=c("#6C9495","#E65C4E"))+
  theme(plot.margin = margin(l=6,r=9,t=3,b=3,unit="mm"))+
  ggtitle("Past period", "(2005 - 2011)")



hist_recent<-ggplot(example_dt,aes(x=topt,y=occ_recent,fill=relative_topt))+
  theme_classic2()+
  geom_col(width=0.55,color="grey5",linewidth=0.75,alpha=0.6,show.legend = F)+
  scale_y_continuous(expand = c(0.02,0.02),limits = c(0,5))+
  labs(y="  ",x="Thermal optimum")+
  scale_x_continuous(breaks = c(1:11))+
  geom_hline(yintercept = 1.2,lty=2,linewidth=0.75,color="grey25")+
  geom_vline(xintercept = 8.4,lty=2,linewidth=0.75)+
  scale_fill_manual(values=c("#6C9495","#E65C4E"))+
  theme(plot.margin = margin(l=6,r=9,t=3,b=3,unit="mm"))+
  ggtitle("Recent period", "(2015 - 2021)")


hist_colo<-ggplot(example_dt_colo,aes(x=topt,y=occ_ext,fill=relative_topt))+
  theme_classic2()+
  geom_col(width=0.55,color="grey5",linewidth=0.75,alpha=0.6,show.legend = F)+
  scale_y_continuous(expand = c(0.02,0.02),limits = c(0,5))+
  labs(y="  ",x="Thermal optimum")+
  scale_x_continuous(breaks = c(1:11))+
  geom_hline(yintercept = 1.2,lty=2,linewidth=0.75,color="grey25")+
  geom_vline(xintercept = 8.4,lty=2,linewidth=0.75)+
  scale_fill_manual(values=c("peru","salmon4","#6C9495","#E65C4E"))+
  theme(plot.margin = margin(l=6,r=9,t=3,b=3,unit="mm"))+
  ggtitle("Extinction and colonization","processes")


arrange_hists<-ggarrange(plotlist = list(hist_past+theme(axis.text=element_text(size=14)),
                                         hist_colo+theme(axis.text=element_text(size=14)),
                                         hist_recent+theme(axis.text=element_text(size=14)),
                                         ggplot()+theme_void(),ggplot()+theme_void(),ggplot()+theme_void()),
                         labels = c("a)","b)","c)","  ","  ","  "))

arrange_hists

ggsave(file.path("Figures_results","fig_1.svg"),arrange_hists,scale=2,width = 45*3,height = 45*2,units = "mm",)


#### Figure 2: histogram of occurrences####

### review 1

list_sp_contrib[sp_relative_occ=="col",weighted.mean(topt_climplant,abs(delta_occ))]
list_sp_contrib[sp_relative_occ=="ext",weighted.mean(topt_climplant,abs(delta_occ))]

summed_sp_occ<-list_sp_contrib[,.(delta_occ=sum(delta_occ),
                                  past_occ=sum(occurrence_past),
                                  recent_occ=sum(occurrence_recent),
                                  occ=sum(occurrence_total),
                                  topt_climplant=unique(topt_climplant)),by=species_name]
summed_sp_occ<-summed_sp_occ[delta_occ!=0,]

summed_sp_occ[,sp_relative_occ:=ifelse(delta_occ >0,"col","ext")]

summed_sp_occ[,sum(delta_occ),by=sp_relative_occ]

label_fig_2<-c(paste0("Gain in occurrences\nmean thermal opt. :",signif(summed_sp_occ[sp_relative_occ=="col",weighted.mean(topt_climplant,abs(delta_occ))],3) ,"°C"),
               paste0("Decline in occurrences\nmean thermal opt. :",signif(summed_sp_occ[sp_relative_occ=="ext",weighted.mean(topt_climplant,abs(delta_occ))],3) ,"°C" ))


out_fig_2<-ggplot(summed_sp_occ[sp_relative_occ!="stable",],aes(x=topt_climplant,weight=abs(delta_occ),fill=sp_relative_occ,lty=sp_relative_occ))+
  geom_histogram(alpha=0.5,position = "identity",breaks=seq(1,16,by=1),color="grey20")+
  scale_fill_manual(breaks = c("col","ext"),values=c("lightcyan4","peru"),label =label_fig_2)+
  scale_linetype_manual(breaks = c("col","ext"),values=c(2,1),label = label_fig_2)+
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16))+
  #facet_wrap(~greco)+
  labs(y="Absolute occurrence change",x="Species thermal optimum",fill="Dynamic",linetype="Dynamic")+
  theme_bw(base_size = 12)+
  theme(legend.background = element_blank(),legend.position = c(0.8,0.65),legend.spacing.y = unit(0.2, 'cm'))  +
  ## important additional element
  guides(fill = guide_legend(byrow = TRUE))


showtext:: showtext_auto(enable = F)
ggsave(file.path("Figures_results","fig_hist_main_reworked.png"),out_fig_2,dpi=400,scale = 1.8,width = 80,height = 70,units = "mm",)


summed_sp_occ<-list_sp_contrib[,.(delta_occ=sum(delta_occ),
                                  past_occ=sum(occurrence_past),
                                  recent_occ=sum(occurrence_recent),
                                  occ=sum(occurrence_total),
                                  topt_climplant=unique(topt_climplant)),by=species_name]

summed_sp_occ[,n_delta_occ:=recent_occ/occ]


occ_thermal_niche_plot<-ggplot(summed_sp_occ,aes(x=topt_climplant,y=(occ)))+ 
  geom_point(size=0.9)+
  scale_y_continuous(trans = "log",breaks = c(0,1,10,100,1000,10000))+
  geom_smooth(method='lm')+
  coord_cartesian(ylim = c(0.9,20000))+
  labs(x="Species thermal optimum",y="Occurrence (log scale)")+
  theme_bw()


list_sp_contrib[,n_delta_occ:=occurrence_recent/occurrence_total]
list_sp_contrib[,sptotser:=sum(occurrence_past),by=ser]
list_sp_contrib[,rela_occ:=occurrence_past/sptotser]


  occ_baseline_occ<-ggplot(summed_sp_occ[],aes(x=(occ),y=(n_delta_occ)))+ 
   scale_x_continuous(trans = "log",breaks = c(0,1,10,100,1000,10000))+
    geom_hline(lty=2,color="grey50",yintercept = 0.5)+
    geom_smooth(method='lm',alpha=0.5)+
    geom_point(size=0.5)+
    labs(x="Occurrences (log scale)",y="Proportion of occurrences\n in the past period (%)")+
    theme_bw()

  
    occ_baseline_occ

ggsave(file.path("Figures_results","suppl_figure_occ.png"),ggarrange(occ_thermal_niche_plot,occ_baseline_occ,align="hv",nrow=2,labels=c("a)","b)")),dpi=300,scale = 1,width = 180,height = 180,units = "mm",)


#### Figure 3: main partition figure ####

##this data.table contains the label used for the different variable, as well as their corresponding color scheme
dt_label_color_reference<-data.table(variable=c("sp_thermo","sp_delta_beta",c("topt_ext","topt_col"),c("contrib_topt_colder_ext","contrib_topt_warmer_ext" ,"contrib_topt_colder_col" ,"contrib_topt_warmer_col"),
                                            c("beta_ext","beta_col"),c("contrib_beta_colder_ext","contrib_beta_warmer_ext" ,"contrib_beta_colder_col" ,"contrib_beta_warmer_col")),
                                 color=c("gold3","olivedrab",c("salmon4","peru"),c("lightcyan4","indianred3","cadetblue","tomato"),c("salmon4","peru"),c("lightcyan4","indianred3","cadetblue","tomato")),
                                 label=c("Thermophilization","\U0394\U03B2-diversity","Extinction","Colonization","Cold-adapted species","Warm-adapted species","Cold-adapted species","Warm-adapted species",
                                         "Extinction","Colonization","Cold-adapted species","Warm-adapted species","Cold-adapted species","Warm-adapted species"))


color_ref_main_fig<-data.table(breaks=c("sp_thermo","sp_delta_beta","ext","col",
                                        "contrib_colder_ext","contrib_warmer_ext","contrib_colder_col","contrib_warmer_col"),
                               color=dt_label_color_reference$color[1:8],
                               #label=c("Variable","Variable","Extinction","Colonization","Ext. Cold-adapted","Ext. Warm-adapted","Col. Cold-adapted","Col. Warm-adapted"))
label=c("Total","Total","Extinction","Colonization","Extinction\n Cold-adapted","Extinction\n Warm-adapted","Colonization\n Cold-adapted","Colonization\n Warm-adapted"))

thermo_var<-c("sp_thermo","topt_ext","topt_col","contrib_topt_colder_ext" ,"contrib_topt_warmer_ext"  ,"contrib_topt_colder_col"  ,"contrib_topt_warmer_col"  )


contrib_therm_beta_ser_melt<-melt(contrib_therm_beta_ser,measure.vars=colnames(contrib_therm_beta_ser)[c(2:9,14:17,33,34)])
contrib_therm_beta_ser_melt[,variable:=as.character(variable)]
contrib_therm_beta_ser_melt[,type:=ifelse(grepl("beta",variable),"2_beta","1_thermo")]
contrib_therm_beta_ser_melt[,variable_2:=str_remove_all(variable,"topt_")]
contrib_therm_beta_ser_melt[,variable_2:=str_remove_all(variable_2,"beta_")]
contrib_therm_beta_ser_melt[,variable_3:=ifelse(variable_2%in% c("sp_thermo","sp_delta_beta"),"Total",variable_2)]



bootstrap_random_topt_melt<-melt(bootstrap_random_topt,measure.vars=colnames(contrib_therm_beta_ser)[c(2:9,14:17,33,34)])
bootstrap_random_topt_melt[,value_random_topt:=value]
bootstrap_random_topt_melt[variable%in%thermo_var,value_random_topt:=value_random_topt]

contrib_therm_beta_ser_melt<-cbind(contrib_therm_beta_ser_melt,bootstrap_random_topt_melt[,"value_random_topt"])

table_test<-contrib_therm_beta_ser_melt[,.(mean_val=mean(value),
                                           mean_val_random=mean(value_random_topt),
                                           p_val=ifelse(!variable%in%thermo_var,
                                                        wilcox.test(value)$p.value,
                                                        wilcox.test(value,(value_random_topt))$p.value )),by=variable]



label_facets<-c(`1_thermo`="Thermophilization",`2_beta`="\U0394\U03B2-diversity")
#label_facets<-c(`1_thermo`="- - - - - 0 - - - - -0 - - - - - - 0 - - - - - 0 - - - - -",`2_beta`="- - - - - 0 - - - - -0 - - - - - - 0 - - - - - 0 - - - - -")



table_test[,signif_star:="n.s."]
table_test[p_val < 0.05,"signif_star"] <- "*"
table_test[p_val < 0.01,"signif_star"] <- "**"
table_test[p_val < 0.001,"signif_star"] <- "***"

table_test[,type:=ifelse(grepl("beta",variable),"2_beta","1_thermo")]
table_test[,label:=paste0("µ = ",round(mean_val*ifelse(type=="1_thermo",10,1),2)," ",signif_star)]
table_test[,variable_2:=str_remove_all(variable,"topt_")]
table_test[,variable_2:=str_remove_all(variable_2,"beta_")]
table_test[,variable_3:=ifelse(variable_2%in% c("sp_thermo","sp_delta_beta"),"Total",variable_2)]
table_test[,x:=ifelse(type=="1_thermo",-0.45,1.93)]
table_test[variable%in%c("contrib_topt_colder_col","contrib_topt_warmer_ext"),x:=0.19]
table_test[variable%in%c("beta_col","contrib_beta_warmer_col","contrib_beta_colder_col"),x:=-2.5]

contrib_therm_beta_ser_melt

contrib_therm_beta_ser[,ecoregion:= ifelse(greco%in% c("J"),"Medit",ifelse(greco%in% c("D","G","E","H","I"),"Mountain","Lowland")),]


plot_thermo<-ggplot(contrib_therm_beta_ser_melt[type=="1_thermo"],aes(x=value*10,y=variable_3,fill=variable_2))+
  geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable_2),show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
  geom_text(data=table_test[type=="1_thermo",],aes(x=x,y=variable_3,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(4.5,6.5),lwd=0.5)+
  geom_point(data=table_test[type=="1_thermo",],aes(x= mean_val_random, y= variable_3),pch=21,fill="white",color="grey20",size=3)+
  scale_fill_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_color_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_y_discrete(limits=c("Total",color_ref_main_fig$breaks[-c(1,2)])[7:1] ,label= color_ref_main_fig$label[8:2] )+
  theme(strip.text.x = element_text(size = 10,face="bold"),plot.margin = margin(r=0,l=4),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#EAE397",linewidth = 0.5))+
  labs(x="Thermophilization °C/ 10years",y="Contributions to the variable")
  
  
plot_beta<-ggplot(contrib_therm_beta_ser_melt[type=="2_beta"],aes(x=value,y=variable_3,fill=variable_2))+
  geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable_2),show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
  geom_text(data=table_test[type=="2_beta",],aes(x=x,y=variable_3,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(4.5,6.5),lwd=0.5)+
  coord_cartesian(xlim=c(-3,4))+
  scale_fill_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_color_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_y_discrete(limits=c("Total",color_ref_main_fig$breaks[-c(1,2)])[7:1] ,label= rep("",7) )+
  theme(strip.text.x = element_text(size = 10,face="bold"),axis.ticks = element_blank(),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#C5D49A",linewidth = 0.5))+
  labs(x="\U0394\U03B2-diversity",y=" ")


out_main_fig<-ggarrange(plotlist = list(plot_thermo,plot_beta+theme(plot.background = element_rect(fill=NA,color=NA))),labels = c("a)","b)"),align = "h",widths = c(1.25,1))



showtext:: showtext_auto(enable = TRUE)
ggsave(file.path("Figures_results","main_figure_reworked.pdf"),out_main_fig,width =180,height =120,units = "mm",scale=0.95)
showtext:: showtext_auto(enable = FALSE) 
ggsave(file.path("Figures_results","main_figure_reworked.jpg"),out_main_fig,width =180,height =120,units = "mm",scale=0.95,dpi=450)
showtext:: showtext_auto(enable = TRUE)


### extended figure of the random topt boot strap

bootstrap_random_topt_full<-readRDS(file.path("Saved_computation","bootstrap_random_topt_full.RData"))

label_hist<-c("Thermophilization","Extinction","Colonization","Ext. Cold-adapted","Ext. Warm-adapted","Col. Cold-adapted","Col. Warm-adapted")

names(label_hist)<-dt_label_color_reference$variable[c(1,3:8)]

vertical_mean<-contrib_therm_beta_ser_melt[variable%in% thermo_var,.(true_mean=mean(value*10)),by=variable]

bootstrap_random_topt_full_melt<-melt(bootstrap_random_topt_full,measure.vars=colnames(contrib_therm_beta_ser)[c(2:9,14:17,33,34)])

extended_fig_random_topt<-ggplot(bootstrap_random_topt_full_melt[variable%in% thermo_var,],aes(x=value*10,fill=variable))+
  theme_bw()+
  geom_histogram(breaks=seq(-1,1,0.025),color="grey40",mapping=aes(weight=1),alpha=0.5,show.legend = F)+
  geom_vline(xintercept=0,lty=2,color="grey60")+
  geom_vline(mapping = aes(xintercept=true_mean,color="Mean observed value"),vertical_mean)+
  facet_wrap(~factor(variable, levels=thermo_var[c(2,4,5,3,6,7,1)]),labeller = as_labeller(label_hist),ncol=3,dir = "v")+
  scale_fill_manual(values = dt_label_color_reference$color,breaks = dt_label_color_reference$variable)+
  scale_color_manual(values="darkred")+
  theme(legend.position = c(0.8,0.55))+
  coord_cartesian(xlim=c(-0.5,0.5))+
  labs(y="Simulated ecoregion count",x="Thermophilization °C/ 10years",colour="")

extended_fig_random_topt

showtext:: showtext_auto(enable = FALSE)
ggsave(file.path("Figures_results","Extended_figure_random_topt.jpg"),extended_fig_random_topt,width =180,height =120,units = "mm",scale=1,dpi=400)
ggsave(file.path("Figures_results","Extended_figure_random_topt.pdf"),extended_fig_random_topt,width =180,height =120,units = "mm",scale=1)


## extended figure ecoregions 

contrib_therm_beta_ser_melt_ecoreg<-contrib_therm_beta_ser_melt[variable %in%c("sp_thermo","sp_delta_beta","beta_ext","beta_col","topt_ext","topt_col"),]

contrib_therm_beta_ser_melt_ecoreg[,ecoregion:=as.factor(ecoregion)]
levels(contrib_therm_beta_ser_melt_ecoreg$ecoregion)<-c("3_Lowland","1_Medit","2_mountain")
contrib_therm_beta_ser_melt_ecoreg<-contrib_therm_beta_ser_melt_ecoreg[order(ecoregion),]


label_ecoreg<-data.table(value=0.04,variable_3=c("Total","Total","Total"),variable_2=c("Total","Total","Total"),ecoregion= c("3_Lowland","1_Medit","2_mountain"),label=c("Lowland","Mediterranean","Mountain"))



plot_thermo<-ggplot(contrib_therm_beta_ser_melt_ecoreg[type=="1_thermo"],
                    aes(x=value*10,y=variable_3,fill=variable_2,group=paste0(ecoregion,variable_2)))+
  #geom_text(data=label_ecoreg,aes(label=label),position=position_jitterdodge(jitter.width = 0.1),size=2)+
  annotate("text",y=c(3.33,2.93,3.13),x=0.39,label=c("Lowland","Mediterranean","Mountain"),size=2.5)+
  geom_point(position=position_jitterdodge(jitter.width = 0.1),mapping=aes(color=variable_2),shape=16,alpha=0.6,show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
  #geom_text(data=table_test[type=="1_thermo",],aes(x=x,y=variable_3,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(2.5),lwd=0.5)+
  
  #geom_point(data=table_test[type=="1_thermo",],aes(x= mean_val_random, y= variable_3),pch=21,fill="white",color="grey20",size=3)+
  scale_fill_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_color_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_y_discrete(limits=c("Total",color_ref_main_fig$breaks[-c(1,2)])[3:1] ,label= color_ref_main_fig$label[4:2] )+
  theme(strip.text.x = element_text(size = 10,face="bold"),plot.margin = margin(r=0,l=4),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#EAE397",linewidth = 0.5))+
  labs(x="Thermophilization °C/ 10years",y="Contributions to the variable")


plot_beta<-ggplot(contrib_therm_beta_ser_melt_ecoreg[type=="2_beta"],
                  aes(x=value,y=variable_3,fill=variable_2,group=paste0(ecoregion,variable_2)))+
  geom_point(position=position_jitterdodge(jitter.width = 0.1),mapping=aes(color=variable_2),shape=16,alpha=0.6,show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
 # geom_text(data=table_test[type=="2_beta",],aes(x=x,y=variable_3,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(2.5),lwd=0.5)+
  coord_cartesian(xlim=c(-3,4))+
  scale_fill_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_color_manual(values = color_ref_main_fig$color,breaks = color_ref_main_fig$breaks)+
  scale_y_discrete(limits=c("Total",color_ref_main_fig$breaks[-c(1,2)])[3:1] ,label= rep("",3) )+
  theme(strip.text.x = element_text(size = 10,face="bold"),axis.ticks = element_blank(),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#C5D49A",linewidth = 0.5))+
  labs(x="\U0394\U03B2-diversity",y=" ")



out_ext_fig_ecoreg<-ggarrange(plotlist = list(plot_thermo,plot_beta+theme(plot.background = element_rect(fill=NA,color=NA))),labels = c("a)","b)"),align = "h",widths = c(1.25,1))



# showtext:: showtext_auto(enable = TRUE)
# ggsave(file.path("Figures_results","main_figure_reworked.pdf"),out_ext_fig_ecoreg,width =180,height =120,units = "mm",scale=0.95)
showtext:: showtext_auto(enable = FALSE) 
ggsave(file.path("Figures_results","extended_ecoreg_figure.jpg"),out_ext_fig_ecoreg,width =180,height =100,units = "mm",scale=0.95,dpi=450)
showtext:: showtext_auto(enable = TRUE)


#### Figure 4: Relationship with MAT and maps ####

## Processing the shapefile maps
SER_sf<-aggregate(SER_sf,by=list(SER_sf$codeser),FUN=unique)
SER_sf<-SER_sf[SER_sf$codeser!="-1",]
SER_sf$ser<-SER_sf$codeser ; SER_sf$codeser<-NULL ; SER_sf$NomSER<-NULL

ser_value_sf<-merge(SER_sf,contrib_therm_beta_ser,by.x="ser",by.y="ser",all.x=T)
ser_value_sf$sampling_intensity<-ser_value_sf$n_plot/ser_value_sf$ser_area

ser_value_sf_no_corsica<-ser_value_sf[!ser_value_sf$ser%in%c("K11","K12","K13"),]
france_bound_no_corsica<-st_as_sf(st_union(ser_value_sf_no_corsica))

## small function that produces a map of the desired component
make_inset_map<-function(var,lims_col){
  inset_plot<-ggplot(ser_value_sf_no_corsica)+geom_sf(aes(fill=get(var)),color=NA,linewidth=0.1,show.legend = F)+
    scale_fill_gradient2(low = if(var%in%c("sp_thermo","topt_ext","topt_col")) "tan4" else "tan4",mid="grey95",high=if(var%in%c("sp_thermo","topt_ext","topt_col")) "gold2" else "olivedrab",midpoint=0,limits=lims_col*c(-1,1),oob=scales::squish)+ 
    geom_sf(data=france_bound_no_corsica,fill="white",color="grey40",size=0.5,alpha=0.3)+
    theme_void()
  
}

ins_beta<-make_inset_map("sp_delta_beta",2.5)
ins_beta_col<-make_inset_map("beta_col",2.5)
ins_beta_ext<-make_inset_map("beta_ext",2.5)
ins_thermo<-make_inset_map("sp_thermo",0.02)
ins_thermo_col<-make_inset_map("topt_col",0.02)
ins_thermo_ext<-make_inset_map("topt_ext",0.02)


label<-dt_label_color_reference$label
names(label)<-dt_label_color_reference$variable
## exception to order the facet right, weird behaviour of facet_wrap



##this function creates the MAT and component relationship, with colors corresponding to the inset maps
make_main_plot<-function(var=c("sp_delta_beta","beta_ext","beta_col"),lims_y=c(-4,4.75),lims_col=c(2.5)){
  data_melt<-melt(contrib_therm_beta_ser,measure.vars = var,variable.factor=F)
  
  ## reordering the facets of facet_wrap()
  if("sp_thermo"%in% var)data_melt[,variable:=factor(variable,levels=c("sp_thermo","topt_ext","topt_col"))]
  if("sp_thermo"%in% var)data_melt[,value:=value*10]
  if("sp_delta_beta"%in% var)data_melt[,variable:=factor(variable,levels=c("sp_delta_beta","beta_ext","beta_col"))]
  
  ## this lm allows to plot a response curve
  small_lm<-lm(value~mean_tmoy*variable,data =data_melt )
  dummy_pred<-data.table(mean_tmoy=rep(seq(min(data_melt$mean_tmoy),max(data_melt$mean_tmoy),length.out=50),3),variable=rep(unique(data_melt$variable),each=50))
  dummy_pred[,value:=predict(small_lm,newdata=dummy_pred)]
  
  ## this lm is the one used to display the statistical tests and description (equation, p value, r squared)
  dt_label_lm<-foreach(i = var,.combine = rbind)%do%{
    lm_test<-lm(value~mean_tmoy,data =data_melt[variable==i,] )
    r2<-signif(summary(lm_test)$r.squared,2)
    pval<-summary(lm_test)$coefficients[2,4]
    a<-signif(summary(lm_test)$coefficients[1,1],2)
    b<-signif(summary(lm_test)$coefficients[2,1],2)
    labs<-paste0("Y = ",a," + ",b," * MAT \nR² = ",r2,ifelse(pval<0.05,"***","   ")," ")
    data.table(variable=i,label=labs)
  }
  if("sp_thermo"%in% var)dt_label_lm[,variable:=factor(variable,levels=c("sp_thermo","topt_ext","topt_col"))]
  if("sp_delta_beta"%in% var)dt_label_lm[,variable:=factor(variable,levels=c("sp_delta_beta","beta_ext","beta_col"))]
  
  
  main_plot<-ggplot(data_melt,aes(x=mean_tmoy,y=value,fill=value))+
    theme_bw()+
    geom_smooth(method='lm',color=NA,fill="grey80")+
    geom_line(data=dummy_pred,color="grey60",lwd=1.7)+
    geom_line(data=dummy_pred,aes(color=value),lwd=1.5)+
    geom_point(size=1.5,shape=21,color="grey60")+
    geom_hline(yintercept = 0,lty=2)+
    geom_text(aes(x=Inf,y=Inf,label=label),data=dt_label_lm,inherit.aes = F,hjust   = 1,vjust   = 1.2,size=2.5)+
    scale_colour_gradient2(aesthetics = c("colour","fill"),
                           low = if(var[1]=="sp_thermo") "tan4" else "tan4",
                           mid="grey95",
                           high=if(var[1]=="sp_thermo") "gold2" else "olivedrab",
                           midpoint=0,limits=lims_col*c(-1,1),oob=scales::squish)+ 
    scale_y_continuous(limits =lims_y )+
    facet_wrap(~variable,labeller = labeller(variable=label))+
    theme(strip.background = element_rect(fill=if(var[1]=="sp_thermo")"#EAE397" else "#C5D49A") )#+scale_y_continuous(labels =function(x)paste0(x,"       - ") )
  
  
  main_plot+ theme(strip.text=element_text(face="bold"),legend.position ="none",plot.margin = margin(4,4,4,1))
  
}


## arrange the MAT plots
part1<-make_main_plot(c("sp_thermo","topt_ext","topt_col"),lims_y=c(-0.25,0.65),lims_col =c(0.22) )
part1<- part1+labs(x="",y="Thermophilization °C/10yr")

part2<-make_main_plot(lims_y=c(-3.1,6.5))
part2<-part2+labs(x="Mean annual temperature (MAT) °C",y="\U0394\U03B2-diversity")

env_plot_final<-ggarrange(part1,part2,nrow=2,align = "hv",labels = c("a)","b)"))

## Add the small maps
# base 1 x =0.05
fact<-0.308
size_map<-0.15
env_plot_final<-ggdraw() +draw_plot(env_plot_final) +
  draw_plot(ins_thermo, x = 0.06, y = 0.79, width = size_map, height = size_map)+ # thermo map
  draw_plot(ins_thermo_ext, x = 0.06+fact, y = 0.79, width = size_map, height = size_map)+# thermo map
  draw_plot(ins_thermo_col, x = 0.06+fact*2, y = 0.79, width = size_map, height = size_map)+# thermo map
  draw_plot(ins_beta, x = 0.06, y = 0.29, width = size_map, height = size_map)+ # beta map
  draw_plot(ins_beta_ext, x = 0.06+fact, y = 0.29, width = size_map, height = size_map)+# beta map
  draw_plot(ins_beta_col, x = 0.06+fact*2, y = 0.29, width = size_map, height = size_map)# beta map



ggsave(file.path("Figures_results","fig_env.pdf"),env_plot_final,width=180,height = 125.2,units = "mm")
showtext:: showtext_auto(enable = FALSE)
ggsave(file.path("Figures_results","fig_env.png"),env_plot_final,width=180,height = 125.2,units = "mm",dpi=600)
showtext:: showtext_auto(enable = TRUE)
rm(ins_beta,ins_beta_col,ins_beta_ext,ins_thermo,ins_thermo_col,ins_thermo_ext,env_plot_final)

#### Extended figures: 8 components figure ####

## we append new labels to the data_table that references color and labels

name_8_comp_common<-c(grep("common",colnames(contrib_therm_beta_ser_8),value=T))
name_8_comp_rare<-c(grep("rare",colnames(contrib_therm_beta_ser_8),value=T))

name_8_comp_topt<-c(grep("topt",name_8_comp_common,value=T),grep("topt",name_8_comp_rare,value=T))
name_8_comp_beta<-c(grep("beta",name_8_comp_common,value=T),grep("beta",name_8_comp_rare,value=T))

name_8_comp_topt<-name_8_comp_topt[c(5,1,2,6,3,7,4,8)]
name_8_comp_beta<-name_8_comp_beta[c(5,1,2,6,3,7,4,8)]


labs_8<-c("Col. rare\ncold-adapted","Col. common\ncold-adapted","Ext. common\ncold-adapted","Ext. rare\ncold-adapted",
          "Col. common\nwarm-adapted","Col. rare\nwarm-adapted","Ext. common\nwarm-adapted","Ext. rare\nwarm-adapted")


dt_color_8<-data.table(variable=name_8_comp_beta,
                       color=c("cadetblue","cadetblue","lightcyan4","lightcyan4",
                                          "tomato","tomato","indianred3","indianred3"),
                       label=labs_8)

dt_color_8_topt<-data.table(variable=name_8_comp_topt,
                            color=c("cadetblue","cadetblue","lightcyan4","lightcyan4","tomato","tomato","indianred3","indianred3"),
                            label=labs_8)

dt_label_color_reference<-rbind(dt_label_color_reference,dt_color_8)
dt_label_color_reference<-rbind(dt_label_color_reference,dt_color_8_topt)



contrib_therm_beta_ser_8$sp_thermo<-contrib_therm_beta_ser_8$sp_thermo*10
contrib_therm_beta_ser_melt_8<-melt(contrib_therm_beta_ser_8,measure.vars=c("sp_thermo",name_8_comp_topt,"sp_delta_beta",name_8_comp_beta))
contrib_therm_beta_ser_melt_8[,variable:=as.character(variable)]
contrib_therm_beta_ser_melt_8[,type:=ifelse(grepl("beta",variable),"2_beta","1_thermo")]


bootstrap_random_topt_melt_8<-melt(bootstrap_random_topt,measure.vars=c("sp_thermo",name_8_comp_topt,"sp_delta_beta",name_8_comp_beta))
bootstrap_random_topt_melt_8[,value_random_topt:=value]
#bootstrap_random_topt_melt_8[variable%in%thermo_var,value_random_topt:=value_random_topt/10]

contrib_therm_beta_ser_melt_8<-cbind(contrib_therm_beta_ser_melt_8,bootstrap_random_topt_melt_8[,"value_random_topt"])

table_test_8<-contrib_therm_beta_ser_melt_8[,.(mean_val=mean(value),
                                           mean_val_random=mean(value_random_topt),
                                           p_val=ifelse(FALSE,
                                                        wilcox.test(value)$p.value,
                                                        wilcox.test(value,(value_random_topt))$p.value )),by=variable]


table_test_8[,signif_star:="n.s."]
table_test_8[p_val < 0.05,"signif_star"] <- "*"
table_test_8[p_val < 0.01,"signif_star"] <- "**"
table_test_8[p_val < 0.001,"signif_star"] <- "***"

table_test_8[,type:=ifelse(grepl("beta",variable),"2_beta","1_thermo")]
table_test_8[,label:=paste0("µ = ",round(mean_val*ifelse(type=="1_thermo",1,1),2)," ",signif_star)]
table_test_8[,x:=ifelse(type=="1_thermo",-0.25,-3.8)]
table_test_8[variable%in%c("contrib_topt_colder_col_rare","contrib_topt_colder_col_common","contrib_topt_warmer_ext_rare","contrib_topt_warmer_ext_common"),x:=0.13]
table_test_8[variable%in%c("contrib_beta_colder_ext_rare","contrib_beta_warmer_ext_rare","contrib_beta_warmer_col_common","contrib_beta_colder_col_common"),x:=1.1]


label_facets<-c(`1_thermo`="Thermophilization",`2_beta`="\U0394\U03B2-diversity")
#label_facets<-c(`1_thermo`="- - - - - 0 - - - - -0 - - - - - - 0 - - - - - 0 - - - - -",`2_beta`="- - - - - 0 - - - - -0 - - - - - - 0 - - - - - 0 - - - - -")

contrib_therm_beta_ser_melt_8[,value:=as.numeric(value)]


plot_thermo<-ggplot(contrib_therm_beta_ser_melt_8[type=="1_thermo"],aes(x=value,y=variable,fill=variable))+
  geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable),show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
  geom_text(data=table_test_8[type=="1_thermo",],aes(x=x,y=variable,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(8.5),lwd=0.5)+
  geom_hline(yintercept = c(4.5),lwd=0.5,color="grey70",lty=2)+
  coord_cartesian(xlim=c(-0.375,0.5))+
  geom_point(data=table_test_8[type=="1_thermo",],aes(x= mean_val_random, y= variable),pch=21,fill="white",color="grey20",size=3)+
  scale_fill_manual(values = dt_label_color_reference$color,breaks = dt_label_color_reference$variable)+
  scale_color_manual(values = dt_label_color_reference$color,breaks = dt_label_color_reference$variable)+
  scale_y_discrete(limits=dt_label_color_reference[variable%in%c("sp_thermo",name_8_comp_topt),]$variable[c(1,4,5,8,9,3,2,6,7)][order(-(1:9))], 
                   label= c("Total",dt_label_color_reference[variable%in%c(name_8_comp_topt),]$label) [c(1,4,5,8,9,3,2,6,7)][order(-(1:9))] )+
  theme(strip.text.x = element_text(size = 10,face="bold"),plot.margin = margin(r=0,l=4),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#EAE397",linewidth = 0.5))+
  labs(x="Thermophilization °C/ 10years",y="Contributions to the variable")


plot_beta<-ggplot(contrib_therm_beta_ser_melt_8[type=="2_beta"],aes(x=value,y=variable,fill=variable))+
  geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable),show.legend = F,size=1.2)+
  geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
  geom_text(data=table_test_8[type=="2_beta",],aes(x=x,y=variable,label=label),nudge_y =0.25,size=3.2,hjust=0)+
  facet_grid(~type,scales = "free",shrink = T,labeller = as_labeller(label_facets))+
  theme_bw()+
  geom_vline(xintercept = 0 , lty=2)+
  geom_hline(yintercept = c(8.5),lwd=0.5)+
  geom_hline(yintercept = c(4.5),lwd=0.5,color="grey70",lty=2)+
  coord_cartesian(xlim=c(-4,4))+
  geom_point(data=table_test_8[type=="2_beta",],aes(x= mean_val_random, y= variable),pch=21,fill="white",color="grey20",size=3)+
  scale_fill_manual(values = dt_label_color_reference$color,breaks = dt_label_color_reference$variable)+
  scale_color_manual(values = dt_label_color_reference$color,breaks = dt_label_color_reference$variable)+
  scale_y_discrete(limits=dt_label_color_reference[variable%in%c("sp_delta_beta",name_8_comp_beta),]$variable[c(1,4,5,8,9,3,2,6,7)][order(-(1:9))], 
                  label= rep("",9) )+
  theme(strip.text.x = element_text(size = 10,face="bold"),axis.ticks = element_blank(),panel.border = element_rect(linewidth=0.5),strip.background =element_rect(fill="#C5D49A",linewidth = 0.5))+
  labs(x="\U0394\U03B2-diversity",y=" ")




out_extended_fig<-ggarrange(plotlist = list(plot_thermo,plot_beta+theme(plot.background = element_rect(fill=NA,color=NA))),labels = c("a)","b)"),align = "h",widths = c(1.25,1))



showtext:: showtext_auto(enable = TRUE)
ggsave(file.path("Figures_results","Extended_figure_reworked.pdf"),out_extended_fig,width =180,height =140,units = "mm",scale=0.95)
showtext:: showtext_auto(enable = FALSE) 
ggsave(file.path("Figures_results","Extended_figure_reworked.jpg"),out_extended_fig,width =180,height =140,units = "mm",scale=0.95,dpi=450)
showtext:: showtext_auto(enable = TRUE)


#### Extended figures: map of the sampling ####


## we create a map of forest ecoregions with the number of pairs inside, open street map as a background
breaks=c(0,50,100,200,300,400,2000)
ser_value_sf$cut_n_duet<-cut(ser_value_sf$n_pair,breaks=breaks)

ser_of_interest<-"G42"
ser_of_interes_sf<-ser_value_sf[ser_value_sf$ser==ser_of_interest,]
#ext_ser<-bbox(as_Spatial(ser_of_interes_sf))
NFI_pairs_info_interest_sf<-st_as_sf( NFI_pairs_info[ser==ser_of_interest,] ,coords=c("xl","yl"),crs=st_crs(2154))

ser_value_sf$ecoregion<-ifelse(ser_value_sf$greco%in% c("J"),"Mediterranean",ifelse(ser_value_sf$greco%in% c("D","G","E","H","I"),"Mountain","Lowland"))
large_ecoregion_sf<-aggregate(ser_value_sf,by=list(ser_value_sf$ecoregion),FUN=function(x)x[1])


map_all_ser_france<-ggplot(ser_value_sf)+
  annotation_map_tile(type="cartolight",zoomin = 0)+
  annotation_north_arrow(location = "tl",height = unit(0.8, "cm"),width = unit(0.8, "cm"))+
  annotation_scale(location = "bl", text_cex = 0.75)+
  geom_sf(aes(fill=(cut_n_duet)),color="grey60",show.legend = T)+
  geom_sf(data=large_ecoregion_sf,fill=NA,color="grey25",linewidth=0.5)+
  geom_sf(data=ser_of_interes_sf,linewidth=0.5,color="darkred",fill=NA)+
  theme_bw()+
  theme(legend.position = "bottom",legend.direction = "vertical")+
  guides(fill=guide_legend(nrow=2,order=c(2)))+
 
  labs(pattern="Ecoregion type")+
  scale_fill_manual(values=colorRampPalette(c("ivory","peachpuff3","olivedrab"))(7),na.value = "grey65",label=c("15:50","51:100","101:200","201:300","301:400",">400","Excluded"),name="Number of plot pair")



## we load a 150m * 150m resolution forest map to vizualize one forest ecoregions
forest_cover_25m<-raster(file.path("Data","Spatial_Data","Forest_cover_one_ecoreg_G42_150m.tif"))
forest_cover_25m_dt<-data.table(xyFromCell(forest_cover_25m,1:ncell(forest_cover_25m)))
forest_cover_25m_dt$cover_f<-getValues(forest_cover_25m)

## we create the single ecoregion map with NFI plots
one_ser_plots<-ggplot(ser_of_interes_sf)+
  geom_sf(fill="cornsilk",color="grey25",linewidth=0.75)+
  geom_raster(data=forest_cover_25m_dt[cover_f>0.5,],aes(x=x,y=y),fill="#B4BF89",show.legend = F)+
  geom_sf(fill="NA",color="grey25",linewidth=0.75)+
  geom_sf(data=NFI_pairs_info_interest_sf,aes(fill=period),color="grey10",size=1.65,alpha=0.75,shape=21)+
  scale_fill_manual(values =  c("darkorange","deepskyblue3"),breaks=c("past","recent"),label=c("Past plot (2005-2011)","Recent plot (2015-2021)"))+
  guides(fill=guide_legend(nrow=2,title.position = "top",override.aes = list(size=3.5)))+
  theme_grey()+
  labs(x="",y="",fill="Time period of the NFI plots")+
  theme_void()+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())



##we combine the two maps
export_map_fig_1<-ggarrange(map_all_ser_france,
                            one_ser_plots,
                            ncol=2,align = "v",labels = c("(a)","(b)"),widths=c(1,0.8) )+theme(panel.background=element_rect(fill="white",color="white"))


showtext:: showtext_auto(enable = FALSE)
ggsave(file.path("Figures_results","export_map_ext_fig_1.png"),device = "png",type="cairo",dpi=400,export_map_fig_1,scale=1,width = 180,height = 120,unit="mm")
ggsave(file.path("Figures_results","export_map_ext_fig_1.pdf"),dpi=400,export_map_fig_1,scale=1,width = 180,height = 120,unit="mm")

rm(map_all_ser_france,one_ser_plots,export_map_fig_1)



