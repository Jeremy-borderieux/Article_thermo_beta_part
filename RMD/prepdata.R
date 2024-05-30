
SER_sf

france_bound_no_corsica

list_sp_contrib

occ_sp_therm<-list_sp_contrib[,.(N_past=sum(occurrence_past),N_recent=sum(occurrence_recent),N=sum(occurrence_total),contrib_thermo=sum(raw_contrib),warm_or_cold=sum(sp_relative_topt=="warmer")/.N,topt_climplant=unique(topt_climplant)),by=species_name]
occ_sp_therm[order(-N),][1:20,]

occ_sp_therm[order(-abs(contrib_thermo)),][1:50,]

occ_sp_therm<-occ_sp_therm[order(-abs(contrib_thermo)),]


needed_data<-list(SER_sf,
                   france_bound_no_corsica,
                   list_sp_contrib,
                  occ_sp_therm)


saveRDS(needed_data,"RMD/abond_data.RData")

Aegopodium podagraria

occ_sp_therm[species_name=="Actaea spicata",]
occ_sp_therm[species_name=="Lunaria rediviva",]
occ_sp_therm[species_name=="Cardamine heptaphylla",]
occ_sp_therm[species_name=="Aegopodium podagraria",]

occ_sp_therm[species_name=="Epilobium angustifolium",]

occ_sp_therm[species_name=="Epilobium montanum",]

Brachypodium pinnatum
occ_sp_therm[species_name=="Brachypodium pinnatum",]
occ_sp_therm[species_name=="Brachypodium pinnatum",]
occ_sp_therm[species_name=="Brachypodium pinnatum",]
occ_sp_therm[species_name=="Brachypodium pinnatum",]



