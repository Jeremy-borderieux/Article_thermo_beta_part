#### Geographical pairing ####

## helper function to quickly get a data.table containing the distances between each pairs of plots (SF object) in a maximum distance threshold
## ifn_sf an sf object, the ID of the points should be named "idp
## name of the column of the factor used to create two classes of plots for pairing, (clust_level_1,clust_level_2)
get_distances<-function(ifn_sf,clust_factor,clust_level_1,clust_level_2,distance_tresh=6){
  
  coord_clust_1<-ifn_sf[clust_factor==clust_level_1,]
  coord_clust_2<-ifn_sf[clust_factor==clust_level_2,]
  coord_clust_1<-coord_clust_1[,"idp"]
  coord_clust_2<-coord_clust_2[,"idp"]
  
  ## the function st _distance return a matrix of distances, transformed into a data.table
  dist_mat<-st_distance(coord_clust_2,coord_clust_1,tolerance=5000)
  units(dist_mat)<-"km"
  colnames(dist_mat)<-coord_clust_1$idp
  rownames(dist_mat)<-coord_clust_2$idp
  
  all_pair<-expand.grid(coord_clust_2$idp,coord_clust_1$idp,stringsAsFactors = FALSE)
  
  all_pair<-data.table(all_pair)
  all_pair[,dist_pair:=as.numeric(unlist(dist_mat))]
  all_pair_2_res<-all_pair[dist_pair<distance_tresh,]
  
  names(all_pair_2_res)<-c("idp2","idp1","dist_pair")
  
  all_pair_2_res<-all_pair_2_res[order(dist_pair),]
  all_pair_2_res<-all_pair_2_res[,c(2,1,3)]
  
  return(all_pair_2_res)
}

delete_dt_row <- function(DT, del.idxs) { ## useful function to fasten get_pair_optimal()
  # delete rows instead of re-writing the data.table into memory
  keep.idxs <- setdiff(DT[, .I], del.idxs)
  cols = names(DT)
  DT.subset <- data.table(DT[[1]][keep.idxs])
  setnames(DT.subset, cols[1])
  for (col in cols[2:length(cols)]) {
    DT.subset[, (col) := DT[[col]][keep.idxs]]
    DT[, (col) := NULL]
  }
  return(DT.subset)
}


## this function uses the same SF object sf_ifn, as well as the distance data.table created by get_distances (dist table)
## the difference of the variable we want to compute can be specified with valid column names in differences_to_compute
## differences_tresh_min and differences_tresh_max are used to discards (before the final selection of pairs) potential pairs with a larger difference of one a the variable
## if set to 0, no differences checks are performed. For the following analysis, the differences cheched are elevation (less than 100m difference) and year of the survey (must be 10 or 9 years appart)
get_pair_optimal<-function(sf_ifn,dist_table,distance_tresh=5,differences_to_compute=NULL,
                           differences_tresh_min=if(is.null(differences_to_compute)) NULL else rep(0,length(differences_to_compute)),
                           differences_tresh_max=if(is.null(differences_to_compute)) NULL else rep(0,length(differences_to_compute)),
                           ident_is_num=T){
  
  dist_table<-dist_table[dist_pair<=distance_tresh,]# remove plots too far for each other:5km
  
  ## data preprocessing to get differences betwenn covariable
  dist_table[,pair_id:=1:nrow(dist_table)]
  subsest_idp1<-merge(dist_table[,c("idp1","pair_id")],as.data.table(sf_ifn),by.x="idp1",by.y="idp")
  subsest_idp2<-merge(dist_table[,c("idp2","pair_id")],as.data.table(sf_ifn),by.x="idp2",by.y="idp")
  subsest_idp1<-subsest_idp1[order(pair_id),]
  subsest_idp2<-subsest_idp2[order(pair_id),]
  
  ## check that the differences to compute and test are correctly provided
  if(length(differences_to_compute)!=length(differences_tresh_min | length(differences_to_compute)!=length(differences_tresh_max)))stop("Length of columns to compute differences is different to the number of difference tresholds")
  ## compute the differences
  for(col in differences_to_compute)dist_table[,paste0(col,"_dif")]<-subsest_idp1[[col]]-subsest_idp2[[col]]
  
  ## check for a differences criterion, for  this analysis: elevation and year of survey threshold only
  foreach(col = differences_to_compute ,trsh_min = differences_tresh_min,trsh_max=differences_tresh_max)%do%{
    new_col<-paste0(col,"_dif")
    if(trsh_min!=0)dist_table<-dist_table[abs(dist_table[[new_col]])>= trsh_min,]
    if(trsh_max!=0)dist_table<-dist_table[abs(dist_table[[new_col]])<= trsh_max,]
    
  }
  
  
  ## this empty data.frame will keep the selected pairs (pairs), and mimic the structure of dist_table
  kept_pair<-data.frame(matrix(NA,nrow = nrow(dist_table),ncol=ncol(dist_table)))
  
  colnames(kept_pair)<-colnames(dist_table)
  
  incr<-1
  while(nrow(dist_table)!=0){
    
    cat(paste("->",incr))
    
    # we use the function table to count the number od time each plot_id (called idp) are found, as it is also the number 
    # of neighbor it has
    nb_neig_idp1_dt<-as.data.table(table(dist_table$idp1))
    colnames(nb_neig_idp1_dt)<-c("idp1","nb_neig1")
    nb_neig_idp2_dt<-as.data.table(table(dist_table$idp2))
    colnames(nb_neig_idp2_dt)<-c("idp2","nb_neig2")
    
    # we match the number of neigboh with dist_table, that containt all the potential pairs
    dist_table[,nb_neig1:=nb_neig_idp1_dt[match(dist_table$idp1,idp1),2]]
    dist_table[,nb_neig2:=nb_neig_idp2_dt[match(dist_table$idp2,idp2),2]]
    
    ## this line check the minimum number of neighbor a plot in the pair has
    dist_table[,priority_low_neig:=pmin(nb_neig1,nb_neig2)]
    ## this line check the maximum number of neighbor a plot in the pair has
    dist_table[,priority_max_neig:=pmax(nb_neig1,nb_neig2)]
    
    # priority is given to the pair containing a plot with the lower minimal neighbor count
    ## then to the pair with the lower maximal count, then to the closest pair
    setorder(dist_table,priority_low_neig,priority_max_neig,dist_pair)
    
    # fast way to remove those columns now that the pairs have been prioritize
    dist_table[,nb_neig1:=NULL]
    dist_table[,nb_neig2:=NULL]
    dist_table[,priority_low_neig:=NULL]  
    dist_table[,priority_max_neig:=NULL]
    
    # the first row of dist_table is the selected one
    kept_pair[incr,]<-as.data.frame(dist_table[1,])
    
    incr<-incr+1
    kept_idp1<-dist_table[1,idp1]
    kept_idp2<-dist_table[1,idp2]
    
    ## we remove every potential pair with a plot that has just been selected
    dist_table<-delete_dt_row(dist_table,(1:nrow(dist_table))[dist_table[,idp1==kept_idp1]])
    dist_table<-delete_dt_row(dist_table,(1:nrow(dist_table))[dist_table[,idp2==kept_idp2]])
    
    
    
  }
  
  colnames(kept_pair)<-colnames(dist_table)
  
  kept_pair<-data.table(kept_pair)
  kept_pair<-kept_pair[!is.na(idp1),] ## remove the empty rows, kept_pair was too large by design
  kept_pair<-kept_pair[order(dist_pair),]
  kept_pair[,pair_id:=1:nrow(kept_pair)]
  
  return(kept_pair)
}


get_centroid_pairs<-function(sf_2rows){
res<-st_as_sf( st_centroid(st_cast(st_combine(sf_2rows),"LINESTRING")),crs=st_crs(2154))
res$pair_id<-sf_2rows$pair_id[1]
return(res)}

#### data transformation ####

##  function to create a [site_id,species] matrix of absence-presence from the flora survey
create_table_sp<-function(survey,id_names="idp"){
  
  table_survey<-table(survey[,get(id_names)],survey$species_name)
  table_survey<-as.data.frame.matrix(table_survey)
  
  di<-dim(table_survey)
  sp<-colnames(table_survey)
  id<-rownames(table_survey)
  
  table_survey<-as.matrix(table_survey)
  table_survey<-as.numeric(table_survey)
  table_survey<-matrix(table_survey,nrow=di[1],ncol =di[2] )
  table_survey<-as.data.frame(table_survey)
  
  rownames(table_survey)<-id
  colnames(table_survey)<-sp
  
  return(table_survey)
}


## this function creates a data.table to compare, for a given set of surveys
## the occurrences of the past and recent of every species.
## the species are also linked with relevant traits and preferences, 
## table_1 and table_2 ar two  [site_id,species] matrixes for two time period created by creat_table_sp

create_comparaison_table<-function(table_1,table_2,ser=NULL){
  
  ## count the number of occurrences in the two tables (the two time period)
  comparison_surv<-data.table(sp_1=apply(table_1,2,sum,na.rm=T),sp_2=apply(table_2,2,sum,na.rm=T),species_name=colnames(table_2))
  comparison_surv[,tot_ocu:=sp_1+sp_2]
  comparison_surv[,dif_norm:=sp_2/tot_ocu]
  comparison_surv[,genre_only:= !grepl(" ",species_name)]
  comparison_surv$genre<-str_extract(comparison_surv$species_name,"[:alpha:]+ ")
  comparison_surv$genre<-ifelse(is.na(comparison_surv$genre),paste0(comparison_surv$species_name," "),comparison_surv$genre)
  
  ## include the trait and preference database
  comparison_surv<-merge(comparison_surv,sp_indicator_value[,c("lb_nom_final","topt_climplant","topt_picq" ,"topt","niche_breadth","niche_breadth_n","N_ellenberg","vi_pH","K_Ellenberg","indFor_Chytry","Area")],by.x="species_name",by.y="lb_nom_final",all.x=T)
  
  
  names_of_col<-c("species_name","occurrence_past","occurrence_recent","occurrence_total","past_recent",
                  "genre_only","genre","topt_climplant","topt_picq" ,"topt","niche_breadth",
                  "L_Ellenberg","N_ellenberg","vi_pH","K_Ellenberg","indFor_Chytry","Area")
  
  # if(is.null(ser))warning("no SER imput, not checking EurForPlant list") else{
  #   greco<-substr(ser,1,1)
  #   biogeo_of_ser<-ifelse(greco%in%c("A","F","B"),"France_atlantic",
  #                         ifelse(greco%in%c("G","I","H","D","E"),"France_mountains",
  #                                ifelse(greco%in%c("C"),"France_continental","France_atlantic")))
  #   
  #   
  #   comparison_surv<-merge(comparison_surv,EurForPlant[biogeo_region==biogeo_of_ser,c("species_name","habitat_categ","forest_species")],by="species_name",all.x=T)
  #   
  #   names_of_col<-c(names_of_col,"habitat_categ","forest_species")
  # }
  # 
  
  colnames(comparison_surv)<-names_of_col
  
  comparison_surv<-comparison_surv[order(occurrence_total,decreasing = T),]
  comparison_surv<-comparison_surv[occurrence_total!=0,]
  
  comparison_surv$ratio_2<-comparison_surv$`past_recent`
  comparison_surv<-comparison_surv[order(-occurrence_total),]
  
  return(comparison_surv)
  
}

## helper function that if given 2 or more list of data.frame (or any data.frame related object)
## return a single list of the same length but with row-binded data.frames
rbind_list_list<-function(...){
  
  n_dt<-sapply(list(...), length)
  if(length(unique(n_dt))!=1) stop("the lists don't have the same number of data.frame inside")
  
  get_dt_of_list<-function(x)lapply(list(...),`[[`,x)
  
  list_of_dt<-lapply((1:n_dt[1]), get_dt_of_list)
  
  list_of_rbind_dt<-lapply(list_of_dt,do.call,what=rbind)
  
  return(list_of_rbind_dt)
}



#### Main analysis and partitionning function ####

## function for partitioning beta-diversity changes into two - four - sp level component
## This function comes from Tastumi et al, 2021 Temporal changes in spatial variation: partitioning the extinction and colonisation components of beta diversity
## DOI: 10.1111/ele.13720
## this function was only slighlty modified to change the format of the output
## it takes as input two matrixes [site_id,species] for two time span, and the type of partition
ecopart.multi.JB<- function(d1, d2, part="two"){
  part <- match.arg(part, c("two", "four", "sp"))
  N <- nrow(d1) # Number of sites (communities)
  S <- ncol(d1) # Number of species
  # Delta subscripts (gamma xyz)
  X <- colSums(d1)
  Y <- colSums(d2)
  Z <- sapply(1:S, function(s) sum(d1[,s]==1 & d2[,s]==1))
  # Whittaker s beta and a scaling factor (H)
  Alpha1 <- mean(rowSums(d1))
  Alpha2 <- mean(rowSums(d2))
  Gamma1 <- sum(colSums(d1) > 0)
  Gamma2 <- sum(colSums(d2) > 0)
  Beta1 <- Gamma1/Alpha1
  Beta2 <- Gamma2/Alpha2
  H <- Beta1 / ((Alpha2-Alpha1)/Alpha1 + 1)
  # Components of temporal changes in beta diversity (delta beta)
  # See Eq. 4 in the main text
  DBeta <- matrix(nrow=4, ncol=S)
  DBeta[1,] <- (X *H/N/Alpha1 - H/Gamma1) * (X>0 & Z==0)
  DBeta[2,] <- (X-Z) *H/N/Alpha1 * (X>Z & Z>0)
  DBeta[3,] <- (-Y *H/N/Alpha1 + H/Gamma1) * (Y>0 & Z==0)
  DBeta[4,] <- (Z-Y) *H/N/Alpha1 * (Y>Z & Z>0)
  # Store delta beta  in a vector or matrix
  if(part=="two"){
    Res <- c(sum(DBeta[1:2,]), sum(DBeta[3:4,]))
    names(Res) <- c("extirpation", "colonisation")
  } else if(part=="four"){
    Res <- c(sum(DBeta[1, DBeta[1,]<0]),
             sum(DBeta[1, DBeta[1,]>0]) + sum(DBeta[2,]),
             sum(DBeta[3, DBeta[3,]>0]),
             sum(DBeta[3, DBeta[3,]<0]) + sum(DBeta[4,]))
    names(Res) <- c("ext.hmgn", "ext.htrg", "col.htrg", "col.hmgn")
  } else if(part=="sp"){
    Res <- rbind(sapply(1:S, function(s) sum(DBeta[1:2, s])),
                 sapply(1:S, function(s) sum(DBeta[3:4, s])))
    rownames(Res) <- c("extirpation", "colonisation")
    colnames(Res) <- colnames(d1)
  }
  # Return the vector or matrix
  if(part=="sp"){
    Res<-data.frame(t(Res))
    Res$species_name<-rownames(Res)
    Res<-data.table(Res)
    Res[,delta_beta:=extirpation + colonisation]
    return(Res)
  }else{
    Res<-t(data.table( Res ))
    colnames(Res)<- if(part=="two")c("beta_extirpation", "beta_colonisation")  else c("ext.hmgn", "ext.htrg", "col.htrg", "col.hmgn")
    if(part=="two"){
      res_tmp<-data.table(Alpha1,Alpha2,Gamma1,Gamma2,Beta1,Beta2,H)
      Res<-cbind(Res,res_tmp)
    }
  }
  return(Res)
}




## get_contrib_one_ser is the core function of the analysis
## given the name of one ecoregion, the function return the thermophilization and delta beta between the two period 
## and the contribution of the extinction and colonization component, as well as contribution from locally cold and warm adapted species 
get_contrib_one_ser<-function(ser_select, # character, if of an ecoregion  
                              table_flora, # [siet_id,species matrix] obtained from create_table_sp
                              n_comp=4,
                              topt_name="topt_climplant", # name of the thermal optimum to use topt_climplant, topt, topt_picq
                              random_topt=F, # should the thermal optima of the species randomize (useful for bootstrap)
                              rarefy=F,# should the time period with the most occurrences be rarefied ? (useful for bootstrap)
                              rarefy_rare_sp=F,
                              year=NULL){ # do a seperate analysis for each year of the "recent" plot
  
  #### 1/data processing
  
  
  subset_pair_ser<-pairs_ser[ser%in%ser_select,] # pair_ser was created by get_pair_optimal()
  if(!is.null(year)) subset_pair_ser<-pairs_ser[ser%in%ser_select & floor(idp1/100000)+2005 ==year,] 
  
  ## mean difference between the date of the two surveys of a pair, used to convert thermophilization to °C/10 years
  mean_year_dif<-mean(subset_pair_ser$campaign_dif)
  
  ## those vectotrs contains the site_id of the plots of the ser_select ecoregion
  idp_to_get_past<-subset_pair_ser[,idp2]
  idp_to_get_recent<-subset_pair_ser[,idp1]
  idp_to_get_past_char<-as.character(idp_to_get_past)
  idp_to_get_recent_char<-as.character(idp_to_get_recent)
  
  N_plot<-nrow(table_flora[idp_to_get_past,])
  
  ## we remove species without a thermal optimum, they are discarded from the thermophilization and beta-diversity partition
  table_flora<-table_flora[,colnames(table_flora)%in%sp_indicator_value[!is.na(get(topt_name)),]$lb_nom_final]
  
  ## We split the occurrence matrix into the past and recent plots
  table_past<-table_flora[rownames(table_flora)%in%idp_to_get_past_char,]
  table_recent<-table_flora[rownames(table_flora)%in%idp_to_get_recent_char,]
  
  ## If we want to test our hypothesis with equal number of species
  ## We resample the table with the most occurrences in order to have two table with the same number of occurrences
  if(rarefy){
    n_sp_past<-sum(table_past)
    n_sp_recent<-sum(table_recent)
    if(n_sp_past==n_sp_recent) break
    
    if(n_sp_past>n_sp_recent)table_past[table_past==1]<-sample(c(rep(1,n_sp_recent),rep(0,n_sp_past-n_sp_recent)),n_sp_past)
    if(n_sp_past<n_sp_recent)table_recent[table_recent==1]<-sample(c(rep(1,n_sp_past),rep(0,n_sp_recent-n_sp_past)),n_sp_recent)
    
  }
  
  ## create_comparaison_table is called to create a data.table of species occurrences evolution 
  # comparison_surv_one_ser<-create_comparaison_table(table_flora,idp_to_get_past,idp_to_get_recent,rarefy =occ_hyp==5 ,random_method=random_method,ser=ser_select)
  
  comparison_surv_one_ser<-create_comparaison_table(table_past,table_recent,ser=ser_select)
  
  
  #### if random_topt==T, each species is given a thermal optimum sampled (without replacement) in the thermal optimum of the species recorded at least one time in the ecoregion
  if(random_topt==T){
    comparison_surv_one_ser[,(topt_name)]<-sample(comparison_surv_one_ser[,get(topt_name)])
  }
  
  
  #### error_topt==T
  
  #### 2/Thermophilization partitioning
  
  
  
  
  ## we count the number of occurrences recorded in the two period
  sp_tot_past<-sum(comparison_surv_one_ser$occurrence_past)
  sp_tot_recent<-sum(comparison_surv_one_ser$occurrence_recent)
  
  ## we can compute the ratio of occurrences between the two period  
  normalized_sp_tot<-sp_tot_recent / sp_tot_past
  
  comparison_surv_one_ser[,ratio_2:=occurrence_recent/(occurrence_recent+occurrence_past)]
  comparison_surv_one_ser[,delta_occ:=occurrence_recent-occurrence_past]
  
  ## get the mean Topt of the occurrences of the past and the recent. (average a the ecoregion  scale)
  ser_it_sp_past<-comparison_surv_one_ser[,weighted.mean(get(topt_name),occurrence_past,na.rm=T)]
  ser_it_sp_recent<-comparison_surv_one_ser[,weighted.mean(get(topt_name),occurrence_recent,na.rm=T)]
  ## we can then compute the thermophlization °C/year
  warming<-ser_it_sp_recent - ser_it_sp_past
  warming<-warming/mean_year_dif
  
  
  ## we assign the two classes used for the partitioning 
  ## sp_relative_topt : the species has a higher or lower thermal optimum than the mean topt (ecoregion scale)
  ## sp_relative_occ : is the species occurrences of the species declining/ increasing ?
  comparison_surv_one_ser[,sp_relative_topt:=ifelse(get(topt_name)<ser_it_sp_past,"colder","warmer")]
  comparison_surv_one_ser[,sp_relative_occ:=ifelse(ratio_2==0.5,"stable",ifelse(ratio_2<0.5,"ext","col"))] 
  
  ## to compute delta topt (thermophilization), relative thermal optimum of the species to the mean of the ecoregion is useful
  comparison_surv_one_ser[,relative_topt:=get(topt_name)-ser_it_sp_past]
  comparison_surv_one_ser[,raw_contrib:=(relative_topt*(delta_occ))/(sp_tot_recent)] # computation of species level contribution to thermophilization
  
  if(rarefy_rare_sp){
    comparison_surv_one_ser[,sp_relative_beta:=ifelse((occurrence_past)<= ((N_plot/2)*0.1) , "rare","common")]
    comparison_surv_one_ser[,categ_4:=paste0(sp_relative_topt,"_",sp_relative_beta)]
    
    table_to_rarefy<-comparison_surv_one_ser[,.N,by=categ_4][order(categ_4)]
    
    if(diff(table_to_rarefy[c(1,3),N])>=0){  ## if >0 they are more warmer common
      tmp_common<-  rbind(comparison_surv_one_ser[categ_4=="colder_common"] ,comparison_surv_one_ser[categ_4=="warmer_common"][sample(.N,table_to_rarefy[1,N])])
      
    }else{
      
      tmp_common<- rbind(comparison_surv_one_ser[categ_4=="warmer_common"] , comparison_surv_one_ser[categ_4=="colder_common"][sample(.N,table_to_rarefy[3,N])])
      
    }
    
    
    if(diff(table_to_rarefy[c(2,4),N])>=0){  ## if >0 they are more warmer common
      tmp_rare<-  rbind(comparison_surv_one_ser[categ_4=="colder_rare"] ,comparison_surv_one_ser[categ_4=="warmer_rare"][sample(.N,table_to_rarefy[2,N])])
      
    }else{
      
      tmp_rare<- rbind(comparison_surv_one_ser[categ_4=="warmer_rare"] , comparison_surv_one_ser[categ_4=="colder_rare"][sample(.N,table_to_rarefy[4,N])])
      
    }
    
    
    comparison_surv_one_ser<-rbind(tmp_common,tmp_rare)
    
    ## we count the number of occurrences recorded in the two period
    sp_tot_past<-sum(comparison_surv_one_ser$occurrence_past)
    sp_tot_recent<-sum(comparison_surv_one_ser$occurrence_recent)
    
    ## we can compute the ratio of occurrences between the two period  
    normalized_sp_tot<-sp_tot_recent / sp_tot_past
    
    ## get the mean Topt of the occurrences of the past and the recent. (average a the ecoregion  scale)
    ser_it_sp_past<-comparison_surv_one_ser[,weighted.mean(get(topt_name),occurrence_past,na.rm=T)]
    ser_it_sp_recent<-comparison_surv_one_ser[,weighted.mean(get(topt_name),occurrence_recent,na.rm=T)]
    ## we can then compute the thermophlization °C/year
    warming<-ser_it_sp_recent - ser_it_sp_past
    warming<-warming
    
    
    ## we assign the two classes used for the partitioning 
    ## sp_relative_topt : the species has a higher or lower thermal optimum than the mean topt (ecoregion scale)
    ## sp_relative_occ : is the species occurrences of the species declining/ increasing ?
    comparison_surv_one_ser[,sp_relative_topt:=ifelse(get(topt_name)<ser_it_sp_past,"colder","warmer")]
    comparison_surv_one_ser[,sp_relative_occ:=ifelse(ratio_2==0.5,"stable",ifelse(ratio_2<0.5,"ext","col"))] 
    
    ## to compute delta topt (thermophilization), relative thermal optimum of the species to the mean of the ecoregion is useful
    comparison_surv_one_ser[,relative_topt:=get(topt_name)-ser_it_sp_past]
    comparison_surv_one_ser[,raw_contrib:=(relative_topt*(delta_occ))/(sp_tot_recent)] # computation of species level contribution to thermophilization
    
    
    
    
    
  }
  
  
  
  ##comparison_surv_one_ser is ready to to compute the component of thermophilization
  
  
  #### 3/ Beta diversity partitioning 
  
  
  ## we run the Tatsumi et al. 2021 function on those plots
  # two components : extinction and colonization component of delta_beta
  ecopart_two_sp_ser<-ecopart.multi.JB(table_past,table_recent,part="two")
  # sp components : each species get an ext and colo component of contribution to delta_beta
  ecopart_multi_sp_ser<-ecopart.multi.JB(table_past,table_recent,part="sp")
  
  if(rarefy_rare_sp){
    ecopart_multi_sp_ser<-ecopart.multi.JB(table_past[,comparison_surv_one_ser$species_name],
                                           table_recent[,comparison_surv_one_ser$species_name],
                                           part="sp")
    
    
    
  }
  
  
  ecopart_four_ser<-ecopart.multi.JB(table_past,table_recent,part="four")
  
  ## we merge the species_level delta_beta component to the data.table containing the species-level thermophilization component
  comparison_surv_one_ser_beta<-merge(comparison_surv_one_ser,ecopart_multi_sp_ser,by.x="species_name",by.y="species_name",all.x=T)
  
  comparison_surv_one_ser_beta[,sp_relative_beta:=ifelse(delta_beta==0,"stable",ifelse(delta_beta<0,"Homogenize","Heterogenize"))]
  
  ## the 4 components such as contrib_thermo_colder_ext are created here, we sum the contribution of species with similar categories (for example cold-adapted and declining)
  decompose_thermo_beta<-comparison_surv_one_ser_beta[,.(N_past=sum(occurrence_past),
                                                         N_recent=sum(occurrence_recent),
                                                         contrib_topt=sum(raw_contrib,na.rm=T), # sum of the contribution to thermophilization, see methods
                                                         contrib_beta=sum(delta_beta)), # sum the species level contribution to beta-diversity
                                                      by=.(sp_relative_occ,sp_relative_topt)][order(sp_relative_topt,sp_relative_occ)] # the categories to create the sum
  
  
  
  ## we remove the contribution of species with the same number of occurrences in the two period, they don"t cotnribution to thermophilization or beta-div
  decompose_thermo_beta<-decompose_thermo_beta[sp_relative_occ!="stable",]
  decompose_thermo_beta<-decompose_thermo_beta[order(sp_relative_topt, sp_relative_occ),]
  
  decompose_thermo_beta[,contrib_topt:=contrib_topt/mean_year_dif]
  
  # we cast this data.table from long to wide, one column per component
  results<-dcast.data.table(decompose_thermo_beta, .~sp_relative_topt+sp_relative_occ,value.var = c("contrib_topt", "contrib_beta"))
  
  
  if(n_comp==8){
    
    comparison_surv_one_ser_beta[,sp_relative_beta:=ifelse(delta_beta==0,"stable",ifelse(delta_beta<0,"Homogenize","Heterogenize"))]
    comparison_surv_one_ser_beta[,common_rare:=ifelse(sp_relative_beta == "Homogenize" & sp_relative_occ=="ext"  , "rare","common")]
    comparison_surv_one_ser_beta[,common_rare:=ifelse(sp_relative_beta == "Heterogenize" & sp_relative_occ=="col"  , "rare",common_rare)]
    
    comparison_surv_one_ser_beta[,sp_relative_beta:=ifelse((occurrence_past+occurrence_recent)<= (5) , "rare","common")]
    comparison_surv_one_ser_beta[,sp_relative_beta:=ifelse((occurrence_past)<= ((N_plot/2)*0.1) , "rare","common")]
    
    
    
    
    bet_test<-comparison_surv_one_ser_beta[,.(N_past=sum(occurrence_past),N=sum(occurrence_recent),raw_contrib=sum(raw_contrib,na.rm=T),beta=sum(delta_beta)),by=.(sp_relative_occ,sp_relative_topt,sp_relative_beta)][order(sp_relative_topt,sp_relative_occ,sp_relative_beta)]#[sp_relative_beta!="stable"]
    bet_test<-bet_test[sp_relative_occ!="stable",]
    bet_test<-bet_test[order(sp_relative_topt, sp_relative_occ),]
    
    decompose_beta_8<-bet_test[,.(N_past=sum(N_past),N=sum(N),contrib_topt=sum(raw_contrib),contrib_beta=sum(beta)),by=.(sp_relative_topt,sp_relative_occ,sp_relative_beta )][sp_relative_occ!="stable",]
    
    #results_8<-dcast.data.table(decompose_beta_8, .~sp_relative_topt+sp_relative_occ+sp_relative_beta,value.var = c("contrib_topt", "contrib_beta"))
    results_8<-dcast.data.table(decompose_beta_8, .~sp_relative_topt+sp_relative_occ+sp_relative_beta,value.var = c("contrib_topt", "contrib_beta"))
    
    
    results_8$.<-NULL
    
    # try(results_8[,topt_contrib_colder_Homogenize :=contrib_topt_colder_ext_Homogenize +contrib_topt_colder_col_Homogenize ])
    # try(results_8[,topt_contrib_colder_Heterogenize :=contrib_topt_colder_ext_Heterogenize +contrib_topt_colder_col_Heterogenize])
    # try(results_8[,topt_contrib_warmer_Homogenize :=contrib_topt_warmer_ext_Homogenize +contrib_topt_warmer_col_Homogenize ])
    # try(results_8[,topt_contrib_warmer_Heterogenize :=contrib_topt_warmer_ext_Heterogenize +contrib_topt_warmer_col_Heterogenize])
    # 
  }
  
  results<-cbind(results,ecopart_four_ser)
  
  ##contribution of every declining species 
  results[,topt_ext:=contrib_topt_colder_ext + contrib_topt_warmer_ext]
  results[,topt_col:=contrib_topt_colder_col + contrib_topt_warmer_col]
  
  results[,beta_ext:=contrib_beta_colder_ext + contrib_beta_warmer_ext]
  results[,beta_col:=contrib_beta_colder_col + contrib_beta_warmer_col]
  
  ##contribution of every locally cold-warl_dapted species  
  results[,topt_warmer:=contrib_topt_warmer_col + contrib_topt_warmer_ext]
  results[,topt_colder:=contrib_topt_colder_col + contrib_topt_colder_ext]
  
  results[,beta_colder:=contrib_beta_colder_ext + contrib_beta_colder_col]
  results[,beta_warmer:=contrib_beta_warmer_ext + contrib_beta_warmer_col]  
  
  # we include the contribution of the exintions and colonization as a whole
  results<-cbind(results,ecopart_two_sp_ser)
  
  
  ## thermophilization and delta beta-diversity
  results[,sp_topt_past:= ser_it_sp_past]
  results[,sp_topt_recent:= ser_it_sp_recent]
  results[,sp_thermo:= warming]
  results[,sp_delta_beta:= sum(ecopart_two_sp_ser[,1:2])]
  
  ## Some diversity metrics and misc values
  results[,delta_alpha:=Alpha2 - Alpha1]
  results[,delta_gamma:=Gamma2 - Gamma1]
  results[,normalized_delta_beta:= 1-(Beta1 /Beta2)]
  results[,sp_tot_past:=sp_tot_past]
  results[,sp_tot_recent:=sp_tot_recent]
  results[,delta_sp_tot:=sp_tot_recent-sp_tot_past]
  results[,normalized_sp_tot:= 1-(sp_tot_past /sp_tot_recent)]
  
  
  results$.<-NULL
  results[,ser:= ser_select[1]]
  results[,greco:=str_extract(ser,"[:alpha:]")] ## large ecoregion name
  
  ## this function will return the data.table of occurrences comparisons 
  table_sp_to_return<-comparison_surv_one_ser_beta
  table_sp_to_return$ser<-ser_select[1]
  table_sp_to_return[,greco:=str_extract(ser,"[:alpha:]")]
  table_sp_to_return$N_plot<-N_plot
  
  
  if(n_comp%in%c(8))results<-cbind(results,results_8)
  
  return(list(results,table_sp_to_return))
  
}


## this function aims to run get_contrib_one_ser mutliple time, in order to bootstrap the cases 
## where thermal optima are randomized or occurrences are rarefied
## if average = T return a data.table of one line corresponding to the average of the simulation for that ecoregion
## same argument as get_contrib_one_ser()
run_contrib_multiple_time<-function(n,ser_select,table_flora,n_comp=4,topt_name="topt_climplant",random_topt=F,rarefy=F,average=T,rarefy_rare_sp = F){
  # the analysis is done n times
  res_boot_one_ser<-foreach(i=1:n,.combine = rbind,.multicombine = T)%do%{
    set.seed(i) # a seed is set for reproducibility
    res<-get_contrib_one_ser(ser_select,table_flora,n_comp,topt_name,random_topt = random_topt,rarefy=rarefy,rarefy_rare_sp=rarefy_rare_sp)[[1]]
    return(res)
  }
  
  average_res<-function(x)if("character"%in%class(x))unique(x) else mean(x)
  out_col<-colnames(res_boot_one_ser)
  res_boot_one_ser<-data.frame(res_boot_one_ser)
  res_boot_one_ser[is.na(res_boot_one_ser)]<- 0
  res_boot_one_ser<-data.table(res_boot_one_ser)
  if(average)res_boot_one_ser<-res_boot_one_ser[,(out_col):=lapply(.SD,average_res)][1,]
  
  return(res_boot_one_ser)
}
