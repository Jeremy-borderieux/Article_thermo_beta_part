
#### fig 2 ####


##initial data formating for the histogram
list_sp_contrib[,topt_class:=cut(topt_climplant,1:16)]
x_scale<-seq(1,16,1)
dt_occ_hist<-list_sp_contrib[,.(occ_past=sum(occurrence_past),occ_recent=sum(occurrence_recent)),by=.(topt_class)]
dt_occ_hist<-dt_occ_hist[order(topt_class),]
dt_occ_hist[,x:=x_scale[c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)]]
dt_occ_hist<-rbind(dt_occ_hist,data.table(topt_class=NA,occ_past=0,occ_recent=0,x=14))


export_histogram<-ggplot(list_sp_contrib,aes(x=topt_climplant),alpha=0.5)+
  geom_histogram(aes(weight=occurrence_past,fill="past"),color="grey60",alpha=0.5,breaks=x_scale)+ # the aes() weight argument allows to create the count of species ourselves
  geom_histogram(aes(weight=occurrence_recent*(1/1),fill="recent"),color="gold4",alpha=0.5,breaks=x_scale)+
  geom_step(data=dt_occ_hist,aes(x=x,y=occ_past),lty=2,linewidth=0.5)+
  geom_step(data=dt_occ_hist,aes(x=x,y=occ_recent),lty=1,linewidth=0.5)+
  #geom_vline(lty=1,color=c("salmon4","gold1"),linewidth=0.9,xintercept = c(list_sp_contrib[,weighted.mean(topt_climplant,occurrence_past)],list_sp_contrib[,weighted.mean(topt_climplant,occurrence_recent)]))+
  theme_bw()+
  labs(fill="Plot period",y="Species occurrences",x="Thermal optimum of a species")+
  scale_x_continuous(breaks = seq(1,18,2),limits = c(1,18))+
  theme(legend.position = c(0.8, 0.5),legend.background = element_rect(fill = "white", color = "grey10"))+
  guides(fill=guide_legend(override.aes = list(lty=c(2,1),color="black",linewidth=0.5,alpha=0.5)))+
  scale_fill_manual(breaks =c("past","recent"),
                    values=c("salmon4","gold1","peru","indianred4"),
                    label=c(paste0("Past period (2005-2011) \nmean Thermal opt. :",signif(list_sp_contrib[,weighted.mean(topt_climplant,occurrence_past)],3)),
                            paste0("Recent period (2015-2021) \nmean Thermal opt. :",signif(list_sp_contrib[,weighted.mean(topt_climplant,occurrence_recent)],3),"0")))

showtext:: showtext_auto(enable = F)
ggsave(file.path("Figures_results","fig_hist_main.png"),export_histogram,dpi=400,scale = 1.7,width = 80,height = 60,units = "mm",)



list_sp_contrib[,topt_class:=cut(topt_climplant,1:16)]
dt_occ_hist<-list_sp_contrib[,.(occ_past=sum(occurrence_past),occ_recent=sum(occurrence_recent)),by=.(greco,topt_class)]
dt_occ_hist<-dt_occ_hist[order(topt_class),]
dt_occ_hist[,x:=as.numeric(word(str_remove(as.character(dt_occ_hist$topt_class),"[(]"),sep=","))]
dt_occ_hist<-rbind(dt_occ_hist,data.table(greco=c("A","B","C","D","E","F","G","H","I","J"),topt_class=NA,occ_past=0,occ_recent=0,x=14))

supplementary_histogram<-ggplot(list_sp_contrib,aes(x=topt_climplant),alpha=0.5)+
  geom_histogram(aes(weight=occurrence_past,fill="past"),color="grey60",alpha=0.5,breaks=x_scale)+
  geom_histogram(aes(weight=occurrence_recent*(1/1),fill="recent"),color="gold4",alpha=0.5,breaks=x_scale)+
  geom_step(data=dt_occ_hist,aes(x=x,y=occ_past),lty=2,linewidth=0.3)+
  geom_step(data=dt_occ_hist,aes(x=x,y=occ_recent),lty=1,linewidth=0.3)+
  theme_classic()+
  facet_wrap(~greco,scales="free_y")+
  labs(fill="Plot period",y="occurrence of a species",x="Thermal optimum of a species")+
  scale_x_continuous(breaks = seq(1,16,2))+
  theme(legend.position = c(0.79, 0.18),
        legend.background = element_rect(fill = "white", color = "grey10"),
        strip.background = element_rect(fill="grey85"))+
  guides(fill=guide_legend(override.aes = list(lty=c(2,1),color="black",linewidth=0.5,alpha=0.5)))+
  scale_fill_manual(breaks =c("past","recent"),
                    values=c("salmon4","gold1","peru","indianred4"),
                    label=c(paste0("Past period (2005-2011)  "),
                            paste0("Recent period (2015-2021)  ")))

showtext:: showtext_auto(enable = F)
ggsave(file.path("Figures_results","fig_hist_suppl.png"),supplementary_histogram,dpi=600,scale = 1.25,width = 180,height = 120,units = "mm",)

#### fig 3 ####


## helper function for facet label
facet_labeller <- function(variable,value)return(dt_label_color_reference[variable==value,label])

##function that create a row of the main figure (Fig.3) if given the corresonding components
create_one_box<-function(var, # vectors of one or several components
                         response="thermo", # thermo or beta
                         lims=c(-0.35,0.6), # the Y axis limit
                         dt=contrib_therm_beta_ser, # the dataset
                         dt_sim=bootstrap_random_topt){ # the dataset of a null model for significance testing. here delta beta is not tested against a null model but 0
  var_save<-var
  first_var<-var[1]
  list_plot<-list()
  for(i in var){
    ## from wide to long data.table
    data<-suppressWarnings(melt(dt,id.vars="ser",verbose =F)[variable%in%i,])
    data_simu<-suppressWarnings(melt(dt_sim,id.vars="ser",verbose =F)[variable%in%i,])
    
    ## mean valu of the component
    mean_val<-round(mean(as.numeric(data$value),na.rm=T),2)
    mean_val_simu<-ifelse(response=="beta" && length(var)!=8,0,round(mean(as.numeric(data_simu$value),na.rm=T),2))
    
    ## we use the wilcox.test to h0= no difference between the null model (thermo) or 0 (delta beta)
    ## When we test the 8 delta_beta components, the value is constrained too, so we test it against a null model too instead of 0
    p_val<-if(response=="beta" && length(var)!=8) wilcox.test(as.numeric(data$value))$p.value else wilcox.test(as.numeric(data$value), as.numeric(data_simu$value))$p.value
    signif_star <- NA                            
    signif_star[p_val < 0.05] <- "*"
    signif_star[p_val < 0.01] <- "**"
    signif_star[p_val < 0.001] <- "***"
    signif_star[is.na(signif_star)] <- "n.s."
    
    
    res<-ggplot(data,aes(x=variable,y=as.numeric(value),fill=variable))+
      geom_hline(yintercept = mean_val_simu,lty=2)+
      geom_jitter(shape=16,alpha=0.6,mapping=aes(color=variable),show.legend = F)+
      geom_boxplot(alpha=0.6,outlier.shape = NA,show.legend = F,linewidth=0.3)+
      theme_bw()+
      ## we make the label appear only once with this command, and we display for the 8 comp figure too
      labs(x="",y=if(length(var)%in%c(2,8)) if(response=="thermo")"Thermophilization °C/ 10years" else "\U0394\U03B2-diversity" else " ")+
      facet_wrap(~variable,labeller = facet_labeller)+
      coord_cartesian(ylim =lims)+
      annotate("text", x=0.45, y=lims[2]-0.2*lims[2],hjust=0, label=paste0("µ = ",mean_val," ",signif_star),size=3.5, color = "grey15")+
      theme(strip.text.x = element_text(size = if(length(var)==8) 8 else 10,face="bold"))
    
    # change the number of signif numbers in the Y axis
    add_signif<-function(x){
      x<-as.character(x)
      x<-ifelse(response=="beta" & x!="0",paste0(x,".0"),x)
      x
    }
    
    res<-res+
      scale_y_continuous(labels =add_signif)+
      scale_discrete_manual(aesthetics = c("colour", "fill"),
                            values=dt_label_color_reference$color,breaks=dt_label_color_reference$variable)
    
    # adjustement for several rows with different numbers of plot to match smoothly, and adjust the size of the boxplot
    res<-res +theme(axis.line.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
                    plot.margin =  margin(0,0,0,0),
                    panel.grid.major.x=element_blank(),
                    strip.background =element_rect(fill=if(response=="thermo")"#EAE397" else "#C5D49A"),
                    strip.text = element_text(margin = margin(0.1,0,0.1,0, "cm")))#+scale_x_discrete(expand = expansion(0.05,0.75))
    
    if(i != first_var)res<-res+theme(axis.title.y =  element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank())
    if(length(var)==1)res<-res+scale_x_discrete(expand = expansion(0,0.75)) # we widen the first box
    
    
    list_plot[[i]]<-res+theme(plot.background = element_rect( fill = NA,colour = NA,linewidth = 1))
  }
  w<-if(length(var)==2) c(1.137,1)else if(length(var)==4)c(1.27,1,1,1) else if(length(var)==8)c(1.45,rep(1,7)) else 1
  
  return(ggarrange(plotlist=list_plot,ncol=length(var),align = "h",widths = w))# we arrange the several plots created into one row
  
}

thermo_lims<-c(-0.35,0.6)/10
beta_lims<-c(-3,4)



main_plot<-ggarrange(labels=c("a)","","","b)","",""),
                     create_one_box(dt=contrib_therm_beta_ser,response = "thermo",lims = thermo_lims,c("sp_thermo")),
                     create_one_box(dt=contrib_therm_beta_ser,response = "thermo",lims = thermo_lims,c("topt_ext","topt_col"))+theme_void(),
                     create_one_box(dt=contrib_therm_beta_ser,response = "thermo",lims = thermo_lims,c("contrib_topt_colder_ext","contrib_topt_warmer_ext" ,"contrib_topt_colder_col" ,"contrib_topt_warmer_col")),
                     create_one_box(dt=contrib_therm_beta_ser,response = "beta",lims = beta_lims,c("contrib_beta_colder_ext","contrib_beta_warmer_ext" ,"contrib_beta_colder_col" ,"contrib_beta_warmer_col")),
                     create_one_box(dt=contrib_therm_beta_ser,response = "beta",lims = beta_lims,c("beta_ext","beta_col")),
                     create_one_box(dt=contrib_therm_beta_ser,response = "beta",lims = beta_lims,c("sp_delta_beta")),
                     nrow=6,label.x = c(-0.01,0,0,-0.01,0,0),align = "hv"
)+theme(plot.margin = margin(1,1,1,1,unit = "mm"))

main_plot


suppl_plot_same_occ<-ggarrange(labels=c("a)","","","b)","",""),
                               create_one_box(dt=bootstrap_same_n_occurrence,response = "thermo",lims = thermo_lims,c("sp_thermo")),
                               create_one_box(dt=bootstrap_same_n_occurrence,response = "thermo",lims = thermo_lims,c("topt_ext","topt_col"))+theme_void(),
                               create_one_box(dt=bootstrap_same_n_occurrence,response = "thermo",lims = thermo_lims,c("contrib_topt_colder_ext","contrib_topt_warmer_ext" ,"contrib_topt_colder_col" ,"contrib_topt_warmer_col")),
                               create_one_box(dt=bootstrap_same_n_occurrence,response = "beta",lims = beta_lims,c("contrib_beta_colder_ext","contrib_beta_warmer_ext" ,"contrib_beta_colder_col" ,"contrib_beta_warmer_col")),
                               create_one_box(dt=bootstrap_same_n_occurrence,response = "beta",lims = beta_lims,c("beta_ext","beta_col")),
                               create_one_box(dt=bootstrap_same_n_occurrence,response = "beta",lims = beta_lims,c("sp_delta_beta")),
                               nrow=6,label.x = c(-0.01,0,0,-0.01,0,0)
)+theme(plot.margin = margin(1,1,1,1,unit = "mm"))



showtext:: showtext_auto(enable = TRUE)
ggsave(file.path("Figures_results","main_figure.pdf"),main_plot,width =180,height =145,units = "mm")
ggsave(file.path("Figures_results","suppl_main_figure.pdf"),suppl_plot_same_occ,width =180,height =145,units = "mm")
showtext:: showtext_auto(enable = FALSE) 
ggsave(file.path("Figures_results","main_figure.jpg"),main_plot,width =180,height =145,units = "mm",dpi=450)
ggsave(file.path("Figures_results","suppl_main_figure.jpg"),suppl_plot_same_occ,width =180,height =145,units = "mm",dpi=450)
showtext:: showtext_auto(enable = TRUE)












## We use the function used for the Main figure here to create 8 boxplots
Extended_figure_thermo<-create_one_box(dt=contrib_therm_beta_ser_8,
                                       dt_sim=bootstrap_random_topt,
                                       response = "thermo",lims = c(-0.35,0.60),var=name_8_comp_topt[c(3,4,7,8,2,1,6,5)])# reordering the label to match figure 3

Extended_figure_beta<-create_one_box(dt=contrib_therm_beta_ser_8,
                                     dt_sim=bootstrap_random_topt,
                                     response = "beta",lims = c(-4,3.8),var=name_8_comp_beta[c(3,4,7,8,2,1,6,5)])


Extended_figure_export<-ggarrange(Extended_figure_thermo,Extended_figure_beta,nrow=2,labels = c("a)","b)"),align = "v")+theme(plot.background = element_rect(fill="white",color = NA),plot.margin=margin(3,3,3,3))

library(svglite) # needed to export svg

ggsave(file.path("Figures_results","Figure_8_comp.png"),Extended_figure_export,units ="mm",width = 180,height =90,scale=1.25)
ggsave(file.path("Figures_results","Figure_8_comp.svg"),Extended_figure_export,units ="mm",width = 180,height =90,scale=1.25)



#### light trait check ####


traits_past<-list_sp_contrib[,.(thermo=weighted.mean(topt_climplant,occurrence_past),
                                mean_n=weighted.mean(N_ellenberg,occurrence_past,na.rm=T),
                                ph=weighted.mean(vi_pH,occurrence_past,na.rm=T),
                                prairie=weighted.mean(indFor_Chytry,occurrence_past,na.rm=T),
                                light=weighted.mean(L_Ellenberg,occurrence_past ,na.rm=T)),by=ser][order(ser)]


traits_recent<-list_sp_contrib[,.(thermo=weighted.mean(topt_climplant,occurrence_recent ),
                                  mean_n=weighted.mean(N_ellenberg,occurrence_recent ,na.rm=T),
                                  ph=weighted.mean(vi_pH,occurrence_recent ,na.rm=T),
                                  prairie=weighted.mean(indFor_Chytry,occurrence_recent ,na.rm=T),
                                  light=weighted.mean(L_Ellenberg,occurrence_recent ,na.rm=T)),by=ser][order(ser)]

traits_recent[,-"greco"] - traits_past[,-"greco"]

summary(traits_recent[,-"ser"] - traits_past[,-"ser"])


diff<-data.table(traits_recent[,-"ser"] - traits_past[,-"ser"])


wilcox.test( data.table(traits_recent[,-"ser"] - traits_past[,-"ser"])$light)
mean(diff$thermo)

ggplot(diff,aes(x=light,y=thermo))+theme_bw()+geom_point()+geom_smooth(method = "lm")
ggplot(diff,aes(x=prairie,y=thermo))+theme_bw()+geom_point()+geom_smooth(method = "lm")


ggplot(list_sp_contrib,aes(x=L_Ellenberg,y=topt_climplant,weight=occurrence_total))+geom_point()+geom_smooth(method="gam")

summary(lm(thermo~light,data=diff))

#### climplant uncertainty bootstrap ####
bootstrap_uncertain_topt<-readRDS(file.path("Saved_computation","bootstrap_uncertain_topt.RData"))

## incorporating the uncertainties in thermal optimum estimation Rodríguez-Sánchez et al, 2012
clust_contrib<-makeCluster(n_cores_to_use)
registerDoParallel(clust_contrib)
bootstrap_uncertain_topt<-foreach(sertest=unique(pairs_ser$ser),
                                  .combine = rbind,.multicombine = T,.errorhandling = "remove",.packages = c("data.table","stringr","foreach"))%dopar%{
                                    res<- run_contrib_multiple_time(500,sertest,table_flora,n_comp = 4,rarefy = F,random_topt = "error",average = T)
                                    cat(paste0(sertest," -> "))
                                    return(res)}

stopCluster(clust_contrib)


bootstrap_uncertain_topt

####  histogram for different ecoreg ####



summary(lm(log(occ)~topt_climplant,data=summed_sp_occ))
summary(lm(n_delta_occ~log(past_occ)+topt_climplant,data=summed_sp_occ[past_occ>1,]))

summary(lm(n_delta_occ~past_occ+topt_climplant,data=summed_sp_occ[past_occ<500 & past_occ>10,]))


list_sp_contrib[,large_nplot:=ser%in%contrib_therm_beta_ser[n_plot>208,ser] ]

ggplot(list_sp_contrib[],aes(x=occurrence_recent,y=ifelse(sp_relative_occ=="col",-delta_beta,delta_beta)))+geom_point(size=0.5)+geom_smooth()+scale_x_continuous(trans="log",breaks = c(0,1,10,100,1000,10000))+geom_hline(yintercept = 0,col="grey50")+theme_bw()+
  facet_wrap(~large_nplot)


list_sp_contrib_small_melt<-melt.data.table(list_sp_contrib,measure.vars = c("occurrence_past" , "occurrence_recent"))


ggplot(list_sp_contrib[greco=="J"],aes(x=topt_climplant))+
  geom_histogram(position="origin",mapping=aes(weight=occurrence_past,color="Past"))+
  geom_histogram(position="origin",mapping=aes(weight=occurrence_recent*0.5,color="Recent"))+
  theme_bw()

ggplot(list_sp_contrib_small_melt[greco=="J"],aes(x=topt_climplant,y=..scaled..,weight=value,fill=variable))+
  geom_density(alpha=0.5,position = "identity",color="grey50",adjust=1.25)+
  theme_bw()

ggplot(list_sp_contrib_small_melt,aes(x=topt_climplant,y=..scaled..,weight=value,fill=variable))+
  geom_density(alpha=0.5,position = "identity",color="grey50",adjust=1.25)+
  theme_bw()+
  facet_wrap(~greco)


ggplot(list_sp_contrib_small_melt[greco=="J"],aes(x=topt_climplant,y=..scaled..,weight=value,fill=variable))+
  geom_density(alpha=0.5,position = "identity",color="grey50",adjust=0.75)+
  theme_bw()+
  facet_wrap(~ser)


contrib_therm_beta_ser[,sum(n_plot),by=ser]

ggplot(list_sp_contrib_small_melt[ser=="J23"],aes(x=topt_climplant,y=..scaled..,weight=value,fill=variable))+
  geom_density(alpha=0.5,position = "identity",color="grey50",adjust=0.75)+
  theme_bw()


sd_table<-list_sp_contrib[,.(sd_past=sd(rep(topt_climplant,occurrence_past)),
                             sd_recent=sd(rep(topt_climplant,occurrence_recent))),by=ser][order(ser),]
sd_table[,diff:= sd_recent- sd_past]

library(sn)
greco_of_interest<-"J"

summary(selm(topt_climplant ~ 1, data=list_sp_contrib[greco==greco_of_interest],weights = list_sp_contrib[greco==greco_of_interest]$occurrence_past))

summary(selm(topt_climplant ~ 1, data=list_sp_contrib[greco==greco_of_interest],weights = list_sp_contrib[greco==greco_of_interest]$occurrence_recent))



list_sp_contrib[,common_rare:=ifelse(sp_relative_beta == "Homogenize" & sp_relative_occ=="ext"  , "rare","common")]
list_sp_contrib[,common_rare:=ifelse(sp_relative_beta == "Heterogenize" & sp_relative_occ=="col"  , "rare",common_rare)]


list_sp_contrib[common_rare=="rare",print(hist(occurrence_past,main=unique(greco))),by=.(common_rare,greco)][common_rare=="rare"]
list_sp_contrib[common_rare=="rare",print(hist(occurrence_total,main=unique(greco))),by=.(common_rare,greco)][common_rare=="rare"]

list_sp_contrib[common_rare=="common" & occurrence_total<10 ,print(hist(occurrence_past,main=unique(greco))),by=.(common_rare,greco)][common_rare=="rare"]
list_sp_contrib[common_rare=="common" & occurrence_total<20,print(hist(occurrence_total,main=unique(greco),breaks=0:20)),by=.(common_rare,greco)][common_rare=="rare"]

list_sp_contrib[common_rare=="rare",.(mean(occurrence_past),mean(occurrence_recent),mean(occurrence_total)),by=greco][order(greco),]
list_sp_contrib[common_rare=="rare",.(min(occurrence_past),min(occurrence_recent),min(occurrence_total)),by=greco][order(greco),]
list_sp_contrib[common_rare=="rare",.(max(occurrence_past),max(occurrence_recent),max(occurrence_total)),by=greco][order(greco),]


list_sp_contrib[,.(sum(raw_contrib)),by=(ser)][,mean(V1)]
list_sp_contrib[sp_relative_occ!="stable",.(sum(raw_contrib)),by=.(sp_relative_topt,sp_relative_occ,occurrence_past<5,ser)][1:16,]
list_sp_contrib[sp_relative_occ!="stable",.(sum(raw_contrib),sum(delta_beta)),by=.(sp_relative_topt,sp_relative_occ,occurrence_past<5,ser)][1:16,]

list_sp_contrib[sp_relative_occ!="stable" &greco=="J" ,.(sum(raw_contrib),sum(delta_beta)),by=.(sp_relative_topt,sp_relative_occ,occurrence_past<5,ser)][1:16,]


list_sp_contrib[occurrence_total==3 & sp_relative_occ!="stable",table(common_rare)]


list_sp_contrib[occurrence_past==5 & sp_relative_occ!="stable",table(common_rare)]

