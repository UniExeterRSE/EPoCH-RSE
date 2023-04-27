library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyTree)
library(RColorBrewer)
library(stringr)

set.seed(67483)
ncols <- 190
expanded_dark2 <- sample(colorRampPalette(brewer.pal(8, "Dark2"))(ncols))

create_exposure_manhattan_plot <- function(df){
  adj_pthreshold <- 0.05/nrow(df)
  p <- ggplot(df,
              aes(Estimate=est,P=p,Outcome=outcome_linker,Cohorts=cohorts,N=total_n,
                  x=str_to_sentence(exposure_subclass_time_dose),y=-log10(p)
              ))+
    geom_jitter(aes(colour=exposure_subclass_time_dose),alpha=0.5,size=0.5, show.legend=FALSE)+
    #facet_grid(person_exposed~.)+
    xlab("")+
    theme_classic()+
    scale_colour_manual(values=expanded_dark2)+
    theme(axis.text.x = element_text(angle = 90,hjust=1), legend.position="none")+
    geom_hline(yintercept = -log10(adj_pthreshold),linetype="dashed",colour="grey40")
  ggplotly(p, height = 720)
}

create_hl_exposure_manhattan_plot <- function(df){
  adj_pthreshold <- 0.05/nrow(df)
  p <- ggplot(df,
              aes(Estimate=est,P=p,Outcome=outcome_linker,Cohorts=cohorts,N=total_n,
                  x=str_to_sentence(exposure_class),y=-log10(p)
              ))+
    geom_jitter(aes(colour=exposure_class),alpha=0.5,size=0.5, show.legend=FALSE)+
    #facet_grid(person_exposed~.)+
    xlab("")+
    theme_classic()+
    scale_colour_manual(values=expanded_dark2)+
    theme(axis.text.x = element_text(angle = 90,hjust=1), legend.position="none")+
    geom_hline(yintercept = -log10(adj_pthreshold),linetype="dashed",colour="grey40")
  ggplotly(p, height = 720)
}

create_outcome_manhattan_plot <- function(df){
  adj_pthreshold <- 0.05/nrow(df)
  p <- ggplot(df,
              aes(Estimate=est,P=p,Exposure=exposure_linker,Cohorts=cohorts,N=total_n,
                  x=str_to_sentence(outcome_subclass_time),y=-log10(p)
              ))+
    geom_jitter(aes(colour=outcome_subclass_time),alpha=0.5,size=0.5)+
    #facet_grid(person_exposed~.)+
    xlab("")+
    theme_classic()+
    scale_colour_manual(values=expanded_dark2)+
    theme(axis.text.x = element_text(angle = 90,hjust=1), legend.position="none")+
    geom_hline(yintercept = -log10(adj_pthreshold),linetype="dashed",colour="grey40")
  ggplotly(p, height = 720)
}

create_hl_outcome_manhattan_plot <- function(df){
  adj_pthreshold <- 0.05/nrow(df)
  p <- ggplot(df,
              aes(Estimate=est,P=p,Exposure=exposure_linker,Cohorts=cohorts,N=total_n,
                  x=str_to_sentence(outcome_class),y=-log10(p)
              ))+
    geom_jitter(aes(colour=outcome_class),alpha=0.5,size=0.5)+
    #facet_grid(person_exposed~.)+
    xlab("")+
    theme_classic()+
    scale_colour_manual(values=expanded_dark2)+
    theme(axis.text.x = element_text(angle = 90,hjust=1), legend.position="none")+
    geom_hline(yintercept = -log10(adj_pthreshold),linetype="dashed",colour="grey40")
  ggplotly(p, height = 720)
}

create_volcano_plot <- function(df){
  pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-0.05))]-1
  adj_pthreshold <- 0.05/nrow(df)
  adj_pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-adj_pthreshold))]-1
  Plot <- ggplot(df,
                 aes(Estimate=est,P=p,Outcome=outcome_linker,Cohorts=cohorts,N=total_n,
                     x=est_SDM,y=rank(-log10(p)),Exposure=exposure_linker
                 ))+
    geom_point(aes(colour=outcome_class),size=0.5,alpha=0.5)+
    geom_vline(xintercept = 0,colour="grey40")+
    theme_classic()+
    scale_colour_brewer(palette = "Dark2")+
    xlab("Standardised effect estimate")+
    ylab("Ranked -log10(P)")+
    facet_grid(.~person_exposed)+  
    coord_cartesian(xlim=c(-0.75,0.75))+
    geom_hline(yintercept = pthreshold_rank,linetype="dashed",colour="blue")+
    geom_hline(yintercept = adj_pthreshold_rank,linetype="dashed",colour="red")
  ggplotly(Plot, tooltip=c("P","Estimate","Outcome","Exposure","Cohorts","N"), height = 580)
}


create_exposure_volcano_plot <- function(df){
  pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-0.05))]-1
  adj_pthreshold <- 0.05/nrow(df)
  adj_pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-adj_pthreshold))]-1
  Plot <- ggplot(df,
                 aes(Estimate=est,P=p,Outcome=outcome_linker,Cohorts=cohorts,N=total_n,
                     x=est_SDM,y=rank(-log10(p)),Exposure=exposure_linker
                 ))+
    geom_point(aes(colour=outcome_class),size=0.5,alpha=0.5)+
    geom_vline(xintercept = 0,colour="grey40")+
    theme_classic()+
    scale_colour_brewer(palette = "Dark2")+
    xlab("Standardised effect estimate")+
    ylab("Ranked -log10(P)")+
    facet_grid(.~person_exposed)+  
    coord_cartesian(xlim=c(-0.75,0.75))+
    geom_hline(yintercept = pthreshold_rank,linetype="dashed",colour="blue")+
    geom_hline(yintercept = adj_pthreshold_rank,linetype="dashed",colour="red")
  ggplotly(Plot,tooltip=c("P","Estimate","Outcome","Exposure","Cohorts","N"))
}

# by outcome

create_outcome_volcano_plot <- function(df){
  pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-0.05))]-1
  adj_pthreshold <- 0.05/nrow(df)
  adj_pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-adj_pthreshold))]-1
  Plot <- ggplot(df,
                 aes(Estimate=est,P=p,Outcome=outcome_linker,Cohorts=cohorts,N=total_n,
                     x=est_SDM,y=rank(-log10(p)),Exposure=exposure_linker
                 ))+
    geom_point(aes(colour=exposure_class),size=0.5,alpha=0.5)+
    geom_vline(xintercept = 0,colour="grey40")+
    theme_classic()+
    scale_colour_brewer(palette = "Dark2")+
    xlab("Standardised effect estimate")+
    ylab("Ranked -log10(P)")+
    facet_grid(.~person_exposed)+  
    coord_cartesian(xlim=c(-0.75,0.75))+
    geom_hline(yintercept = pthreshold_rank,linetype="dashed",colour="blue")+
    geom_hline(yintercept = adj_pthreshold_rank,linetype="dashed",colour="red")
  ggplotly(Plot,tooltip=c("P","Estimate","Outcome","Exposure","Cohorts","N"))
}

#######################
# Coef plot functions #
#######################


create_coef_plot_faceted<- function(dat){
  
  dat <- dat[order(dat$comparison,dat$est),]
  dat$outcome_text <- unlist(lapply(strsplit(dat$outcome_linker,split="-"),function(x) paste(x[3:4],collapse = " - ")))
  substr(dat$outcome_text,1,1) <- toupper(substr(dat$outcome_text,1,1))
  dat$outcome_text <- factor(dat$outcome_text,ordered=T,levels=unique(dat$outcome_text))
  
  dat$lcl <- dat$est-(1.96 * dat$se)
  dat$ucl <- dat$est+(1.96 * dat$se)
  
  intercept_n <- 0
  
  if("binary" %in% dat$outcome_type){
    dat[,grep(colnames(dat),pattern="est|ucl|lcl")] <- exp(dat[,grep(colnames(dat),pattern="est|ucl|lcl")])
    intercept_n <- 1
  }
  
  coef_plot <- ggplot(dat,aes(x=est,y=outcome_text,xmin=lcl,xmax=ucl))+
    geom_vline(xintercept = intercept_n,colour="grey36")+
    geom_pointrange()+
    facet_grid(outcome_text~comparison,scales="free",space="free_y")+
    
    xlab("Std Dev. Difference")+ylab("")+
    theme_minimal()+
    theme(strip.text.y= element_text(colour=NA,size=0),
          strip.text.x = element_text(colour = "white",size=10),
          panel.spacing = unit(0.1, "lines"),
          panel.grid=element_blank(),
          panel.background = element_rect(fill="grey90",colour="white"),
          strip.background = element_rect(fill="grey36",colour = "white"))
  
  
  if("binary" %in% dat$outcome_type){
    coef_plot <- coef_plot + xlab("Odds Ratio")
  }
  
  ggplotly(coef_plot)
}


 

create_coef_plot_same_axis<- function(dat){
  
  dat <- dat[order(dat$comparison,dat$est),]
  dat$outcome_text <- unlist(lapply(strsplit(dat$outcome_linker,split="-"),function(x) paste(x[3:4],collapse = " - ")))
  substr(dat$outcome_text,1,1) <- toupper(substr(dat$outcome_text,1,1))
  dat$outcome_text <- factor(dat$outcome_text,ordered=T,levels=unique(dat$outcome_text))
  
  dat$lcl <- dat$est-(1.96 * dat$se)
  dat$ucl <- dat$est+(1.96 * dat$se)
  
  intercept_n <- 0
  
  if("binary" %in% dat$outcome_type){
    dat[,grep(colnames(dat),pattern="est|ucl|lcl")] <- exp(dat[,grep(colnames(dat),pattern="est|ucl|lcl")])
    intercept_n <- 1
  }
  
  coef_plot <- ggplot(dat,aes(x=est,y=outcome_text,xmin=lcl,xmax=ucl))+
    geom_vline(xintercept = intercept_n,colour="grey36")+
    geom_pointrange(aes(shape=comparison,colour=comparison),position = position_dodge(1))+
    facet_grid(outcome_text~.,scales="free",space="free_y")+
  
  xlab("Std Dev. Difference")+ylab("")+
    theme_minimal()+
    theme(strip.text.y= element_blank(),
          strip.text.x = element_text(colour = "white",size=10),
          panel.spacing = unit(0.1, "lines"),
          panel.grid=element_blank(),
          panel.background = element_rect(fill="grey90",colour="white"),
          strip.background = element_rect(fill="grey36",colour = "white"))
  
  
  if("binary" %in% dat$outcome_type){
    coef_plot <- coef_plot + xlab("Odds Ratio")
  }
  
  ggplotly(coef_plot)
}