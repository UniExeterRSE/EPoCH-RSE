library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyTree)
library(RColorBrewer)
library(stringr)

graph_colours = "Dark2"

# Introduce a function to allow horizontal line plotting in plot_ly
hline <- function(y = 0, colour = "#898989") {
  list(type = "line", x0 = 0, x1 = 1,
       xref = "paper", y0 = y, y1 = y,
       line = list(color = colour, dash="dash")
  )
}


create_exposure_manhattan_plotly <- function(df, height){
  adj_pthreshold <- 0.05/nrow(df)
  print
  df %>%
    plot_ly(height = height, colors = graph_colours) %>% 
    add_markers(x = ~jitter(as.numeric(exposure_subclass_time_dose), amount=0.3), y =~-log10(p),
                color = ~as.character(exposure_subclass_time_dose),
                marker = list(size = 6), alpha=0.5,
                hoverinfo = "text",
                text = ~paste0("<b>Exposure class</b>: ",exposure_class,
                               "<br><b>Exposure type</b>: ",exposure_subclass_time_dose,
                               "<br><b>Outcome class</b>: ",outcome_class,
                               "<br><b>Outcome type</b>: ",outcome_subclass_time,
                               "<br><b>Cohorts</b>: ",cohorts,
                               "<br><b>Total N</b>: ",total_n,
                               "<br><b>Estimate</b>: ",est,
                               "<br><b>p value</b>: ",p),
                showlegend = FALSE) %>% 
    layout(shapes = list(hline(-log10(adj_pthreshold))),
           xaxis = list(title = "Exposure type",
                        ticktext = ~str_to_sentence(exposure_subclass_time_dose),
                        tickvals = ~as.numeric(exposure_subclass_time_dose),
                        tickmode = "array")) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
}

create_hl_exposure_manhattan_plotly <- function(df, height){
  adj_pthreshold <- 0.05/nrow(df)
  df %>%
    plot_ly(height = height, colors = graph_colours) %>% 
    add_markers(x = ~jitter(as.numeric(as.factor(exposure_class)), amount=0.3), y =~-log10(p), color = ~exposure_class,
                marker = list(size = 6), alpha=0.5,
                hoverinfo = "text",
                text = ~paste0("<b>Exposure class</b>: ",exposure_class,
                               "<br><b>Exposure type</b>: ",exposure_subclass_time_dose,
                               "<br><b>Outcome class</b>: ",outcome_class,
                               "<br><b>Outcome type</b>: ",outcome_subclass_time,
                               "<br><b>Cohorts</b>: ",cohorts,
                               "<br><b>Total N</b>: ",total_n,
                               "<br><b>Estimate</b>: ",est,
                               "<br><b>p value</b>: ",p),
                showlegend = FALSE) %>% 
    layout(shapes = list(hline(-log10(adj_pthreshold))),
           xaxis = list(title = "Exposure class",
                        ticktext = ~str_to_sentence(exposure_class),
                        tickvals = ~as.numeric(as.factor(exposure_class)),
                        tickmode = "array")) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
}

create_outcome_manhattan_plotly <- function(df, height){
  adj_pthreshold <- 0.05/nrow(df)
  print(df)
  print(as.numeric(unique(df$outcome_subclass_time)))
  df %>%
    plot_ly(height = height, colors = graph_colours) %>%   
    add_markers(x = ~jitter(as.numeric(outcome_subclass_time)), y =~-log10(p),
                color = ~as.character(outcome_subclass_time),
                marker = list(size = 6), alpha=0.5,
                hoverinfo = "text",
                text = ~paste0("<b>Exposure class</b>: ",exposure_class,
                               "<br><b>Exposure type</b>: ",exposure_subclass_time_dose,
                               "<br><b>Outcome class</b>: ",outcome_class,
                               "<br><b>Outcome type</b>: ",outcome_subclass_time,
                               "<br><b>Cohorts</b>: ",cohorts,
                               "<br><b>Total N</b>: ",total_n,
                               "<br><b>Estimate</b>: ",est,
                               "<br><b>p value</b>: ",p),
                showlegend = FALSE) %>% 
    layout(shapes = list(hline(-log10(adj_pthreshold))),
           xaxis = list(title = "Outcome type",
                        ticktext = ~str_to_sentence(outcome_subclass_time),
                        tickvals = ~as.numeric(outcome_subclass_time),
                        tickmode = "array")) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
}

create_hl_outcome_manhattan_plotly <- function(df, height){
  adj_pthreshold <- 0.05/nrow(df)
  df %>%
    plot_ly(height = height, colors=graph_colours) %>% 
    add_markers(x = ~jitter(as.numeric(as.factor(outcome_class))), y =~-log10(p), color = ~outcome_class,
                marker = list(size = 6), alpha=0.5,
                hoverinfo = "text",
                text = ~paste0("<b>Exposure class</b>: ",exposure_class,
                               "<br><b>Exposure type</b>: ",exposure_subclass_time_dose,
                               "<br><b>Outcome class</b>: ",outcome_class,
                               "<br><b>Outcome type</b>: ",outcome_subclass_time,
                               "<br><b>Cohorts</b>: ",cohorts,
                               "<br><b>Total N</b>: ",total_n,
                               "<br><b>Estimate</b>: ",est,
                               "<br><b>p value</b>: ",p),
                showlegend = FALSE) %>% 
    layout(shapes = list(hline(-log10(adj_pthreshold))),
           xaxis = list(title = "Outcome class",
                        ticktext = ~str_to_sentence(outcome_class),
                        tickvals = ~as.numeric(as.factor(outcome_class)),
                        tickmode = "array")) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
}

create_exposure_box_plotly <- function(df){
  adj_pthreshold <- 0.05/nrow(df)
  df %>%
    plot_ly(height = 600, colors=graph_colours) %>% 
    add_trace(x = ~as.numeric(exposure_subclass_time_dose),y = ~-log10(p), color = ~exposure_subclass_time_dose,
              type = "box", 
              hoverinfo = "text",
              text = ~paste0("<b>Exposure class</b>: ",exposure_class,
                               "<br><b>Exposure type</b>: ",exposure_subclass_time_dose,
                               "<br><b>Outcome class</b>: ",outcome_class,
                               "<br><b>Outcome type</b>: ",outcome_subclass_time,
                               "<br><b>Cohorts</b>: ",cohorts,
                               "<br><b>Total N</b>: ",total_n,
                               "<br><b>Estimate</b>: ",est,
                               "<br><b>p value</b>: ",p),
              showlegend = FALSE) %>%
    layout(shapes = list(hline(-log10(adj_pthreshold))),
           xaxis = list(title = "Exposure type",
                        ticktext = ~str_to_sentence(exposure_subclass_time_dose),
                        tickvals = ~as.numeric(exposure_subclass_time_dose),
                        tickmode = "array")) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
}

create_volcano_plot <- function(df){
  pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-0.05))]-1
  adj_pthreshold <- 0.05/nrow(df)
  adj_pthreshold_rank <- rank(-log10(df$p))[which.min(abs(df$p-adj_pthreshold))]-1

  ttext <- str_to_sentence(unique(df$person_exposed))
  df %>%
    plot_ly(height = 540, colors=graph_colours) %>%
    add_markers(x = ~est_SDM,y = ~rank(-log10(p)), color = ~outcome_class,
              marker = list(size = 6), alpha=0.5,
              hoverinfo = "text",
              text = ~paste0("<b>Exposure class</b>: ",exposure_class,
                             "<br><b>Exposure type</b>: ",exposure_subclass_time_dose,
                             "<br><b>Outcome class</b>: ",outcome_class,
                             "<br><b>Outcome type</b>: ",outcome_subclass_time,
                             "<br><b>Cohorts</b>: ",cohorts,
                             "<br><b>Total N</b>: ",total_n,
                             "<br><b>Estimate</b>: ",est,
                             "<br><b>p value</b>: ",p),
              showlegend = FALSE) %>% 
    add_annotations(text = ttext,
                    x = 0.5,
                    y = 1,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "left",
                    yanchor = "top",
                    showarrow = FALSE) %>%
    layout(xaxis = list(title = "Standardised effect estimate",
                        range = list(-0.75, 0.75)),
           yaxis = list(title = "Ranked -log10(P)",
                        rangemode = "tozero")) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
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
    scale_colour_brewer(palette = graph_colours)+
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
    scale_colour_brewer(palette = graph_colours)+
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