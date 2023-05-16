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

plot_df_manhattan <- function(fig, df, x_data, label){
  fig <- fig %>%
    add_markers(name = label, x = jitter(as.numeric(as.factor(df[[x_data]])), amount=0.3), y =-log10(df$p),
                color = as.character(df[[x_data]]),
                marker = list(size = 6), alpha=0.5,
                hoverinfo = "text",
                text = paste0("<b>Exposure class</b>: ",df$exposure_class,
                               "<br><b>Exposure type</b>: ",df$exposure_subclass_time_dose,
                               "<br><b>Outcome class</b>: ",df$outcome_class,
                               "<br><b>Outcome type</b>: ",df$outcome_subclass_time,
                               "<br><b>Cohorts</b>: ",df$cohorts,
                               "<br><b>Parent Exposed</b>: ",df$person_exposed,
                               "<br><b>Total N</b>: ",df$total_n,
                               "<br><b>Estimate</b>: ",df$est,
                               "<br><b>p value</b>: ",df$p),
                showlegend = FALSE)
}

create_manhattan_plot <- function(df, height, x_data, x_label){
  adj_pthreshold <- 0.05/nrow(df)
  df_mother <- df[df$person_exposed=="mother",]
  df_father <- df[df$person_exposed=="father",]
  fig <- plot_ly(height = height, colors = graph_colours)
  fig <- plot_df_manhattan(fig, df_mother, x_data, label="Mother")
  fig <- plot_df_manhattan(fig, df_father, x_data, label="Father")
  lmap_mother <- length(unique(df_mother[[x_data]]))
  lmap_father <- length(unique(df_father[[x_data]]))
  fig <- fig %>% layout(shapes = list(hline(-log10(adj_pthreshold))),
                        xaxis = list(title = x_label,
                                     ticktext = str_to_sentence(unique(df[[x_data]])),
                                     tickvals = unique(as.numeric(as.factor(df[[x_data]]))),
                                     tickmode = "array"),
                        updatemenus = list(
                                    list(
                                      active = -1,
                                      type = 'buttons',
                                      buttons = list(
                                        list(label = "Mother",
                                             method = "update",
                                             args = list(list(visible = c(rep(TRUE,lmap_mother), rep(FALSE,lmap_father))))),
                                        list(label = "Father",
                                             method = "update",
                                             args = list(list(visible = c(rep(FALSE,lmap_mother), rep(TRUE,lmap_father))))),
                                        list(label = "Both",
                                             method = "update",
                                             args = list(list(visible = c(TRUE))))
                                      )
                                    )
                                  )

                        ) %>%
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