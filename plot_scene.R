library(rjson)
library(reshape2)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(plotly)
library(classInt)
library("styler")
library(lubridate)
library(httpgd)
library(RColorBrewer)
source("get_time_policy_plot.R")
source("get_time_multi_policy_plot.R")
#
plot_scene_01 <-
  function(policy_file_name = "light_traffic_policy.csv",
           controlled_sol_file_name = "controlled_solution.csv",
           contrafactual_sol_file_name = "reference_solution.csv",
           fig_file_name = "figure_01.html") {
    #
    policy <- 
      read_csv(policy_file_name, show_col_types = FALSE)
    controlledSolution <- 
      read_csv(controlled_sol_file_name, show_col_types = FALSE)
    df <- 
      read_csv(contrafactual_sol_file_name, show_col_types = FALSE)
    colNames <- names(df)[-1] ## assuming date is the first column
    colNames <- colNames[-8]
    colors_counterfact <- setNames(
      c(
        '#7a7fc97f',
        '#7abeaed4',
        '#7afdc086',
        '#7affff99',
        '#7a386cb0',
        '#7af0027f',
        '#7abf5b17',
        '#7a666666'
      ), colNames
    )
    colors_controlled <- setNames(
      c(
        '#4dbeaed4',
        '#4dfdc086',
        '#4dffff99',
        '#4d386cb0',
        '#4df0027f',
        '#4dbf5b17',
        '#4d666666',
        '#4d7fc97f'
      ), colNames
    )
    #
    line_type <- setNames(c(
      "solid", "solid",
      "solid", "solid",
      "solid", "solid",
      "solid", "solid"), colNames)
    #
    fig_1 <- plot_ly(
      df,
      type = "scatter",
      mode = "none"
    ) %>%
      add_trace(
        x = ~date,
        y = ~xI,
        mode = "lines",
        line =
          list(
            color = colors_counterfact[["xI"]],
            dash = line_type[["xI"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_counterfact[["xI"]],
        showlegend = TRUE,
        name = "NC Prevalence"
      )
    #
    fig_1 <-
      fig_1 %>%
      add_trace(
        data = controlledSolution,
        x = ~date,
        y = ~xI,
        mode = "lines",
        line =
          list(
            color = colors_controlled[["xI"]],
            dash = line_type[["xI"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_controlled[["xI"]],
        showlegend = TRUE,
        name = "WC Prevalence"
      )
    #
    fig_2 <- plot_ly(
      df,
      type = "scatter",
      mode = "none"
    ) %>%
      add_trace(
        x = ~date,
        y = ~R_t,
        mode = "lines",
        line =
          list(
            color = colors_counterfact[["R_t"]],
            dash = line_type[["R_t"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_counterfact[["R_t"]],
        showlegend = TRUE,
        name = "NC R_t"
      )
    #
    fig_2 <- fig_2 %>%
      add_trace(
        data = controlledSolution,
        x = ~date,
        y = ~R_t,
        mode = "lines",
        line =
          list(
            color = colors_controlled[["R_t"]],
            dash = line_type[["R_t"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_controlled[["R_t"]],
        showlegend = TRUE,
        name = "WC R_t"
      )
    #
    fig_3 <- plot_ly(
      df,
      type = "scatter",
      mode = "none"
    ) %>%
      add_trace(
        data = df,
        x = ~date,
        y = ~xC,
        mode = "lines",
        line =
          list(
            color = colors_counterfact[["xC"]],
            dash = line_type[["xC"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_counterfact[["xC"]],
        showlegend = TRUE,
        name = "NC Risk"
      )
    fig_3 <- fig_3 %>%
      add_trace(
        data = controlledSolution,
        x = ~date,
        y = ~xC,
        mode = "lines",
        line =
          list(
            color = colors_controlled["xC"],
            dash = line_type[["xC"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_controlled["xC"],
        showlegend = TRUE,
        name = "WC Risk"
      )
    #
    fig_4 <- get_time_policy_plot()
    # subplot
    fig <-
      subplot(
        fig_1, fig_2, fig_3, fig_4,
        nrows = 4,
        heights = c(0.3, 0.3, 0.3, 0.1 ),
        shareX = TRUE
         #titleY=TRUE,
         #titleX=FALSE,
         #margin = 0.1
      )
    fig <-
      fig %>%
      layout(
        title = "Contrafactual vs controlled dynamics",
        legend = list(orientation = 'h')
      )
    htmlwidgets::saveWidget(as_widget(fig), "figure01.html")

    if (!require("processx")) {
      install.packages("processx")
    }
    # orca(fig, "figure_01.png")
    save_image(fig, "figure_01.png", width = 1417, height = 875.7726)
    return(fig)
  }
#
#
#
plot_scene_02 <-
  function(policy_file_name = "light_traffic_policy.csv",
           controlled_sol_file_name = "controlled_solution.csv",
           contrafactual_sol_file_name = "reference_solution.csv",
           fig_file_name = "figure.html") {
    #
    policy <- 
      read_csv(policy_file_name, show_col_types = FALSE)
    controlledSolution <- 
      read_csv(controlled_sol_file_name, show_col_types = FALSE)
    refSol <- 
      read_csv(contrafactual_sol_file_name, show_col_types = FALSE)
    colNames <- names(refSol)[-1] ## assuming date is the first column
    colNames <- colNames[-8]
    colors_counterfact <- setNames(
      c(
        '#7a7fc97f',
        '#7abeaed4',
        '#7afdc086',
        '#7affff99',
        '#7a386cb0',
        '#7af0027f',
        '#7abf5b17',
        '#7a666666'
      ), colNames
    )
    colors_controlled <- setNames(
      c(
        '#4dbeaed4',
        '#4dfdc086',
        '#4dffff99',
        '#4d386cb0',
        '#4df0027f',
        '#4dbf5b17',
        '#4d666666',
        '#4d7fc97f'
      ), colNames
    )
    #
    line_type <- setNames(c(
      "solid", "solid",
      "solid", "solid",
      "solid", "solid",
      "solid", "solid"), colNames
    )
    fig_1 <- plot_ly(
      refSol,
      type = "scatter",
      mode = "none"
    ) %>%
      add_trace(
        data = refSol,
        x = ~date,
        y = ~xF,
        mode = "lines",
        line =
          list(
            color = colors_counterfact[["xF"]],
            dash = line_type[["xF"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_counterfact[["xF"]],
        showlegend = TRUE,
        name = "NC Incidence"
      )
    fig_1 <- fig_1 %>%
      add_trace(
        data = controlledSolution,
        x = ~date,
        y = ~xF,
        mode = "lines",
        line =
          list(
            color = colors_controlled[["xF"]],
            dash = line_type[["xF"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_controlled[["xF"]],
        showlegend = TRUE,
        name = "WC Incidence"
      )
    #
    fig_2 <- plot_ly(
      refSol,
      type = "scatter",
      mode = "none"
    ) %>%
      add_trace(
        data = refSol,
        x = ~date,
        y = ~xJ,
        mode = "lines",
        line =
          list(
            color = colors_counterfact[["xJ"]],
            dash = line_type[["xJ"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_counterfact[["xJ"]],
        showlegend = TRUE,
        name = "NC Sol. Cost"
      )
    fig_2 <- fig_2 %>%
      add_trace(
        data = controlledSolution,
        x = ~date,
        y = ~xJ,
        mode = "lines",
        line =
          list(
            color = colors_controlled[["xJ"]],
            dash = line_type[["xJ"]]
          ),
        fill = 'tozeroy',
        fillcolor = colors_controlled[["xJ"]],
        showlegend = TRUE,
        name = "WC Sol. Cost"
      )
    fig_3 <- get_time_policy_plot()
    # subplot
    fig <-
      subplot(
        fig_1, fig_2, fig_3,
        nrows = 3,
        heights = c(0.45, 0.45, 0.1 ),
        shareX = TRUE
        # titleY=TRUE,
        # titleX=FALSE,
        # margin = 0.1
      )
    fig <-
      fig %>%
      layout(
        title = "Contrafactual vs controlled dynamics",
        legend = list(orientation = 'h')
      )
    htmlwidgets::saveWidget(as_widget(fig), "figure02.html")
    if (!require("processx")) {
      install.packages("processx")
    }
    # orca(fig, "figure_01.png")
    save_image(fig, "figure_02.png", width = 1417, height = 875.7726)
    return(fig)
}
#
plot_scene_03 <-
  function( 
    data_folder = '',
    contrafactual_sol_file_name = "reference_solution.csv",
    fig_file_name = "figure_03",
    file_solutions_list = list(
      "controlled_solution_a_beta_3.csv",
      "controlled_solution_a_beta_2.csv",
      "controlled_solution_a_beta_1.csv"
    ), 
    file_events_list = list(
      "light_traffic_policy_transitions_a_beta_1.csv",
      "light_traffic_policy_transitions_a_beta_2.csv",
      "light_traffic_policy_transitions_a_beta_3.csv"
    )
  ){
  #
    controlled_sol_file_name_list <- file_solutions_list
    df <- 
      read_csv(contrafactual_sol_file_name, show_col_types = FALSE)
    df$policy <-"Counterfactual"
    counter <- 1
    for (file in controlled_sol_file_name_list){
      path <- paste(data_folder, file, sep="")
      df_aux <- read_csv(path, show_col_types = FALSE)
      df_aux$policy <- paste("Policy", counter) 
      df <- bind_rows(df, df_aux)
      counter <- counter + 1
    }
    cnames<- c(
      "time",
      "xS",
      "Prevalence",
      "xV",
      "xR",
      "xC",
      "xF",
      "Cost",
      "date",
      "R_t",
      "policy")
    
    names(df) <- cnames
    color_palette <- "RdYlBu"
    color_period <- length(file_solutions_list) + 1
    fig_01 <- 
      df%>%
        group_by(policy) %>%
        plot_ly(
          x = ~date,
          y = ~Prevalence,
          type = "scatter",
          legendgroup = ~policy,
          color=~policy,
          mode = "lines",
          fill = 'tozeroy',
          showlegend = TRUE,
          colors =  brewer.pal(color_period, color_palette)
        )
    fig_02 <- 
      df %>%
      subset((policy != 'Counterfactual')) %>%
      group_by(policy) %>%
      plot_ly(
        x = ~date,
        y = ~Cost,
        type = "scatter",
        legendgroup = ~policy,
        color = ~policy,
        mode = "lines",
        fill='tonexty',
        showlegend = F,
        colors =  brewer.pal(color_period, color_palette)
      )
    #
    fig_03 <- 
      get_time_multi_policy_plot(file_events = file_events_list)
    fig <-
      subplot(
        fig_01, fig_02, fig_03,
        nrows = 3,
        heights = c(0.40, .40, .20),
        shareX = TRUE,
        titleY = TRUE
      )
    fig <-
      fig %>%
      layout(
        title = "The influence of the movility restriction expense 
        over prevalence and cost",
        legend = list(orientation = 'h')
      )
    htmlwidgets::saveWidget(
      as_widget(fig),
      paste(fig_file_name,".html", sep='')
    )
    if (!require("processx")) {
      install.packages("processx")
    }
    save_image(
      fig,
      paste(fig_file_name, ".png", sep=''),
      width = 1417,
      height = 875.7726
    )
    return(fig)
}
##
plot_gradient_scene <-
  function( 
    data_folder = '',
    contrafactual_sol_file_name = "reference_solution.csv",
    fig_file_name = "figure_03",
    file_solutions_list = list(
      "controlled_solution_a_beta_3.csv",
      "controlled_solution_a_beta_2.csv",
      "controlled_solution_a_beta_1.csv"
    ), 
    file_events_list = list(
      "light_traffic_policy_transitions_a_beta_1.csv",
      "light_traffic_policy_transitions_a_beta_2.csv",
      "light_traffic_policy_transitions_a_beta_3.csv"
    ),
    title = "The influence of the expenses due to prevalence on cost",
    color_palette = 'RdYlGn'
  ){
    #
    controlled_sol_file_name_list <- file_solutions_list
    df <- 
      read_csv(contrafactual_sol_file_name, show_col_types = FALSE)
    df$policy <-"Counterfactual"
    counter <- 1
    for (file in controlled_sol_file_name_list){
      path <- paste(data_folder, file, sep="")
      df_aux <- read_csv(path, show_col_types = FALSE)
      df_aux$policy <- paste("Policy", counter) 
      df <- bind_rows(df, df_aux)
      counter <- counter + 1
    }
    cnames<- c(
      "time",
      "xS",
      "Prevalence",
      "xV",
      "xR",
      "xC",
      "xF",
      "Cost",
      "date",
      "R_t",
      "policy"
    )
    names(df) <- cnames
    color_period <- length(file_solutions_list) + 1
    color_palette <- rev(brewer.pal(color_period, color_palette))
    fig_01 <- 
      df%>%
      group_by(policy) %>%
      plot_ly(
        x = ~date,
        y = ~Prevalence,
        type = "scatter",
        legendgroup = ~policy,
        color=~policy,
        mode = "lines",
        fill = 'tozeroy',
        showlegend = TRUE,
        colors = color_palette,
        xaxis='x1',
        yaxis='y1'
      )
    fig_02 <- plot_ly()
    # Obtaining data
    df_fig_2 <- df %>%
      subset((policy != 'Counterfactual')) %>%
      rev() %>%
      group_by(policy)
    df_fig_2_a <- 
      df %>%
      filter(
        policy == 'Policy 1' | policy == 'Policy 2'| policy == 'Policy 3'
      ) %>%
      group_by(policy)
    #
    # Drawing
    fig_02 <- 
      add_trace(
        fig_02,
        x = ~df_fig_2[["date"]],
        y = ~df_fig_2[["Cost"]],
        type = "scatter",
        legendgroup = ~df_fig_2[["policy"]],
        color = ~df_fig_2[["policy"]],
        mode = "lines",
        fill='tozeroy',
        showlegend = F,
        xaxis = 'x2',
        yaxis = 'y2',
        colors = color_palette[2:length(color_palette)]
    )
    fig_02 <- 
      add_trace(
        fig_02,
        x = ~df_fig_2_a[["date"]],
        y = ~df_fig_2_a[["Cost"]],
        type = "scatter",
        legendgroup = ~df_fig_2_a[["policy"]],
        color = ~df_fig_2_a[["policy"]],
        mode = "lines",
        fill='tozeroy',
        showlegend = F,
        xaxis = 'x3',
        yaxis = 'y3',
        colors = color_palette[2:length(color_palette)]
      )
    #
    # Drawing inset figure
    fig_02 <- layout(
      fig_02,
      xaxis2 = list(
        domain = c(0.0, 0.95),
        anchor = 'y2'
      ),
      yaxis2 = list(
        domain = c(0.0, 0.95),
        anchor = 'x2'
      ),
      xaxis3 = list(
        domain = c(0.1, 0.4),
        range = c("2022-06-01", "2022-12-31"),
        anchor = 'y3'
      ),
      yaxis3 = list(
        domain = c(0.7, 0.95),
        range = list(0, 350),
        anchor = 'x3'
      )
    )    
    fig_03 <- 
      get_time_multi_policy_plot(file_events = file_events_list)
    fig <-
      subplot(
        fig_01, fig_02, fig_03,
        nrows = 3,
        heights = c(0.40, 0.40, 0.20),
        shareX = TRUE,
        titleY = TRUE
      )
    fig <-
      fig %>%
      layout(
        title = title,
        legend = list(orientation = 'h'), 
        yaxis = list(
          range = list(0.0, 1.05 * max(df[["Prevalence"]]))
        ),
        yaxis2 = list(
          range = list(0.0,  max(df[["Cost"]]))
        )
      )
    htmlwidgets::saveWidget(
      as_widget(fig),
      paste(fig_file_name,".html", sep='')
    )
    if (!require("processx")) {
      install.packages("processx")
    }
    save_image(
      fig,
      paste(fig_file_name, ".png", sep=''),
      width = 1417,
      height = 875.7726
    )
    return(fig)
  }
###
plot_scene_3d <-
  function( 
    data_folder = '',
    contrafactual_sol_file_name = "reference_solution.csv",
    fig_file_name = "figure_03",
    file_solutions_list = list(
      "controlled_solution_a_beta_3.csv",
      "controlled_solution_a_beta_2.csv",
      "controlled_solution_a_beta_1.csv"
    ), 
    file_events_list = list(
      "light_traffic_policy_transitions_a_beta_1.csv",
      "light_traffic_policy_transitions_a_beta_2.csv",
      "light_traffic_policy_transitions_a_beta_3.csv"
    ),
    title = "The influence of the expenses due to prevalence on cost",
    color_palette = 'RdYlGn'
  ){
    #
    controlled_sol_file_name_list <- file_solutions_list
    df <- 
      read_csv(contrafactual_sol_file_name, show_col_types = FALSE)
    df$policy <-"Counterfactual"
    counter <- 1
    for (file in controlled_sol_file_name_list){
      path <- paste(data_folder, file, sep="")
      df_aux <- read_csv(path, show_col_types = FALSE)
      df_aux$policy <- paste("Policy", counter) 
      df <- bind_rows(df, df_aux)
      counter <- counter + 1
    }
    cnames<- c(
      "time",
      "xS",
      "Prevalence",
      "xV",
      "xR",
      "xC",
      "xF",
      "Cost",
      "date",
      "R_t",
      "policy")
    
    names(df) <- cnames
    color_period <- length(file_solutions_list) + 1
    color_palette <- rev(brewer.pal(color_period, "Spectral"))
    fig_01 <- 
      df%>%
      group_by(policy) %>%
      plot_ly(
        x = ~xC,
        y = ~date,
        z = ~R_t,
        type = "scatter3d",
        legendgroup = ~policy,
        color=~policy,
        mode = "lines",
        #fill = 'tozeroy',
        line = list(width = 6, color=color_palette, reverscale=FALSE),
        marker = list(symbol = 'circle', size = 2),
        showlegend = TRUE
        #colors = color_palette
      )
    fig_02 <- 
      df %>%
      subset((policy != 'Counterfactual')) %>%
      rev() %>%
      group_by(policy) %>%
      plot_ly(
        x = ~date,
        y = ~Cost,
        type = "scatter",
        legendgroup = ~policy,
        color = ~policy,
        mode = "lines",
        fill='tozeroy',
        showlegend = F,
        colors = color_palette[2:length(color_palette)]
      )
    
    #
    fig_03 <- 
      get_time_multi_policy_plot(file_events = file_events_list)
    fig <-
      subplot(
        fig_01, fig_02, fig_03,
        nrows = 3,
        heights = c(0.40, .40, .20),
        shareX = TRUE,
        titleY = TRUE
      )
    fig <-
      fig %>%
      layout(
        title = title,
        legend = list(orientation = 'h'), 
        yaxis = list(
          range = list(0.0, 1.05 * max(df[["Prevalence"]]))
        ),
        yaxis2 = list(
          range = list(0.0,  max(df[["Cost"]]))
        )
      )
    htmlwidgets::saveWidget(
      as_widget(fig),
      paste(fig_file_name,".html", sep='')
    )
    if (!require("processx")) {
      install.packages("processx")
    }
    save_image(
      fig,
      paste(fig_file_name, ".png", sep=''),
      width = 1417,
      height = 875.7726
    )
    return(fig)
  }