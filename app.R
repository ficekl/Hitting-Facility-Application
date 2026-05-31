#clear
rm(list=ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(fmsb)
library(bslib)
library(shiny)

#data
strength_selected <- read_excel("COSC 5500 Final App Data.xlsx", 
                                sheet = "StrengthSelected")
swings_selected <- read_excel("COSC 5500 Final App Data.xlsx",
                              sheet = "SwingSelected")
bios <- read_excel("COSC 5500 Final App Data.xlsx",
                   sheet = "Bios")
strength_condensed <- read_excel("COSC 5500 Final App Data.xlsx",
                                 sheet = "StrengthCondensed")
strength_condensed_time_series <- read_excel("COSC 5500 Final App Data.xlsx",
                                             sheet = "StrengthCondensedTimeSeries")

strength_targets <- read_excel("COSC 5500 Final App Data.xlsx",
                               sheet = "StrengthTargets")

strength_percentiles <- read_excel("COSC 5500 Final App Data.xlsx",
                                   sheet = "StrengthPercentiles")

kinematic_data <- read_excel("COSC 5500 Final App Data.xlsx",
                             sheet = "KinematicCurveData")

kinematic_percentiles <- read_excel("COSC 5500 Final App Data.xlsx",
                                    sheet = "KinematicPercentiles")

#FINAL APP

#ui
ui <- fluidPage(

#FORMATING/STYLIZING  
  
  tags$style(HTML("
  * {
    font-family: 'Arial', sans-serif;
    color: #2c3e50000;
  }
  .center-box {
    max-width: 400px;
    margin: 80px auto 0 auto;
    text-align: center;
  }
  .metric-card {
    border: 2px solid #ddd;
    border-radius: 8px;
    padding: 10px;
    text-align: center;
  }
  .nav-tabs > li > a {
    color: #000000;
    font-weight: bold;
  }
  .nav-tabs > li.active > a {
    color: #000000;
    font-weight: bold;
  }
  .banner-text span, .banner-text {
    color: fff;
  }
  .selectize-input, .selectize-dropdown {
    color: black !important;
  }
  .control-label {
    color: black !important;
  }
")),

  #APP HOMEPAGE TAB
    
  tabsetPanel(
    id = "tabs",
    
    tabPanel(
      "Select Athlete",
      
      br(),
      
      fluidRow(
        column(
          width = 4,
          offset = 4,
          div(
            style = "
              border: 2px solid #ddd;
              border-radius: 8px;
              padding: 30px;
              text-align: center;
              color: black;
            ",
          
            #SWING SCIENCE LOGO
          
            img(
              src = "SwingScienceLogo3.png",
              style = "width:100%; height:auto; margin-bottom:20px;"
            ),
            
            #BOX AROUND ATHLETE SELECTOR
            
            div(
              style = "max-width: 250px; margin: 0 auto;",
              selectInput(
                "athlete",
                "Choose Athlete:",
                choices = c("Athlete Name" = "", unique(bios$athlete)),
                selected = ""
              )
            )
          )
        )
      )
    ),
    
    #OVERVIEW TAB
    
    tabPanel(
      "Overview",
      
      #TOP ROW
      fluidRow(
        column(
          width = 12,
          div(
            style = "
              background-color: #000;
              color: white;
              padding: 10px 20px;
              border-radius: 8px;
              display: flex;
              align-items: center;
              justify-content: space-between;
            ",
            
            #ATHLETE NAME
            div(
              class = "banner-text",
              style = "font-size: 22px; font-weight: bold;",
              tags$span(style = "color: white !important;", textOutput("athlete"))
            ),
            
            #HEIGHT, WEIGHT, LEVEL
            div(
              style = "display: flex; gap: 40px; align-items: center;",
              
              div(
                div(style = "font-size: 11px; color: #ddd; text-transform: uppercase; letter-spacing: 1px;", "Height"),
                tags$span(style = "color: white !important; font-size: 16px; font-weight: bold;", textOutput("height"))
              ),
              
              div(
                div(style = "font-size: 11px; color: #ddd; text-transform: uppercase; letter-spacing: 1px;", "Weight"),
                tags$span(style = "color: white !important; font-size: 16px; font-weight: bold;", textOutput("weight"))
              ),
              
              div(
                div(style = "font-size: 11px; color: #ddd; text-transform: uppercase; letter-spacing: 1px;", "Level"),
                tags$span(style = "color: white !important; font-size: 16px; font-weight: bold;", textOutput("playing_level"))
              )
            )
          )
        )
      ),
      
      br(),
      
      #SECOND ROW: BAT SPEED PLOT
      fluidRow(
        column(
          width = 12,
          div(
            style = "border: 2px solid #ddd; border-radius: 8px; padding: 10px;",
            plotOutput("bat_speed_plot")
          )
        )
      ),
      
      br(),
      
      #PERFORMANCE TESTING TITLE
      
      h4("Performance Testing Scores", style = "font-weight: bold; 
         margin-bottom: 10px; text-align: center;"),
      
      div(
        style = "display: flex; justify-content: center; gap: 20px; margin-bottom: 10px;",
        
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          div(style = "width: 16px; height: 16px; background-color: #2ecc71; border-radius: 3px;"),
          tags$span(style = "font-size: 12px; color: gray;", "At or above target at most recent test")
        ),
        
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          div(style = "width: 16px; height: 16px; background-color: #f39c12; border-radius: 3px;"),
          tags$span(style = "font-size: 12px; color: gray;", "Within 10% of target")
        ),
        
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          div(style = "width: 16px; height: 16px; background-color: #e74c3c; border-radius: 3px;"),
          tags$span(style = "font-size: 12px; color: gray;", "More than 10% from target")
        )
      ),
      
      
      #BOTTOM ROW: 3 PERFORMANCE TESTING METRIC CARDS
      fluidRow(
        column(
          width = 4,
          div(
            class = "metric-card",
            img(src = "cmj.png", style = "width:60px; height:60px; object-fit:contain; margin-bottom:5px;"),
            p("Jump Height (cm)", style = "font-size:15px; font-weight:bold; margin-bottom:0;"),
            uiOutput("jump_percentile"),
            div(
              style = "display: flex; align-items: center; justify-content: center; gap: 8px; margin-top: 2px; margin-bottom: 4px;",
              tags$hr(style = "width: 30px; border-top: 2px dotted black; margin: 0;"),
              tags$span(style = "font-size: 11px; color: gray;", "Target Score")
            ),
            plotOutput("plot_jump", height = "180px")
          )
        ),
        column(
          width = 4,
          div(
            class = "metric-card",
            img(src = "sprint.png", style = "width:60px; height:60px; object-fit:contain; margin-bottom:5px;"),
            p("20m Sprint Time (s)", style = "font-size:15px; font-weight:bold; margin-bottom:0;"),
            uiOutput("sprint_percentile"),
            div(
              style = "display: flex; align-items: center; justify-content: center; gap: 8px; margin-top: 2px; margin-bottom: 4px;",
              tags$hr(style = "width: 30px; border-top: 2px dotted black; margin: 0;"),
              tags$span(style = "font-size: 11px; color: gray;", "Target Score")
            ),
            plotOutput("plot_sprint", height = "180px")
          )
        ),
        column(
          width = 4,
          div(
            class = "metric-card",
            img(src = "pull.png", style = "width:60px; height:60px; object-fit:contain; margin-bottom:5px;"),
            p("Peak Vertical Force (N/kg)", style = "font-size:15px; font-weight:bold; margin-bottom:0;"),
            uiOutput("power_percentile"),
            div(
              style = "display: flex; align-items: center; justify-content: center; gap: 8px; margin-top: 2px; margin-bottom: 4px;",
              tags$hr(style = "width: 30px; border-top: 2px dotted black; margin: 0;"),
              tags$span(style = "font-size: 11px; color: gray;", "Target Score")
            ),
            plotOutput("plot_power", height = "180px")
          ),
        br()
        )
      )
    ),
    
    #HITTING TAB
    tabPanel(
      "Hitting Detail",
      
      #TOP ROW: USING THE SAME ATHLETE H, W, AND L
      fluidRow(
        column(
          width = 12,
          div(
            style = "
              background-color: #000;
              color: white;
              padding: 10px 20px;
              border-radius: 8px;
              display: flex;
              align-items: center;
              justify-content: space-between;
            ",
            
            div(
              class = "banner-text",
              style = "font-size: 22px; font-weight: bold;",
              tags$span(style = "color: white !important;", textOutput("athlete_hitting"))
            ),
            
            div(
              style = "display: flex; gap: 40px; align-items: center;",
              
              div(
                div(style = "font-size: 11px; color: #ddd; text-transform: uppercase; letter-spacing: 1px;", "Height"),
                tags$span(style = "color: white !important; font-size: 16px; font-weight: bold;", textOutput("height_hitting"))
              ),
              
              div(
                div(style = "font-size: 11px; color: #ddd; text-transform: uppercase; letter-spacing: 1px;", "Weight"),
                tags$span(style = "color: white !important; font-size: 16px; font-weight: bold;", textOutput("weight_hitting"))
              ),
              
              div(
                div(style = "font-size: 11px; color: #ddd; text-transform: uppercase; letter-spacing: 1px;", "Level"),
                tags$span(style = "color: white !important; font-size: 16px; font-weight: bold;", textOutput("playing_level_hitting"))
              )
            )
          )
        )
      ),
      
      h4("Hitting KPIs", style = "font-weight: bold; text-align: center; margin-bottom: 10px;"),
      
      #ATTACK ANGLE, SMASH FACTOR, EXIT VELO METRIC CARDS
      fluidRow(
        column(
          width = 4,
          div(
            class = "metric-card",
            p("Avg Attack Angle", style = "font-size:15px; font-weight:bold; margin-bottom:0;"),
            plotOutput("plot_attack_angle", height = "220px")
          )
        ),
        column(
          width = 4,
          div(
            class = "metric-card",
            p("Avg Smash Factor", style = "font-size:15px; font-weight:bold; margin-bottom:0;"),
            plotOutput("plot_smash_factor", height = "220px")
          )
        ),
        column(
          width = 4,
          div(
            class = "metric-card",
            p("Max Exit Velo", style = "font-size:15px; font-weight:bold; margin-bottom:0;"),
            plotOutput("plot_exit_velo", height = "220px")
          )
        )
      ),
      
      br(),
      
      #KINEMATIC CURVE IMAGE
      fluidRow(
        column(
          width = 12,
          div(
            style = "border: 2px solid #ddd; border-radius: 8px; padding: 10px;",
            uiOutput("kinematic_plot")
          )
        )
      ),
      
      br(),
      
      #KINEMATIC TABLE
      fluidRow(
        column(
          width = 12,
          div(
            style = "border: 2px solid #ddd; border-radius: 8px; padding: 10px;",
            h4("Angular Velocities of Swing Phase Segments", style = "font-weight: bold; text-align: center; margin-bottom: 10px;"),
            DT::dataTableOutput("kinematic_table")
          )
        )
      ),
      br()
    )
  )
)


#############################################################################
server <- function(input, output, session) {
  
  
  #BIOS - CREATING REACTIVE WHEN ATHLETE IS CHANGED
  
  bios_filtered <- reactive({
    req(input$athlete)
    
    bios %>%
      dplyr::filter(athlete == input$athlete) %>%
      dplyr::slice_tail(n = 1)
  })
  
  
  #SWINGS - CREATING REACTIVE FOR BAT SPEED PLOT WHEN ATHLETE IS CHANGED
  
  swings_filtered <- reactive({
    req(input$athlete)
    
    swings_selected %>%
      dplyr::filter(athlete == input$athlete) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        date = as.Date(date),
        date_label = format(date, "%m/%d/%Y"),
        date_label = factor(date_label, levels = unique(date_label)),
        avg_bat_speed_session = as.numeric(avg_bat_speed_session)
      )
  })
  
  
  #STRENGTH - CREATING REACTIVE FOR STRENGTH VIZ'S WHEN ATHLETE IS CHANGED
  
  strength_ts_filtered <- reactive({
    req(input$athlete)
    
    strength_condensed_time_series %>%
      dplyr::filter(athlete == input$athlete) %>%
      dplyr::arrange(test_date) %>%
      dplyr::mutate(
        test_date = as.Date(test_date),
        date_label = format(test_date, "%m/%d/%Y"),
        date_label = factor(date_label, levels = unique(date_label))
      )
  })
  
  
  #TARGETS - CREATING REACTIVE FOR TARGETS WHEN ATHLETE IS CHANGED
  
  targets_filtered <- reactive({
    req(input$athlete)
    
    strength_targets %>%
      dplyr::filter(athlete == input$athlete)
  })
  
  
  #PERCENTILES - CREATING REACTIVE FOR PERCENTILES WHEN ATHLETE IS CHANGED
  
  percentiles_filtered <- reactive({
    req(input$athlete)
    
    strength_percentiles %>%
      dplyr::filter(athlete == input$athlete)
  })
  
  
  #KINEMATIC PERCENTILES ON BOTTOM OF HITTING TAB
  
  kinematic_percentiles_filtered <- reactive({
    req(input$athlete)
    
    df <- kinematic_percentiles %>%
      dplyr::filter(athlete == input$athlete)
    
    req(nrow(df) > 0)
    
    #ADDING IN PERCENTILES
    fmt <- function(val_col, pct_col) {
      val <- round(df[[val_col]], 1)
      pct <- round(df[[pct_col]])
      paste0(val, " deg/s (", pct, "%)")
    }
    
    tibble::tibble(
      Segment = c("Pelvis", "Torso", "Upper Arm", "Hand"),
      
      `First Move (pct)` = c(
        fmt("pelvis_angular_velocity_fm_x",  "pelvis_angular_velocity_fm_x_pct"),
        fmt("torso_angular_velocity_fm_x",   "torso_angular_velocity_fm_x_pct"),
        fmt("upper_arm_speed_mag_fm_x",      "upper_arm_speed_mag_fm_x_pct"),
        fmt("hand_speed_mag_fm_x",           "hand_speed_mag_fm_x_pct")
      ),
      
      `Foot Plant (pct)` = c(
        fmt("pelvis_angular_velocity_fp_x",  "pelvis_angular_velocity_fp_x_pct"),
        fmt("torso_angular_velocity_fp_x",   "torso_angular_velocity_fp_x_pct"),
        fmt("upper_arm_speed_mag_fp_x",      "upper_arm_speed_mag_fp_x_pct"),
        fmt("hand_speed_mag_fp_x",           "hand_speed_mag_fp_x_pct")
      ),
      
      `Max Hip Shoulder Sep (pct)` = c(
        fmt("pelvis_angular_velocity_maxhss_x",  "pelvis_angular_velocity_maxhss_x_pct"),
        fmt("torso_angular_velocity_maxhss_x",   "torso_angular_velocity_maxhss_x_pct"),
        fmt("upper_arm_speed_mag_maxhss_x",      "upper_arm_speed_mag_maxhss_x_pct"),
        fmt("hand_speed_mag_maxhss_x",           "hand_speed_mag_maxhss_x_pct")
      ),
      
      `Max (pct)` = c(
        fmt("pelvis_angular_velocity_seq_max_x",  "pelvis_angular_velocity_seq_max_x_pct"),
        fmt("torso_angular_velocity_seq_max_x",   "torso_angular_velocity_seq_max_x_pct"),
        fmt("upper_arm_speed_mag_seq_max_x",      "upper_arm_speed_mag_seq_max_x_pct"),
        fmt("hand_speed_mag_seq_max_x",           "hand_speed_mag_seq_max_x_pct")
      )
    )
  })
  
  
  #AUTO SWITCH TAB
  
  observeEvent(input$athlete, {
    if (input$athlete != "") {
      updateTabsetPanel(session, "tabs", selected = "Overview")
    }
  })
  
  
  #TEXT OUTPUTS RENDERING IN OVERVIEW TAB FOR BIO BANNER
  
  output$athlete <- renderText({ bios_filtered()$athlete })
  output$height <- renderText({ bios_filtered()$height })
  output$weight <- renderText({ bios_filtered()$weight })
  output$playing_level <- renderText({ bios_filtered()$playing_level })
  
  
  #TEXT OUTPUTS RENDERING IN HITTING TAB FOR BIO BANNER
  
  output$athlete_hitting <- renderText({ bios_filtered()$athlete })
  output$height_hitting <- renderText({ bios_filtered()$height })
  output$weight_hitting <- renderText({ bios_filtered()$weight })
  output$playing_level_hitting <- renderText({ bios_filtered()$playing_level })
  
  
  #TEXT OUTPUTS RENDERING IN OVERVIEW TAB FOR JUMP, SPRINT, AND VERTICAL FORCE CARDS
  
  output$jump_percentile <- renderUI({
    pct <- percentiles_filtered()
    req(nrow(pct) > 0)
    p(paste0(round(pct$jump_height_cm_cmj_pct), "th Percentile"),
      style = "text-align:center; color:gray; font-size:12px; margin-top:2px; margin-bottom:4px;")
  })
  
  output$sprint_percentile <- renderUI({
    pct <- percentiles_filtered()
    req(nrow(pct) > 0)
    p(paste0(round(pct$`20m_sprint_pct`), "th Percentile"),
      style = "text-align:center; color:gray; font-size:12px; margin-top:2px; margin-bottom:4px;")
  })
  
  output$power_percentile <- renderUI({
    pct <- percentiles_filtered()
    req(nrow(pct) > 0)
    p(paste0(round(pct$peak_vertical_force_imtp_bw_pct), "th Percentile"),
      style = "text-align:center; color:gray; font-size:12px; margin-top:2px; margin-bottom:4px;")
  })
  
  
  #BAT SPEED PLOT RENDERING IN OVERVIEW TAB
  
  output$bat_speed_plot <- renderPlot({
    
    df <- swings_filtered()
    req(nrow(df) > 0)
    
    bat_speed_target <- unique(df$bat_speed_target)
    latest <- dplyr::last(df$avg_bat_speed_session)
    
    line_color <- if (latest >= bat_speed_target) "#2ecc71" else if (latest >= bat_speed_target * 0.9) "#f39c12" else "#e74c3c"
    
    ggplot(df, aes(x = date_label, y = avg_bat_speed_session, group = 1)) +
      geom_line(color = line_color, linewidth = 1) +
      geom_point(color = line_color, size = 2) +
      
      geom_hline(
        yintercept = bat_speed_target,
        linetype = "dotted",
        color = "black",
        linewidth = 1
      ) +
      
      annotate(
        "text",
        x = 1,
        y = bat_speed_target,
        label = "Target",
        color = "black",
        hjust = 1,
        vjust = -1,
        size = 4
      ) +
      
      coord_cartesian(ylim = c(40, 80)) +
      
      labs(
        title = "Bat Speed Progression",
        x = "Session Date",
        y = "Avg Bat Speed (mph)"
      ) +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
      )
  })
  
  
  #KINEMATIC TABLE IN HITTING TAB
  
  output$kinematic_table <- DT::renderDT({
    df <- kinematic_percentiles_filtered()
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        dom = "t",
        ordering = FALSE
      )
    )
  })
  
  
  #KINEMATIC CURVE IMAGE RENDERING IN HITTING TAB
  
  output$kinematic_plot <- renderUI({
    req(input$athlete)
    
    filename <- paste0("kinematic_", tolower(gsub(" ", "_", input$athlete)), ".png")
    
    tags$img(
      src = filename,
      style = "width:100%; height:auto; border-radius: 8px;"
    )
  })
  
  
  #METRIC PLOT FORMATTING FOR ALL 3
  
  render_metric_plot <- function(df, col, target, y_label, y_min, y_max, lower_is_better = FALSE) {
    
    latest <- dplyr::last(df[[col]])
    
    if (lower_is_better) {
      line_color <- if (latest <= target) "#2ecc71" else if (latest <= target * 1.1) "#f39c12" else "#e74c3c"
    } else {
      line_color <- if (latest >= target) "#2ecc71" else if (latest >= target * 0.9) "#f39c12" else "#e74c3c"
    }
    
    ggplot(df, aes(x = date_label, y = .data[[col]], group = 1)) +
      geom_line(color = line_color, linewidth = 1) +
      geom_point(color = line_color, size = 2) +
      
      geom_hline(
        yintercept = target,
        linetype = "dotted",
        color = "black",
        linewidth = 0.8
      ) +
      
      coord_cartesian(ylim = c(y_min, y_max)) +
      
      labs(x = NULL, y = y_label) +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        plot.margin = margin(5, 10, 5, 10)
      )
  }
  
  
  #JUMP HEIGHT PLOT RENDERING IN OVERVIEW TAB
  
  output$plot_jump <- renderPlot({
    df <- strength_ts_filtered()
    tgt <- targets_filtered()
    req(nrow(df) > 0, nrow(tgt) > 0)
    
    render_metric_plot(
      df,
      col = "jump_height_cm_cmj",
      target = tgt$jump_height_target,
      y_label = "cm",
      y_min = 20,
      y_max = 60
    )
  })
  
  
  #SPRINT TIME PLOT RENDERING IN OVERVIEW TAB
  
  output$plot_sprint <- renderPlot({
    df <- strength_ts_filtered()
    tgt <- targets_filtered()
    req(nrow(df) > 0, nrow(tgt) > 0)
    
    render_metric_plot(
      df,
      col = "20m_sprint",
      target = tgt$`20m_sprint_target`,
      y_label = "seconds",
      y_min = 2.5,
      y_max = 3.5,
      lower_is_better = TRUE
    )
  })
  
  
  #PEAK VERTICAL FORCE PLOT RENDERING IN OVERVIEW TAB
  
  output$plot_power <- renderPlot({
    df <- strength_ts_filtered()
    tgt <- targets_filtered()
    req(nrow(df) > 0, nrow(tgt) > 0)
    
    render_metric_plot(
      df,
      col = "peak_vertical_force_imtp_bw",
      target = tgt$peak_vertical_force_target,
      y_label = "N/kg",
      y_min = 10,
      y_max = 40
    )
  })
  
  
   #ATTACK ANGLE DIAL RENDERING IN HITTING TAB
  
  output$plot_attack_angle <- renderPlot({
    df <- swings_filtered()
    req(nrow(df) > 0)
    
    value <- df$attack_angle_contact_x_avg[1]
    req(!is.na(value))
    
    #settings
    min_val     <- -5
    max_val     <- 25
    target_low  <- 5
    target_high <- 20
    
    #color based on whether value is in target zone
    arc_color <- if (value >= target_low & value <= target_high) "#2ecc71" else if (value >= target_low - 2 & value <= target_high + 2) "#f39c12" else "#e74c3c"
    
    #convert values to angles
    to_angle <- function(val) {
      180 - (val - min_val) / (max_val - min_val) * 180
    }
    
    value_angle       <- to_angle(value)
    target_low_angle  <- to_angle(target_low)
    target_high_angle <- to_angle(target_high)
    
    #background arc
    bg_arc <- data.frame(angle = seq(0, 180, by = 1)) %>%
      dplyr::mutate(
        x = cos(angle * pi / 180),
        y = sin(angle * pi / 180)
      )
    
    #target zone arc
    target_arc <- data.frame(
      angle = seq(min(target_high_angle, target_low_angle),
                  max(target_high_angle, target_low_angle), by = 0.5)
    ) %>%
      dplyr::mutate(
        x = cos(angle * pi / 180),
        y = sin(angle * pi / 180)
      )
    
    #needle
    needle_angle_rad <- value_angle * pi / 180
    needle <- data.frame(
      x = c(0, 0.85 * cos(needle_angle_rad)),
      y = c(0, 0.85 * sin(needle_angle_rad))
    )
    
    #needle base triangle
    base_left  <- (value_angle + 8) * pi / 180
    base_right <- (value_angle - 8) * pi / 180
    needle_triangle <- data.frame(
      x = c(0.12 * cos(base_left), 0.85 * cos(needle_angle_rad), 0.12 * cos(base_right)),
      y = c(0.12 * sin(base_left), 0.85 * sin(needle_angle_rad), 0.12 * sin(base_right))
    )
    
    ggplot() +
      
      #background arc
      geom_path(data = bg_arc, aes(x = x, y = y), color = "#ecf0f1", linewidth = 8) +
      
      #target zone arc
      geom_path(data = target_arc, aes(x = x, y = y), color = "#2ecc71", linewidth = 8, alpha = 0.4) +
      
      #needle triangle
      geom_polygon(data = needle_triangle, aes(x = x, y = y), fill = arc_color, color = arc_color) +
      
      #needle line
      geom_line(data = needle, aes(x = x, y = y), color = arc_color, linewidth = 1.5) +
      
      #center dot
      annotate("point", x = 0, y = 0, size = 4, color = "gray30") +
      
      #center value label
      annotate("text", x = 0, y = -0.2, label = paste0(round(value, 1), "°"),
               size = 9, fontface = "bold", color = arc_color) +
      
      #min and max labels
      annotate("text", x = -1, y = -.1, label = paste0(min_val, "°"), size = 3.5, color = "gray40") +
      annotate("text", x =  1, y = -.1, label = paste0(max_val, "°"), size = 3.5, color = "gray40") +
      
      #target zone label
      annotate("text", x = 0, y = 0.5, label = paste0("Target: ", target_low, "° – ", target_high, "°"),
               size = 3, color = "gray40") +
      
      coord_fixed() +
      xlim(-1.3, 1.3) +
      ylim(-0.3, 1.3) +
      
      theme_void() +
      theme(legend.position = "none")
  })
  
  #SMASH FACTOR LIQUID FILL RENDERING IN HITTING TAB
  
  output$plot_smash_factor <- renderPlot({
    df <- swings_filtered()
    req(nrow(df) > 0)
    
    value <- df$avg_smash_factor[1]
    req(!is.na(value))
    
    #settings
    min_val <- 1.1
    max_val <- 1.3
    target  <- 1.2
    
    #clamp value within range
    value_clamped <- max(min_val, min(max_val, value))
    
    #rill proportion
    fill_pct <- (value_clamped - min_val) / (max_val - min_val)
    
    #color based on target
    fill_color <- if (value >= target) "#2ecc71" else if (value >= target * 0.95) "#f39c12" else "#e74c3c"
    
    #target line position
    target_pct <- (target - min_val) / (max_val - min_val)
    
    ggplot() +
      
      #glass background
      annotate("rect",
               xmin = 0, xmax = 1,
               ymin = 0, ymax = 1,
               fill = "#ecf0f1", color = "#bdc3c7", linewidth = 1.5) +
      
      #liquid fill
      annotate("rect",
               xmin = 0, xmax = 1,
               ymin = 0, ymax = fill_pct,
               fill = fill_color, alpha = 0.7, color = NA) +
      
      #target line
      annotate("segment",
               x = 0, xend = 1,
               y = target_pct, yend = target_pct,
               linetype = "dotted", color = "black", linewidth = 1) +
      
      #target label
      annotate("text",
               x = 1.05, y = target_pct,
               label = paste0("Target: ", target),
               hjust = 0, size = 3.5, color = "black") +
      
      #min label
      annotate("text",
               x = -0.05, y = 0,
               label = min_val,
               hjust = 1, size = 3.5, color = "gray40") +
      
      #max label
      annotate("text",
               x = -0.05, y = 1,
               label = max_val,
               hjust = 1, size = 3.5, color = "gray40") +
      
      #value label inside fill
      annotate("text",
               x = 0.5, y = fill_pct / 2,
               label = round(value, 3),
               size = 8, fontface = "bold", color = "white") +
      
      xlim(-0.3, 1.5) +
      ylim(-0.05, 1.1) +
      
      theme_void() +
      theme(legend.position = "none")
  })
  
  #EXIT VELO SPEEDOMETER RENDERING IN HITTING TAB
  
  output$plot_exit_velo <- renderPlot({
    df <- swings_filtered()
    req(nrow(df) > 0)
    
    value <- df$max_exit_velo[1]
    req(!is.na(value))
    
    #settings
    min_val <- 80
    max_val <- 120
    
    #clamp and convert to angle
    value_clamped <- max(min_val, min(max_val, value))
    value_angle   <- 180 - (value_clamped - min_val) / (max_val - min_val) * 180
    
    #needle color
    needle_color <- if (value >= 105) "#2ecc71" else if (value >= 95) "#f39c12" else "#e74c3c"
    
    #full background arc
    bg_arc <- data.frame(angle = seq(0, 180, by = 1)) %>%
      dplyr::mutate(x = cos(angle * pi / 180), y = sin(angle * pi / 180))
    
    #filled arc up to value
    fill_arc <- data.frame(angle = seq(value_angle, 180, by = 0.5)) %>%
      dplyr::mutate(x = cos(angle * pi / 180), y = sin(angle * pi / 180))
    
    #needle
    needle_rad <- value_angle * pi / 180
    needle <- data.frame(
      x = c(0, 0.8 * cos(needle_rad)),
      y = c(0, 0.8 * sin(needle_rad))
    )
    
    ggplot() +
      geom_path(data = bg_arc,   aes(x = x, y = y), color = "#ecf0f1", linewidth = 10) +
      geom_path(data = fill_arc, aes(x = x, y = y), color = needle_color, linewidth = 10) +
      geom_line(data = needle,   aes(x = x, y = y), color = "gray30", linewidth = 1.5) +
      annotate("point", x = 0, y = 0, size = 5, color = "gray30") +
      annotate("text", x = 0,    y = -0.15, label = paste0(round(value, 1), " mph"), size = 8, fontface = "bold", color = needle_color) +
      annotate("text", x = -1, y = -.1,     label = paste0(min_val, " mph"), size = 3.5, color = "gray40") +
      annotate("text", x =  1, y = -.1,     label = paste0(max_val, " mph"), size = 3.5, color = "gray40") +
      coord_fixed() +
      xlim(-1.3, 1.3) +
      ylim(-0.3, 1.3) +
      theme_void()
  })
}

#run the application
shinyApp(ui = ui, server = server)
