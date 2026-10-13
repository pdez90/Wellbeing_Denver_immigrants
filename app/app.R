# =============================================================================
# app/app.R
# Interactive explorer for "Is the built environment associated with
# neighborhood belonging and subjective well-being among immigrants?"
#
#   shiny::runApp("app")            # after app/prepare_app_data.R has run
#
# Reads only aggregate outputs (see app/data/MANIFEST.md). No respondent row
# is loaded; the map shows counts by census tract suppressed below MIN_CELL and
# public area-level measures.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(readr); library(ggplot2); library(DT)
  library(leaflet); library(sf)
})

D <- function(...) file.path("data", ...)
rd <- function(f, ...) readr::read_csv(D(f), show_col_types = FALSE, ...)
has <- function(f) file.exists(D(f))

# ---- data ---------------------------------------------------------------
coef_all <- rd("all_model_coefficients.csv")
fit      <- rd("model_fit_summary.csv")
fig2     <- rd("Figure2_Coefficients.csv")
fig3     <- rd("Figure3_IntegratedModel.csv")
med      <- if (has("Table09.csv")) rd("Table09.csv") else NULL
med_sens <- if (has("mediation_sensitivity.csv")) rd("mediation_sensitivity.csv") else NULL
green    <- if (has("greenness_sensitivity.csv")) rd("greenness_sensitivity.csv") else NULL
buf      <- if (has("buffer_sensitivity.csv")) rd("buffer_sensitivity.csv") else NULL
rob_cs   <- if (has("robustness_common_sample.csv")) rd("robustness_common_sample.csv") else NULL
rob_se   <- if (has("robustness_standard_errors.csv")) rd("robustness_standard_errors.csv") else NULL
rob_mm   <- if (has("robustness_robust_regression.csv")) rd("robustness_robust_regression.csv") else NULL
rob_loo  <- if (has("robustness_swb_leave_one_out.csv")) rd("robustness_swb_leave_one_out.csv") else NULL
vartab   <- if (has("variable_table.csv")) rd("variable_table.csv", col_types = cols(.default = "c")) else NULL
verif    <- if (has("rebuild_verification.csv")) rd("rebuild_verification.csv") else NULL
desc_idx <- if (has("descriptives/Table_index.csv")) rd("descriptives/Table_index.csv") else NULL
geo      <- if (has("geo.rds")) readRDS(D("geo.rds")) else NULL

label_of <- c(setNames(fig2$label, fig2$term), setNames(fig3$label, fig3$term))
lab <- function(t) { l <- label_of[t]; l2 <- label_of[sub("_z$", "", t)]; ifelse(!is.na(l), l, ifelse(!is.na(l2), l2, t)) }
sigfill <- function(sig, grp) ifelse(sig, as.character(grp), NA_character_)
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .10, "+", "")))))
fmt <- function(x, d = 3) ifelse(is.na(x), "", formatC(x, digits = d, format = "f"))

SWB_COL <- "#A5243D"; BEL_COL <- "#1F4E5F"; BOTH_COL <- "#C8862B"
theme_app <- function() theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = "bottom", plot.title.position = "plot")

domains_fig2 <- unique(fig2$group)

# ---- UI -----------------------------------------------------------------
ui <- navbarPage(
  title = "Built environment, belonging and well-being — Denver immigrant survey",
  id = "nav", collapsible = TRUE,
  header = tags$head(tags$style(HTML("
    .note { color:#555; font-size: 0.92em; max-width: 60em; }
    .navbar { margin-bottom: 14px; }
    .container-fluid { max-width: 1500px; }
    table.dataTable td, table.dataTable th { font-size: 0.9em; }
    h4 { margin-top: 18px; }"))),

  tabPanel("Overview",
    fluidRow(column(8,
      h3("What this is"),
      p(class = "note", "An interactive companion to the paper. Every number here is read from the files the analysis code writes; nothing is computed on individual responses in this app, and no respondent-level data is loaded. Code: github.com/pdez90/Wellbeing_Denver_immigrants."),
      h4("Design"),
      tags$ul(
        tags$li("381 foreign-born adults surveyed in metropolitan Denver in 2019; 358 geocoded."),
        tags$li("Two outcomes: subjective well-being (6 items) and neighborhood belonging (4 items), both standardized."),
        tags$li("Neighborhood built environment in five domains, measured in 800 m buffers or at the block group / tract; neighborhood demographics and individual characteristics as controls."),
        tags$li("One OLS model per domain and outcome; an integrated model; formal mediation through belonging; buffer-radius and robustness checks.")),
      h4("Verification"),
      if (!is.null(verif)) {
        tb <- table(verif$verdict)
        p(class = "note", sprintf("The last rebuild was checked against the published values: %s.",
          paste(sprintf("%d %s", as.integer(tb), names(tb)), collapse = ", ")))
      } else p(class = "note", "rebuild_verification.csv not found."),
      if (file.exists("www/Graphical_Abstract.png")) img(src = "Graphical_Abstract.png", style = "width:100%; max-width: 1100px; margin-top: 12px;")
    ), column(4,
      h4("Model fit"),
      DTOutput("fit_tbl")
    ))
  ),

  tabPanel("Domain models",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("dom", "Variable group", choices = domains_fig2, selected = domains_fig2[min(3, length(domains_fig2))]),
        checkboxGroupInput("outc", "Outcome", choices = c("Subjective well-being" = "swb", "Neighborhood belonging" = "bel"),
                           selected = c("swb", "bel")),
        checkboxInput("ci90", "Show 90% instead of 95% intervals", FALSE),
        p(class = "note", "Each built-environment measure is shown as estimated in its own domain model, which adjusts for individual characteristics and neighborhood demographics. Filled markers: p < 0.05. Coefficients are in standard deviations of the outcome per standard deviation of the predictor (binary predictors: per group difference).")
      ),
      mainPanel(width = 9,
        plotOutput("coef_plot", height = "520px"),
        h4("Coefficients"),
        DTOutput("coef_tbl")
      )
    )
  ),

  tabPanel("Integrated model & mediation",
    h4("Integrated model (n = 304)"),
    fluidRow(column(8, plotOutput("fig3_plot", height = "420px")),
             column(4, p(class = "note", "The strongest predictor from each domain (smallest p across the two outcomes, kept if < 0.10), estimated with and without belonging: SWB (circles), belonging (triangles), SWB with belonging added (squares). Selected on the domain results, so a synthesis rather than independent confirmation."))),
    h4("Formal mediation through belonging"),
    DTOutput("med_tbl"),
    p(class = "note", "ACME = indirect effect through belonging; ADE = direct effect. 1,000 bootstrap resamples, Holm correction across the six tests. Exposures were selected on their domain-model significance, so this analysis is exploratory."),
    if (!is.null(med_sens)) tagList(h4("Sensitivity to unmeasured confounding"), DTOutput("sens_tbl"),
      p(class = "note", "rho* is the correlation between the errors of the belonging and well-being equations at which the indirect effect would be zero; the R² columns give the share of variance the measured covariates explain, for scale."),
      if (file.exists("www/FigureS13_MediationSensitivity.png")) img(src = "FigureS13_MediationSensitivity.png", style = "width:100%; max-width: 1400px;"))
  ),

  tabPanel("Sensitivity checks",
    tabsetPanel(
      tabPanel("Buffer radius",
        br(),
        if (!is.null(buf)) fluidRow(
          column(3, selectInput("buf_exp", "Exposure", choices = sort(unique(buf$exposure))),
                 p(class = "note", "Every buffer-derived measure re-estimated at 400 m, 800 m and 1,600 m on the same respondents, holding every other term fixed. The paper reports 800 m.")),
          column(9, plotOutput("buf_plot", height = "380px"), DTOutput("buf_tbl"))
        ) else p("buffer_sensitivity.csv not found.")),
      tabPanel("Greenness specification",
        br(),
        if (!is.null(green)) fluidRow(
          column(3, radioButtons("green_oc", "Outcome", choices = c("Subjective well-being" = "swb_z", "Neighborhood belonging" = "belonging_z")),
                 p(class = "note", "The Tree Equity Score is derived from its companion canopy measure (r = .89). A: both entered, as in the domain model; B: Score only; C: canopy only; D: neither, which recovers 66 respondents.")),
          column(9, plotOutput("green_plot", height = "420px"))
        ) else p("greenness_sensitivity.csv not found.")),
      tabPanel("Common sample (n = 189)", br(), DTOutput("rob_cs_tbl")),
      tabPanel("Spatial standard errors", br(), DTOutput("rob_se_tbl")),
      tabPanel("Robust regression", br(), DTOutput("rob_mm_tbl")),
      tabPanel("Item overlap", br(), DTOutput("rob_loo_tbl"))
    )
  ),

  tabPanel("Descriptives",
    sidebarLayout(
      sidebarPanel(width = 3,
        if (!is.null(desc_idx)) selectInput("desc_group", "Variable group",
          choices = as.list(setNames(desc_idx$table, paste0(desc_idx$group, " (", desc_idx$kind, ")")))),
        p(class = "note", "Descriptive statistics for every variable used in any model, and the distribution panel for its group.")),
      mainPanel(width = 9, DTOutput("desc_tbl"), uiOutput("desc_panel"))
    )
  ),

  tabPanel("Map",
    sidebarLayout(
      sidebarPanel(width = 3,
        if (!is.null(geo)) selectInput("map_layer", "Layer", choices = as.list(c(
          "Respondents per census tract" = "counts",
          setNames(vapply(geo$area_vars, `[[`, "", "col"), vapply(geo$area_vars, `[[`, "", "lab"))))),
        p(class = "note", if (!is.null(geo)) sprintf("Respondent counts are shown by census tract and suppressed where fewer than %d respondents live in a tract; no residential location is mapped. The other layers are public area-level measures, shown for the tracts or block groups in which respondents live.", geo$min_cell) else "geo.rds not found; run app/prepare_app_data.R.")),
      mainPanel(width = 9, leafletOutput("map", height = "640px"))
    )
  ),

  tabPanel("Variables",
    p(class = "note", "Every variable with its definition, unit, geography, vintage and source (Table S1 of the paper)."),
    DTOutput("var_tbl")
  )
)

# ---- server -------------------------------------------------------------
server <- function(input, output, session) {

  output$fit_tbl <- renderDT({
    fit %>% transmute(Model = model, n, `Adj. R²` = round(adj_r_squared, 3), Parameters = n_parameters) %>%
      datatable(rownames = FALSE, options = list(pageLength = 25, dom = "t"), class = "compact stripe")
  })

  sel_fig2 <- reactive({
    z <- if (isTRUE(input$ci90)) qnorm(.95) else qnorm(.975)
    fig2 %>% filter(group == input$dom) %>%
      mutate(swb_lo = swb_est - z * swb_se, swb_hi = swb_est + z * swb_se,
             bel_lo = bel_est - z * bel_se, bel_hi = bel_est + z * bel_se,
             label = factor(label, levels = rev(unique(label))))
  })

  output$coef_plot <- renderPlot({
    d <- sel_fig2()
    long <- bind_rows(
      if ("swb" %in% input$outc) d %>% transmute(label, outcome = "Subjective well-being", est = swb_est, lo = swb_lo, hi = swb_hi, p = swb_p),
      if ("bel" %in% input$outc) d %>% transmute(label, outcome = "Neighborhood belonging", est = bel_est, lo = bel_lo, hi = bel_hi, p = bel_p))
    validate(need(nrow(long) > 0, "Choose at least one outcome."))
    long$sig <- long$p < .05
    ggplot(long, aes(x = est, y = label, colour = outcome)) +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, position = position_dodge(width = .6), linewidth = .7) +
      geom_point(aes(shape = outcome, fill = sigfill(sig, outcome)), size = 3.2, position = position_dodge(width = .6), stroke = 1) +
      scale_colour_manual(values = c("Subjective well-being" = SWB_COL, "Neighborhood belonging" = BEL_COL)) +
      scale_fill_manual(values = c("Subjective well-being" = SWB_COL, "Neighborhood belonging" = BEL_COL), na.value = "white", guide = "none") +
      scale_shape_manual(values = c("Subjective well-being" = 21, "Neighborhood belonging" = 24)) +
      labs(x = "Standardized coefficient", y = NULL, colour = NULL, shape = NULL, title = input$dom) +
      theme_app()
  })

  output$coef_tbl <- renderDT({
    sel_fig2() %>% transmute(Measure = as.character(label),
      `SWB β` = fmt(swb_est), `SWB SE` = fmt(swb_se), `SWB p` = paste0(fmt(swb_p), stars(swb_p)),
      `Belonging β` = fmt(bel_est), `Belonging SE` = fmt(bel_se), `Belonging p` = paste0(fmt(bel_p), stars(bel_p))) %>%
      datatable(rownames = FALSE, options = list(pageLength = 25, dom = "t"), class = "compact stripe")
  })

  output$fig3_plot <- renderPlot({
    d <- fig3 %>% mutate(label = factor(label, levels = rev(label)))
    long <- bind_rows(
      d %>% transmute(label, model = "SWB", est = swb_est, lo = swb_lo, hi = swb_hi, p = swb_p),
      d %>% transmute(label, model = "Belonging", est = bel_est, lo = bel_lo, hi = bel_hi, p = bel_p),
      d %>% transmute(label, model = "SWB + belonging", est = swbb_est, lo = swbb_lo, hi = swbb_hi, p = swbb_p)) %>%
      filter(!is.na(est)) %>% mutate(model = factor(model, levels = c("SWB", "Belonging", "SWB + belonging")), sig = p < .05)
    ggplot(long, aes(x = est, y = label, colour = model, shape = model)) +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, position = position_dodge(width = .7), linewidth = .7) +
      geom_point(aes(fill = sigfill(sig, model)), size = 3.2, position = position_dodge(width = .7), stroke = 1) +
      scale_colour_manual(values = c("SWB" = SWB_COL, "Belonging" = BEL_COL, "SWB + belonging" = BOTH_COL)) +
      scale_fill_manual(values = c("SWB" = SWB_COL, "Belonging" = BEL_COL, "SWB + belonging" = BOTH_COL), na.value = "white", guide = "none") +
      scale_shape_manual(values = c("SWB" = 21, "Belonging" = 24, "SWB + belonging" = 22)) +
      labs(x = "Standardized coefficient (95% CI)", y = NULL, colour = NULL, shape = NULL) + theme_app()
  })

  output$med_tbl <- renderDT({
    validate(need(!is.null(med), "Table09.csv not found."))
    datatable(med, rownames = FALSE, width = "100%", options = list(pageLength = 10, dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "compact stripe nowrap")
  })
  output$sens_tbl <- renderDT({
    med_sens %>% transmute(Exposure = label, n, `ACME (bootstrap)` = fmt(acme_boot), `ACME (quasi-Bayes)` = fmt(acme_qb),
                           `rho*` = fmt(rho_star, 2), `R² mediator model` = fmt(r2_mediator_model), `R² outcome model` = fmt(r2_outcome_model)) %>%
      datatable(rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  output$buf_plot <- renderPlot({
    d <- buf %>% filter(exposure == input$buf_exp) %>%
      mutate(lo = estimate - 1.96 * std_error, hi = estimate + 1.96 * std_error, radius = factor(radius_m), sig = p_value < .05)
    ggplot(d, aes(x = radius, y = estimate, colour = outcome, group = outcome)) +
      geom_hline(yintercept = 0, colour = "grey60") +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, position = position_dodge(width = .4)) +
      geom_line(position = position_dodge(width = .4), alpha = .5) +
      geom_point(aes(fill = sigfill(sig, outcome), shape = outcome), size = 3.4, position = position_dodge(width = .4), stroke = 1) +
      scale_colour_manual(values = c("Subjective well-being" = SWB_COL, "Neighborhood belonging" = BEL_COL)) +
      scale_fill_manual(values = c("Subjective well-being" = SWB_COL, "Neighborhood belonging" = BEL_COL), na.value = "white", guide = "none") +
      scale_shape_manual(values = c("Subjective well-being" = 21, "Neighborhood belonging" = 24)) +
      labs(x = "Buffer radius (m)", y = "Standardized coefficient (95% CI)", colour = NULL, shape = NULL,
           title = unique(d$domain)[1], subtitle = sprintf("n = %d at every radius", unique(d$n)[1])) + theme_app()
  })
  output$buf_tbl <- renderDT({
    buf %>% filter(exposure == input$buf_exp) %>%
      transmute(Outcome = outcome, `Radius (m)` = radius_m, n, β = fmt(estimate), SE = fmt(std_error), p = paste0(fmt(p_value), stars(p_value)), `Adj. R²` = fmt(adj_r2)) %>%
      datatable(rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  output$green_plot <- renderPlot({
    d <- green %>% filter(outcome == input$green_oc) %>%
      mutate(lo = estimate - 1.96 * std_error, hi = estimate + 1.96 * std_error, sig = p_value < .05,
             term = lab(term), spec = sub(" \\(as in the domain model\\)", "", spec))
    ggplot(d, aes(x = estimate, y = term, colour = spec)) +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, position = position_dodge(width = .7)) +
      geom_point(aes(fill = sigfill(sig, spec)), shape = 21, size = 3, position = position_dodge(width = .7), stroke = 1) +
      scale_fill_discrete(na.value = "white", guide = "none", na.translate = TRUE) +
      labs(x = "Standardized coefficient (95% CI)", y = NULL, colour = "Specification") + theme_app() +
      theme(legend.position = "right")
  })

  simple_dt <- function(d) { validate(need(!is.null(d), "File not found.")); datatable(d, rownames = FALSE, width = "100%", options = list(pageLength = 25, scrollX = TRUE), class = "compact stripe nowrap") }
  output$rob_cs_tbl  <- renderDT(simple_dt(rob_cs))
  output$rob_se_tbl  <- renderDT(simple_dt(rob_se))
  output$rob_mm_tbl  <- renderDT(simple_dt(rob_mm))
  output$rob_loo_tbl <- renderDT(simple_dt(rob_loo))

  output$desc_tbl <- renderDT({
    req(input$desc_group, desc_idx)
    f <- desc_idx$file[desc_idx$table == input$desc_group]
    d <- rd(file.path("descriptives", f), col_types = cols(.default = "c"))
    datatable(d, rownames = FALSE, options = list(pageLength = 30, dom = "t"), class = "compact stripe")
  })
  output$desc_panel <- renderUI({
    req(input$desc_group, desc_idx)
    g <- desc_idx$group[desc_idx$table == input$desc_group]
    png <- paste0("Panel_", gsub("[^A-Za-z0-9]+", "_", g), ".png")
    if (file.exists(file.path("www", png))) tagList(h4("Distributions"), img(src = png, style = "width:100%; max-width: 1000px;"))
  })

  output$map <- renderLeaflet({
    validate(need(!is.null(geo), "geo.rds not found; run app/prepare_app_data.R"))
    m <- leaflet() %>% addTiles(options = tileOptions(opacity = 0.7)) %>%
      addPolygons(data = geo$metro, fill = FALSE, color = "#555", weight = 1.2, group = "counties")
    bb <- sf::st_bbox(geo$metro)
    m %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  observe({
    req(geo, input$map_layer)
    proxy <- leafletProxy("map") %>% clearGroup("layer") %>% clearControls()
    if (input$map_layer == "counts") {
      g <- geo$tracts[!is.na(geo$tracts$label), ]
      pal <- colorBin("Blues", domain = g$n_shown, bins = 4, na.color = "#DCDCDC")
      proxy %>% addPolygons(data = g, group = "layer", fillColor = ~pal(n_shown), fillOpacity = .75, color = "white", weight = .5,
                            label = ~paste0("Tract ", GEOID, ": ", label, " respondents")) %>%
        addLegend("bottomright", pal = pal, values = g$n_shown, title = "Respondents per tract",
                  na.label = sprintf("fewer than %d", geo$min_cell))
    } else {
      v <- Filter(function(x) x$col == input$map_layer, geo$area_vars)[[1]]
      g <- if (v$geo == "tract") geo$tracts else geo$block_groups
      g <- g[!is.na(g[[v$col]]), ]
      pal <- colorQuantile("YlGnBu", domain = g[[v$col]], n = 5)
      proxy %>% addPolygons(data = g, group = "layer", fillColor = ~pal(g[[v$col]]), fillOpacity = .75, color = "white", weight = .4,
                            label = ~paste0(v$lab, ": ", signif(g[[v$col]], 4))) %>%
        addLegend("bottomright", pal = pal, values = g[[v$col]], title = paste0(v$lab, "<br><small>", v$unit, ", quintiles</small>"))
    }
  })

  output$var_tbl <- renderDT({
    validate(need(!is.null(vartab), "variable_table.csv not found."))
    datatable(vartab, rownames = FALSE, options = list(pageLength = 50, scrollX = TRUE), class = "compact stripe")
  })
}

shinyApp(ui, server)
