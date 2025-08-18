# app.R — Real Estate Investment Analyzer (R 4.2.2) — Year selector bounded by Term & Hold
# Packages ----
library(shiny)
library(DT)
library(plotly)
library(rsconnect)
suppressWarnings({ if (requireNamespace("bslib", quietly = TRUE)) library(bslib) })

# ---------- Helpers ----------
pmt <- function(rate, nper, pv) { if (rate == 0) return(-pv / nper);  -(rate * pv) / (1 - (1 + rate)^(-nper)) }
npv_at <- function(rate, cashflows) sum(cashflows / (1 + rate)^(seq_along(cashflows) - 1))
irr <- function(cashflows, lower = -0.99, upper = 2.0) {
  f <- function(r) npv_at(r, cashflows)
  if (f(lower) * f(upper) > 0) return(NA_real_)
  tryCatch(uniroot(f, lower = lower, upper = upper)$root, error = function(e) NA_real_)
}
fmt_dollar <- function(x, digits = 0) paste0("$", formatC(x, big.mark = ",", format = "f", digits = digits))
fmt_perc   <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
kpi_card <- function(title, value) wellPanel(
  div(style="font-size:12px;color:#666;", title),
  div(style="font-size:28px;font-weight:600;margin-top:4px;", value)
)

# ---------- UI ----------
ui <- fluidPage(
  # select theme
  theme = if ("package:bslib" %in% search()) bslib::bs_theme(
    version   = 5,
    bg        = "white",      # page background
    fg        = "#333333",    # text color
    primary   = "#3366CC",       # main accent (buttons, active tabs, slider bar)
    secondary = "#FF9900",     # secondary accent
    base_font = bslib::font_google("Inter")
  ),
  
  # Custom CSS tweaks 
  tags$head(
    tags$style(HTML("
    /* ---- base ---- */
    body { background-color: var(--bs-body-bg); color: var(--bs-body-color); }
    h1, h2, h3, h4 { color: var(--bs-primary); font-weight: 700; }
    .help-block { color:#6b6b6b; }

    /* ---- sidebar well ---- */
    .sidebarPanel, .well {
      background:#f8f9fa !important;      /* light gray */
      border:1px solid #e9ecef !important; /* subtle border */
      border-radius:12px !important;
      box-shadow: 0 2px 6px rgba(0,0,0,0.04);
    }

    /* ---- inputs ---- */
    .form-control, .selectize-input, .selectize-dropdown {
      background:#ffffff !important;
      border-color:#e9ecef !important;
      color: var(--bs-body-color) !important;
    }
    .form-control:focus {
      border-color: var(--bs-primary) !important;
      box-shadow: 0 0 0 0.2rem rgba(var(--bs-primary-rgb), .25);
    }

    /* ---- buttons ---- */
    .btn-primary {
      background: var(--bs-primary) !important;
      border-color: var(--bs-primary) !important;
      color: #fff !important;
    }
    .btn-primary:hover, .btn-primary:focus { filter: brightness(0.95); }

    /* ---- tabs (BS3 + BS5) ---- */
    .nav-tabs > li > a,
    .nav-tabs .nav-link {
      background:#f8f9fa;
      border:1px solid #e9ecef;
      color: var(--bs-body-color);
      border-bottom-color: transparent;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover,
    .nav-tabs .nav-link.active,
    .nav-tabs .nav-link.active:focus,
    .nav-tabs .nav-link.active:hover {
      background: var(--bs-secondary) !important;
      color:#fff !important;
      border-color: var(--bs-secondary) !important;
    }
    .tab-content {
      background:#ffffff;
      border:1px solid #e9ecef;
      border-top:none;
      padding:16px;
      border-radius:0 0 12px 12px;
    }

    /* ---- tables ---- */
    table.table {
      background:#f8f9fa;
      border-color:#e9ecef;
    }
    table.table > thead > tr > th {
      background: var(--bs-secondary);
      color:#fff;
      border-color: var(--bs-secondary);
    }
    table.table > tbody > tr > td {
      border-color:#e9ecef !important;
    }

    /* ---- KPI wells (your kpi_card uses .well) ---- */
    .well > div:first-child { color:#6b6b6b; } /* small title */
    .well > div:nth-child(2) { color:#2b2b2b; } /* big number */

    /* ---- sliders (ion.rangeSlider) ---- */
    .irs-bar, .irs-bar-edge { background: var(--bs-primary) !important; border-color: var(--bs-primary) !important; }
    .irs-line { background:#f1f3f5 !important; border:1px solid #e9ecef !important; }
    .irs-single, .irs-from, .irs-to { background: var(--bs-primary) !important; }
    .irs-handle > i:first-child { background: var(--bs-primary) !important; }

    /* ---- plotly containers ---- */
    .plotly, .js-plotly-plot .plotly .modebar { border-radius:12px; }
  "))
  ),
  
  
  # ---- UI content  ----
  if ("package:bslib" %in% search()) bslib::bs_theme_dependencies(bslib::bs_theme(version = 5)),
  titlePanel(
    div(
      h1("Long-term Real Estate Investment Analyzer", style = "margin-bottom:0;"),
      h4("by Liz Ortiz", 
         style = "margin-top:5px; color:#6b6b6b; font-weight:400;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Visualization"),
                 checkboxInput("include_sale","Include sale proceeds in charts", TRUE),
                 sliderInput("view_year","Year to view", min = 1, max = 30, value = 1, step = 1, animate = TRUE),
                 helpText("Bounded by the shorter of Loan Term and Holding Period."),
                 tags$hr(),
                 
                 h4("Property"),
                 numericInput("price","Purchase price",350000,1,step=1000),
                 numericInput("closing","Buyer closing costs (% of price)",0.03,0,0.15,0.005),
                 numericInput("hold_yrs","Holding period (years)",10,1,step=1),
                 numericInput("appr","Annual appreciation",0.03,-0.2,0.2,0.005),
                 numericInput("sell_costs","Selling costs (% of sale price)",0.06,0,0.12,0.005),
                 tags$hr(),
                 
                 h4("Financing"),
                 numericInput("down","Down payment (%)",0.20,0,1,0.01),
                 numericInput("rate","Interest rate (APR)",0.065,0,0.25,0.001),
                 numericInput("term","Loan term (years)",30,1,40,1),
                 tags$hr(),
                 
                 h4("Income & Ops"),
                 numericInput("rent","Monthly rent (Year 1)",2600,0,step=50),
                 numericInput("other_inc","Other monthly income (Year 1)",0,0,step=10),
                 numericInput("rent_growth","Rent growth (annual)",0.025,-0.2,0.2,0.005),
                 numericInput("vac","Vacancy & credit loss (%)",0.05,0,0.5,0.005),
                 numericInput("tax_rate","Property tax rate (% of value)",0.013,0,0.05,0.001),
                 numericInput("insur_y","Insurance (Year 1, annual $)",2200,0,step=50),
                 numericInput("hoa_y","HOA (Year 1, annual $)",0,0,step=10),
                 
                 numericInput("maint","Maintenance & repairs (% of EGI)",0.08,0,0.4,0.005),
                 numericInput("mgmt","Property management (% of EGI)",0.08,0,0.25,0.005),
                 numericInput("capex","CapEx reserves (% of EGI)",0.05,0,0.25,0.005),
                 
                 numericInput("other_opex_y","Other OpEx (Year 1, annual $)",0,0,step=50),
                 
                 numericInput("exp_growth","OpEx growth (annual)",0.02,-0.2,0.2,0.005),
                 tags$hr(),
                 
                 h4("Tax (optional)"),
                 checkboxInput("use_tax","Include income taxes",FALSE),
                 conditionalPanel(
                   condition = "input.use_tax == true",
                   numericInput("tax_bracket","Marginal income tax rate",0.24,0,0.5,0.01),
                   numericInput("dep_yrs","Depreciation years (res. 27.5)",27.5,1,step=0.5),
                   numericInput("land_frac","Land fraction (non-depreciable)",0.2,0,0.9,0.05),
                   numericInput("cg_tax","Capital gains tax rate (at sale)",0.15,0,0.37,0.01)
                 ),
                 
                 h4("Compare / Save"),
                 textInput("scenario_name","Scenario name","Subject Property A"),
                 actionButton("add_scn","Add scenario to comparison"),
                 fileInput("csv","Upload CSV to compare (optional)",accept=c(".csv")),
                 downloadButton("dl_template","Download CSV template")
    ),
    
    mainPanel(width = 9,
              tabsetPanel(id="tabs",
                          
                          tabPanel("About",
                                   tags$h3("How to use"),
                                   tags$ol(
                                     tags$li("Enter assumptions in the sidebar."),
                                     tags$li("Use 'Year to view' to see results for that specific year (bounded by Loan Term & Holding Period)."),
                                     tags$li("Results, Cash Flow, and Sensitivity reflects the selected year."),
                                     tags$li("Use the Advisor tab to set a target profit year and cash flow goal. The program will suggest the rent required to meet that goal."),
                                     tags$li("Add scenarios or upload a CSV to compare properties.")
                                   )
                          ),
                          
                          tabPanel("Results",
                                   fluidRow(
                                     column(4, uiOutput("vb_cap")),
                                     column(4, uiOutput("vb_coc")),
                                     column(4, uiOutput("vb_dscr"))
                                   ),
                                   fluidRow(
                                     column(6,
                                            h4("Summary"),
                                            tableOutput("summary_tbl"),
                                            h4("Year Detail"),
                                            tableOutput("year_tbl"),
                                            h4(textOutput("acf_title")),
                                            plotlyOutput("cf_plot")
                                     ),
                                     column(6,
                                            h4("Key Metrics"),
                                            DTOutput("metrics_tbl"),
                                            h4("Cash In vs Cash Out"),
                                            plotlyOutput("pie_io"),
                                            h4("Cumulative Cash Flow & Payback"),
                                            plotlyOutput("cum_cf_plot")
                                     )
                                   )
                          ),
                          
                          tabPanel("Cash Flow",
                                   h4(textOutput("cf_tab_title")),
                                   tableOutput("cashflow_tbl"),
                                   tags$hr(),
                                   h4("Cash Flow Over Time"),
                                   plotlyOutput("cashflow_over_time")
                          ),
                          
                          tabPanel(
                            "Sensitivity",
                            tagList(
                              # Tab-level intro
                              tags$div(
                                style = "margin-bottom:12px;",
                                tags$h3("Sensitivity Analysis"),
                                tags$p("Explore how performance changes if your assumptions shift. The selected year (from the sidebar) drives these views.")
                              ),
                              
                              fluidRow(
                                column(
                                  6,
                                  h4(textOutput("sens_title1")),
                                  #how-it-works (collapsible)
                                  tags$details(
                                    open = FALSE,
                                    tags$summary("How this works (Rent vs Interest — Cash-on-Cash)"),
                                    tags$div(
                                      style = "margin:6px 0 10px 0; color:#6b6b6b;",
                                      tags$ul(
                                        tags$li(tags$b("Sliders:"), " define the ± range of change to test around your current rent and loan interest rate."),
                                        tags$li("For example, if rent is $2,000 and you choose ±15%, the heatmap runs from $1,700 → $2,300."),
                                        tags$li("If your loan rate is 6% and you choose ±1%, the heatmap runs from 5% → 7%."),
                                        tags$li(tags$b("Grid cells:"), " each cell shows the resulting Cash-on-Cash in the selected year."),
                                        tags$li(tags$b("Use it to:"), " see which rent/interest combinations keep CoC positive or hit a target.")
                                      )
                                    )
                                  ),
                                  sliderInput("sens_rent", "± % around current rent",
                                              min = 0.05, max = 0.3, value = 0.15, step = 0.05
                                  ),
                                  sliderInput("sens_rate", "± % points around current rate",
                                              min = 0.5, max = 3, value = 1, step = 0.25
                                  ),
                                  plotlyOutput("sens_heat")
                                ),
                                column(
                                  6,
                                  h4(textOutput("sens_title2")),
                                  # how-it-works (collapsible)
                                  tags$details(
                                    open = FALSE,
                                    tags$summary("How this works (Price vs Vacancy — Cap Rate & DSCR)"),
                                    tags$div(
                                      style = "margin:6px 0 10px 0; color:#6b6b6b;",
                                      tags$ul(
                                        tags$li(tags$b("Sliders:"), " define the ± range of change around the Year-N value and vacancy rate."),
                                        tags$li("For example, if property value is $500k and you choose ±20%, the scatter tests $400k → $600k."),
                                        tags$li("If vacancy is 5% and you choose ±0.05 (5 points), the scatter tests 0% → 10%."),
                                        tags$li(tags$b("Dots:"), " each point shows the resulting ", tags$b("Cap Rate (x)"), " and ", tags$b("DSCR (y)"), "."),
                                        tags$li(tags$b("Cap Rate:"), " NOI ÷ Value in the selected year."),
                                        tags$li(tags$b("DSCR:"), " NOI ÷ Annual debt service (>", tags$i("1.20 is a common threshold"), ")."),
                                        tags$li(tags$b("Use it to:"), " spot safe ranges where DSCR stays above lender thresholds and Cap Rate meets your goals.")
                                      )
                                    )
                                  ),
                                  sliderInput("sens_price", "± % around current Year-N value",
                                              min = 0.05, max = 0.3, value = 0.15, step = 0.05
                                  ),
                                  sliderInput("sens_vac", "± points around current vacancy",
                                              min = 0.02, max = 0.15, value = 0.05, step = 0.01
                                  ),
                                  plotlyOutput("sens_scatter")
                                )
                              )
                            )
                          ),
                          
                          tabPanel("Advisor",
                                   tagList(
                                     # ---- Intro section / Explanation ----
                                     tags$div(
                                       style = "margin-bottom:16px;",
                                       tags$h3("How to Use the Advisor"),
                                       tags$p("This tool helps you set profit targets and see what rent you’d need today to reach them."),
                                       tags$ul(
                                         tags$li("Pick the year you want to be profitable."),
                                         tags$li("Set a target annual cash flow (NOI - Debt service)."),
                                         tags$li("The app suggests what rent you’d need, starting today, to hit that target."),
                                         tags$li("Compare across years to understand how rent adjustments affect profitability.")
                                       ),
                                       tags$p(style="color:#6b6b6b;font-size:90%;",
                                              "Note: This does not include sale proceeds. It focuses on operating cash flow only.")
                                     ),
                                     
                                     # ---- Main layout/ Plots and Tables ----
                                     fluidRow(
                                       column(4,
                                              h4("Profit Target"),
                                              sliderInput("advisor_year", "Target Year to be Profitable",
                                                          min = 1, max = 10, value = 1, step = 1),
                                              numericInput("advisor_target_cf", "Target annual cash flow in that year ($)",
                                                           value = 0, step = 500, min = -1e6),
                                              helpText("This uses operating cash flow only (NOI - Debt service). Sale proceeds are not considered.")
                                       ),
                                       column(8,
                                              h4("Suggested Rent to Hit Target"),
                                              tableOutput("advisor_table"),
                                              tags$hr(),
                                              h4("What rent would you need (set today) to hit the target in each year?"),
                                              plotlyOutput("advisor_curve"),
                                              div(style='margin-top:8px;color:#6b6b6b;',
                                                  "Two lines: the solid line is the rent you’d need today to meet your target in each future year; ",
                                                  "the dashed line is your current base rent grown by your rent-growth assumption.")
                                       )
                                     ),
                                     checkboxInput("advisor_no_decrease",
                                                   "Don't suggest decreasing rent (clamp to current base)", TRUE
                                     )
                                   )
                          ),
                          tabPanel("Compare Properties", DTOutput("compare_tbl"))
              )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # Keep "Year to view" bounded by the shorter of loan term and holding period
  observeEvent(list(input$term, input$hold_yrs), {
    max_year <- max(1, min(input$term, input$hold_yrs))
    updateSliderInput(session, "view_year",
                      min = 1, max = max_year, value = min(input$view_year, max_year)
    )
  })
  
  calc <- reactive({
    price <- input$price
    down  <- input$down
    loan_amt <- price * (1 - down)
    close_costs <- price * input$closing
    cash_to_close <- price * down + close_costs
    m_rate <- input$rate / 12
    nper <- input$term * 12
    pay <- if (loan_amt > 0) pmt(m_rate, nper, loan_amt) else 0
    ann_debt <- -12 * pay
    
    yrs <- seq_len(input$hold_yrs)
    
    # Year-1 base (monthly), then grow to Year N
    base_rent_monthly_y1 <- input$rent + input$other_inc
    rent_series_monthly  <- base_rent_monthly_y1 * (1 + input$rent_growth)^(yrs - 1)
    
    # Income / Vacancy (annual)
    gross_potential <- 12 * rent_series_monthly
    vacancy_loss    <- gross_potential * input$vac
    egi             <- gross_potential - vacancy_loss  # Rental Revenue displayed
    
    # Value growth & OpEx growth (annual)
    values <- price * (1 + input$appr)^(yrs - 1)
    taxes  <- values * input$tax_rate
    ins    <- input$insur_y * (1 + input$exp_growth)^(yrs - 1)
    hoa    <- input$hoa_y    * (1 + input$exp_growth)^(yrs - 1)
    maint  <- input$maint * egi
    mgmt   <- input$mgmt  * egi
    capex  <- input$capex * egi
    other_opex <- input$other_opex_y * (1 + input$exp_growth)^(yrs - 1)
    
    opex   <- taxes + ins + hoa + maint + mgmt + capex + other_opex
    noi    <- egi - opex
    
    # Debt service: zero after the loan term ends
    ann_debt_vec <- if (input$term >= length(yrs)) rep(ann_debt, length(yrs)) else
      c(rep(ann_debt, input$term), rep(0, length(yrs) - input$term))
    
    pre_tax_cf <- noi - ann_debt_vec
    
    # Optional income taxes 
    tax_cf <- rep(0, length(yrs))
    if (isTRUE(input$use_tax)) {
      basis_impr <- price * (1 - input$land_frac)
      ann_dep <- basis_impr / input$dep_yrs
      taxable_income <- pmax(0, noi - ann_debt_vec - ann_dep)
      tax_cf <- -input$tax_bracket * taxable_income
    }
    
    # Sale at end of hold
    sale_price    <- price * (1 + input$appr)^input$hold_yrs
    selling_costs <- sale_price * input$sell_costs
    
    # Remaining balance at sale (0 if hold_yrs >= term)
    if (loan_amt > 0) {
      k <- min(input$hold_yrs, input$term) * 12
      bal <- loan_amt * (1 + m_rate)^k + pay * ((1 + m_rate)^k - 1) / m_rate
      if (input$hold_yrs >= input$term) bal <- 0
      bal <- max(0, bal)
    } else bal <- 0
    
    sale_tax <- 0
    if (isTRUE(input$use_tax)) {
      total_dep <- min(input$hold_yrs, input$dep_yrs) * (price * (1 - input$land_frac) / input$dep_yrs)
      adj_basis <- price - total_dep
      gain <- max(0, sale_price - selling_costs - adj_basis)
      sale_tax <- -input$cg_tax * gain
    }
    
    net_sale_proceeds <- sale_price - selling_costs - bal + sale_tax
    
    # Cash flows: t0 + operating years (+ sale in final year)
    op_flows <- c(-cash_to_close, (pre_tax_cf + tax_cf))
    cfs <- op_flows; cfs[length(cfs)] <- cfs[length(cfs)] + net_sale_proceeds
    
    list(
      yrs = yrs,
      price = price,
      current_value = values,
      
      # Income / Vacancy
      gross_potential = gross_potential,
      vacancy_loss    = vacancy_loss,
      egi = egi,
      
      # OpEx components
      taxes = taxes, ins = ins, hoa = hoa, maint = maint, mgmt = mgmt, capex = capex, other_opex = other_opex,
      opex = opex,
      
      # Debt & CF
      ann_debt_vec = ann_debt_vec,
      pre_tax_cf   = pre_tax_cf + tax_cf,
      
      # Other
      cash_to_close = cash_to_close,
      loan_amt = loan_amt,
      net_sale_proceeds = net_sale_proceeds,
      cfs = cfs, op_flows = op_flows,
      irr = irr(cfs), npv10 = npv_at(0.10, cfs),
      base_rent_monthly_y1 = base_rent_monthly_y1
    )
  })
  
  # Selected year index (bounded by min(term, hold_yrs))
  sel_idx <- reactive({
    max_year <- max(1, min(input$term, input$hold_yrs))
    max(1, min(input$view_year, max_year))
  })
  
  # ---------- Results (Year N) ----------
  output$vb_cap <- renderUI({
    i <- sel_idx(); clc <- calc()
    cap_rate <- if (clc$price > 0) (clc$egi[i] - clc$opex[i]) / clc$price else NA
    kpi_card(paste0("Cap Rate (Year ", i, ")"), if (is.na(cap_rate)) "—" else fmt_perc(cap_rate, 2))
  })
  output$vb_coc <- renderUI({
    i <- sel_idx(); clc <- calc()
    coc <- ((clc$egi[i] - clc$opex[i]) - clc$ann_debt_vec[i]) / clc$cash_to_close
    kpi_card(paste0("Cash-on-Cash (Year ", i, ")"), fmt_perc(coc, 2))
  })
  output$vb_dscr <- renderUI({
    i <- sel_idx(); clc <- calc()
    noi_i <- clc$egi[i] - clc$opex[i]
    dscr <- if (clc$ann_debt_vec[i] > 0) noi_i / clc$ann_debt_vec[i] else Inf
    kpi_card(paste0("DSCR (Year ", i, ")"), if (is.infinite(dscr)) "No debt" else format(round(dscr, 2), nsmall = 2))
  })
  
  output$acf_title <- renderText({ paste0("Annual Cash Flow (Year 0 to Year ", input$hold_yrs, ")") })
  
  output$year_tbl <- renderTable({
    i <- sel_idx(); clc <- calc()
    data.frame(
      Item = c("EGI","Taxes","Insurance","HOA","Maintenance","Management","CapEx","Other OpEx","Total OpEx","NOI","Debt service","Operating CF"),
      Value = c(fmt_dollar(clc$egi[i]), fmt_dollar(clc$taxes[i]), fmt_dollar(clc$ins[i]), fmt_dollar(clc$hoa[i]),
                fmt_dollar(clc$maint[i]), fmt_dollar(clc$mgmt[i]), fmt_dollar(clc$capex[i]), fmt_dollar(clc$other_opex[i]),
                fmt_dollar(clc$opex[i]), fmt_dollar(clc$egi[i]-clc$opex[i]), fmt_dollar(clc$ann_debt_vec[i]),
                fmt_dollar((clc$egi[i]-clc$opex[i]) - clc$ann_debt_vec[i])),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE)
  
  output$summary_tbl <- renderTable({
    i <- sel_idx(); clc <- calc()
    data.frame(
      Metric = c("Price","Down payment","Loan amount","Cash to close",
                 paste0("NOI (Year ", i, ")"), paste0("Debt service (Year ", i, ")"),
                 paste0("Net sale proceeds (Year 0 to Year ", input$hold_yrs, ")")),
      Value = c(fmt_dollar(clc$price), fmt_perc(input$down), fmt_dollar(clc$loan_amt), fmt_dollar(clc$cash_to_close),
                fmt_dollar(clc$egi[i]-clc$opex[i]), fmt_dollar(clc$ann_debt_vec[i]), fmt_dollar(clc$net_sale_proceeds)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE)
  
  output$cf_tab_title <- renderText({
    paste0("Annual & Monthly Cash Flow Summary — Year ", sel_idx())
  })
  
  # ---------- Cash Flow tab (driven by sidebar inputs; Year N) ----------
  output$cashflow_tbl <- renderTable({
    i <- sel_idx(); clc <- calc()
    
    # Annual numbers (Year i)
    rental_rev_a  <- clc$egi[i]                       # after vacancy
    opex_a        <- clc$opex[i]
    pni_a         <- clc$ann_debt_vec[i]
    ins_a         <- clc$ins[i]
    tax_a         <- clc$taxes[i]
    hoa_a         <- clc$hoa[i]
    cap_maint_a   <- clc$capex[i] + clc$maint[i]      # combined line
    mgmt_a        <- clc$mgmt[i]
    vacancy_a     <- clc$vacancy_loss[i]
    other_a       <- clc$other_opex[i]
    
    cash_flow_a   <- (rental_rev_a - opex_a) - pni_a
    coc           <- if (clc$cash_to_close > 0) cash_flow_a / clc$cash_to_close else NA_real_
    
    # Monthly numbers
    to_m <- function(x) x / 12
    data.frame(
      Category = c(
        "Cash Flow", "Cash-on-Cash Return",
        "Rental Revenue", "Operating Expenses",
        "Principal and Interest", "Insurance", "Property Tax", "HOA", "Other",
        "CapEx & Maintenance", "Management", "Vacancy"
      ),
      Annual = c(
        fmt_dollar(cash_flow_a),
        if (is.na(coc)) "—" else fmt_perc(coc, 2),
        fmt_dollar(rental_rev_a),
        fmt_dollar(opex_a),
        fmt_dollar(pni_a),
        fmt_dollar(ins_a),
        fmt_dollar(tax_a),
        fmt_dollar(hoa_a),
        fmt_dollar(other_a),
        fmt_dollar(cap_maint_a),
        fmt_dollar(mgmt_a),
        fmt_dollar(vacancy_a)
      ),
      Monthly = c(
        fmt_dollar(to_m(cash_flow_a)),
        "",  # CoC shown as annual ratio only
        fmt_dollar(to_m(rental_rev_a)),
        fmt_dollar(to_m(opex_a)),
        fmt_dollar(to_m(pni_a)),
        fmt_dollar(to_m(ins_a)),
        fmt_dollar(to_m(tax_a)),
        fmt_dollar(to_m(hoa_a)),
        fmt_dollar(to_m(other_a)),
        fmt_dollar(to_m(cap_maint_a)),
        fmt_dollar(to_m(mgmt_a)),
        fmt_dollar(to_m(vacancy_a))
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # --- Cash Flow Over Time Plot ---
  output$cashflow_over_time <- renderPlotly({
    clc <- calc()
    df <- data.frame(
      Year = clc$yrs,
      AnnualCashFlow = (clc$egi - clc$opex) - clc$ann_debt_vec
    )
    
    plot_ly(
      df,
      x = ~Year,
      y = ~AnnualCashFlow,
      type = "scatter",
      mode = "lines+markers+text",  # add text mode
      text = ~paste0("$", format(round(AnnualCashFlow, 2), big.mark = ",")), # label text
      textposition = "top center",  # place labels above points
      hoverinfo = "x+y"
    ) %>%
      layout(
        title = "Cash Flow Over Time",
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Annual Cash Flow ($)")
      )
  })
  
  output$cf_plot <- renderPlotly({
    clc <- calc()
    df <- data.frame(
      Year = 0:length(clc$yrs),
      CashFlow = clc$cfs
    )
    
    i <- sel_idx()
    vline <- list(
      type = "line",
      x0 = i, x1 = i,
      y0 = min(df$CashFlow, na.rm = TRUE),
      y1 = max(df$CashFlow, na.rm = TRUE),
      line = list(dash = "dot", width = 2)
    )
    
    plot_ly(
      df,
      x = ~Year,
      y = ~CashFlow,
      type = "bar",
      text = ~paste0("$", format(round(CashFlow, 2), big.mark = ",")),  # label text
      textposition = "outside",  # place above bars (use "auto" if bars can be negative)
      hoverinfo = "x+y"
    ) %>%
      layout(
        yaxis = list(title = "Annual Cash Flow ($)"),
        xaxis = list(dtick = 1),
        bargap = 0.15,
        title = paste0("Cash Flows (Year 0 to Year ", input$hold_yrs, ")"),
        shapes = list(vline)
      )
  })
  
  
  output$metrics_tbl <- renderDT({
    i <- sel_idx(); clc <- calc()
    
    noi_i <- clc$egi[i] - clc$opex[i]                 # <- compute NOI inline
    cap_rate <- if (clc$price > 0) noi_i / clc$price else NA
    coc <- (noi_i - clc$ann_debt_vec[i]) / clc$cash_to_close
    dscr <- if (clc$ann_debt_vec[i] > 0) noi_i / clc$ann_debt_vec[i] else Inf
    
    dat <- data.frame(
      Metric = c(
        paste0("Cap Rate (Year ", i, ")"),
        paste0("Cash-on-Cash (Year ", i, ")"),
        paste0("DSCR (Year ", i, ")"),
        "IRR (H-year)", "NPV @ 10%", "Holding Years"
      ),
      Value  = c(
        if (is.na(cap_rate)) "—" else fmt_perc(cap_rate, 2),
        fmt_perc(coc, 2),
        if (is.infinite(dscr)) "No debt" else format(round(dscr, 2), nsmall = 2),
        if (is.na(clc$irr)) "N/A" else fmt_perc(clc$irr, 2),
        fmt_dollar(clc$npv10),
        length(clc$yrs)
      ),
      stringsAsFactors = FALSE
    )
    datatable(dat, rownames = FALSE, options = list(dom = 't', ordering = FALSE))
  })
  
  
  # ---------- Horizon visuals (full period) ----------
  output$pie_io <- renderPlotly({
    flows <- if (isTRUE(input$include_sale)) calc()$cfs else calc()$op_flows
    invested <- sum(abs(flows[flows < 0])); received <- sum(flows[flows > 0])
    plot_ly(labels=c("Invested (Cash Out)","Received (Cash In)"), values=c(invested, received), type="pie",
            textinfo="label+percent", hoverinfo="label+value+percent") %>%
      layout(title=if (isTRUE(input$include_sale)) "Total Cash Out vs Cash In (Including Sale)"
             else "Total Cash Out vs Cash In (Operating Only)", showlegend=TRUE)
  })
  
  output$cum_cf_plot <- renderPlotly({
    flows <- if (isTRUE(input$include_sale)) calc()$cfs else calc()$op_flows
    yrs   <- 0:(length(flows)-1)
    cumcf <- cumsum(flows)
    df <- data.frame(Year = yrs, Cumulative = cumcf)
    idx <- which(cumcf >= 0)
    shapes <- list(); ann <- list()
    if (length(idx) > 0) {
      k <- idx[1]
      pay_x <- if (k == 1) 0 else {
        y0 <- cumcf[k - 1]; y1 <- cumcf[k]; (k - 1) + if ((y1 - y0) != 0) (-y0) / (y1 - y0) else 0
      }
      shapes <- list(list(type="line", x0=pay_x, x1=pay_x, y0=min(cumcf, na.rm=TRUE), y1=max(cumcf, na.rm=TRUE),
                          line=list(dash="dash", width=2)))
      ann <- list(list(x=pay_x, y=max(cumcf, na.rm=TRUE),
                       text=paste0("Payback ~ Year ", round(pay_x, 2)),
                       showarrow=TRUE, ax=0, ay=-40, bgcolor="rgba(255,255,255,0.8)"))
    }
    plot_ly(df, x=~Year, y=~Cumulative, type="scatter", mode="lines+markers", hoverinfo="x+y") %>%
      layout(title=if (isTRUE(input$include_sale)) "Cumulative Cash Flow (Including Sale)" else "Cumulative Cash Flow (Operating Only)",
             xaxis=list(title="Year", dtick=1), yaxis=list(title="Cumulative ($)"),
             shapes=shapes, annotations=ann)
  })
  
  # ---------- Sensitivity (Year N) ----------
  output$sens_title1 <- renderText({ paste0("Sensitivity (Year ", sel_idx(), "): Rent vs Interest (Cash-on-Cash)") })
  output$sens_title2 <- renderText({ paste0("Sensitivity (Year ", sel_idx(), "): Price vs Vacancy (Cap Rate & DSCR)") })
  
  # Rent vs Interest (Year N)
  output$sens_heat <- renderPlotly({
    i <- sel_idx(); clc <- calc()
    taxes_i <- clc$taxes[i]; ins_i <- clc$ins[i]; hoa_i <- clc$hoa[i]
    maint_pct <- input$maint; mgmt_pct <- input$mgmt; capex_pct <- input$capex
    vacancy <- input$vac; loan_amt <- clc$loan_amt; term <- input$term
    r0 <- clc$base_rent_monthly_y1
    rr <- seq(r0 * (1 - input$sens_rent), r0 * (1 + input$sens_rent), length.out = 11)
    rates <- seq(input$rate - input$sens_rate/100, input$rate + input$sens_rate/100, length.out = 11)
    
    mat <- outer(rr, rates, Vectorize(function(rent_try, rate_try) {
      egi_i <- 12 * (rent_try * (1 + input$rent_growth)^(i - 1)) * (1 - vacancy)
      maint_i <- maint_pct * egi_i; mgmt_i <- mgmt_pct * egi_i; capex_i <- capex_pct * egi_i
      opex_i <- taxes_i + ins_i + hoa_i + maint_i + mgmt_i + capex_i
      noi_i <- egi_i - opex_i
      pay <- if (loan_amt > 0) pmt(rate_try/12, term * 12, loan_amt) else 0
      ann_debt_i <- -12 * pay
      (noi_i - ann_debt_i) / clc$cash_to_close
    }))
    
    plot_ly(x = rates, y = rr, z = t(mat), type = "heatmap") %>%
      layout(xaxis = list(title = "Interest rate (APR)"),
             yaxis = list(title = "Monthly rent (Year 1 baseline; grown to Year N)"),
             title = paste0("Cash-on-Cash Sensitivity — Year ", i))
  })
  
  # Price vs Vacancy (Year N)
  output$sens_scatter <- renderPlotly({
    i <- sel_idx(); clc <- calc()
    base_value_i <- clc$current_value[i]
    prices <- seq(base_value_i * (1 - input$sens_price), base_value_i * (1 + input$sens_price), length.out = 25)
    vacs   <- seq(max(0, input$vac - input$sens_vac), min(0.5, input$vac + input$sens_vac), length.out = 25)
    
    loan_amt <- clc$loan_amt; rate <- input$rate; term <- input$term
    ins_i <- clc$ins[i]; hoa_i <- clc$hoa[i]
    maint_pct <- input$maint; mgmt_pct <- input$mgmt; capex_pct <- input$capex
    rent_i <- (clc$base_rent_monthly_y1) * (1 + input$rent_growth)^(i - 1)
    
    lst <- lapply(prices, function(p) {
      do.call(rbind, lapply(vacs, function(v) {
        egi_i <- 12 * rent_i * (1 - v)
        maint_i <- maint_pct * egi_i; mgmt_i <- mgmt_pct * egi_i; capex_i <- capex_pct * egi_i
        taxes_i <- p * input$tax_rate
        opex_i <- taxes_i + ins_i + hoa_i + maint_i + mgmt_i + capex_i
        noi_i <- egi_i - opex_i
        cap_i <- noi_i / p
        pay <- if (loan_amt > 0) pmt(rate/12, term * 12, loan_amt) else 0
        dscr_i <- if (loan_amt > 0) noi_i / (-12 * pay) else Inf
        data.frame(Price = p, Vacancy = v, CapRate = cap_i, DSCR = dscr_i, stringsAsFactors = FALSE)
      }))
    })
    df <- do.call(rbind, lst)
    txt <- paste("Value:", fmt_dollar(df$Price), "<br>Vacancy:", fmt_perc(df$Vacancy))
    plot_ly(df, x = ~CapRate, y = ~DSCR, type = "scatter", mode = "markers", text = txt, hoverinfo = "text") %>%
      layout(xaxis = list(title = paste0("Cap Rate (Year ", i, ")")),
             yaxis = list(title = paste0("DSCR (Year ", i, ")")),
             title = paste0("Cap Rate vs DSCR across Value & Vacancy — Year ", i))
  })
  
  # ---------- Compare (Year-1 snapshot) ----------
  scenarios <- reactiveVal(data.frame(
    Name=character(), Price=numeric(), Down=numeric(),
    CapRate=numeric(), CoC=numeric(), DSCR=numeric(),
    IRR=numeric(), NPV10=numeric(), stringsAsFactors=FALSE
  ))
  
  observeEvent(input$add_scn, {
    clc <- calc()
    noi1 <- clc$egi[1] - clc$opex[1]
    cap_rate_y1 <- if (clc$price > 0) noi1 / clc$price else NA
    coc_y1 <- (noi1 - clc$ann_debt_vec[1]) / clc$cash_to_close
    dscr_y1 <- if (clc$ann_debt_vec[1] > 0) noi1 / clc$ann_debt_vec[1] else Inf
    df <- scenarios()
    df[nrow(df)+1,] <- list(input$scenario_name, clc$price, input$down,
                            cap_rate_y1, coc_y1, dscr_y1, clc$irr, clc$npv10)
    scenarios(df)
  })
  
  output$dl_template <- downloadHandler(
    filename = function() "real_estate_template.csv",
    content = function(file) {
      tpl <- data.frame(
        Name=c("Property A","Property B"),
        Price=c(350000,425000),
        ClosingPct=c(0.03,0.03),
        HoldYears=c(10,10),
        Appreciation=c(0.03,0.03),
        SellCostPct=c(0.06,0.06),
        DownPct=c(0.20,0.25),
        RateAPR=c(0.065,0.0675),
        TermYears=c(30,30),
        RentMonthlyY1=c(2600,3000),
        OtherIncMonthly=c(0,100),
        RentGrowth=c(0.025,0.02),
        VacancyPct=c(0.05,0.06),
        TaxRate=c(0.013,0.012),
        InsuranceY1=c(2200,2400),
        HOA_Y1=c(0,1500),
        MaintPctEGI=c(0.08,0.07),
        MgmtPctEGI=c(0.08,0.08),
        CapexPctEGI=c(0.05,0.05),
        OpExGrowth=c(0.02,0.02),
        stringsAsFactors=FALSE
      )
      write.csv(tpl, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$csv, {
    req(input$csv)
    dat <- tryCatch(read.csv(input$csv$datapath, stringsAsFactors=FALSE), error=function(e) NULL)
    if (is.null(dat)) return()
    needed <- c("Name","Price","ClosingPct","HoldYears","Appreciation","SellCostPct","DownPct",
                "RateAPR","TermYears","RentMonthlyY1","OtherIncMonthly","RentGrowth","VacancyPct",
                "TaxRate","InsuranceY1","HOA_Y1","MaintPctEGI","MgmtPctEGI","CapexPctEGI","OpExGrowth")
    if (!all(needed %in% names(dat))) {
      showNotification("CSV missing required columns. Download the template to see the expected format.", type="error"); return()
    }
    rows <- lapply(seq_len(nrow(dat)), function(i) {
      r <- dat[i, ]
      price <- r$Price; down <- r$DownPct; loan_amt <- price * (1 - down)
      close <- price * r$ClosingPct; cash_to_close <- price * down + close
      egi <- 12 * (r$RentMonthlyY1 + r$OtherIncMonthly) * (1 - r$VacancyPct)
      opex <- price * r$TaxRate + r$InsuranceY1 + r$HOA_Y1 +
        r$MaintPctEGI * egi + r$MgmtPctEGI * egi + r$CapexPctEGI * egi
      noi <- egi - opex; cap <- noi / price
      pay <- if (loan_amt > 0) pmt(r$RateAPR/12, r$TermYears*12, loan_amt) else 0
      ann_debt <- -12 * pay; coc <- (noi - ann_debt) / cash_to_close
      yrs <- r$HoldYears
      rent_series <- (r$RentMonthlyY1 + r$OtherIncMonthly) * (1 + r$RentGrowth)^(0:(yrs-1))
      egi_vec <- 12 * rent_series * (1 - r$VacancyPct)
      values <- price * (1 + r$Appreciation)^(0:(yrs-1))
      taxes <- values * r$TaxRate
      ins <- r$InsuranceY1 * (1 + r$OpExGrowth)^(0:(yrs-1))
      hoa <- r$HOA_Y1 * (1 + r$OpExGrowth)^(0:(yrs-1))
      maint <- r$MaintPctEGI * egi_vec; mgmt <- r$MgmtPctEGI * egi_vec; capx <- r$CapexPctEGI * egi_vec
      opex_vec <- taxes + ins + hoa + maint + mgmt + capx
      noi_vec <- egi_vec - opex_vec
      cfs <- c(-cash_to_close, noi_vec - ann_debt)
      sale_price <- price * (1 + r$Appreciation)^yrs
      selling <- sale_price * r$SellCostPct
      bal <- if (loan_amt > 0) { m_rate <- r$RateAPR/12; k <- yrs*12; loan_amt * (1 + m_rate)^k + pmt(m_rate, r$TermYears*12, loan_amt) * ((1 + m_rate)^k - 1) / m_rate } else 0
      cfs[length(cfs)] <- cfs[length(cfs)] + (sale_price - selling - max(0, bal))
      irr_val <- irr(cfs); npv10 <- npv_at(0.10, cfs)
      data.frame(Name=r$Name, Price=price, Down=down, CapRate=cap, CoC=coc,
                 DSCR = if (loan_amt>0) noi/(-12*pay) else Inf,
                 IRR=irr_val, NPV10=npv10, stringsAsFactors=FALSE)
    })
    scenarios(rbind(scenarios(), do.call(rbind, rows)))
    showNotification("CSV loaded and added to comparison.", type="message")
  })
  
  output$compare_tbl <- renderDT({
    df <- scenarios()
    if (!nrow(df)) return(datatable(data.frame(Note="Add scenarios on the left, or upload a CSV."),
                                    options=list(dom='t'), rownames=FALSE))
    fmt <- df
    fmt$Price <- fmt_dollar(fmt$Price); fmt$Down <- fmt_perc(fmt$Down,2)
    fmt$CapRate <- fmt_perc(fmt$CapRate,2); fmt$CoC <- fmt_perc(fmt$CoC,2)
    fmt$DSCR <- ifelse(is.infinite(fmt$DSCR), "No debt", format(round(fmt$DSCR,2), nsmall=2))
    fmt$IRR <- ifelse(is.na(fmt$IRR), "N/A", fmt_perc(fmt$IRR,2)); fmt$NPV10 <- fmt_dollar(fmt$NPV10)
    datatable(fmt, rownames=FALSE, options=list(pageLength=10, autoWidth=TRUE))
  })
  
  #advisor 
  observeEvent(list(input$term, input$hold_yrs), {
    max_year <- max(1, min(input$term, input$hold_yrs))
    updateSliderInput(session, "view_year",
                      min = 1, max = max_year, value = min(input$view_year, max_year)
    )
    # NEW: bound Advisor year as well
    if (!is.null(input$advisor_year)) {
      updateSliderInput(session, "advisor_year",
                        min = 1, max = max_year, value = min(input$advisor_year, max_year)
      )
    }
  })
  
  # --- Helper to compute required Year-1 base rent (monthly) to hit a CF target in Year i
  advisor_required_rent_y1 <- function(i, target_cf, clc, input) {
    # i: target year index (1..H)
    # target_cf: desired annual cash flow in year i (e.g., 0 for breakeven)
    # clc: list from calc(); input: reactive inputs
    
    # Parts that don't scale with rent in year i:
    fixed_i <- clc$taxes[i] + clc$ins[i] + clc$hoa[i] + clc$other_opex[i]
    debt_i  <- clc$ann_debt_vec[i]  # annual debt service for year i
    
    # Parts that scale with EGI in year i (as % of EGI):
    var_pct <- input$maint + input$mgmt + input$capex  # e.g., 0.08 + 0.08 + 0.05
    
    denom <- (1 - var_pct)
    if (denom <= 0) return(NA_real_)   # can't solve; too much variable OpEx
    
    # Solve for EGI_i from: CF_i = (1 - var_pct)*EGI_i - fixed_i - debt_i = target_cf
    egi_needed_i <- (target_cf + fixed_i + debt_i) / denom
    if (!is.finite(egi_needed_i) || egi_needed_i <= 0) return(NA_real_)
    
    # EGI_i relates to rent_i via vacancy: EGI_i = 12 * rent_i * (1 - vacancy)
    if (input$vac >= 1) return(NA_real_)  # degenerate
    rent_i_needed <- egi_needed_i / (12 * (1 - input$vac))
    
    # Convert required year-i rent back to a Year-1 base (because rent grows each year)
    growth_factor <- (1 + input$rent_growth)^(i - 1)
    rent_y1_total_needed <- rent_i_needed / growth_factor  # total monthly income baseline (rent + other_inc in Y1)
    
    # Split into "suggested base rent" vs "other income" baseline
    rent_y1_suggested <- max(0, rent_y1_total_needed - input$other_inc)
    
    list(
      rent_y1_total_needed = rent_y1_total_needed,
      rent_y1_suggested    = rent_y1_suggested,
      egi_needed_i         = egi_needed_i
    )
  }
  
  # --- Advisor table for selected target year
  output$advisor_table <- renderTable({
    clc <- calc()
    max_year <- max(1, min(input$term, input$hold_yrs))
    i <- max(1, min(if (is.null(input$advisor_year)) 1 else input$advisor_year, max_year))
    
    current_cf_i <- clc$pre_tax_cf[i]
    ans <- advisor_required_rent_y1(i, input$advisor_target_cf, clc, input)
    if (is.null(ans) || any(is.na(unlist(ans)))) {
      return(data.frame(Note = "Cannot compute with current settings (check % and vacancy)."))
    }
    
    growth <- (1 + input$rent_growth)^(i - 1)
    req_y1_rent   <- ans$rent_y1_suggested
    req_yi_rent   <- (req_y1_rent + input$other_inc) * growth - input$other_inc
    
    # Optional clamp so we never suggest a base below current
    if (isTRUE(input$advisor_no_decrease)) {
      req_y1_rent <- max(req_y1_rent, input$rent)
      req_yi_rent <- (req_y1_rent + input$other_inc) * growth - input$other_inc
    }
    
    current_yi_rent <- (input$rent + input$other_inc) * growth - input$other_inc
    gap_cf <- input$advisor_target_cf - current_cf_i
    
    fmt_dol <- function(x) paste0("$", formatC(x, big.mark = ",", format = "f", digits = 0))
    data.frame(
      Item = c(
        paste0("Target Year"),
        paste0("Current annual CF in Year ", i),
        "Target annual CF in Year i",
        "Gap to target (annual)",
        paste0("Suggested base RENT in Year 1"),
        paste0("Suggested RENT in Year ", i),
        paste0("Current base RENT in Year 1"),
        paste0("Current RENT in Year ", i),
        paste0("Change needed in base RENT (Y1)")
      ),
      Value = c(
        i,
        fmt_dol(current_cf_i),
        fmt_dol(input$advisor_target_cf),
        fmt_dol(gap_cf),
        fmt_dol(req_y1_rent),
        fmt_dol(req_yi_rent),
        fmt_dol(input$rent),
        fmt_dol(current_yi_rent),
        fmt_dol(req_y1_rent - input$rent)
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # --- Curve: for each target year, what Year-1 base RENT would you need today?
  output$advisor_curve <- renderPlotly({
    clc <- calc()
    max_year <- max(1, min(input$term, input$hold_yrs))
    yrs <- 1:max_year
    
    req_y1 <- sapply(yrs, function(k) {
      ans <- advisor_required_rent_y1(k, input$advisor_target_cf, clc, input)
      if (is.null(ans) || any(is.na(unlist(ans)))) NA_real_ else ans$rent_y1_suggested
    })
    
    # Optional clamp
    if (isTRUE(input$advisor_no_decrease)) {
      req_y1 <- pmax(req_y1, input$rent)
    }
    
    growth <- (1 + input$rent_growth)^(yrs - 1)
    req_yi <- (req_y1 + input$other_inc) * growth - input$other_inc
    
    df <- data.frame(Year = yrs, RequiredY1Base = req_y1, RequiredRentYearI = req_yi)
    
    p <- plot_ly(df, x = ~Year) %>%
      add_trace(y = ~RequiredY1Base, type = "scatter", mode = "lines+markers+text",
                name = "Required base RENT (Y1)",
                text = ~paste0("$", format(round(RequiredY1Base, 0), big.mark=",")),
                textposition = "top center", hoverinfo = "x+y+name") %>%
      add_trace(y = ~RequiredRentYearI, type = "scatter", mode = "lines+markers+text",
                name = "Required RENT in Year i",
                text = ~paste0("$", format(round(RequiredRentYearI, 0), big.mark=",")),
                textposition = "top center", hoverinfo = "x+y+name") %>%
      layout(
        title = "Rent needed to hit target: today’s base vs rent in that year",
        xaxis = list(title = "Target Year", dtick = 1),
        yaxis = list(title = "Rent ($/month)")
      )
    p
  })
  
}

shinyApp(ui, server)
