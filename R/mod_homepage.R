#' Homepage UI Module
#'
#' @param id,input,output,session Internal parameters.
#' @noRd
#' @importFrom shiny NS tagList tags HTML div fluidRow column actionButton
#' @importFrom shinyjs useShinyjs
mod_homepage_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    # --- Custom CSS for this page ---
    tags$style(HTML("
      /* Hero Section */
      .hero-section {
        background: linear-gradient(135deg, #008080 0%, #20c997 100%);
        color: white;
        padding: 80px 0 100px;
        position: relative;
        overflow: hidden;
        margin-top: -1rem;
      }
      .hero-content h1 {
        font-weight: 800;
        font-size: 3.5rem;
        margin-bottom: 1.5rem;
        letter-spacing: -1px;
      }
      .hero-content p {
        font-size: 1.25rem;
        opacity: 0.9;
        max-width: 600px;
        margin-bottom: 2rem;
      }
      .btn-hero {
        background-color: white;
        color: #008080;
        font-weight: 700;
        padding: 12px 30px;
        border-radius: 50px;
        border: none;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
      }
      .btn-hero:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.3);
        background-color: #f8f9fa;
        color: #006666;
      }

      /* Features Section */
      .features-container {
        margin-top: -60px;
        position: relative;
        z-index: 2;
        padding-bottom: 60px;
      }
      .feature-card {
        background: white;
        border-radius: 15px;
        padding: 2rem;
        height: 100%;
        box-shadow: 0 10px 30px rgba(0,0,0,0.08);
        transition: transform 0.3s ease;
        border-top: 5px solid transparent;
      }
      .feature-card:hover {
        transform: translateY(-10px);
        border-top-color: #008080;
      }
      .feature-icon {
        font-size: 2.5rem;
        color: #008080;
        margin-bottom: 1rem;
      }
      .feature-title {
        font-weight: 700;
        font-size: 1.2rem;
        margin-bottom: 0.5rem;
        color: #333;
      }
      .feature-desc {
        color: #666;
        font-size: 0.95rem;
        line-height: 1.6;
      }

      /* Workflow Section */
      .workflow-section {
        padding: 60px 0;
        background-color: #f8f9fa;
      }
      .section-title {
        text-align: center;
        margin-bottom: 3rem;
        color: #333;
        font-weight: 700;
      }
      .workflow-img {
        border-radius: 12px;
        box-shadow: 0 15px 40px rgba(0,0,0,0.1);
        transition: all 0.3s;
      }
      .workflow-img:hover {
        transform: scale(1.02);
      }

      /* Citation Section */
      .citation-section {
        padding: 60px 0;
        background-color: white;
      }
      .citation-card {
        background-color: #f8f9fa;
        border-left: 5px solid #008080;
        padding: 1.5rem;
        border-radius: 8px;
        margin-bottom: 1rem;
      }
      .citation-text {
        font-size: 0.95rem;
        color: #495057;
        margin-bottom: 0;
        line-height: 1.6;
      }

      /* Footer */
      .footer-section {
        background-color: #343a40;
        color: #adb5bd;
        padding: 40px 0;
        font-size: 0.9rem;
      }
      .footer-link {
        color: #fff;
        text-decoration: none;
      }
      .footer-link:hover {
        color: #20c997;
        text-decoration: underline;
      }
    ")),

    # 1. Hero Section
    div(class = "hero-section",
        div(class = "container",
            div(class = "row align-items-center",
                div(class = "col-lg-6 hero-content",
                    h1("Unlocking Plant Metabolomics"),
                    p("MetMiner is a user-friendly, server-ready pipeline powered by TidyMass. From raw data to biological insights, we make mining easier."),
                    actionButton(ns("go_init"), "Start Analysis", class = "btn-hero me-3"),
                    a(href = "https://shawnwx2019.github.io/metminer-cookbook/",
                      target = "_blank",
                      class = "btn btn-outline-light rounded-pill px-4 py-2 fw-bold",
                      "View Documentation")
                ),
                div(class = "col-lg-6 d-none d-lg-block text-center",
                    img(src = "www/MetMiner.jpg",
                        style = "max-width: 80%; border-radius: 20px; box-shadow: 0 20px 50px rgba(0,0,0,0.3); transform: rotate(-3deg);")
                )
            )
        )
    ),

    # 2. Key Features Cards
    div(class = "container features-container",
        div(class = "row g-4",
            div(class = "col-md-3",
                div(class = "feature-card",
                    div(class = "feature-icon", icon("layer-group")),
                    div(class = "feature-title", "TidyMass Core"),
                    div(class = "feature-desc", "Built on the robust `mass_dataset` structure ensuring traceability and reproducibility across all steps.")
                )
            ),
            div(class = "col-md-3",
                div(class = "feature-card",
                    div(class = "feature-icon", icon("leaf")),
                    div(class = "feature-title", "Plant Specific"),
                    div(class = "feature-desc", "Integrated PM Database and specialized annotation tools tailored for plant metabolome research.")
                )
            ),
            div(class = "col-md-3",
                div(class = "feature-card",
                    div(class = "feature-icon", icon("chart-pie")),
                    div(class = "feature-title", "One-Stop Stats"),
                    div(class = "feature-desc", "From missing value imputation to differential analysis and pathway enrichment in one go.")
                )
            ),
            div(class = "col-md-3",
                div(class = "feature-card",
                    div(class = "feature-icon", icon("network-wired")),
                    div(class = "feature-title", "WGCNA Mining"),
                    div(class = "feature-desc", "Advanced iterative WGCNA strategy to discover co-expression modules and hub metabolites.")
                )
            )
        )
    ),

    # 3. Workflow Section
    div(class = "workflow-section",
        div(class = "container",
            h2("Streamlined Workflow", class = "section-title"),
            div(class = "row align-items-center",
                div(class = "col-lg-8 offset-lg-2 text-center",
                    img(src = "www/Fig1.StructureAndStrategy.webp",
                        class = "img-fluid workflow-img",
                        alt = "MetMiner Workflow")
                )
            ),
            div(class = "text-center mt-5",
                p("MetMiner integrates upstream processing, downstream analysis, and advanced mining into a cohesive pipeline.",
                  class = "text-muted fs-5")
            )
        )
    ),

    # 4. Citation Section
    div(class = "citation-section",
        div(class = "container",
            h2("How to Cite", class = "section-title"),
            div(class = "row justify-content-center",
                div(class = "col-lg-10",
                    # Citation 1: MetMiner
                    div(class = "citation-card",
                        p(class = "citation-text",
                          HTML("Wang, X., Liang, S., Yang, W., Yu, K., Liang, F., Zhao, B., Zhu, X., Zhou, C., Mur, L.A.J., Roberts, J.A., Zhang, J., Zhang, X., <strong>2024</strong>. MetMiner: A user-friendly pipeline for large-scale plant metabolomics data analysis. <em>J. Integr. Plant Biol.</em> 66, 2329-2345. <a href='https://doi.org/10.1111/jipb.13774' target='_blank'>https://doi.org/10.1111/jipb.13774</a>")
                        )
                    ),
                    # Citation 2: TidyMass2
                    div(class = "citation-card",
                        p(class = "citation-text",
                          HTML("Wang, X., Liu, Y., Jiang, C., Huang, Z., Yan, H., Wong, S., Johnson, C.H., Zhang, J., Ge, Y., Zhang, F., Lai, R., Gao, P., Zhang, X., Shen, X., <strong>2025</strong>. TidyMass2: Advancing LC-MS Untargeted Metabolomics Through Metabolite Origin Inference and Metabolic Feature-based Functional Module Analysis. <em>Nat. Commun.</em> <a href='https://doi.org/10.1101/2025.05.09.652992' target='_blank'>https://doi.org/10.1101/2025.05.09.652992</a>")
                        )
                    ),
                    # Citation 3: TidyMass
                    div(class = "citation-card",
                        p(class = "citation-text",
                          HTML("Shen, X., Yan, H., Wang, C., Gao, P., Johnson, C.H., Snyder, M.P., <strong>2022</strong>. TidyMass an object-oriented reproducible analysis framework for LC-MS data. <em>Nat. Commun.</em> 13, 4365. <a href='https://doi.org/10.1038/s41467-022-32155-w' target='_blank'>https://doi.org/10.1038/s41467-022-32155-w</a>")
                        )
                    )
                )
            )
        )
    ),

    # 5. Footer
    div(class = "footer-section",
        div(class = "container",
            div(class = "row",
                div(class = "col-md-6",
                    h5("MetMiner", class = "text-white mb-3"),
                    p("State Key Laboratory of Crop Stress Adaptation and Improvement"),
                    p("Henan University, Kaifeng 475004, China")
                ),
                div(class = "col-md-6 text-md-end",
                    h5("Resources", class = "text-white mb-3"),
                    p(a(href="https://shawnwx2019.github.io/metminer-cookbook/", class="footer-link", "Cookbook")),
                    p(a(href="https://github.com/ShawnWx2019/MetMiner", class="footer-link", "Source Code")),
                    p("Version 2.0.0")
                )
            ),
            tags$hr(style = "border-color: rgba(255,255,255,0.1); margin-top: 2rem;"),
            div(class = "text-center",
                "(c) 2024-2025 Prof. Xuebin Zhang's Lab. All rights reserved."
            )
        )
    )
  )
}

#' Homepage Server Module
#'
#' @noRd
mod_homepage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$go_init, {
      showModal(modalDialog(
        title = "Let's Get Started!",
        "Please navigate to the 'Project Init' tab in the navigation bar to set up your workspace.",
        easyClose = TRUE,
        footer = modalButton("Got it")
      ))
    })

  })
}
