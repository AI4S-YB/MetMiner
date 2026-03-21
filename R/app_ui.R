#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom golem add_resource_path bundle_resources favicon
#' @importFrom bsicons bs_icon
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # 启用 bslib 的主题
    bslib::page_navbar(
      # --- 1. 修正标题区域 ---
      # 加上 vertical-align: middle 确保与菜单项基线对齐
      title = tags$span(
        "MetMiner",
        # 添加下标版本号
        tags$sub("version 2.0.0",
                 style = "font-size: 0.75rem; color: #6c757d; font-weight: normal; margin-left: 5px; letter-spacing: 0;"),
        class = "fw-bold",
        style = "font-size: 1.6rem; color: #008080; letter-spacing: -0.5px; vertical-align: middle;"
      ),

      # --- 2. 优化主题 ---
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "zephyr",
        primary = "#008080",
        "navbar-bg" = "#ffffff", # 纯白背景
        base_font = font_google("Inter"),
        heading_font = font_google("Plus Jakarta Sans"),
        # 增加一点 navbar 的高度，让内容呼吸感更好，同时垂直居中更容易
        "navbar-padding-y" = "0.8rem"
      ),

      # 这是一个小技巧：强制修正 navbar items 的对齐
      header = tags$style(HTML("
        .navbar-nav .nav-link {
            font-weight: 500;
            font-size: 1.05rem;
            display: flex;
            align-items: center; /* 确保文字垂直居中 */
            height: 100%;
        }
        /* 选中状态下加一个底部边框，像你截图里的 Home 下划线 */
        .navbar-nav .nav-link.active {
            color: #008080 !important;
            border-bottom: 3px solid #008080;
            margin-bottom: -3px; /* 防止布局跳动 */
        }
      ")),

      # --- 3. 导航菜单 ---
      bslib::nav_panel("Home", mod_homepage_ui("home_1")),

      bslib::nav_panel(
        title = "Project Init",
        # 直接调用模块，让模块内部的 page_fluid 接管整个宽度
        mod_project_init_ui("project_init_1")
      ),

      bslib::nav_panel(
        title = "Data import",
        # 直接调用模块，让模块内部的 page_fluid 接管整个宽度
        mod_data_import_ui("data_import_1")
      ),

      bslib::nav_menu(
        title = "Cleaning",
        bslib::nav_panel("Overview", mod_data_overview_ui('data_overview_1')),
        bslib::nav_panel("Remove Noise", mod_data_rm_noise_ui('data_rm_noise_1')),
        bslib::nav_panel("Outlier Detection", mod_data_outlier_ui('data_outlier_1')),
        bslib::nav_panel("Imputation", mod_data_impute_ui('data_imputation_1')),
        bslib::nav_panel("Normalization", mod_data_norm_ui('data_norm_1'))
      ),

      bslib::nav_menu(
        title = "Analysis",
        bslib::nav_panel("Annotation", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Filter Annotations", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Merge Data", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Classification", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Differential Analysis", div(class="p-4", "Placeholder")),
        bslib::nav_panel("Enrichment", div(class="p-4", "Placeholder"))
      ),

      # --- 4. 右侧 GitHub 按钮优化 ---
      bslib::nav_spacer(), # 推到最右侧

      bslib::nav_item(
        tags$a(
          # 使用 btn-outline 样式，更加精致
          class = "btn btn-outline-secondary d-flex align-items-center gap-2",
          style = "padding: 0.375rem 0.75rem; border-color: #dee2e6;",
          href = "https://github.com/ShawnWx2019/MetMiner",
          target = "_blank",
          # 确保图标和文字对齐
          bsicons::bs_icon("github", size = "1.1rem"),
          span("GitHub", style = "font-size: 0.95rem;")
        )
      )
    ),
    # --- Global Widgets ---
    # Add the console monitor UI here, it will float on top of everything
    mod_console_monitor_ui("console_1")
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path bundle_resources favicon
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MetMiner"
    )
  )
}
