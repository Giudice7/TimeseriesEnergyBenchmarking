sidebar <- shinydashboard::dashboardSidebar(
  disable = FALSE,
  collapsed = TRUE,
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::menuItem("About", tabName = "about"),
    shinydashboard::menuItem("Load data", tabName = "load_data"),
    shinydashboard::menuItem("Pre-processing", tabName = "pre-processing"),
    shinydashboard::menuItem("Peer identification", tabName = "peer_identification"),
    shinydashboard::menuItem("KPI", tabName = "kpi"),
    shinydashboard::menuItem("Benchmarking", tabName = "benchmarking")
  )
)