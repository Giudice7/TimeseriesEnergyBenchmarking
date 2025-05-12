sidebar <- shinydashboard::dashboardSidebar(
  disable = FALSE,
  collapsed = FALSE,
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::menuItem("Load data", tabName = "load_data"),
    shinydashboard::menuItem("Pre-processing", tabName = "pre_processing"),
    shinydashboard::menuItem("Peer identification", tabName = "peer_identification"),
    shinydashboard::menuItem("KPI", tabName = "kpi"),
    shinydashboard::menuItem("Benchmarking", tabName = "benchmarking"),
    shinydashboard::menuItem("Result summary", tabName = "summary")
  )
)
