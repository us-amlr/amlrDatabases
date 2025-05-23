#' General functions used by other US AMLR packages
#'
#' General functions used by other US AMLR packages
#'
#' @name amlrDatabases-package
#' @aliases amlrDatabases
#' @title General Functions for US AMLR Database Packages
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#'
#' @import shiny
#'
#' @importFrom dplyr %>% case_when if_else select tbl collect filter pull
#' @importFrom DT DTOutput renderDT
#' @importFrom ggplot2 ggplot aes geom_point ggtitle ggsave
#' @importFrom lubridate days_in_month ymd
#' @importFrom odbc odbc odbcListDrivers
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom pool dbGetInfo dbGetQuery dbIsValid dbPool poolClose poolReturn
#'   poolCheckout localCheckout
#' @importFrom purrr compact set_names
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   sidebarMenu menuItem dashboardBody tabItems tabItem box
#' @importFrom stringr str_pad str_sub
#' @importFrom utils globalVariables write.csv
#'
#' @keywords internal
"_PACKAGE"

# https://github.com/r-lib/tidyselect/issues/248
# https://r-pkgs.org/package-within.html#echo-a-working-package
utils::globalVariables(c("attribute", "name", "value"))
