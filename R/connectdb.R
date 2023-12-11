
#' Title connectAQdb
#' @return
#' @export
#' @examples
#'

connectAQdb <- function(){
connectdb <- dbConnect(
  RPostgres::Postgres(),
  user = "postgres",
  password = "Nova!1994!",
  host = "197.242.151.158",
  port = 5432,
  dbname = "nova_aq_db"
)
return(connectdb)
}
