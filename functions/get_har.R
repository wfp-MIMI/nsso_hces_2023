
get_har <- function(){
 

  con <- DBI::dbConnect(RMySQL::MySQL(),
                  dbname = Sys.getenv("DB_NAME"),
                  host = "127.0.0.1",
                  port = 3306,
                  user = Sys.getenv("DB_USER"),
                  password =  Sys.getenv("DB_PASSWORD"))


  # collect information from database

  h_ar <- DBI::dbReadTable(con, "h_ar")
  return(h_ar)
}