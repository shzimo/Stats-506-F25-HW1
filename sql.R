library(DBI)
install.packages("RSQLite")
lahman <- dbConnect(RSQLite::SQLite(), "data/lahman_1871-2022.sqlite")
lahman

dbListTables(lahman)

dbListFields(lahman, "HomeGames")
dbListFields()


install.packages(c("DBI", "RSQLite"))
library(DBI)
library(RSQLite)
sakila_master <- dbConnect(RSQLite::SQLite(), "sakila_master.db")
dbListTables(sakila_master)



gg <- function(query) {
  dbGetQuery(sakila, query)
}

gg("
   SELECT invertory_id
    FROM rental
   LIMIT 20
   ")


gg("
   SELECT i.film_id, r.inventory_id
    FROM inventory AS i
     RIGHT JOIN rental AS r ON r.inventory_id = i.inventory_id
   LIMIT 40
   ")

## WRITE DOWN what piece of information you wanted

gg("
SELECT 
   FROM film_actor AS fa
        RIGHT JOIN (SELECT i.file_id
                      FROM inventory AS i
                           RIGHT JOIN ")

