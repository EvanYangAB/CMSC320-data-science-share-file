
sqldf("attach 'Test1.sqlite' as new")

db <- DBI::dbConnect(RSQLite::SQLite(), "hwk2Data/baseball-archive-sqlite/lahman2016.sqlite")

DBI::dbGetQuery(db, 'SELECT m.playerID, sum(b.H) as total_hits, nameFirst, nameLast
                FROM Master as m
                  left join(SELECT distinct playerID
                        FROM HallofFame
                        WHERE inducted = "y") as hof
                      on m.playerID = hof.playerID
                  join Batting as b
                      on m.playerID = b.playerID
                WHERE birthCountry = "P.R."
                    and hof.playerID is NULL
                
                group by m.playerID
                order by total_hits desc
                limit 10')

DBI::dbListTables(db)
sql, connection=db
