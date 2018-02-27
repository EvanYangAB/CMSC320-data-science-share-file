# missing chapter 10, 11

data_tab <- read_csv("bpd-arrests-1.csv")
summary(data_tab$Sex)
summary(factor(data_tab$Sex))

# factoring
data_tab$Sex <- factor(data_tab$Sex)
levels(data_tab$Sex)

arrange(data_tab, desc(age))

# 15th oldest subject
data_tab %>%
  arrange(desc(Age)) %>%
  slice(1) # slice(c(1,23,199)) or slice(1:5) or slice(seq(2, nrow(arrest_tab), by=2))
                                             #from 2 to nrow(arrest_tab), increase by 2
data_tab %>%
  mutate(age_month = 12*Age) %>%  #mutate create new attribute
  select(data_tab, Age, age_months)

data_tab %>%
  filter(Age != 0) %>%
  summarize(min_age = min(Age), mean_age = mean(Age), max_age=max(Age))

data_tab %>%
  sample_n(10) # or sample_frac(.1)

data_tab %>%
  arrange(desc(age)) #sort by

# group by 
data_tab %>%
  filter(Age != 0) %>%
  group_by(District) %>%
  summarize(mean_age = mean(Age)) # functions: mean, median, min, max, n or n_distinct, any or all (only for True/False), sd for standard deviation


data_tab %>%
  filter(Age != 0) %>%
  group_by(District, Sex) %>%
  summarize(mean_age = mean(Age), median_age = median(Age))


analysis_df <- data_tab %>%
  filter(Age != 0) %>%
  group_by(District) %>%
  summarize(mean_age = mean(Age))

data_tab %>%
  pull(age) %>% # pull creates a vector of [v1, v2, v3 ... vn]
  mean()

# function
summarize_district <- function(df) {
  df %>%
    filter(age >= 21) %>%
    group_by(district, sex) %>%
    summarize(mean_age=mean(age))
}

# graphing
<data_frame> %>%
  ggplot(mapping=aes(<graphical_characteristic>=<attribute>)) +
  geom_<representation>()

 geom_text(aes(lable = District))

# scatter points
# Used to visualize the relationship between two attributes
mpg %>%
   ggplot(mapping=aes(x=displ, y=hwy)) +
   geom_point(mapping=aes(color=cyl))

# bar graph
# visualize the relationship between a continuous variable to a categorical (or discrete) attribute
mpg %>%
  group_by(cyl) %>%
  summarize(mean_mpg=mean(hwy)) %>%
  ggplot(mapping=aes(x=cyl, y=mean_mpg)) +
  geom_bar(stat="identity")

# histogram
# Used to visualize the distribution of the values of a numeric attribute
mpg %>%
  ggplot(mapping=aes(x=hwy)) +
  geom_histogram()

# boxplot
# Used to visualize the distribution of a numeric attribute based on a categorical attribute
mpg %>%
  ggplot(mapping=aes(x=class, y=hwy)) +
  geom_boxplot()

# joins
library(nycflights13)
data(flights)
data(airlines)

# all observations on left operand (LHS) are retained
flights %>%
  left_join(airlines)
flights %>%
  left_join(airlines, by="carrier")
flights %>%
  left_join(airlines, by=c("carrier" = "name"))
# all observations on right operand (RHS) are retained
flights %>%
  right_join(airlines, by="carrier")
# only observations matching on both tables are retained
flights %>%
  inner_join(airlines, by="carrier")
# all observations are retained, regardless of matching condition
flights %>%
full_join(airlines, by="carrier")
# filters out flights from airlines that are not included in the airlines table
flights %>% anti_join(airlines, by="carrier")
# samething in sql but outer
# reading problematic datas
df <- read_csv(readr_example("challenge.csv"))
problems(df)

df <- read_csv(readr_example("challenge.csv"), col_types = cols(
  x = col_double(), y = col_date()))

df <- read_csv(readr_example("challenge.csv"), col_types = cols(.default=col_character()))
df <- read_lines(readr_example("challenge.csv"))






# scraping data
library(rvest)
url <- "https://www.rottentomatoes.com/celebrity/diego_luna"
dl_tab <- url %>%
  read_html() %>%
  html_node("#filmographyTbl") %>%  
  # same css selector type = "table" ".class" "[attribute="value"]" "#id"
  # "p:first-of-type"
  # "td:nth-of-type(2)"
  html_table() 
# messy data
scrape_billboard <- function(year, baseurl="https://en.wikipedia.org/wiki/List_of_Billboard_Hot_100_number-one_singles_of_") {
  url <- paste0(baseurl, year)
  # find table node
  singles_tab_node <- read_html(url) %>%
    html_node(".plainrowheaders") 
  # extract dates
  dates <- singles_tab_node %>% html_nodes("[scope]") %>% html_text()
  # extract titles and spans
  title_nodes <- singles_tab_node %>% html_nodes("tr") %>% html_node("td:first-of-type") %>% magrittr::extract(-1)
  song_titles <- title_nodes %>% html_text()
  title_spans <- title_nodes %>% html_attr("rowspan")
  # make data frame
  data_frame(month_day=dates, year=year, song_title_raw=song_titles, title_span=title_spans,
             artist_raw=artists)
}
scrape_billboard("2016")
# maping function to table
billboard_tab <- as.character(2010:2017) %>%
  purrr::map_df(scrape_billboard)

billboard_df <- data_frame(month_day=dates, year="2017", song_title_raw=song_titles, title_span=title_spans,
                           artist_raw=artists)


# gather(table, type, value, -whatever stays the same)
# gather into key-value column
tidy_pew <- gather(pew, income, frequency, -religion)

# multiple catigory in columns
tidy_tb <- tb %>% 
  gather(demo, n, -iso2, -year)  %>%
  separate(demo, c("sex", "age"), sep=1)

# variables stored in both rows and columns
# gather by day and value in cols d1:d31, rm na
# them spread by element and value.
weather %>%
  gather(day, value, d1:d31, na.rm=TRUE) %>%
  spread(element, value)

# multiple relation/data in one table
# seperate into 2
song <- tidy_billboard %>%
  select(artist, track, year, time, date.entered) %>% 
  unique() %>%
  mutate(song_id = row_number())
rank <- tidy_billboard %>%
  left_join(song, c("artist", "year", "track", "time", "date.entered")) %>%
  select(song_id, week, rank)

# stringr
str_length()
str_c(a, b, sep=". ")
str_sub(a, 2, 5) # starts 1
str_trim("    I am padded    ", side="both") # "both", "left", "right"

# regex
\\. for dot
start (^), end ($)
\d digit
\s spaces
[^abc] anything except this set
| match any of one or more patterns
?: zero or one
+: one or more
*: zero or more
mathching groups:
str_view(fruit, "(..)\\1")
str_detect(vectorString, regex) => T/F
str_count(vectorString, regex) => num
str_subset(vectorString, regexe) => subset
str_extract(vectorString, regexe) => matched content
str_match=>First column: complete match, one column for each capture group.
str_split
# sql commands
# create table w/ integrity constraints
CREATE TABLE customer (
  ssn CHAR(9) PRIMARY KEY, 
  cname CHAR(15), address CHAR(30), city CHAR(10), 
  UNIQUE (cname, address, city));
CREATE TABLE <name> ( <field> <domain>, ... )
INSERT INTO <name> (<field names>) VALUES (<field values>)
DELETE FROM <name> WHERE <condition>
UPDATE <name> SET <field name> = <value> WHERE <condition>
SELECT <fields> FROM <name> WHERE <condition>


