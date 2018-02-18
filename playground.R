data_tab <- read_csv("bpd-arrests-1.csv")
summary(data_tab$Sex)
summary(factor(data_tab$Sex))

data_tab$Sex <- factor(data_tab$Sex)

arrange(data_tab, desc(age))

# 15th oldest subject
data_tab %>%
  arrange(desc(Age)) %>%
  slice(1)

data_tab %>%
  mutate(age_month = 12*Age) %>%
  select(data_tab, Age, age_months)

data_tab %>%
  filter(Age != 0) %>%
  summarize(min_age = min(Age), mean_age = mean(Age), max_age=max(Age))


# group by 
data_tab %>%
  filter(Age != 0) %>%
  group_by(District) %>%
  summarize(mean_age = mean(Age))

data_tab %>%
  filter(Age != 0) %>%
  group_by(District, Sex) %>%
  summarize(mean_age = mean(Age), median_age = median(Age))


analysis_df <- data_tab %>%
  filter(Age != 0) %>%
  group_by(District) %>%
  summarize(mean_age = mean(Age))

# graphing
analysis_df %>%
  ggplot(mapping = aes(x = mean_age, y = District, color = District)) + geom_point() 


+ geom_text(aes(lable = District))


geom_text(aes(lable = District))


# joins
library(nycflights13)
data(flights)
data(airlines)

View(flights %>%
  left_join(airlines))
View(flights %>%
       left_join(airlines), by="carrier")

View(flights %>%
  right_join(airlines, by="carrier"))



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
  html_node("#filmographyTbl") %>%  # same css selector type = "table" class:  ".class" "[attribute]" "id"
  html_table() 

head(dl_tab)