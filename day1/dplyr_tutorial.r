library("dplyr")

df = data.frame(color = c("red", "orange", "yellow", "green", "blue", "violet"),
                value = ((1:6) * rnorm(6, 1, 0.5)),
                row.names = c("USA", "EU", "NATO", "USSR", "OZ", "WTF"))
df

filter(df, color=="orange" | value > 4)
# note: filter takes off the row names

filter(df, substr(color, 2, 2) %in% c("e", "i"))
select(df, value)
select(df, -value)
select(df, -value, -color)

arrange(df, color)
arrange(df, desc(value))
arrange(df, row.names.data.frame(df))

mutate(df, magic = paste(color, value))
df

by_color <- group_by(mutate(df,rounded=round(value)), rounded)
by_color
summarize(by_color, total=sum(value))

n_distinct(df$value)
mad(df$value)
sd(df$value)

df %>%
  filter(value > 3) %>%
      arrange(desc(color))

#vignette("window-functions")
df %>%
  filter(value > 3) %>%
  mutate(rounded = round(value)) %>%
  group_by(rounded) %>%
  mutate(rownum = row_number(value))

# inner_join
# left_join
# semi_join  # rows of x that match y
# anti_join # rows of x that don't match y
