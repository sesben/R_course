pack <- c("tidyverse", "purrr", "broom")
lapply(pack, library, character.only = TRUE)

gap <- gapminder::gapminder %>% mutate(year1950 = year - 1950)

country_model <- function(df) {
  lm(lifeExp ~ year1950, data = df)
}

gapnest <- gap %>%
  group_by(continent, country) %>%
  nest() %>%
  mutate(mod = data %>% map(country_model))

gapnest <- gapnest %>%
  mutate(
    tidy = mod %>% map(tidy),
    glance = mod %>% map(glance),
    augment = mod %>% map(augment),
    rsq = glance %>% map_dbl("r.squared")
  )

gapnest %>% ggplot(aes(rsq, reorder(country, rsq))) +
  geom_point(aes(color = continent))

unnest(gapnest, tidy, .drop = TRUE) %>% view()

# EXERCISES
# 1
x <- 1:40
k <- 3
split_ind <- function(x, k) {
  sample(rep_len(1:k, length(x)))
  split(x, f)
}

split_ind(1:40, 3)

# 2
set.seed(1)
x <- rnorm(10)
y <- replicate(1e4, mean(sample(x, replace = TRUE)))
hist(y)
abline(v = mean(x), col = "red")
quantile(y, probs = c(0.025, 0.975))

#3
my_mtcars <- mtcars[c("mpg", "hp")]
my_mtcars$my_col <- sample(c("mpg", "hp"), size = nrow(my_mtcars), replace = TRUE)
head(my_mtcars)
ind_col <- match(my_mtcars$my_col, names(my_mtcars))
ind_row <- seq_len(nrow(my_mtcars))
my_mtcars$my_val <- my_mtcars[1:2][cbind(ind_row, ind_col)]

#4
df <- data.frame(
  id1 = c("a", "f", "a"),
  id2 = c("b", "e", "e"), 
  id3 = c("c", "d", "f"),
  inter = c(7.343, 2.454, 3.234),
  stringsAsFactors = FALSE
)
df
(code <- setNames(1:6, letters[1:6]))
code[df$id1]
df$id1 <- code[df$id1]
df[1:3] <- lapply(df[1:3], function(x) code[x])
df
