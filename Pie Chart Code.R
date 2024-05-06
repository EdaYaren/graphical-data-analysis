
# Kütüphanelerin indirilmesi:
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("forcats")
#install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(forcats)
library(egg)
library(gridExtra)

# Veri setinin yüklenmesi:
data <- read.csv("C:/Users/Yaren/Desktop/DataScience_salaries_2024.csv")
#View(data)

# Veri önislemenin yapilmasi:
head(data)
summary(data)
colSums(is.na(data)) # Kayip veri kontrolü
colnames(data)

## Kullanilacak nicel degiskenin araliklara ayrilarak kategoriklestirilmesi: 
df <- data.frame(year = data$work_year,
                 salary = as.character(cut(data$salary_in_usd, 
                                           breaks = 3, 
                                           labels = c("Low", "Mid", "High"))))

## Maas degiskeninin kategorilerine göre yillari gruplayarak toplam maaslari 
## hesaplama:
df_summary <- data %>%
  mutate(salary_category = cut(salary_in_usd, 
                               breaks = 3, 
                               labels = c("Low", "Mid", "High"))) %>%
  group_by(work_year, salary_category) %>%
  summarise(count = n()) %>%
  ungroup()

## Her bir yil için yüzdelik degerleri hesaplama:
df_summary <- df_summary %>%
  group_by(work_year) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


## Yeni veri seti olusturma:
df <- data.frame(year = df_summary$work_year,
                 salary_category = df_summary$salary_category,
                 percentage = df_summary$percentage)


## Yüzdelik degerleri "Y" olarak atama:
df$c <- df$percentage
df$c <- sprintf("%.2f", df$c)

df <- df[-(1:4), ]

df <- df %>%
  select(-percentage)

new_row <- data.frame(year = 2022, salary_category = "High", c = 0.00)
df <- rbind(df, new_row)

df$salary_category <- as.factor(df$salary_category)

df <- df %>%
  arrange(desc(year))

# Grafik çizimi:
df$Y <- as.numeric(as.character(df$c))
foo <- data.frame(cumsum(table(df$year)) + 1:length(unique(df$year)))
foo$year <- rownames(foo)
colnames(foo)[1] <- "row"

## Maas kategorileri için satirlarin atanmasi:
df$row <- which(do.call(rbind, by(df, df$year, rbind, ""))$year != "")
## Renklerin belirlenmesi:
colors <- c("#ee799f", "#9370db", "#32cd32" )


### Ilk grafigin çizimi:
p1 <- ggplot(df, aes(factor(year), Y, fill = salary_category)) + 
  geom_bar(stat = "identity", width = 1, size = 1, color = "white") +
  coord_polar("y") + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = colors)

p1

### Ikinci grafigin çizimi:
p2 <- ggplot(df, aes(y = row)) + 
  geom_point(aes(0, color = salary_category), size = 4) +
  geom_text(data = foo, aes(0, label = rev(year)), size = 5, color = "grey50") +
  geom_text(aes(0.5, label = paste0(salary_category, ": ", c, "%"))) +
  theme_void() +
  theme(legend.position = "none") +
  scale_x_discrete() +
  scale_color_manual(values = colors)

p2

# Grafiklerin birlestirilmesi:
ggarrange(p1, p2, nrow = 1, widths = c(3, 1))


