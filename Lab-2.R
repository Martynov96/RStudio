# загрузка пакетов
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # функции skewness() и kurtosis()

# загружаем файл с данными по импорту масла в РФ (из прошлой практики)
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
# создаём директорию для данных, если она ещё не существует:
if (!file.exists('./data')) {
  dir.create('./data')
}
# создаём файл с логом загрузок, если он ещё не существует:
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}
# загружаем файл, если он ещё не существует,
#  и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
  download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
  # сделать запись в лог
  write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
        file = './data/download.log', append = T)
}
# читаем данные из загруженного .csv во фрейм, если он ещё не существует
if (!exists('DT')){
  DT <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', as.is = T))
}
DT[, Netweight.kg.model := Netweight.kg]

DT[, Netweight.kg.median := 
     round(median(.SD$Netweight.kg, na.rm = T), 0), 
   by = Year]
# затем заменяем пропуски на медианы
DT[!is.na(Netweight.kg), 
   Netweight.kg.median := Netweight.kg]


# предварительный просмотр
dim(DT)     # размерность таблицы
str(DT)     # структура (характеристики столбцов)
DT          # удобный просмотр объекта data.table

png('Pic-01231.png', width = 500, height = 500)

# ГРАФИК ПЛОТНОСТИ РАСПРЕДЕЛЕНИЯ
d1 <- density(DT$Netweight.kg, na.rm = T)
d2 <- density(DT$Netweight.kg.model, na.rm = T)
d3 <- density(DT$Netweight.kg.median, na.rm = T)
# граница
plot(d1, 
     main = 'Плотности распределения показателей',
     ylab = 'Плотность')

# заливка
polygon(d1, col = rgb(1, 0, 0, 
                     alpha = 0.3),
        border = 'darkred')
polygon(d2, col = rgb(0, 1, 0, 
                     alpha = 0.3),
        border = 'darkgreen')
polygon(d3, col = rgb(0, 0, 1, 
                     alpha = 0.3),
        border = 'lightblue')
# удаляем временные объекты
rm(cls, d, mnth.f)
dev.off()

