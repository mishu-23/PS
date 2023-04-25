#

setwd('C:/storage/facultate/ps/lab2')
tablou = read.csv('life_expect.csv', header = T, sep = ',')
femei = tablou[['female']]
barbati = tablou[['male']]
hist(femei, breaks = 7, freq = T,
     main = "Speranta de viata la nastere pentru femei",)
hist(barbati, breaks = 7, freq = T,
     main = "Speranta de viata la nastere pentru barbati",)

#

sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)

m = mean(sample)
s = sd(sample)
outliers = vector()
j = 0
for(i in 1:length(sample))
 if(sample[i] < m - 2*s | sample[i] > m + 2*s) {
    j = j + 1
    outliers[j] = sample[i]
    }
outliers

#

sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)

outliers_mean = function(sample) {
  m = mean(sample)
  s = sd(sample)
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] < m - 2*s | sample[i] > m + 2*s) {
      j = j + 1
      outliers[j] = sample[i]
    }
  return (outliers)
}

#

sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)

outliers_iqr = function(sample) {
  q1 = as.vector(quantile(sample))[2]
  q3 = as.vector(quantile(sample))[4]
  iqr = q3 - q1
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] < q1 - 1.5*iqr | sample[i] > q3 + 1.5*iqr) {
      j = j + 1
      outliers[j] = sample[i]
    }
  return (outliers)
}
sample2 = read.table("sample2.txt")
summary(sample2)
outliers_iqr(sample2)
outliers_mean(sample2)