# --- Final Project : Heart Disease --- #

# install.packages(c('gapminder', 'gridExtra', 'data.table', 'gginference', 'yarrr', 'dplyr', 'ggplot2', 'plotly', 'corrplot', 'psych', 'kableExtra'))
library(psych)
library(dplyr)
library(gridExtra)
library(gapminder)
library(yarrr)
library(ggplot2)
library(plotly)
library(corrplot)
library(data.table)
library(gginference)
library(kableExtra)

getwd()
heartDF <- read.csv('C:\\Users\\Uzzi\\Downloads\\heart.csv')
str(heartDF)
summary(heartDF)
psych::describe(heartDF)
dplyr::glimpse(heartDF)

colnames(heartDF) <- c('Age', 'Sex', 'ChestPainType', 'RestingBP', 'Cholesterol', 'FastingBSugar', 'RestECG', 'MaxHeartRate',
                       'ExerciseInducedPain', 'STDepression', 'STSlope', 'NumMajorVessels', 'Thalassemia', 'Target')
rapply(heartDF,function(x)length(unique(x)))

heartDF <- subset(heartDF, select = c('Age', 'Sex', 'RestingBP', 'Cholesterol', 'RestECG', 'MaxHeartRate', 'STDepression', 'Target'))

heartDF$Sex <- as.factor(heartDF$Sex)
heartDF$Target <- as.factor(heartDF$Target)

AgeGrp <- cut(heartDF$Age, 
              breaks = c(-Inf,40,65,Inf), 
              labels = c('1','2','3'),
              right = FALSE)
heartDF <- data.frame(heartDF, AgeGrp)

heartDF <- subset(heartDF, select = c('Target', 'Sex', 'AgeGrp', 'MaxHeartRate', 'STDepression', 'RestingBP', 'Cholesterol', 'RestECG'))

heart_group <- heartDF %>% group_by(Sex, AgeGrp, Target) %>% tally()
heart_disease_group <- subset(heart_group, heart_group$Target == 1)
not_heart_disease_group <- subset(heart_group, heart_group$Target == 0)

plot1 <- ggplot2::ggplot(heart_disease_group,
                aes(x = AgeGrp, y = n, fill = Sex, label = n)) + 
  xlab('Age-Group') + ylab('Number of Observation') +
  scale_x_discrete(labels = c('Below 40 yrs', '40-65 yrs', 'Above 65 yrs')) +
  scale_fill_manual('Gender', values = c('#19A0AA', '#F15F36'), labels=c('Males', 'Female')) +
  ggtitle('Plot of Heart Disease') +
  geom_bar(position='stack', stat='identity') +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

plot2 <- ggplot2::ggplot(not_heart_disease_group,
                aes(x = AgeGrp, y = n, fill = Sex, label = n)) +
  xlab('Age-Group') +
  ylab('Number of Observation') +
  scale_x_discrete(labels = c('Below 40 yrs', '40-65 yrs', 'Above 65 yrs')) +
  scale_fill_manual('Gender', values = c('#19A0AA', '#F15F36'), labels=c('Males', 'Female')) +
  ggtitle('Plot of No Heart Disease') +
  geom_bar(position='stack', stat='identity') +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

grid.arrange(plot1, plot2, ncol= 2)

plot3 <- ggplot2::ggplot(data = heartDF, 
                         mapping = aes(x = AgeGrp, y = MaxHeartRate, fill = Target)) +
  xlab('Age-Group') +
  ylab('Maximum Heart Rate') +
  ggtitle('Box-Plot of Heart Disease') +
  scale_x_discrete(labels = c('Below 40 yrs', '40-65 yrs', 'Above 65 yrs')) +
  scale_fill_manual('Heart\nDisease', values = c('#FC766AFF', '#5B84B1FF'), labels=c('Yes', 'No')) +
  ylim(c(110, 180)) +
  geom_boxplot() + 
  geom_violin(trim = FALSE, alpha = 0.2)

plot4 <- ggplot2::ggplot(data = heartDF, 
                        mapping = aes(x = Sex, y = MaxHeartRate, fill = Target)) +
  xlab('Age-Group') +
  ylab('Maximum Heart Rate') +
  ggtitle('Box-Plot of Heart Disease') +
  scale_x_discrete(labels = c('Males', 'Females')) +
  scale_fill_manual('Heart\nDisease', values = c('#FC766AFF', '#5B84B1FF'), labels=c('Yes', 'No')) +
  ylim(c(110, 180)) +
  geom_boxplot()  + 
  geom_violin(trim = FALSE, alpha = 0.2)

grid.arrange(plot3, plot4, ncol= 2)

set.seed(1)
heartDFCor <- cor(heartDF %>% type.convert(as.is=TRUE))
heartDFCor[is.na(heartDFCor)] = 0
heartDFCor <- round(heartDFCor, 2)
png(file='corr.png', res=150, width=10000, height=4500)
corrplot(as.matrix(heartDFCor), tl.cex = 3, tl.col = 'black', method = 'color', 
         outline = T,  order='hclust', 
         addCoef.col = 'black', number.digits = 2, number.cex = 3, 
         cl.pos = 'b', cl.cex = 3, addrect = 3, rect.lwd = 3, 
         col = colorRampPalette(c('midnightblue', 'white','darkred'))(100))
dev.off()

heart_below_40 <- subset(heartDF, Target == 1 | AgeGrp == 1, 
                         select = c('MaxHeartRate'))
heart_below_40 <- data.frame(heart_below_40, group = 'heart_below_40')

heart_40_to_65 <- subset(heartDF, Target == 1 | AgeGrp == 2, 
                         select = c('MaxHeartRate'))
heart_40_to_65 <- data.frame(heart_40_to_65, group = 'heart_40_to_65')

heart_above_65 <- subset(heartDF, Target == 1 | AgeGrp == 3, 
                         select = c('MaxHeartRate'))
heart_above_65 <- data.frame(heart_above_65, group = 'heart_above_65')

# H0: There is no difference in max heart rate with heart disease in the three groups of age (claim).
# H1: There is a difference in max heart rate with heart disease in the three groups of age.

alpha <- 0.05

data <- rbind(heart_below_40, heart_40_to_65, heart_above_65)
result <- kruskal.test(MaxHeartRate ~ group, data = data)
result.data <- data.frame( 
  Values = c(result$statistic, '5.991', result$p.value, alpha, result$parameter),
  stringsAsFactors = FALSE
)

tidyr::as_tibble(result.data)

t(result.data) %>% kable(col.names = c('Kruskal-Wallis chi-squared', 'Critical Value', 'p-value', 'alpha', 'Degree of freedom'),
                         caption = 'The Kruskal-Wallis Test') %>%
  kable_styling(bootstrap_options = c('striped', 'hover', 'condensed'),
                html_font = 'Calibri', 
                font_size = 20)

