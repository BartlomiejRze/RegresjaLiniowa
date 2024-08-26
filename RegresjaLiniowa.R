> cor(economics$pce, economics$psavert)
[1] -0.7928546

> cor(economics[, c(2, 4:6)])

                pce    psavert    uempmed   unemploy
pce       1.0000000 -0.7928546  0.7269616  0.6145176
psavert  -0.7928546  1.0000000 -0.3251377 -0.3093769
uempmed   0.7269616 -0.3251377  1.0000000  0.8693097
unemploy  0.6145176 -0.3093769  0.8693097  1.0000000

> GGally::ggpairs(economics[, c(2, 4:6)])                                                                                                                                    
> econCor <- cor(economics[, c(2, 4:6)])
> econMelt <- melt(econCor, varnames=c('x', 'y'), value.name='Korelacja')
> econMelt <- econMelt[order(econMelt$Korelacja), ]
> econMelt

          x        y  Korelacja
2   psavert      pce -0.7928546
5       pce  psavert -0.7928546
7   uempmed  psavert -0.3251377
10  psavert  uempmed -0.3251377
8  unemploy  psavert -0.3093769
14  psavert unemploy -0.3093769
4  unemploy      pce  0.6145176
13      pce unemploy  0.6145176
3   uempmed      pce  0.7269616
9       pce  uempmed  0.7269616
12 unemploy  uempmed  0.8693097
15  uempmed unemploy  0.8693097
1       pce      pce  1.0000000
6   psavert  psavert  1.0000000
11  uempmed  uempmed  1.0000000
16 unemploy unemploy  1.0000000

> ggplot(econMelt, aes(x=x, y=y)) + geom_tile(aes(fill=Korelacja)) +
+     scale_fill_gradient2(low=muted('red'), mid='white', high='steelblue',
+                          guide=guide_colorbar(ticks=FALSE, barheight=10), limits=c(-1, 1)) +
+     theme_minimal() + labs(x=NULL, y=NULL)


> wiek.plantacji <- c(1, 3, 2, 3, 4, 3)
> plony <- c(85, 105, 100, 125, 115, 130)
> cor(wiek.plantacji, plony)
[1] 0.7522288

> cov(economics$pce, economics$psavert)
[1] -8359.069

> cov(economics[, c(2, 4:6)])
                  pce      psavert      uempmed    unemploy
pce      12650851.944 -8359.069071 10618.386190 5774578.978
psavert     -8359.069     8.786360    -3.957847   -2422.805
uempmed     10618.386    -3.957847    16.864531    9431.652
unemploy  5774578.978 -2422.805358  9431.652268 6979948.309

> identical(cov(economics$pce, economics$psavert), cor(economics$pce, economics$psavert)
+           * sd(economics$pce) *sd(economics$psavert))
[1] TRUE

> liczba.studentowAGH <- c(4000, 12000, 6000, 21000, 9000, 1000, 3000)
> powierzchnia.sal.dydaktycznych <- c(4500, 8800, 4200, 17000, 6000, 1800, 2500)
> model.YX <- lm(formula = liczba.studentowAGH ~ powierzchnia.sal.dydaktycznych)		#lm (linear model)- funkcja do budowania modeli liniowych 
> model.XY <- lm(formula = powierzchnia.sal.dydaktycznych ~ liczba.studentowAGH)
> a1 <- model.YX$coefficients[2]
> a0 <- model.YX$coefficients[1]
> b1 <- model.XY$coefficients[2]
> b0 <- model.XY$coefficients[1]
> names(a1) <- NULL
> names(a0) <- NULL
> names(b1) <- NULL
> names(a0) <- NULL
> sqrt(a1*b1)
[1] 0.9859583

> cor(liczba.studentowAGH, powierzchnia.sal.dydaktycznych)
[1] 0.9859583

> cor(powierzchnia.sal.dydaktycznych, liczba.studentowAGH)
[1] 0.9859583

> data(father.son, package='UsingR')
> force(father.son)

     fheight  sheight
1   65.04851 59.77827
2   63.25094 63.21404
3   64.95532 63.34242
...
499 68.77033 67.48077
500 70.49154 67.67057

 [ reached 'max' / getOption("max.print") -- omitted 578 rows ]

> ggplot(father.son, aes(x=fheight, y=sheight)) + geom_point() +geom_smooth(method='lm') + labs(x='Fathers', y='Sons')
`geom_smooth()` using formula = 'y ~ x'

> heightsLM <- lm(sheight ~ fheight, data=father.son)
> heightsLM

Call:
lm(formula = sheight ~ fheight, data = father.son)

Coefficients:
(Intercept)      fheight  
    33.8866       0.5141  

> summary(heightsLM)

Call:
lm(formula = sheight ~ fheight, data = father.son)

Residuals:
    Min      1Q  Median      3Q     Max 
-8.8772 -1.5144 -0.0079  1.6285  8.9685 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 33.88660    1.83235   18.49   <2e-16 ***
fheight      0.51409    0.02705   19.01   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.437 on 1076 degrees of freedom
Multiple R-squared:  0.2513,	Adjusted R-squared:  0.2506 
F-statistic: 361.2 on 1 and 1076 DF,  p-value: < 2.2e-16


> library(ggplot2)
> data("diamonds")
> head(diamonds)
# A tibble: 6 × 10
  carat cut       color clarity depth table price     x     y     z
  <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48

> hist(diamonds$carat, main="Carat Histogram", xlab="Carat")

> plot(price~carat, data=diamonds)

> plot(diamonds$carat, diamonds$price)

> boxplot(diamonds$carat)

> ggplot(data=diamonds) + geom_histogram(aes(x=carat))
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

> ggplot(data=diamonds) + geom_density(aes(x=carat), fill="grey50")

> ggplot(diamonds, aes(x=carat, y=price)) + geom_point()

> g <- ggplot(diamonds, aes(x=carat, y=price))

> g + geom_point(aes(color=color))

> g + geom_point(aes(color=color)) + facet_wrap(~color)

#ZADANIE
[1]
a) Zdecyduj, które z poniższych przypadków są problemami klasyfikacji, a które regresji 
Przy poszczególnych przykładach zaznacz K (klasyfikacji) lub R (regresji) 

b) Jakie dane należy zebrać, aby się nimi zająć? 
- Wykrywanie spamu w wiadomościach e-mail- K 
- Przewidywanie rynkowej ceny akcji- R
- Przewidywanie sympatii do nowego ogłoszenia- K
- Ocena ryzyka kredytowego- K 
- Wykrywanie tkanek nowotworowych na zdjęciach medycznych- K 
- Przewidywany czas powrotu do zdrowia pacjentów chorych na raka- R 
- Rozpoznawać uśmiechnięte/smutne twarze na zdjęciach- K 
- Wykrywanie nienadzorowanego bagażu w nagraniach z kamer ochrony lotniska- K 
- Włączyć hamowanie awaryjne, aby uniknąć kolizji z pieszymi- K

[2] Wyjaśnij Różnice między klasyfikacją, a aregresją w analizie danych.
Klasyfikacja i regresja to dwa rodzaje uczenia maszynowego, które są sotsowane w celu rozwiązywania różnych problemów.
Klasyfikacja jest stosowana w celu przypisywania danych wejściowych do jednej z określonych klas, 
natomiast regresja jest stosowana do przewidywania wartości numerycznych na podstawie danych wejściowych. 