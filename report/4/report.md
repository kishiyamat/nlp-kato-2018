# 課題

> 以下に示す単語辞書と連結可否行列を用いて、
> 「頭痛を飲み薬でなおして」を形態素解析せよ。

```r
df <- data.frame(
    word =c( '薬', '風邪', '熱', '喉', '胃', '消化'),

    df   =c(9030 ,   670 , 184 ,  27 , 428 ,   359 ),

    tf.d1=c(   2 ,     3 ,   0 ,   1 ,   0 ,     0 ),
    tf.d2=c(   1 ,     1 ,   1 ,   0 ,   0 ,     0 ),
    tf.d3=c(   2 ,     0 ,   1 ,   1 ,   0 ,     0 ),
    tf.d4=c(   4 ,     0 ,   0 ,   0 ,   3 ,     2 ),
    tf.d5=c(   1 ,     0 ,   0 ,   0 ,   1 ,     4 ))

```

```r
library(tidyverse)
library(magrittr)

N <- 10000
df %>% mutate(
    tf.idf.1= tf.d1 * (log10(N/df)+1),
    tf.idf.2= tf.d2 * (log10(N/df)+1),
    tf.idf.3= tf.d3 * (log10(N/df)+1),
    tf.idf.4= tf.d4 * (log10(N/df)+1),
    tf.idf.5= tf.d5 * (log10(N/df)+1)) ->
df.tf.idf

>   word   df tf.d1 tf.d2 tf.d3 tf.d4 tf.d5 df.1 
> 1   薬 9030     2     1     2     4     1    5 
> 2 風邪  670     3     1     0     0     0    2 
> 3   熱  184     0     1     1     0     0    2 
> 4   喉   27     1     0     1     0     0    2 
> 5   胃  428     0     0     0     3     1    2 
> 6 消化  359     0     0     0     2     4    2 
>   tf.idf.1 tf.idf.2 tf.idf.3 tf.idf.4 tf.idf.5
> 1 2.088624 1.044312 2.088624 4.177249 1.044312
> 2 6.521776 2.173925 0.000000 0.000000 0.000000
> 3 0.000000 2.735182 2.735182 0.000000 0.000000
> 4 3.568636 0.000000 3.568636 0.000000 0.000000
> 5 0.000000 0.000000 0.000000 7.105669 2.368556
> 6 0.000000 0.000000 0.000000 4.889811 9.779622
```

```r
# lとrの内積を返す中置関数を定義
'%ip%' <- function (l,r) 
    # 入力のベクトルl,rを列に格納
    data.frame(l=l,r=r) %>% 
    # l.rという列名の各列にl*rを格納
    mutate(l.r=l*r) %$%
    # l.rの和を取る
    sum(l.r)

# ベクトル(Ws)の長さを返す関数を定義
D <- function(Ws) sqrt(sum(Ws ** 2))

# 文書1 = <2,1,3,0> と 質問=<4,3,0,5> で動作確認
d1 <- c(2,1,3,0) 
q1 <- c(4,3,0,5)

q1 %ip% d1 / (D(d1) * D(q1))
```


```r
# 1薬、2風邪、3熱、4のど、5胃、6消化のベクトル
df.tf.idf $ tf.idf.1 
# 1薬、2風邪、3熱のベクトル
d1 = df.tf.idf $ tf.idf.1 [c(1,2,3)]
d2 = df.tf.idf $ tf.idf.2 [c(1,2,3)]
d3 = df.tf.idf $ tf.idf.3 [c(1,2,3)]
d4 = df.tf.idf $ tf.idf.4 [c(1,2,3)]
d5 = df.tf.idf $ tf.idf.5 [c(1,2,3)]

# 検索質問: 薬、風邪、熱
q1 = c(1,3,2)

data.frame(
    cos.d1.q1 = q1 %ip% d1 / (D(d1) * D(q1)),
    cos.d2.q1 = q1 %ip% d2 / (D(d2) * D(q1)),
    cos.d3.q1 = q1 %ip% d3 / (D(d3) * D(q1)),
    cos.d4.q1 = q1 %ip% d4 / (D(d4) * D(q1)),
    cos.d5.q1 = q1 %ip% d5 / (D(d5) * D(q1)))

# 1薬、5胃のベクトル
df.tf.idf $ tf.idf.1 [c(1,5)]
# 検索質問: 薬、胃
d1 = df.tf.idf $ tf.idf.1 [c(1,5)]
d2 = df.tf.idf $ tf.idf.2 [c(1,5)]
d3 = df.tf.idf $ tf.idf.3 [c(1,5)]
d4 = df.tf.idf $ tf.idf.4 [c(1,5)]
d5 = df.tf.idf $ tf.idf.5 [c(1,5)]

q2 = c(1,2)
data.frame(
    cos.d1.q2 = q2 %ip% d1 / (D(d1) * D(q2)),
    cos.d2.q2 = q2 %ip% d2 / (D(d2) * D(q2)),
    cos.d3.q2 = q2 %ip% d3 / (D(d3) * D(q2)),
    cos.d4.q2 = q2 %ip% d4 / (D(d4) * D(q2)),
    cos.d5.q2 = q2 %ip% d5 / (D(d5) * D(q2)))
```
