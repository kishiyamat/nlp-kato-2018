# 課題 {-}

> 「ヒロシが病院でもらった薬を飲んだ」を
> CYK法で解析せよ。

<!-- 文法解析も沼が深いが -->
<!-- 形態素解析も沼が深い -->

# 概要

CYK法(Cocke--Younger--Kasami)法は
最も基本的なボトムアップアルゴリズムの一つである。
行列 $a$ の対角成分に品詞(POS)を配置し、
3重のfor文で解析を行っていく。
品詞列の長さを $d$ とすると、
解析が終わった時点での $a_{1d}$ が持つ状態で
文が適格か不適格かを判断できる。

肝心なのは品詞の規則が"S -> PP VP"のような形、
つまり左辺(LHS)と右辺(RHS)の
統合する手順、アルゴリズムである。
まず $a_{ij}$ をLHSとして解析する場合を考える。
その場合、$a_{ii}$から$a_{jj}$を包括するRHSが必要となる。
そのようなRHSは
$a_{ii}$から$a_{jj}$まで動く点$k$を仮定すると
$a_{ik}$がRHSの左となり、$a_{k+1,j}$がRHSのの右側となることが分かる。

つまり、最初のforには解析する層が該当し、
これには`1:(n-1)`が該当し`d`とする。
右に進んだ分(`d`)だけ、
走査する層の範囲$a_{ij}$は大きくなり(`j <- i + d`)、
動けるiの範囲はどんどんと狭くなる(`i in 1:(n-d)`、
つまり`d`が進むほどiは小さくなる。)。
そして`i`から`j-1`の間の`k`を回すことで
層のfor(d)、三角の移動のfor(i,j),
そして三角内の分割のfor(k)を回せることになる。

```r
n <- 5
# debug
example <- array(dim = c(n, n, 1))
for(d in 1:(n-1)){
    for (i in 1:(n-d)){
        j <- i + d
        for (k in i:(j-1)){
            Sys.sleep(1)
            example[ i ,j,1] <- "LHS"
            example[ i ,k,1] <- "L"
            example[k+1,j,1] <- "R"
            print(example)
            print(sprintf("d is %d, i is %d, j is %d, and k is %d",
                          d, i, j, k))
            example[ i ,j, 1] <- NA
            example[ i ,k, 1] <- NA
            example[k+1,j, 1] <- NA
        }
    }
}
```

以下はCYK解析を行なう準備とし、
CYKの節で問題の回答を示す。

# 下準備

## 文法

CYK法のような上昇型文法解析を実行するためには
1. 左辺と右辺から成る文法
と、
2. 右辺から左辺を取得する関数
の2つがまず必要となる。
まず、この節では与えられた文法を
1. 句構造規則`rule.p`と
2. 辞書規則`rule.d`に分け、
Char型で定義する。
文脈自由文法のなかでも、
句構造規則は非終端記号を左辺に持ち、
右辺には2つの非終端記号からのみなる
チョムスキー標準形を用いる。
なお、辞書規則は前終端記号(品詞)から
終端記号への遷移を示す。

```r
# 句構造(phrase)規則
rule.p <- c("S -> PP VP" ,  "PP -> NP P",
            "NP -> VP NP",  "VP -> PP VP")
# 辞書(dictionary)規則
rule.d <- c("NP -> 薬",     "NP -> 病院",     "NP -> ヒロシ",
            "VP -> 飲んだ", "VP -> もらった", 
            "P -> が",      "P -> で",        "P -> を")
```

次に、上の各規則に含まれる ` -> `で左辺と右辺を分割し
Char型のvectorを生成する関数 `char2cfg` も定義する。
この`char2cfg`関数に "S -> PP VP" という文字列を与えた場合、
左辺の"S"が第一要素に、右辺の"PP VP"が第二要素に返される。

```r
char2cfg <- function(s) unlist(strsplit(s, " -> "))
char2cfg("S -> PP VP")
# [1] "S"     "PP VP"
```

上で行なった規則の定義には`c`関数を使っているが、
`c`関数の"c"は"concatenate"を意味し、
ベクトルの各要素が同型であることを保証できる。
先ほど定義した `char2cfg` はChar型をベクトルに変換する関数だった。
ここで `sapply` 関数を使えば
Char型のベクトルである `rule.p` や `rule.d` の各要素に対し、
`char2cfg` をapplyできる。
したがって、`rule.p`の各要素に`char2cfg`を`sapply`すると
以下のような結果となる。

```r
rule.p
# [1] "S -> PP VP"  "PP -> NP P"  "NP -> VP NP" "VP -> PP VP"
sapply(rule.p, char2cfg)
#      S -> PP VP PP -> NP P NP -> VP NP VP -> PP VP
# [1,] "S"        "PP"       "NP"        "VP"       
# [2,] "PP VP"    "NP P"     "VP NP"     "PP VP"    
```

## 統合操作

CYK解析は上昇型の解析であるため、
'太郎' を見たら 'NP' を返し、
'PP'と'VP'を見れば統合し'S'を返してくれる、
右辺を統合し左辺を求める機能が必要となる。
先ほどの`char2cfg`を規則に`sapply`すると、
各リストの要素にはLHSとRHSが格納された。
このリストを`vector.p`に格納し、行に`LHS`と`RHS`の名前をつける。
そして`t`関数で転置し `as.data.frame` で`data.frame`に変換する。
この「規則のベクトルから規則のテーブルへの変換(`vector2table`)」を
句構造規則`rule.p`対して行なうと
`table.p` という `data.frame` が作れる。

```r
vector.p <- sapply(rule.p, char2cfg)
vector.p
#      S -> PP VP PP -> NP P NP -> VP NP VP -> PP VP
# [1,] "S"        "PP"       "NP"        "VP"       
# [2,] "PP VP"    "NP P"     "VP NP"     "PP VP"    
t(vector.p)
#             [,1] [,2]   
# S -> PP VP  "S"  "PP VP"
# PP -> NP P  "PP" "NP P" 
# NP -> VP NP "NP" "VP NP"
# VP -> PP VP "VP" "PP VP"
vector2table <- function(l) {
    dimnames(l) <- list(c("LHS","RHS"), NULL)
    as.data.frame(t(l), stringsAsFactors=FALSE) }
table.p <- vector2table(vector.p)
table.p
#   LHS   RHS
# 1   S PP VP
# 2  PP  NP P
# 3  NP VP NP
# 4  VP PP VP
```

上の様なテーブルがあれば、
`RHS`を参照、統合し`LHS`を取得することが容易となる。
例えば、仮に"PP VP"を統合する規則が欲しい場合を考える。
その際は「`RHS`列が"PP VP"であるデータフレーム」
を`subset`として取り、そのデータフレームの`LHS`行を取得すれば良い。
そのような関数を以下に`rhs2lhs`と定義する。

```r
subset(table.p, table.p$RHS=="PP VP")
#             LHS   RHS
# S -> PP VP    S PP VP
# VP -> PP VP  VP PP VP
rhs2lhs <- function(table) function(rhs){
    subset(table, table$RHS==rhs)$LHS}
```

この関数は引数をとって関数を返す、
いわゆる高階関数である。
この関数はまず統合の可否を決めるために参照するテーブルを
`table`として引数に取り、以下の性質を持つ関数を返す。

> `RHS`をChar型として引数に取り、
> 先に部分適用している`table`の`LHS`を返す。

仮に`table.p`を与えてから"PP VP"を与えた場合、
"S"と"VP"の2つがベクトルとして返される。
これは"PP VP"が可能な統合先には
"S"と"VP"があることを示している。

```r
table.p
#   LHS   RHS
# 1   S PP VP <- これと
# 2  PP  NP P
# 3  NP VP NP
# 4  VP PP VP <- これがsubsetとなる
rhs2lhs(table.p)("PP VP")
# [1] "S"  "VP"
```

以上の操作を辞書規則でも行ない、
`vector.d`と`table.d`を作成する。
これらにより、単語から前終端記号へのマップが可能になる。

```r
vector.d <- sapply(rule.d, char2cfg)
table.d <- vector2table(vector.d)
table.d
#   LHS      RHS
# 1  NP       薬
# 2  NP     病院
# 3  NP   ヒロシ
# 4  VP   飲んだ
# 5  VP もらった
# 6   P       が
# 7   P       で
# 8   P       を
```

まず、半角スペースので与えられたChar型をvectorに変換する
関数`char2vector`を考えた時、
問題となっている"ヒロシ が 病院 で もらった 薬 を 飲んだ"
はChar型のベクトル`vector.input`に変換できる。
このベクトルを先ほど作った`rhs2lhs`に
辞書(`table.d`)を部分適用して作った関数に`sapply`すると、
CYKに必要な対角ベクトルが得られる。

```r
char2vector <- function(s) unlist(strsplit(s, "[ ]"))
input <- "ヒロシ が 病院 で もらった 薬 を 飲んだ"
vector.input <- char2vector(input)
vector.input
# ヒロシ   が   病院    で  もらった    薬   を  飲んだ 

class(rhs2lhs(table.d))
# [1] "function"
sapply(vector.input, rhs2lhs(table.d))
# ヒロシ   が   病院    で  もらった    薬   を  飲んだ 
#   "NP"  "P"   "NP"   "P"      "VP"  "NP"  "P"    "VP" 
```

## 三角行列

以上で規則に関する道具は揃ったため、CYKで使う行列に移る。
CKY法は三角行列の $a_{ij}$ をベースに構文解析を進めていくため、
最後の準備として三角行列(対角線で区切られている行列)が必要となる。
行列の$i$も$j$も長さは入力文字列がもつ前終端記号数
(今回の場合は8)となる。
なお、行列内に行列を含める言語もあるが、
今回は行列を多次元に拡張した、配列[^non]の使用が好ましい。
そこで`array`関数を使った8x8x1の配列を`triangle`として作る。
イメージとしては,8x8の行列が1枚ある感じである。

[^non]: 配列の特徴を述べている「関係節の非制限用法」なのだが、日本語だと表現が難しい。制限用法だといかにも「多次元に拡張しない配列」があるように聞こえる。「配列、多次元に拡張するデータ形式」のように表現すべきなのかもしれない。

```r
char2vector <- function(s) unlist(strsplit(s, "[ ]"))
input <- "ヒロシ が 病院 で もらった 薬 を 飲んだ"
vector.input <- char2vector(input)
vector.input
# [1] "ヒロシ" "が" "病院"   "で"  "もらった" "薬"    "を"      
# [8] "飲んだ"  
n <- length(vector.input)
# [1] 8
triangle <- array(dim = c(n, n, 1))
triangle
# , , 1
# 
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
# [1,]   NA   NA   NA   NA   NA   NA   NA   NA
# [2,]   NA   NA   NA   NA   NA   NA   NA   NA
# [3,]   NA   NA   NA   NA   NA   NA   NA   NA
# [4,]   NA   NA   NA   NA   NA   NA   NA   NA
# [5,]   NA   NA   NA   NA   NA   NA   NA   NA
# [6,]   NA   NA   NA   NA   NA   NA   NA   NA
# [7,]   NA   NA   NA   NA   NA   NA   NA   NA
# [8,]   NA   NA   NA   NA   NA   NA   NA   NA
```

まずは`rhs2lhs`を終端記号(単語)に`sapply`して
前終端記号のベクトル、`pos`とする。
つまり、"ヒロシ"を`rhs2lhs`にapplyすると`NP`となる。
これを上の行列の$i$行目$j$列目の
`triangle`に組み込む操作を一般化すると、
文字列の$i$番目の要素を行列の$i$行目、$i$列目に挿入(`<<-`)、
という操作になる。

```r
pos <- sapply(vector.input, rhs2lhs(table.d))
pos
#   ヒロシ  が   病院   で もらった    薬   を   飲んだ 
#     "NP"  "P"   "NP"  "P"     "VP"  "NP"  "P"     "VP" 
mapply((function(x,i) triangle[i,i,1] <<- x),
       pos, 1:(length(pos)))
       # global変数への挿入をするため仕方なく、 <<- を使っている。
triangle
# , , 1
# 
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
# [1,] "NP" NA   NA   NA   NA   NA   NA   NA  
# [2,] NA   "P"  NA   NA   NA   NA   NA   NA  
# [3,] NA   NA   "NP" NA   NA   NA   NA   NA  
# [4,] NA   NA   NA   "P"  NA   NA   NA   NA  
# [5,] NA   NA   NA   NA   "VP" NA   NA   NA  
# [6,] NA   NA   NA   NA   NA   "NP" NA   NA  
# [7,] NA   NA   NA   NA   NA   NA   "P"  NA  
# [8,] NA   NA   NA   NA   NA   NA   NA   "VP"

triangle[1,1,]
# [1] "NP"
```

## ライブラリーのインポート

最後に配列を扱う上で便利なライブラリーを一つだけ導入する。
外部パッケージには`rbind`を配列にも拡張した`abind`を利用する。

```r
install.packages("abind")
library(abind)
```

# CYK

概要にてCYKのアルゴリズムをおおかた確認したが、
実際に動かす段階では幾つか問題がある。
まずは問題に回答し、
実際に回答を得るまでの問題を述べる。

最終的に得られるCYKの解析結果は以下のものである。

```r
     [,1] [,2] [,3] [,4] [,5]    [,6] [,7] [,8]                 
[1,] "NP" "PP" ""   ""   "S, VP" "NP" "PP" "S, VP, S, VP, S, VP"
[2,] ""   "P"  ""   ""   ""      ""   ""   ""                   
[3,] ""   ""   "NP" "PP" "S, VP" "NP" "PP" "S, VP, S, VP"       
[4,] ""   ""   ""   "P"  ""      ""   ""   ""                   
[5,] ""   ""   ""   ""   "VP"    "NP" "PP" "S, VP"              
[6,] ""   ""   ""   ""   ""      "NP" "PP" "S, VP"              
[7,] ""   ""   ""   ""   ""      ""   "P"  ""                   
[8,] ""   ""   ""   ""   ""      ""   ""   "VP"                 
```

一番左上$a_{i8}$のレベルでは"S"が3つ作成されているため、
解析の過程で3つの曖昧性が発生したと分かる。
まず$a_{i8}$にSを返した組み合わせを考えると、
$a_{22}$のPP「ヒロシが」
と$a_{38}$のVP「病院でもらった薬を飲んだ」
がある。
今回の場合、$a_{11}$のPP「ヒロシが」で曖昧性は発生せず、
$a_{38}$のVPで曖昧性が発生している。
そこで$i$が3から始まるPP($a_{3k}$)と
$j$が8で終わるVP$a_{k_{+1}_8}$を探すと、
「病院でもらった薬を(3--7)」「飲んだ(8--8)」
と
「病院で(3--4)」「もらった薬を飲んだ(5--8)」
のパターンが該当する。
したがって、[ヒロシが [[病院でもらった薬を]飲んだ]]
という構造と
[ヒロシが [[病院で]もらった薬を飲んだ]]
という曖昧性が見てとれる。

他方、$a_{17}$「ヒロシが病院でもらった薬を(PP)」と
$a_{88}$「飲んだ(VP)」の組み合わせもある。
こちらに構造的な曖昧性は発生しておらず、
「病院で」というPPと
「もらったという」VPが
マージして「病院でもらった」というVPとなり、
さらに「ヒロシが」というPPと
「病院でもらった」というVPがマージして
「ヒロシが病院でもらった」というVPが作られている。
このVPと「薬」がマージしNP、そしてさらに
「を」とマージしてPPとなる。
これは「ヒロシが病院でもらってきた薬」を誰かは知らないけど
誰かが飲んだ、という意味になる。

以上で回答を終えるが、
以下を解析に使ったコードの釈明とする。
最後に実装する際の問題としては、
$a_{ik}$や$a_{k_{+1}j}$の中身が
一つとは限らない、という点がある。
そこで今回は行列ではなく配列を使用し、
各$a$の座標に奥行きを持たせており、
$a_{ik}$にあるベクトルが欲しければ`[i,k,]`と指定することで
ベクトルを取得している。
そうして右辺の左のベクトル`ik`、右のベクトル`k1.j`をとり、
`product <- c(outer(ik, k1.j, FUN=paste))`として
組み合わせを作って`rhs2lhs`を適用している[^outer]。
結果はベクトルだが、配列の奥行きとしてベクトルを保存した。

[^outer]: https://stackoverflow.com/questions/29959759/r-language-cross-product-combination-of-two-string-arrays や https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/outer を参考にした。

```r
n <- length(vector.input)
triangle <- array(dim = c(n, n, 1))
mapply((function(x,i) triangle[i,i,1] <<- x),
       pos, 1:(length(pos)))
triangle

n <- length(vector.input)

# debug
example <- array(dim = c(n, n, 1))
for(d in 1:(n-1)){
    for (i in 1:(n-d)){
        j <- i + d
        for (k in i:(j-1)){
            Sys.sleep(1)
            example[ i ,j,1] <- "LHS"
            example[ i ,k,1] <- "L"
            example[k+1,j,1] <- "R"
            print(example)
            print(sprintf("d is %d, i is %d, j is %d, and k is %d",
                          d, i, j, k))
            example[ i ,j, 1] <- NA
            example[ i ,k, 1] <- NA
            example[k+1,j, 1] <- NA
        }
    }
}

for(d in 1:(n-1)){
    for (i in 1:(n-d)){
        j <- i + d
        for (k in i:(j-1)){
            ik   <- triangle[ i , k, ]
            k1.j <- triangle[k+1, j, ]
            product <- c(outer(ik, k1.j, FUN=paste))
            m <- unlist(sapply(product, rhs2lhs(table.p)))
            if(length(m)==1){
                triangle[i,j,1] <- m
            }else if(length(m)>1){
                len.m <- length(m)
                tmp <- array(dim = c(n, n, len.m))
                tmp[i,j,1:len.m] <- m
                triangle <- abind(triangle,tmp)
            }
        }
    }
}
```

最後の出力は配列の奥行きの要素を文字列として
結合するのみである。

```r

cat.table <- array(dim = c(n, n))
for(i in 1:8){
    for(j in 1:8){
    cat.table[i,j]<- toString(na.omit(triangle[i,j,]))
    }
}
cat.table
#      [,1] [,2] [,3] [,4] [,5]    [,6] [,7] [,8]                 
# [1,] "NP" "PP" ""   ""   "S, VP" "NP" "PP" "S, VP, S, VP, S, VP"
# [2,] ""   "P"  ""   ""   ""      ""   ""   ""                   
# [3,] ""   ""   "NP" "PP" "S, VP" "NP" "PP" "S, VP, S, VP"       
# [4,] ""   ""   ""   "P"  ""      ""   ""   ""                   
# [5,] ""   ""   ""   ""   "VP"    "NP" "PP" "S, VP"              
# [6,] ""   ""   ""   ""   ""      "NP" "PP" "S, VP"              
# [7,] ""   ""   ""   ""   ""      ""   "P"  ""                   
# [8,] ""   ""   ""   ""   ""      ""   ""   "VP"                 
```
