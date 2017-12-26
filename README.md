Hw3
================
Naor Dalal & Koren Levenbrown
12/26/2017

1.A
---

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
ga.data <- read.csv('ga_edgelist.csv', header = T)
g <- graph.data.frame(ga.data,directed = F)
plot(g)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
gDerived <- delete.vertices(g , c('adele' , 'chief' , 'susan grey' , 'thatch grey' , 'ellis grey' , 'tucker' , 'bailey' , 'ben'))
plot(gDerived)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-2-1.png)

### i. By Betweenness

``` r
between <- betweenness(gDerived)
between[which.max(between)]
```

    ##    sloan 
    ## 115.3667

### ii. By closeness

``` r
close <- closeness(gDerived)
close[which.max(close)]
```

    ##     torres 
    ## 0.01754386

### iii. By Eigenvector

``` r
eig <- eigen_centrality(gDerived)
eig$vector[which.max(eig$vector)]
```

    ## karev 
    ##     1

1.B
---

### Girvan-Newman community detection

``` r
gc <- edge.betweenness.community(g)
gc
```

    ## IGRAPH clustering edge betweenness, groups: 7, mod: 0.58
    ## + groups:
    ##   $`1`
    ##   [1] "lexi"         "sloan"        "karev"        "kepner"      
    ##   [5] "addison"      "nancy"        "mrs. seabury" "avery"       
    ##   
    ##   $`2`
    ##   [1] "owen"    "yang"    "altman"  "colin"   "preston"
    ##   
    ##   $`3`
    ##   [1] "torres"   "o'malley" "arizona"  "olivia"  
    ##   
    ##   + ... omitted several groups/vertices

``` r
memb <- membership(gc)
memb
```

    ##         lexi         owen        sloan       torres        derek 
    ##            1            2            1            3            4 
    ##        karev     o'malley         yang         grey        chief 
    ##            1            3            2            4            5 
    ##   ellis grey   susan grey       bailey        izzie       altman 
    ##            5            5            6            7            2 
    ##      arizona        colin      preston       kepner      addison 
    ##            3            2            2            1            1 
    ##        nancy       olivia mrs. seabury        adele  thatch grey 
    ##            1            3            1            5            5 
    ##       tucker         hank        denny         finn        steve 
    ##            6            7            7            4            4 
    ##          ben        avery 
    ##            6            1

Plot the graph with unique color for each community accordingly

``` r
plot(g, vertex.size=7, #vertex.label=NA,
     vertex.color=memb, asp=FALSE)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-8-1.png)

There is 7 communities

``` r
length(unique(memb))
```

    ## [1] 7

The size of each community

``` r
t <- as.data.frame(table(memb))
colnames(t) <- c('ID' , 'Size')
t
```

    ##   ID Size
    ## 1  1    8
    ## 2  2    5
    ## 3  3    4
    ## 4  4    4
    ## 5  5    5
    ## 6  6    3
    ## 7  7    3

The modularity

``` r
gc$modularity
```

    ##  [1] -0.04584775 -0.01773356  0.01081315  0.03849481  0.06617647
    ##  [6]  0.09472318  0.12326990  0.14965398  0.17560554  0.20285467
    ## [11]  0.23096886  0.25865052  0.28633218  0.31358131  0.34083045
    ## [16]  0.36894464  0.39576125  0.41479239  0.44247405  0.46712803
    ## [21]  0.49134948  0.50778547  0.52681661  0.54974048  0.57050173
    ## [26]  0.57742215  0.56098616  0.53416955  0.45804498  0.30449827

### walktrap community

``` r
gc1 <- walktrap.community(g)
gc1
```

    ## IGRAPH clustering walktrap, groups: 7, mod: 0.51
    ## + groups:
    ##   $`1`
    ##   [1] "owen"    "yang"    "altman"  "colin"   "preston"
    ##   
    ##   $`2`
    ##    [1] "lexi"         "sloan"        "torres"       "derek"       
    ##    [5] "karev"        "o'malley"     "arizona"      "kepner"      
    ##    [9] "addison"      "nancy"        "olivia"       "mrs. seabury"
    ##   [13] "avery"       
    ##   
    ##   $`3`
    ##   + ... omitted several groups/vertices

``` r
memb1 <- membership(gc1)
memb1
```

    ##         lexi         owen        sloan       torres        derek 
    ##            2            1            2            2            2 
    ##        karev     o'malley         yang         grey        chief 
    ##            2            2            1            6            3 
    ##   ellis grey   susan grey       bailey        izzie       altman 
    ##            3            5            7            4            1 
    ##      arizona        colin      preston       kepner      addison 
    ##            2            1            1            2            2 
    ##        nancy       olivia mrs. seabury        adele  thatch grey 
    ##            2            2            2            3            5 
    ##       tucker         hank        denny         finn        steve 
    ##            7            4            4            6            6 
    ##          ben        avery 
    ##            7            2

Plot the graph with unique color for each community accordingly

``` r
plot(g, vertex.size=7, #vertex.label=NA,
     vertex.color=memb1, asp=FALSE)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-14-1.png)

There is 7 communities

``` r
length(unique(memb1))
```

    ## [1] 7

The size of each community

``` r
t1 <- as.data.frame(table(memb1))
colnames(t1) <- c('ID' , 'Size')
t1
```

    ##   ID Size
    ## 1  1    5
    ## 2  2   13
    ## 3  3    3
    ## 4  4    3
    ## 5  5    2
    ## 6  6    3
    ## 7  7    3

The modularity

``` r
gc1$modularity
```

    ##  [1]  0.00000000 -0.01730106  0.01081313  0.03676469  0.06487888
    ##  [6]  0.09256054  0.12024221  0.14749134  0.17387544  0.19982699
    ## [11]  0.22837371  0.25692043  0.28460205  0.31185120  0.33910033
    ## [16]  0.36678201  0.39489621  0.42171276  0.44939446  0.45544982
    ## [21]  0.48226649  0.47923881  0.49567476  0.48875433  0.49394464
    ## [26]  0.51470590  0.48269898  0.50562286  0.45804498  0.30449831
    ## [31]  0.00000000  0.00000000

2
-

``` r
library(igraph)
library(twitteR)
library(tm)
```

    ## Warning: package 'tm' was built under R version 3.4.3

    ## Loading required package: NLP

``` r
library(httr)
```

    ## 
    ## Attaching package: 'httr'

    ## The following object is masked from 'package:NLP':
    ## 
    ##     content

Set twitter keys

``` r
consumer_key <- "Y0NniYiJCKL7qqbrreh6p9P4F"
consumer_secret <- "yZlDnWZEB20LrrdVPXwTIY6skmuj9N3iljcO3cGvugrwSJlhYu"
access_token <- "945458627669786624-LsmOz4oCzo0lT6UHwSkPv6hT0inP47x"
access_secret <- "2jwReQYUJHFTDAjQhs0y3Yt1v4MAO2zyAddddBXMVJF4N"
```

Set up the OAuth credentials for a twitteR session

``` r
sig <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
```

    ## [1] "Using direct authentication"

    ## Warning in strptime(x, fmt, tz = "GMT"): unknown timezone 'zone/tz/2017c.
    ## 1.0/zoneinfo/Asia/Jerusalem'

2.A
---

### Search tweets on bitcoin on english since 01/12/2017

``` r
tweets <- searchTwitter("#bitcoin", n=200 , lang = "en" , since = "2017-12-01")
```

convert the tweets to dataFrame

``` r
tweetsDf <- twListToDF(tweets)
summary(tweetsDf)
```

    ##      text           favorited       favoriteCount  replyToSN        
    ##  Length:200         Mode :logical   Min.   :0.0   Length:200        
    ##  Class :character   FALSE:200       1st Qu.:0.0   Class :character  
    ##  Mode  :character                   Median :0.0   Mode  :character  
    ##                                     Mean   :0.1                     
    ##                                     3rd Qu.:0.0                     
    ##                                     Max.   :6.0                     
    ##     created                    truncated        replyToSID       
    ##  Min.   :2017-12-26 01:48:55   Mode :logical   Length:200        
    ##  1st Qu.:2017-12-26 01:50:23   FALSE:165       Class :character  
    ##  Median :2017-12-26 01:51:58   TRUE :35        Mode  :character  
    ##  Mean   :2017-12-26 01:52:24                                     
    ##  3rd Qu.:2017-12-26 01:54:37                                     
    ##  Max.   :2017-12-26 01:56:41                                     
    ##       id             replyToUID        statusSource      
    ##  Length:200         Length:200         Length:200        
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##   screenName         retweetCount     isRetweet       retweeted      
    ##  Length:200         Min.   :   0.00   Mode :logical   Mode :logical  
    ##  Class :character   1st Qu.:   0.00   FALSE:84        FALSE:200      
    ##  Mode  :character   Median :   7.00   TRUE :116                      
    ##                     Mean   :  71.28                                  
    ##                     3rd Qu.:  95.00                                  
    ##                     Max.   :1955.00                                  
    ##  longitude      latitude      
    ##  Mode:logical   Mode:logical  
    ##  NA's:200       NA's:200      
    ##                               
    ##                               
    ##                               
    ## 

Get users that publish the tweets

``` r
users <- twListToDF(lookupUsers(tweetsDf$screenName))
```

2.B
---

Our vertexes are user's names and the edge between two users means that their account created on the same month

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:igraph':
    ## 
    ##     %--%

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
users1Edge <- c()
users2Edge <- c()

for(i in 1:nrow(users))
{
  for(j in 1:nrow(users))
  {
    user1 <- users[i,]
    user2 <- users[j,]
    user1Month <- month(as.POSIXlt(user1$created, format="%d/%m/%Y"))
    user2Month <- month(as.POSIXlt(user2$created, format="%d/%m/%Y"))
    
    if((user1$screenName != user2$screenName) && user1Month == user2Month)
    {
      users1Edge <- c(users1Edge , user1$screenName)
      users2Edge <- c(users2Edge , user2$screenName)
    }
  }
}
```

2.C
---

Create file from users1Edge and users2Edge and read the file to graph

``` r
res <- cbind(from = users1Edge , to = users2Edge)
write.csv(res , file = "tweets.csv" , row.names = FALSE)
ga.data <- read.csv('tweets.csv', header = T)
g <- graph.data.frame(ga.data,directed = F)
plot(g, vertex.size=7, vertex.label=NA, asp=FALSE)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-25-1.png)

2.D
---

### 1.A

### i. By Betweenness

``` r
between <- betweenness(g)
between[which.max(between)]
```

    ## BitcoinWrld 
    ##           0

### ii. By closeness

``` r
close <- closeness(g)
close[which.max(close)]
```

    ## CryptNotBlood 
    ##  4.316671e-05

### iii. By Eigenvector

``` r
eig <- eigen_centrality(g)
eig$vector[which.max(eig$vector)]
```

    ## NewsJunkieJon 
    ##             1

### 1.B

### Girvan-Newman community detection

``` r
gc <- edge.betweenness.community(g)
gc
```

    ## IGRAPH clustering edge betweenness, groups: 12, mod: 0.88
    ## + groups:
    ##   $`1`
    ##    [1] "BitcoinWrld"     "NeilOKeefe"      "djprincealby"   
    ##    [4] "fakent_"         "TugBoatTrader"   "TrevorJ9257"    
    ##    [7] "BuddySav"        "Noemie__Taylor"  "eztechwin"      
    ##   [10] "Maeva__Davis"    "Oceane_Rodrigue" "Samuel___Smith" 
    ##   [13] "EntrepreneurDon" "John_____Brown"  "Katie____Taylor"
    ##   [16] "Perezreed5"      "ICOcheck"        "Karmicstar"     
    ##   
    ##   $`2`
    ##    [1] "BrutallyDoke"    "bitcoinfirehose" "socialirnews"   
    ##   + ... omitted several groups/vertices

``` r
memb <- membership(gc)
memb
```

    ##     BitcoinWrld    BrutallyDoke       paleodead        sydni519 
    ##               1               2               3               4 
    ## bitcoinfirehose   CryptNotBlood    socialprnews    socialirnews 
    ##               2               5               4               2 
    ## RandallGoulding       ssn3media socialstartnews       ssn1tweet 
    ##               4               6               3               7 
    ##   ssn4marketing       GetOvarIt 1JustinMcCollum     hotload2000 
    ##               8               2               2               2 
    ## MzCh11KiETHaNg9      KovshBeats       roni20731    Kayy_Kayy_44 
    ##               6               6               9              10 
    ##         ondhro1      UndersHead  TheSideHusband        goldseek 
    ##              11              12              11               9 
    ##        btcmrkts    realSatoshiN   egaconsulting   findingreview 
    ##               9              10               5               5 
    ##       davidar12         g0t3nk5      NeilOKeefe      37angelsny 
    ##               9               3               1               8 
    ##    King_Tuesday  hexagram_power         xbtnews   leola_joergen 
    ##               6               5               6               9 
    ##      KalEl_1987     Crystal0182      cryptomiao   MdKayumUddin2 
    ##               2               5               5               3 
    ##   BASSLINE_BOP2  CryptoNewswire    midousujikun      jekiedugn1 
    ##              12               3               3               6 
    ##      btc_update        matslats   TheBlockchain     OmegaLuther 
    ##               2              12              11               9 
    ##   VeryVeriViral eStream_Studios  eBargainsToday    bitcointonic 
    ##               7               5               9              11 
    ##     TheBitForum        NazzyN21   MikeAlden2012        DMVLife1 
    ##               8               8               2               8 
    ##   mempool_stats   Buddhamangler           hitjo    djprincealby 
    ##              10              12               4               1 
    ##         fakent_   TugBoatTrader     CryptoW0rld      CA_Minho25 
    ##               1               1               4               5 
    ##   shushmashri21      markuspdee      JacekSalaj     tell_taylor 
    ##               5               9              10               8 
    ##     Priya_upala      MattLeft99    theonevortex  politicalHEDGE 
    ##               5              12               6               9 
    ##     CryptoKid77   blockchainbot  BitcoinCashApp michael55038689 
    ##              10              10              10               5 
    ##      McclamNeva    iBroughtFood      buzzkil420    JulioSilvaJr 
    ##               6               5               5              12 
    ##    JessVerSteeg     TrevorJ9257   India_Bitcoin        ivivekkm 
    ##              11               1               2               9 
    ##    CryptoSykora      jasongaved   NewsJunkieJon        BuddySav 
    ##               3               6              12               1 
    ##       ccrypto27          j4f288     bitcoinID69       samdaurua 
    ##               5              11               9              12 
    ##     handsomegui    Bakerlewis85  galdinus_gomes         rh13478 
    ##               5               2               7              10 
    ##  Noemie__Taylor  Digitally_Your FanChicagoBears        CryptoFP 
    ##               1               4               4               5 
    ##      Alice44567       EmGeetwit       eztechwin    E_Pluribus_1 
    ##               3               9               1              12 
    ##    Maeva__Davis     Allenlee585 Oceane_Rodrigue      AdamSilva3 
    ##               1               2               1               5 
    ##      cryptofuse   RobertaCushey  Samuel___Smith      SaeedBaygi 
    ##              10               4               1               4 
    ##     RosyDecosta EntrepreneurDon  John_____Brown        wharveyc 
    ##               3               1               1               6 
    ## Katie____Taylor        al_dia17    AlicePaul_CA    TheMedlockss 
    ##               1               8               3               9 
    ##      Perezreed5   rgkrobertwill     JohannNelZI   DavidMackey77 
    ##               1              12               2               4 
    ##       NYCPunter         veht290          JWake5 pWielandTrading 
    ##               2               5               7              12 
    ##   MotherOfMoney    kevinchenNYC         mabling        ICOcheck 
    ##               3               4              12               1 
    ##    city_bitcoin      Karmicstar        mapiasal Thuyngu22268437 
    ##               8               1              10               5 
    ##         all3els        fibrolit         StatXbt    Dry_Observer 
    ##              10               2               6               2 
    ##     RightRiseUK       TelThomas  JenniferAlsop7    JacobGrant31 
    ##               9              12              12              12 
    ## BellaRoberts181    StackmyBCHup  Carlos05247004     fxolivia_sh 
    ##              12               4               5              10 
    ##     patrickford    frank_darsey      SamKelly63    Bitcoin_Post 
    ##              12               8              12              12 
    ##        rootdude  Mark_Lewis_NYC  socmediaimpact     getitoutnow 
    ##              12               4              11               8 
    ## socialnewstweet       socmrktng    nowsocialinc  socialstartnow 
    ##              11               5               9               2 
    ##        bob32423  StevenDavidso3   Toyota__Yaris 
    ##               9              12               8

Plot the graph with unique color for each community accordingly

``` r
plot(g, vertex.size=7, #vertex.label=NA,
     vertex.color=memb, asp=FALSE)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-31-1.png)

There is 12 communities

``` r
length(unique(memb))
```

    ## [1] 12

The size of each community

``` r
t <- as.data.frame(table(memb))
colnames(t) <- c('ID' , 'Size')
t
```

    ##    ID Size
    ## 1   1   18
    ## 2   2   17
    ## 3   3   11
    ## 4   4   13
    ## 5   5   21
    ## 6   6   11
    ## 7   7    4
    ## 8   8   11
    ## 9   9   16
    ## 10 10   12
    ## 11 11    8
    ## 12 12   21

The modularity

``` r
gc$modularity
```

    ##   [1] -6.735300e-03 -5.963960e-03 -5.147119e-03 -4.311713e-03 -3.604075e-03
    ##   [6] -2.188800e-03 -6.588603e-05  1.567796e-03  4.398348e-03  5.105986e-03
    ##  [11]  5.906810e-03  9.444999e-03  1.369083e-02  1.864429e-02  2.430539e-02
    ##  [16]  2.584807e-02  2.660813e-02  2.812824e-02  2.893744e-02  3.035271e-02
    ##  [21]  3.197111e-02  3.442163e-02  3.516967e-02  4.153841e-02  4.314006e-02
    ##  [26]  5.021644e-02  5.233935e-02  5.461952e-02  5.702200e-02  6.006222e-02
    ##  [31]  6.784623e-02  6.934232e-02  7.165634e-02  7.247318e-02  7.574054e-02
    ##  [36]  7.798467e-02  7.880151e-02  8.179369e-02  8.553390e-02  8.716758e-02
    ##  [41]  9.037088e-02  9.485914e-02  9.886327e-02  1.012909e-01  1.041214e-01
    ##  [46]  1.093577e-01  1.131580e-01  1.191423e-01  1.276340e-01  1.343664e-01
    ##  [51]  1.374517e-01  1.399023e-01  1.434404e-01  1.482454e-01  1.538512e-01
    ##  [56]  1.613316e-01  1.645684e-01  1.654184e-01  1.699787e-01  1.742245e-01
    ##  [61]  1.834238e-01  1.872805e-01  1.889513e-01  1.939048e-01  2.021333e-01
    ##  [66]  2.120402e-01  2.136739e-01  2.169412e-01  2.215693e-01  2.268897e-01
    ##  [71]  2.358662e-01  2.383724e-01  2.489870e-01  2.546481e-01  2.610168e-01
    ##  [76]  2.634673e-01  2.705437e-01  2.745897e-01  2.794449e-01  2.851092e-01
    ##  [81]  2.905086e-01  2.937760e-01  3.050982e-01  3.128822e-01  3.169664e-01
    ##  [86]  3.234400e-01  3.296107e-01  3.381024e-01  3.473017e-01  3.537083e-01
    ##  [91]  3.634328e-01  3.739054e-01  3.851261e-01  3.923335e-01  4.043633e-01
    ##  [96]  4.116461e-01  4.165471e-01  4.226276e-01  4.283455e-01  4.348802e-01
    ## [101]  4.382218e-01  4.451639e-01  4.550708e-01  4.567708e-01  4.644842e-01
    ## [106]  4.686612e-01  4.813987e-01  4.882392e-01  4.923234e-01  4.964076e-01
    ## [111]  5.013086e-01  5.147537e-01  5.204716e-01  5.310862e-01  5.424084e-01
    ## [116]  5.500090e-01  5.584937e-01  5.633947e-01  5.754246e-01  5.811425e-01
    ## [121]  5.884940e-01  6.004627e-01  6.069975e-01  6.162535e-01  6.289910e-01
    ## [126]  6.424361e-01  6.505281e-01  6.605555e-01  6.713543e-01  6.763667e-01
    ## [131]  6.905195e-01  6.963673e-01  7.052685e-01  7.168386e-01  7.233733e-01
    ## [136]  7.307249e-01  7.390855e-01  7.482062e-01  7.580869e-01  7.662553e-01
    ## [141]  7.688052e-01  7.761568e-01  7.843252e-01  7.923334e-01  8.029742e-01
    ## [146]  8.117833e-01  8.259360e-01  8.373369e-01  8.469467e-01  8.551152e-01
    ## [151]  8.672760e-01  8.799928e-01

### walktrap community

``` r
gc1 <- walktrap.community(g)
gc1
```

    ## IGRAPH clustering walktrap, groups: 12, mod: 0.88
    ## + groups:
    ##   $`1`
    ##   [1] "ssn1tweet"      "VeryVeriViral"  "galdinus_gomes" "JWake5"        
    ##   
    ##   $`2`
    ##   [1] "ondhro1"         "TheSideHusband"  "TheBlockchain"  
    ##   [4] "bitcointonic"    "JessVerSteeg"    "j4f288"         
    ##   [7] "socmediaimpact"  "socialnewstweet"
    ##   
    ##   $`3`
    ##    [1] "ssn4marketing" "37angelsny"    "TheBitForum"   "NazzyN21"     
    ##   + ... omitted several groups/vertices

``` r
memb1 <- membership(gc1)
memb1
```

    ##     BitcoinWrld    BrutallyDoke       paleodead        sydni519 
    ##              10               9               4               7 
    ## bitcoinfirehose   CryptNotBlood    socialprnews    socialirnews 
    ##               9              12               7               9 
    ## RandallGoulding       ssn3media socialstartnews       ssn1tweet 
    ##               7               5               4               1 
    ##   ssn4marketing       GetOvarIt 1JustinMcCollum     hotload2000 
    ##               3               9               9               9 
    ## MzCh11KiETHaNg9      KovshBeats       roni20731    Kayy_Kayy_44 
    ##               5               5               8               6 
    ##         ondhro1      UndersHead  TheSideHusband        goldseek 
    ##               2              11               2               8 
    ##        btcmrkts    realSatoshiN   egaconsulting   findingreview 
    ##               8               6              12              12 
    ##       davidar12         g0t3nk5      NeilOKeefe      37angelsny 
    ##               8               4              10               3 
    ##    King_Tuesday  hexagram_power         xbtnews   leola_joergen 
    ##               5              12               5               8 
    ##      KalEl_1987     Crystal0182      cryptomiao   MdKayumUddin2 
    ##               9              12              12               4 
    ##   BASSLINE_BOP2  CryptoNewswire    midousujikun      jekiedugn1 
    ##              11               4               4               5 
    ##      btc_update        matslats   TheBlockchain     OmegaLuther 
    ##               9              11               2               8 
    ##   VeryVeriViral eStream_Studios  eBargainsToday    bitcointonic 
    ##               1              12               8               2 
    ##     TheBitForum        NazzyN21   MikeAlden2012        DMVLife1 
    ##               3               3               9               3 
    ##   mempool_stats   Buddhamangler           hitjo    djprincealby 
    ##               6              11               7              10 
    ##         fakent_   TugBoatTrader     CryptoW0rld      CA_Minho25 
    ##              10              10               7              12 
    ##   shushmashri21      markuspdee      JacekSalaj     tell_taylor 
    ##              12               8               6               3 
    ##     Priya_upala      MattLeft99    theonevortex  politicalHEDGE 
    ##              12              11               5               8 
    ##     CryptoKid77   blockchainbot  BitcoinCashApp michael55038689 
    ##               6               6               6              12 
    ##      McclamNeva    iBroughtFood      buzzkil420    JulioSilvaJr 
    ##               5              12              12              11 
    ##    JessVerSteeg     TrevorJ9257   India_Bitcoin        ivivekkm 
    ##               2              10               9               8 
    ##    CryptoSykora      jasongaved   NewsJunkieJon        BuddySav 
    ##               4               5              11              10 
    ##       ccrypto27          j4f288     bitcoinID69       samdaurua 
    ##              12               2               8              11 
    ##     handsomegui    Bakerlewis85  galdinus_gomes         rh13478 
    ##              12               9               1               6 
    ##  Noemie__Taylor  Digitally_Your FanChicagoBears        CryptoFP 
    ##              10               7               7              12 
    ##      Alice44567       EmGeetwit       eztechwin    E_Pluribus_1 
    ##               4               8              10              11 
    ##    Maeva__Davis     Allenlee585 Oceane_Rodrigue      AdamSilva3 
    ##              10               9              10              12 
    ##      cryptofuse   RobertaCushey  Samuel___Smith      SaeedBaygi 
    ##               6               7              10               7 
    ##     RosyDecosta EntrepreneurDon  John_____Brown        wharveyc 
    ##               4              10              10               5 
    ## Katie____Taylor        al_dia17    AlicePaul_CA    TheMedlockss 
    ##              10               3               4               8 
    ##      Perezreed5   rgkrobertwill     JohannNelZI   DavidMackey77 
    ##              10              11               9               7 
    ##       NYCPunter         veht290          JWake5 pWielandTrading 
    ##               9              12               1              11 
    ##   MotherOfMoney    kevinchenNYC         mabling        ICOcheck 
    ##               4               7              11              10 
    ##    city_bitcoin      Karmicstar        mapiasal Thuyngu22268437 
    ##               3              10               6              12 
    ##         all3els        fibrolit         StatXbt    Dry_Observer 
    ##               6               9               5               9 
    ##     RightRiseUK       TelThomas  JenniferAlsop7    JacobGrant31 
    ##               8              11              11              11 
    ## BellaRoberts181    StackmyBCHup  Carlos05247004     fxolivia_sh 
    ##              11               7              12               6 
    ##     patrickford    frank_darsey      SamKelly63    Bitcoin_Post 
    ##              11               3              11              11 
    ##        rootdude  Mark_Lewis_NYC  socmediaimpact     getitoutnow 
    ##              11               7               2               3 
    ## socialnewstweet       socmrktng    nowsocialinc  socialstartnow 
    ##               2              12               8               9 
    ##        bob32423  StevenDavidso3   Toyota__Yaris 
    ##               8              11               3

Plot the graph with unique color for each community accordingly

``` r
plot(g, vertex.size=7, #vertex.label=NA,
     vertex.color=memb1, asp=FALSE)
```

![](hw3_files/figure-markdown_github/unnamed-chunk-37-1.png)

There is 12 communities

``` r
length(unique(memb1))
```

    ## [1] 12

The size of each community

``` r
t1 <- as.data.frame(table(memb1))
colnames(t1) <- c('ID' , 'Size')
t1
```

    ##    ID Size
    ## 1   1    4
    ## 2   2    8
    ## 3   3   11
    ## 4   4   11
    ## 5   5   11
    ## 6   6   12
    ## 7   7   13
    ## 8   8   16
    ## 9   9   17
    ## 10 10   18
    ## 11 11   21
    ## 12 12   21

The modularity

``` r
gc1$modularity
```

    ##   [1]  0.0000000000 -0.0060276650 -0.0053200270 -0.0039047510 -0.0031971128
    ##   [6] -0.0017818369  0.0003410764  0.0017563524  0.0038792663  0.0123709198
    ##  [11]  0.0152014717  0.0159091093  0.0166167468  0.0173243843  0.0244007632
    ##  [16]  0.0258160383  0.0272313133  0.0321847796  0.0371382460  0.0378458872
    ##  [21]  0.0385535248  0.0392611623  0.0399688035  0.0406764448  0.0413840786
    ##  [26]  0.0427993573  0.0442146286  0.0456298999  0.0470451787  0.0491680913
    ##  [31]  0.0512910075  0.0541215576  0.0569521040  0.0739354044  0.0909187198
    ##  [36]  0.1064867526  0.1220547855  0.1496526599  0.1772505343  0.2338615507
    ##  [41]  0.2904725671  0.2912206054  0.2919686437  0.2934647501  0.2949608564
    ##  [46]  0.2957088947  0.2964569330  0.2972049713  0.3016932309  0.3061814904
    ##  [51]  0.3076775670  0.3263786435  0.3286227882  0.3293707967  0.3592925072
    ##  [56]  0.3607886136  0.3630327284  0.4049231708  0.4056832194  0.4064432681
    ##  [61]  0.4094835222  0.4102435708  0.4110035896  0.4125237167  0.4148039222
    ##  [66]  0.4178440869  0.4208843112  0.4246846139  0.4284848869  0.4330452085
    ##  [71]  0.4376055598  0.4429259300  0.4550868273  0.5082907081  0.5090621114
    ##  [76]  0.5098334551  0.5106047988  0.5121474266  0.5129187703  0.5175467730
    ##  [81]  0.5206320882  0.5360589027  0.5430010557  0.5507143736  0.5591991544
    ##  [86]  0.5684551597  0.5692265034  0.5707692504  0.6008514762  0.6016523242
    ##  [91]  0.6024530530  0.6032539010  0.6048555970  0.6096605659  0.6176688075
    ##  [96]  0.6184696555  0.6192703843  0.6208720803  0.6256770492  0.6537058949
    ## [101]  0.6633157730  0.6641249657  0.6657433510  0.6665526032  0.6673617363
    ## [106]  0.6681709886  0.6705985665  0.6722169518  0.6786904931  0.6811181307
    ## [111]  0.6908285022  0.7167227864  0.7175396085  0.7183565497  0.7191733718
    ## [116]  0.7199901938  0.7208071351  0.7216239572  0.7232576609  0.7265250087
    ## [121]  0.7273418307  0.7306091785  0.7330597639  0.7338765860  0.7355102897
    ## [126]  0.7371439338  0.7387775779  0.7420449257  0.7453122139  0.7485795021
    ## [131]  0.7608321309  0.7730847597  0.7853373885  0.7861542702  0.7869711518
    ## [136]  0.7877879739  0.7894216776  0.7910553813  0.7926889658  0.8122931719
    ## [141]  0.8318973780  0.8515015841  0.8523370624  0.8531724811  0.8565140367
    ## [146]  0.8573494554  0.8640326858  0.8690451384  0.8748930097  0.8757429123
    ## [151]  0.8765929341  0.8799927831  0.0000000000  0.0000000000  0.0000000000
    ## [156]  0.0000000000  0.0000000000  0.0000000000  0.0000000000  0.0000000000
    ## [161]  0.0000000000  0.0000000000  0.0000000000
