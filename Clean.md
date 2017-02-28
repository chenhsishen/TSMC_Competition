##資料清理
- 前面已經提過SVID資料的形式，這裡要進一步說明我們如何去有效率地去整理資料，合併pattern相同的欄位，並利用PCA完成降維。
- 選擇用自訂函數的方式，建立一個Reduction(x)函數，去完成所有我需要的工作。
---

## Data Input
- 讀檔是很耗時的，尤其是這種特別肥大的檔案。在這邊我們使用```data.table```套件中的```fread()```函數，用有效率的方式讀取檔案。
- 實際讀取的結果，一個500MB左右的.csv檔(幾萬rows乘上幾千columns)，大約需要花上30秒左右的時間。
- 值得注意的是，```fread()```讀進來的還不是完整的DataFrame，請記得用```as.Data.frame```轉換。
```
library(data.table)
setwd('C:/Users/Desktop/Competition/FDC Data')
file_name <- list.files('.') #把所有的檔名存在一個list，方便之後直接用lapply

Reduction <- function(x){
  result <- tryCatch({       #設置除錯
  raw <- as.data.frame(fread(x))    # 讀檔並轉為資料框
  B_raw <- raw[,-c(1:6)]        #去除不重要的欄位
  },error=function(y) {
  y <- NULL})
return(result)
}
```

## Variables Merge
- 透過觀察，我們發現在同一個Stage下，同一個SVID的編號中，不同的step所表現測量的值，不論是數值或是分配都不會相差太多。
- 因此，把這些特徵相近的欄位進行平均，平均為一個新的欄位，我們認為是可行的做法
- 沿著欄位名稱的規律去索引，就能得出我們需要的特定欄位的平均；也能確保他能適應不同維度的資料。
```
library(data.table)
library(plyr)
setwd('C:/Users/Desktop/Competition/FDC Data')
file_name <- list.files('.') #把所有的檔名存在一個list，方便之後直接用lapply

Reduction <- function(x){
  result <- tryCatch({setwd('C:/Users/Desktop/Competition/FDC Data')       #設置除錯
  raw <- as.data.frame(fread(x))    # 讀檔並轉為資料框
  B_raw <- raw[,-c(1:6)]        #去除不重要的欄位
  
  L <- list()
  for(i in 1:as.integer(gsub("SVID","",strsplit(colnames(B_raw)[ncol(B_raw),"_"])[[1]][2])){#把欄位名稱中的SVID的編號取出來index
    Test <- B_raw[,grep(paste("_SVID",i,"_",sep=""),colnames(B_raw))] #對應資料中包含此次迭代編號的欄位
    Test <- apply(Test,2,function(x) as.numeric(x)) #確定每個值都轉換成數值
    Test_new <- rowMeans(Test,na.rm=T) #將每一筆觀測值的不同欄位的數值，進行平均；由於各Stage的不同，某些數值可能包含空值，所以要將其去除
    L[[i]] <- Test_new
  }
  L <- t(ldply(L)) #把list中的各元素合併回Matrix
  L[is.nan(L)] <- 0 #把nan轉為0，這個做法有爭議，但我想不到更好的方法
  L <- as.data.frame(L)
  for(i in 1:ncol(L){
    colnames(L) <- paste(substring(colnames(B_raw)[1],1,6),'SVID',i,"_",sep="")
  }
  },error=function(y) {
  y <- NULL})
return(result)
}
```
- 到這裡為止，我們就把肥大的欄位們，取出一個具有代表性的平均值囉！
