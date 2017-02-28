##建模
- 雖然在介紹的時候，有說我用了```Lasso Regression```，但由於這邊做出來的結果實在是有點爛，程式碼也沒有「截圖」得很完整，就略過這部分了
- 還是提醒一下，如果是要做Lasso Regression，記得要用```model.matrix```把類別變數全都轉換為```dummy variables```，才吃得進去
- 再來是神經網絡，Kohonen，R裡面有個library就叫做kohonen，安裝後直接```library(kohonen)```就可以使用；建模前的整理

---

###Data Preprocess
- 由於比賽中有特別提到不同「Chamber」對於良率的影響，所以同一個Stage中，屬於不同Chamber的Data，分開來處理，這邊我是寫了一個```Sep()```函數，來幫我完成這件事情
```R
library(data.table)
library(plyr)
library(kohonen)
setwd('C:/Users/Desktop/Competition/FDC Data_New')
file_name <- list.files('.')

##################Sep###################
Sep <- function(x){
  a <- as.data.frame(fread(x))  #寫入我們將維過後的檔案
  a <- split(a,a$Chamber.ID)  #將屬於不同Chamber的資料分離出來，存入一個list
  a <- lapply(a, function(x){
    for(i in 1:ncol(x)){
      colnames(x)[i] <- paste("Stage",x[,6][1],"_",x[,3][1],"_",colnames(x)[i],sep="")  #用迴圈把分離出來的資料欄位，標上對應的Chamber.ID
    }
    return(x)
  })
  return(a) #回傳包含不同Chamber.ID的list
}

```
- 為了使用這些監督式的學習方法，我們必須要把良率的資訊也合併回資料集，這裡我用```Mer()```函數，用主Key```Wafer.ID```來合併良率資料。
```R
Mer <- function(x){
  k <- Sep(x)   #就是上面寫的Sep()函數
  for(i in 1:length(k)){
    Yield <- cbind(Yield,k[[i]][match(Yield[,1],k[[i]][,5]),])  #利用```match()```進行比對，這是一個超級好用的函數，可以幫你比對相符合的欄位，如果沒東西回傳還會幫你傳空值
    }
    Yield[is.na(Yield)] <- 0  #跟資料清理那邊一樣的理由
    return(Yield)
}

final_data <- lapply(file_name,Mer)  ###執行下去就得到整理好的300個資料集，並存放在一個list裡面

d <- final_data[[1]]
for(i in 1:300){
 d <- cbind(d,final_data[[i+1]]  ###最後合併所有的元素
}
```
- 但大家有沒有發現哪裡怪怪的？每一個元素中都包含了良率的欄位，合併起來就有300個良率的欄位啊！！！！我沒事要那麼多良率的欄位幹嘛！！！
- 所以必須要再用一小段程式，把這個小小錯誤修正
```R
num <- sapply(d,is.numeric) #先把數字的部分留下來，因為需要的文字資訊(包括階段、Chamber等，我們都做在欄位名稱上了)
b <- d[,num]
c <- b[,-grep("Yield",colnames(b))] #先從留下數字的資料中，把Yeild的欄位全都拿掉
y <- d[,grep("Yield",colnames(b))][2]  #再從原始資料中，隨便取一個Yield欄位出來
raw <- rbind(c,y)  #接著把他們合併起來，就大功告成啦！
```

###Modeling
- 終於把資料處理好後，就可以來切分訓練資料、測試資料的routine
```R
