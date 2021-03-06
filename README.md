# TSMC_Competition

- 台積電的半導體大數據分析競賽，已經舉辦了三屆；在下本人小弟我，有幸參加第三屆，雖然在複賽陣亡，沒有進入到決賽，但這個資料專案仍是學習資料科學的路上，一個滿有趣的經驗。接下來會分成三個部分，介紹這個專案。
- 先講結論，這個大數據專案，除了數據很大之外，流程其實不太智慧；但對於一個對資料科學工作有憧憬的學生而言，該練的粗活都有練到，已經綽綽有餘囉！也算是善盡了啟蒙的工作。
- 沒有把過程中所有的程式碼都留下來，僅擷取比較好說明以及與比賽結果有直接相關的。

---

## [1. 環境與資料型態](https://github.com/chenhsishen/TSMC_Competition/blob/master/Data_Introduction.md)
- 基本上，台積電和AWS合作，提供給每隊一個獨立的EC2上的一台機器，擁有10個CPU還有超猛的記憶體(好像200多G吧，忘了)，透過遠端連線可以進入；但有趣的事情是，這台機器是無法連網的，所以操作上會有點不方便，資料和程式碼也只能透過「截圖」這麼原始的方式帶到自己的環境中，真的是，很重視安全。
- 到了複賽，除了初賽給的300組晶圓的「製程資料」以外，又加上了「SVID參數資料」，總共加起來應該有個快120GB的惱人資料，而且還要在單機的環境中處理...不太確定本組截圖有多完整，反正會盡量說明清楚就對了。
- 喔對了，整個資料專案都是用R做的，真是土法煉鋼；即使數據很大，這種不怎麼聰明的作法，不太符合大數據的精神。

## [2. 資料清理](https://github.com/chenhsishen/TSMC_Competition/blob/master/Dimension_Reduction.md)
- 其實就是整理資料，這邊會著重在複賽新增的「SVID參數資料」，300個資料集(.csv)總共115GB，指的是300個不同製程階段，所偵測出的一些參數(說真的，我也不是很懂，難怪沒進決賽，幫QQ)
- 每一個資料集都大約是幾萬個row乘上幾千個column，隨著不同階段的製程或有所不同，但大抵而言是這樣。但那幾千個column並不是沒有意義的，是「不同儀器在不同步驟偵測到不同的參數」，所以可以把它們拿來做一些事情。
- 115GB的資料，從按下執行後，最後大約要花一個多小時跑完。包括資料輸入、合併pattern相似的column、降維，再合併為一個可以建模的檔案。
- 感謝摯友，帥哥李唐，在這個過程中的教導。

## [3. 建模](https://github.com/chenhsishen/TSMC_Competition/blob/master/Modeliing.md)
- 建模的目的有兩個：
    - 1. 找出影響良率的Root Cause
    - 2. 預測良率
- 由於當時的我(不過才三個月前)，還不是真的那麼熟機器學習或是深度學習的東西(講得現在很熟一樣...)，所以也沒有選到那麼好的工具。
    - 1. 用Lasso Regression來找個變數的影響
    - 2. 用在裁判老師的出的書中找到的類神經網絡，並使用以這為基礎改良的演算法，去預測良率。
- 最後的結果，找原因找得一蹋糊塗，預測良率經過Cross Validation，MAPE是3.74%，ㄏㄏ。
 

