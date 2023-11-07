# anna.valk.github.io

> data <- read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")
> browser = read.csv("C:\\Users\\deeni\\Downloads\\web-browsers.csv")

> head (data)

> mean(brower$spend)

> data <- read.table(text = "id;anychildren;broadband;hispanic;race;region;spend
+ + 1;0;1;0;white;MW;424
+ + 2;1;1;0;white;MW;2335
+ + 3;1;1;0;white;MW;279
+ + 4;0;1;0;white;MW;829
+ + 5;0;1;0;white;S;221
+ + 6;0;1;0;white;MW;2305", header = TRUE, sep = ";")
> print(data)

> mean(data$spend); var(data$spend)/1e4; sqrt(var(data$spend)/1e4)
[1] 1065.5
[1] 98.94415
[1] 9.947067

 rm (b,B)
> B <- 1000
> mub <- c()
> for (b in 1:1000){
+     samp_b <- sample.int(nrow(data), replace=TRUE)
+     mub <- c(mub, mean(data$spend[samp_b]))
+ }
> sd(mub)
[1] 366.7413