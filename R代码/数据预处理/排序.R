### 排序
# R中涉及排序的基本函数有order、sort和rank三个。
# order函数返回的是排序数据所在向量中的索引，
# rank函数返回该值处于第几位（在统计学上称为秩），
# sort函数则返回的是按次排好的数据。
(x <- c(19,84,64,2))
order(x)
rank(x)
sort(x)
# 下面再看一个例子，来更加深入了解order的用法
d <- data.frame(x=c(19,84,64,2,2),
                y=c(20,13,5,40,21))
d
# 按x的升序排序，如果x一样，则按y的升序排序
d[order(d$x,d$y),]
# 按x的升序排序，如果x一样，则按y的降序排序
d[order(d$x,-d$y),]
# 按y的升序排序，如果y一样，则按x的升序排序
d[order(d$y,d$x),]