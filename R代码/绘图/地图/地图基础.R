library(maps)
map("world", fill = TRUE, col = rainbow(200),
    ylim = c(-60, 90), mar = c(0, 0, 0, 0))
title("世界地图")

map("state", fill = TRUE, col = rainbow(209),
    mar = c(0, 0, 2, 0))
title("美国地图")

map('state', region = c('new york', 'new jersey', 'penn'),
    fill = TRUE, col = rainbow(3), mar = c(2, 3, 4, 3))
title("美国三州地图")


library(maps)
library(mapdata)
map("china", col = "red4", ylim = c(18, 54), panel.first = grid())
title(" 中国地图")


library(ggmap)
geocode("Beijing") # 返回一个地方的经纬度
