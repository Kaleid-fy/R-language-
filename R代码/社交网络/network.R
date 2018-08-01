if(!require(igraph)) install.packages('igraph')
library(igraph)

actors<-read.csv("C:/Users/dell/Desktop/nodes.csv")
relations<-read.csv("C:/Users/dell/Desktop/edges.csv")

actors<- as.data.frame(actors)
relations<- as.data.frame(relations)

g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
plot(g)

net_indices<-function(nodex,edgex){
  edgex<-as.data.frame(edgex)
  nodex<-as.data.frame(nodex)
  g <- graph_from_data_frame(edgex, directed=TRUE, vertices=nodex)
  #节点指标
  Vertex_betweenness<-betweenness(g)
  k_core<-coreness(g)
  
  Degree_all<-degree(g,mode="all") #mode=in点入度；out=点出度；all点度中心度，三者统称绝对点中心度
  Degree_in<-degree(g,mode="in")
  Degree_out<-degree(g,mode="out")
  Degree_normalized<-degree(g,normalized=T)#相对点中心度=绝对点中心度/最大度数（可以作为不同网络结构的比较，相对数与绝对数的区别）
  
  Cloness<-closeness(g)
  Eigen_centrality<-eigen_centrality(g)$vector
  out<-cbind(nodex,Vertex_betweenness,k_core,Degree_all,Degree_in,Degree_out,Degree_normalized,Cloness,Eigen_centrality)
}

output<-net_indices(actors,relations)


##将结果写入Excel，注意修改文件路径
write.csv(output,"C:/Users/dell/Desktop/output.csv",row.names = F)


#####不执行
# ## 从图中取出顶点和边的信息
# as_data_frame(g, what="vertices")
# as_data_frame(g, what="edges")