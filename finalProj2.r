library(igraph)

el=read.csv(file.choose()) # read the 'runDiffEdgeList2.csv' file 
g=graph.data.frame(el)

plot(g,layout=layout.fruchterman.reingold,edge.width=E(g)$weight*100)

E(g)$weight

adj = get.adjacency(g, attr='Weight')
colnames(adj)
rownames(adj)
colnames(adj)
rownames(adj)

adj[1,]

gp = get.adjacency(g, attr='GP')
colnames(gp)
rownames(gp)

jg <- read.csv(file.choose(), header = TRUE, sep = ",") # read 'redictedGames2.csv
g2 <-as.matrix(jg)
g2

home = g2[,1]
away = g2[,2]
actualrd = g2[,3]

pcn = vector(mode = "integer", length = length(home)) # prediction based on common matchups
ph2h = vector(mode = "integer", length = length(home)) # prediction based on head to head matchups
prd = vector(mode = "integer", length = length(home))  #prediction  based on difference in overall run differentials

for(m in 1:length(home)){
x = home[m]
y = away[m]
ph2h[m] = adj[x,y] #head to head record

hrd  = 0;
ard = 0;
nx <- neighbors(g,x)
ny <- neighbors(g,y)
intersect = intersection(nx, ny)
hg = 0
ag = 0
for(i in intersect[]){
  hrd = hrd + adj[x,i] * gp[x,i] 
  ard = ard + adj[y,i] * gp[i,y]
  hg = hg + gp[x,i] 
  ag = ag + gp[i,y] 
}
pcn[m] = (hrd/hg) - (ard/ag)

homerd = sum(adj[x,]*gp[x,])/sum(gp[x,])
awayrd = sum(adj[y,]*gp[y,])/sum(gp[y,])
prd[m] = (homerd - awayrd)

}

results = cbind(prd, pcn, ph2h, actualrd)
results
write.table(results, "C:/MDM/2017SS1/bia658/results.txt", sep = "\t")
 prd[1]
pcn[1]
ph2h[1]
actualrd[1]





plot(prd, actualrd)
abline(lm(prd ~ actualrd))
plot(pcn, actualrd)
abline(lm(pcn ~ actualrd))
plot(ph2h, actualrd)
abline(lm(ph2h ~ actualrd))

