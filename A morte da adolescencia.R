

# Ninguem, além do autor, sabe o quão angustiante e aterrador este script é
# ele representa a diferença que os métodos aprendidos podem gerar no desenvolvimento
# de novas formas de raciocinar.


x <- 1:10
y <- 1:6

k <- NULL

for(j in y){
  k <- rbind(k, unlist(lapply(x, function(i) (sum(c(1:i)^j)))))
}


i = 7
A <- as.matrix(t(as.data.frame(lapply(1:i, function(j) j^c(1:i)  ))))

S <- solve(A, k[i-1,1:i])
names(S) <- paste0("x^", 1:i)
print(S)
