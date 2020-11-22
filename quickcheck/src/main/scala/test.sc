val stream = (1 to 100).to(LazyList) filter (_%2==0)

for(i <- stream) println(i + "\n")

