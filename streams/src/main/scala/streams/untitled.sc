implicit val n: Int = 42
def f(implicit x: Int) = x
println(f(0))