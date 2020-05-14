//Функция coprimes: List[Int] => List[(Int, Int)],
//выполняющая поиск в списке целых чисел пар взаимно простых чисел. Функция должна возвращать список найденных пар, причём в каждой паре первое число должно быть меньше второго.

def gcd(x:Int, y:Int): Int= {
if (y == 0) x else gcd(y, x % y)
}

val find: (Int, List[Int]) => List[(Int, Int)] = {
case (a, Nil) => Nil;
case (a, x :: xs) if (gcd(a, x) == 1) =>
                if (a<x) List((a, x)):::find(a, xs) else List((x, a)):::find(a, xs);
case (a, x :: xs) => find(a, xs);
}

val coprimes: List[Int] => List[(Int, Int)] = {
case Nil => Nil;
case (x :: xs)  => find(x, xs):::coprimes(xs);
}

val a=coprimes(List(2, 5, 6, 12, 7));
println(a);
