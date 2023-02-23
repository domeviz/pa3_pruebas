package com.programacion

object Prueba1 {
  def main(args: Array[String]): Unit = {

    println("Prueba 1")
    println("\n 1) Sumar un digito")

    val lista1=List(1,0,1,1)
    val lista2=List(1,1,1,1,0)

    println("Lista 1")
    println(lista1)
    println("Lista 2")
    println(lista2)

    val suma1=sumarDigito(1,lista1.reverse)
    val suma2=sumarDigito(1,lista2.reverse)

    println("Lista 1 + 1: "+ suma1.reverse)
    println("Lista 2 + 1: "+ suma2.reverse)

    println("\n 2) Sumar dos numeros binarios")
    val listaA = List(1, 1, 1, 1,0)
    val listaB = List(0, 1, 0, 1, 1)

    println("Lista A")
    println(lista1)
    println("Lista B")
    println(lista2)

    val sumaBinarios=sumarBinarios(listaA,listaB)
    println("Lista A + Lista B")
    println(sumaBinarios.reverse)

    println("\n 3) Tuplas Numeros a Letras")

    val l1 = List(3, 2, 3, 7, 5)
    val l2 = List(6, 8, 4, 2, 8)

    println("Lista 1")
    println(l1)
    println("Lista 2")
    println(l2)

    val listaTuplas = generarTuplas(l1, l2, numerosALetras, numerosALetras)
    println("Tuplas Numeros a Letras")
    println(listaTuplas)

    println("\n 4) Listas Letras a Numeros")

    val tuplas = List(("tres", "seis"), ("dos", "ocho"), ("tres", "cuatro"), ("siete", "dos"), ("cinco", "ocho"))
    val (ld1, ld2) = obtenerListas(tuplas)

    println("Tuplas")
    println(tuplas)

    println("Lista 1")
    println(ld1.reverse)
    println("Lista 2")
    println(ld2.reverse)

  }

  //1) Sumar un digito
  def sumarDigito(digito: Int, lista: List[Int]): List[Int] = lista match {
    case Nil => List(digito)
    case h :: t =>
      if (digito == 0) lista
      else {
        val suma = h + digito
        (suma % 2) :: sumarDigito(suma / 2, t)
      }
  }

  //2) Sumar dos numeros binarios
  def sumarBinarios(num1: List[Int], num2: List[Int]): List[Int] = (num1, num2) match {
    case (Nil, Nil) => Nil
    case (Nil, n) => n
    case (m, Nil) => m
    case (h1 :: t1, h2 :: t2) =>
      val suma = h1 + h2
      (suma % 2) :: sumarDigito(suma / 2, sumarBinarios(t1, t2))
  }

  //3) Tuplas Numeros a Letras
  def generarTuplas[T, U, V](lista1: List[T], lista2: List[U], funcion1: T => V, funcion2: U => V): List[(V, V)] = {
    if (lista1.isEmpty || lista2.isEmpty) {
      List()
    } else {
      val head1 = lista1.head
      val head2 = lista2.head
      val tail1 = lista1.tail
      val tail2 = lista2.tail
      val tupla = (funcion1(head1), funcion2(head2))
      tupla :: generarTuplas(tail1, tail2, funcion1, funcion2)
    }
  }

  def numerosALetras(num: Int): String = {
    num match {
      case 0 => "cero"
      case 1 => "uno"
      case 2 => "dos"
      case 3 => "tres"
      case 4 => "cuatro"
      case 5 => "cinco"
      case 6 => "seis"
      case 7 => "siete"
      case 8 => "ocho"
      case 9 => "nueve"
    }
  }

  //4) Listas Letras a Numeros
  def convertirNumero(numString: String): Int = numString match {
    case "cero" => 0
    case "uno" => 1
    case "dos" => 2
    case "tres" => 3
    case "cuatro" => 4
    case "cinco" => 5
    case "seis" => 6
    case "siete" => 7
    case "ocho" => 8
    case "nueve" => 9
    case _ => throw new Exception("Número invalido")
  }

  def obtenerListas(tuplas: List[(String, String)]): (List[Int], List[Int]) = {
    tuplas.foldLeft((List[Int](), List[Int]())) {
      case ((lista1, lista2), (num1, num2)) =>
        (convertirNumero(num1) :: lista1, convertirNumero(num2) :: lista2)
    }
  }

}
