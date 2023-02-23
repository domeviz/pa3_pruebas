package com.programacion

object Prueba2 {

  def main(args: Array[String]): Unit = {

    println("Prueba 2")
    val lista=List(4, 6, 2, 3, 8)
    println("\n Lista")
    println(lista)

    println("\n 1) Maximo")
    val maxRecursivo=maximoRecursivo(lista)
    println("Maximo Recursivo: " + maxRecursivo)
    val maxFolding = maximoFolding(lista)
    println("Maximo Folding: " + maxFolding)

    println("\n 2) Maximo")
    val minRecursivo = minimoRecursivo(lista)
    println("Minimo Recursivo: " + minRecursivo)
    val minFolding = minimoFolding(lista)
    println("Minimo Folding: " + minFolding)

    println("\n 3) Sublista sin maximo")
    val subSinMax=sublistaSinMaximo(lista)
    println(subSinMax)

    println("\n 4) Ordenar Metodo de la Burbuja")
    val ordenar = ordenamientoBurbuja(lista)
    println(ordenar)
  }

  //1) Maximo
  //Version Recursiva
  def maximoRecursivo(lst: List[Int]): Int = lst match {
    case Nil => throw new NoSuchElementException("La lista esta vacia")
    case x :: Nil => x
    case x :: xs => Math.max(x, maximoRecursivo(xs))
  }

  //1) Maximo
  //Version Folding Izq a Der
  def maximoFolding(lst: List[Int]): Int = lst match {
    case Nil => throw new NoSuchElementException("La lista esta vacia")
    case x :: xs => xs.foldLeft(x)((max, x) => if (x > max) x else max)
  }

  //2) Minimo
  //Version Recursiva
  def minimoRecursivo(lst: List[Int]): Int = lst match {
    case Nil => throw new NoSuchElementException("La lista esta vacia")
    case x :: Nil => x
    case x :: xs => Math.min(x, minimoRecursivo(xs))
  }

  //2) Minimo
  //Version Folding Der a Izq
  def minimoFolding(lst: List[Int]): Int = lst match {
    case Nil => throw new NoSuchElementException("La lista esta vacia")
    case x :: xs => xs.foldRight(x)((min, x) => if (x < min) x else min)
  }

  //3) Sublista sin maximo
  def sublistaSinMaximo(lst: List[Int]): List[Int] = {
    val max = lst.max
    lst.filter(_ != max)
  }

  //4) Metodo de la Burbuja
  def insertarOrdenado(ls: List[Int], value: Int): List[Int] = ls match {
    case Nil => List(value)
    case x :: xs =>
      if (value < x) {
        value :: ls
      } else {
        x :: insertarOrdenado(xs, value)
      }
  }

  def ordenamientoBurbuja(ls: List[Int]): List[Int] = ls match {
    case Nil => throw new NoSuchElementException("La lista esta vacia")
    case x :: Nil => List(x)
    case x :: xs => insertarOrdenado(ordenamientoBurbuja(xs), x)
  }

}
