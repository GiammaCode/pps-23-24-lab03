package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.AnonymousFunctions.h

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      first match
        case Nil() => Nil()  
        case Cons(headA, tailA) => second match
          case Nil() => Nil()
          case Cons(headB, tailB) => Cons((headA, headB), zip(tailA, tailB)) 
        
      
    def take[A](l: Sequence[A])(n: Int): Sequence[A] =
      l match
        case Nil() => Nil() 
        case Cons(head, tail) => n match
          case 0 => Nil()
          case _ => Cons(head, take(tail)(n-1)) 
  
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = 
      l1 match
        case Nil() => l2
        case Cons(head, tail) => l2 match
          case Nil() => l1
          case _ => Cons(head, concat(tail, l2))

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = ???

    def min(l: Sequence[Int]): Optional[Int] = 
      def minNotOptional(l: Sequence[Int]) : Int = 
        l match
          case Nil() => 10_00_000
          case Cons(head, tail) => head.min(minNotOptional(tail))
        
      l match
        case Nil() => Optional.Empty()
        case Cons(head, tail) => Optional.Just(head.min(minNotOptional(tail)))
      
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
