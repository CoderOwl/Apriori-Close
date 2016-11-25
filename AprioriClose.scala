import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object AprioriClose {
  def main(args: Array[String]): Unit = {
    val fileName = "dataset.txt"
    val data = readDataFromFile(fileName)
    val generators = findGenerators(data)
	  println("THE GENERATORS OF ALL LEVEL GENERATED ARE:")
	  generators.foreach(generator => println(generator))
    val closedSets = findClosedSets(generators, data)
	  println("THE CLOSED FREQUENT ITEMSETS GENERATED ARE:")
	  closedSets.foreach(set => println(set))
    val n = data.size.toDouble
	  println("THE RULES GENERATED FROM THE CLOSED SETS:")
    closedSets.foreach({ set =>
      val subsets = set.subsets.filter(subset => subset.nonEmpty && subset.size != set.size)
      subsets.foreach({ subset =>
        val disjoint = set -- subset
        val supportValue = supportCount(subset.union(disjoint), data)/n
        val confidenceValue = confidence(subset, disjoint, data)
        if(confidenceValue > 0.3)
          println(subset.toString + " -> " + disjoint + " support = " + supportValue + " confidence = " + confidenceValue)
      })
    })
  }

  def findClosedSets(generators: Seq[Set[String]], data: Seq[Set[String]]): Seq[Set[String]] = {
      var result = new mutable.MutableList[Set[String]]
      var intersection : Set[String] = Set("")
      intersection = intersection.empty
      generators.foreach({generator =>
          data.foreach({set =>
              if(generator.forall(set.contains)) {
                if(intersection.isEmpty) {
                    intersection = set
                }
                else
                    intersection = intersection.intersect(set)
              }
          })
          result += intersection
          intersection = intersection.empty
      })
      result.distinct
  }

  //Takes in one sets of itesms and counts how manyt sets in data it is a subset of.
  def supportCount(combination: Set[String], data: Seq[Set[String]]): Int = {
    //For each set in data, check if combination is a subset of it.
    var count = 0
    for(record <- data)
    {
        if(combination subsetOf record)
          count = count + 1
    }
    count
  }

  def support(conditionSet: Set[String], implicationSet: Set[String], data: Seq[Set[String]]): Double = {
    var numberOfMatches = 0

    data.foreach({ transaction =>
      if (conditionSet.intersect(transaction).size == conditionSet.size &&
        implicationSet.intersect(transaction).size == implicationSet.size) {
        numberOfMatches += 1
      }
    })
    numberOfMatches.toDouble / data.size.toDouble
  }

  def confidence(conditionSet: Set[String], implicationSet: Set[String], data: Seq[Set[String]]): Double = {
    var numberOfMatchesBoth = 0
    var numberOfMatchesCondition = 0

    data.foreach({ transaction =>
      if (conditionSet.intersect(transaction).size == conditionSet.size) {
        numberOfMatchesCondition += 1
        if (implicationSet.intersect(transaction).size == implicationSet.size) {
          numberOfMatchesBoth += 1
        }
      }
    })
    numberOfMatchesBoth.toDouble / numberOfMatchesCondition.toDouble
  }

  //To return the list of frequent closed itemsets.
  def findGenerators(data: Seq[Set[String]]): Seq[Set[String]] = {

      // generators will get all the distinct items that are part of the data.
      var generators = data.flatten.map({str =>
        val set = new mutable.HashSet[String]()
        set += str
        set.toSet
      }).toSet

      // result to be returned. Yet to be populated.
      var result = new mutable.MutableList[Set[String]]
      var A: Map[Set[String], Int] = Map()
      while(!generators.isEmpty)
      {
          generators.foreach(generator => A += (generator -> supportCount(generator, data)))
          generators = generators.filter(generator => A(generator) >= 2)                  // Pruning infrequent.
          generators.foreach(generator => result+=generator)
          //println(generators)
          //Generating next level itemsets from generators.
          val newItemSet = new mutable.HashSet[Set[String]]
          generators.foreach({ set1 =>
              generators.foreach({ set2 =>
                  if (set1.size - set1.intersect(set2).size == 1) {
                      val newSet = set1.union(set2)
                      val sup = supportCount(newSet, data)
                      val sup1 = A(set1)
                      val sup2 = A(set2)
                      if(sup1 > sup && sup2 > sup)                                                        // Pruning the closed ones.
                          newItemSet += newSet
		      else
			println(newSet + " has been been pruned out.")
                  }
              })
          })
          generators = newItemSet.toSet
	  println("THE GENERATORS OF NEXT level are:")
          generators.foreach(generator => println(generator))
      }
      result
  }

  //Reads from the file and forms a List of Sets of Strings(Items).
  def readDataFromFile(fileName: String): Seq[Set[String]] = {
    val input = Source.fromFile(fileName)
    val inputScanner = new Scanner(input.bufferedReader())
    val result = new mutable.MutableList[Set[String]]
    while (inputScanner.hasNext) {
      val line = inputScanner.nextLine()

      val lineScanner = new Scanner(line)

      val transactionData = new mutable.HashSet[String]()
      while (lineScanner.hasNext) {
        transactionData += lineScanner.next("\\w+")
      }
      result += transactionData.toSet
    }
    // println(result)
    result
  }
}
