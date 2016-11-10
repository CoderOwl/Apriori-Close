import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object AprioriClose {
  def main(args: Array[String]): Unit = {
    val fileName = "apriori.txt"
    val data = readDataFromFile(fileName)
    // data is a mutableList of Sets of strings.
    // supportCount(Set("pepper", "corn", "cabbage"), data)
    // println(supportCount(Set("pepper", "corn", "cabbage"), data))
    val closedSets = findFrequentClosed(data)
  }



  def supportCount(combination: Set[String], data: Seq[Set[String]]): Int = {
    //For each set in data, check if combination is a subset of it.
    var count = 0
    println(combination)
    for(record <- data)
    {
        if(combination subsetOf record)
          count = count + 1
    }
    println(count)
    count
  }

  def findFrequentClosed(data: Seq[Set[String]]): Unit = {
      var generators = data.flatten.map({str =>
        val set = new mutable.HashSet[String]()
        set += str
        set.toSet
      }).toSet

      var result = new mutable.MutableList[Set[String]]
      while(!generators.isEmpty)
      {
          generators = generators.filter(generator => supportCount(generator, data) >= 2)                  //Pruning infrequent.
          println(generators)
          //Generating next level itemsets.
          val newItemSet = new mutable.HashSet[Set[String]]
          generators.foreach({ set1 =>
              generators.foreach({ set2 =>
                  if (set1.size - set1.intersect(set2).size == 1) {
                      val newSet = set1.union(set2)
                      val sup = supportCount(newSet, data)
                      val sup1 = supportCount(set1, data)
                      val sup2 = supportCount(set2, data)
                      if(sup1 > sup && sup2 > sup)
                          newItemSet += newSet
                  }
              })
          })
          generators = newItemSet.toSet
          //Check for closure and remove.

          
          // generators.foreach(generator => result+=generator)

      }
      result = result.distinct
      generators.foreach(generator => result+=generator)
      println(result)
  }

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
    result
  }
}
