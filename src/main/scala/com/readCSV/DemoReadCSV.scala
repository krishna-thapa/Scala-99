package com.readCSV

import scala.io.Source

object DemoReadCSV {

  def main(args: Array[String]): Unit = {
    val demoList = readFile("Sample_Data.csv")
    if(demoList.nonEmpty){
      demoList.map{ drugId =>
        println(drugId)
      }
    }
    //println(s"${demoList}")
  }


  def readFile(filename: String): Set[String] = {
    val bufferedSource = Source.fromResource(filename)
    val lines = (for {line <- bufferedSource.getLines().drop(1)} yield {
      line.split(",").map(_.trim)
    }).flatten.toSet
    bufferedSource.close
    lines
  }

}
