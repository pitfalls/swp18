object Demo {
   def main(args: Array[String]) {
      println( "Returned Value : " + countName(1,List(1, 3, 2, 1, 1, 3)) );
   }
   
    def countName(needle:Int, haystack: List[Int]) : Int = {
          var sum = 0
          for(i <- haystack)
          {
               if(i == needle)
               {
                    sum +=1
               }
          }
          return sum
     }    
}