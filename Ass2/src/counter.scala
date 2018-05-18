object Demo {
   def main(args: Array[String]) {
      println( "Returned Value : " + countName(1,List()) );
   }
   
    def countName(needle:Int, haystack: List[Int]) : Int = {
          var sum = 0
          if(haystack == Nil)
          {
            println("Empty List");   
            return 0; 
          }

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