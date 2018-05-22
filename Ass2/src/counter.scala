object Demo {
   def main(args: Array[String]) {
      println( "Returned Value : " + countName(1,List(1, 1, 3, 2)) );
   }
   
    def countName(needle:Int, haystack: List[Int]) : Int = haystack match{
          case Nil => 0
          case x::xs if(x == needle) => 1 + countName(x, xs)
          case x::xs if(x != needle) => 0

    
  
         /* var sum = 0
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
          */ 
     }
        
}