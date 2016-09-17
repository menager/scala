/* ******************
 * Morgan Nager			*
 * HWK 9						*
 ********************/



trait Type

object DoubleHung extends Type { override def toString = "double hung" }
object TwoLiteSlider extends Type { override def toString = "2 lite slider" }
object ThreeLiteSlider extends Type { override def toString = "3 lit slider" }
object PictureWindow extends Type { override def toString = "picture window" }

case class Size (width: Range, height: Range) {
  def contains(w: Double, h: Double) = width.contains(w) && height.contains(h)
  override def toString = "(width:" + width + ", height:" + height + ")"
}

trait Bracket extends Ordered[Bracket] {
  def intersects(x: Double) : Double
}

case class Range(low: Double, high: Double) extends Bracket {
  def contains(x: Double) = low <= x && x <= high
  def intersects(x: Double) = if (x <= low) 0 else if (x <= high) x-low else high-low
  override def toString = "[" + low + ", " + high + "]"
  
  def compare(other: Bracket) = {
    other match {
      case Upper(_) => -1
      case Lower(_) => 1
      case Range(l, h) => if (h == high) 0 else if (h < low) 1 else -1
    }
  }
}
case class Upper (limit: Double) extends Bracket {
  def intersects(x: Double) = if (x <= limit) 0 else x - limit
  
  def compare(other: Bracket) = 1
}
case class Lower (limit: Double) extends Bracket {
  def intersects(x: Double) = if (limit <= x) limit else x
  
  def compare(other: Bracket) = -1
}

trait Price {
  val bracket: Bracket
  
  def cost(x: Double): Double
}

case class FixedPrice(bracket: Bracket, dollar: Double) extends Price {
  def cost(x: Double) = dollar
}
case class UnitPrice(bracket: Bracket, dollar: Double) extends Price {
  def cost(x: Double) = x * dollar
}

case class SizeException(t: Type, limit: Size) extends Exception("size exceeds limit: " + limit + " of type: " + t)

object window {
  val pricing = Map(
        DoubleHung -> ( Size(Range(15.25, 48), Range(26.5, 78)),
            
                          List(FixedPrice(Lower(73),      200), 
                               FixedPrice(Range(73, 83),  250),
                               FixedPrice(Range(83, 93),  300),
                               FixedPrice(Range(93, 101), 350),
                               UnitPrice(Upper(101),     4))
        ),
        
        TwoLiteSlider -> ( Size(Range(25, 71), Range(15.25, 66)),
            
                          List(FixedPrice(Lower(73),      200), 
                               FixedPrice(Range(73, 83),  250),
                               FixedPrice(Range(83, 93),  300),
                               FixedPrice(Range(93, 101), 350),
                               UnitPrice(Upper(101),     4))
        ),
                               
        ThreeLiteSlider -> ( Size(Range(40, 108), Range(15.25, 66)),
            
                           List(UnitPrice(Lower(120), 4),
                                UnitPrice(Upper(120), 5))
        ),
                                
        PictureWindow -> ( Size(Range(15.75, 84), Range(14.5, 84)),
            
                           List(UnitPrice(Lower(120), 3),
                                UnitPrice(Upper(120), 5))
        )          
  )

      
  def quote(t: Type, w: Double, h: Double) = {
	   if(!pricing(t)._1.contains(w,h))
	     throw new SizeException(t, pricing(t)._1)
	   
	   var cost = 0.0
	   val UI = w+h
	   val priceList = pricing(t)._2
	   val fixedPriceDummy = new FixedPrice(Range(0,0),0)
	  
	   for(x <- priceList.indices){
	     if(priceList(x).getClass == fixedPriceDummy.getClass){
	       if(priceList(x).bracket.intersects(UI) >0){
	         cost = priceList(x).cost(UI)
	       }
	     }else{
	       if(priceList(x).bracket.intersects(UI) >0)
	         cost = cost+priceList(x).cost(priceList(x).bracket.intersects(UI))
	     }
	   }
	   cost
  }
  
   def main(arg: Array[String]) {
     
     println("double hung, 40 x 50 => price = " + quote(DoubleHung, 40, 50))
     
     println("two lite slider, 60 x 60 => price = " + quote(TwoLiteSlider, 60, 60))
     
     println("three lite slider, 80 x 60 => price = " + quote(ThreeLiteSlider, 80, 60))
     
     println("picture window, 40 x 60 => price = " + quote(PictureWindow, 40, 60))
     
     println("picture window, 40 x 90 => price = " + quote(PictureWindow, 40, 90))
   }
}

