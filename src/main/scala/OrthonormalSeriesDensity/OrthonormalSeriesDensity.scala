package OrthonormalSeriesDensity

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

class OrthonormalSeriesDensity(basis:Basis) {
  
  /* J: number of basis elements to use */
  private var J: Int = 5
  
  def setJ(J:Int): this.type ={
    this.J = J
    this
  }
  
  /*
   * This is the basic method for estimating weights. It does not yet
   * incorporate shrinkage
   */
  def train(data: RDD[Double]):OrthonormalSeriesDensityModel={
    val weights = data.aggregate(new Array.fill[Double](J)
  }

}

/*
 * Model takes as parameters an array of weights
 * and a Basis class
 */
class OrthonormalSeriesDensityModel(weights: Array[Double], basis:Basis){
  private val pdf(x:Double):Double ={
    val len = weights.length
    (0 until len)
    .map(index=>weights(index)*basis.eval(x,index))
    .reduce(_+_)
  }
  
  def estimate(x:Double):Double={
    pdf(x)
  }
  
  def estimate(x:RDD[Double]):RDD[Double]={
    //TODO: better to broadcast basis?
    x.map(pdf(_))
  }

}

/*
 * case class representing a generic basis indexed over the integers
 */
abstract class Basis(){
  def eval(x:Double,j:Int):Double`
}

case class CosineBasis() extends Basis{
 override def eval(x:Double,j:Int):Double ={
   require(x>=0 & x<= 1,"Must be inside [0,1] for cosine basis")
    math.sqrt(2) * math.cos(math.Pi*j*x)
  }
}