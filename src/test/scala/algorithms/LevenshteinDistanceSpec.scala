package algorithms

import com.algorithms.LevenshteinDistance
import org.p99.scala.UnitSpec
import org.scalatest.Matchers

/**
 * Created by korneliusz on 06.01.15.
 */
class LevenshteinDistanceSpec extends UnitSpec with Matchers {
   it should "detect the distance for a simple case" in {
    LevenshteinDistance.compute("kitten", "sitting") should be (3)
   }
}
