package org.uqbar.math.vectors

import scala.math._
import org.scalatest._
import org.scalactic._
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class VectorTest extends FreeSpec with Matchers {

	//*********************************************************************************************
	// TEST CASES
	//*********************************************************************************************

	"vector" - {
		val u: Vector = (4, 5)
		val u2: Vector = (4, 5)
		val u3: Vector = (4, 5)
		val v: Vector = (1, 6)
		val w: Vector = (-1.5, 3)
		val λ = 2.0
		val µ = 5.5

		"Origin should be (0,0)" in { Origin should equal (0, 0) }

		"equality" - {
			"should be consistent" in { v should not (be (null) or be ("foo")) }
			"should be reflexive" in { v should be (v) }
			"should be symmetric" in {
				u should be (u2)
				u2 should be (u)
			}
			"should be transitive" in {
				u should be (u2)
				u2 should be (u3)
				u should be (u3)
			}
		}

		"hash should be consistent" in { u2.hashCode should be (u.hashCode) }

		"opposite should be (-x,-y)" in { -v should equal (-v.x, -v.y) }

		"addition" - {
			"should compute well" in { u + v should equal (u.x + v.x, u.y + v.y) }
			"should be commutative" in { u + v should be (v + u) }
			"should be associative" in { u + v + w should (be ((u + v) + w) and be (u + (v + w))) }
			"should have Origin as identity element" in { v + Origin should be (v) }
			"against opposite should yield Origin" in { v + -v should be (Origin) }
		}

		"subtraction" - {
			"should compute well" in { u - v should equal (u.x - v.x, u.y - v.y) }
			"should be same as addition with opposite" in { u - v should be (u + -v) }
			"should have Origin as identity element" in { v - Origin should be (v) }
		}

		"scalar multiplication" - {
			"should compute well" in { v * λ should equal(v.x * λ, v.y * λ) }
			"should be additive" in { (u + v) * λ should be(u * λ + v * λ) }
			"should be compatible" in { v * (λ * µ) should be((v * λ) * µ) }
			"should be symmetric" in { λ * v should be(v * λ) }
			"should have 1 as identity element" in { v * 1 should be(v) }
			"should have 0 as absorbent element" in { v * 0 should be(Origin) }
			"against -1 should yield opposite" in { v * -1 should be (-v) }
		}

		"scalar division" - {
			"should compute well" in { v / λ should equal(v.x / λ, v.y / λ) }
			"should be equal to the product with scalar inverse" in { v / λ should be(v * 1 / λ) }
			"should be symmetric" in { λ / v should be(v / λ) }
		}

		"dot product should compute well" in {
			v ° u should be(v.x * u.x + v.y * u.y)
			v ° u should equal (v.module * u.module * cos(v.angleTo(u)))
		}

		"module should compute well" in { v.module should be (sqrt(v.x * v.x + v.y * v.y)) }

		"as versor should have module 1" in { v.asVersor.module should be (1) }

		"distance" - {
			"should compute well" in { v distanceTo (5, 3) should be (5) }
			"should be symmetric" in { v distanceTo u should be (u distanceTo v) }
			"to Origin should be the vector module" in { v distanceTo Origin should equal (v.module) }
			"to self should be 0" in { v distanceTo v should be (0) }
		}

		"square distance" - {
			"should compute well" in { v squareDistanceTo u should equal (pow(v distanceTo u, 2)) }
			"should be symmetric" in { v squareDistanceTo u should be (u squareDistanceTo v) }
			"to Origin should be the square of the vector module" in { v squareDistanceTo Origin should equal (pow(v.module, 2)) }
			"to self should be 0" in { v squareDistanceTo v should be (0) }
		}

		"manhattan distance" - {
			"should compute well" in { v manhattanDistanceTo u should be (4) }
			"should be symmetric" in { v manhattanDistanceTo u should be (u manhattanDistanceTo v) }
			"to self should be 0" in { v manhattanDistanceTo v should be (0) }
		}
	}

	//*********************************************************************************************
	// EQUALITY DEFINITIONS
	//*********************************************************************************************

	val tolerance = 0.00000001

	implicit val VectorEquality = new Equality[Vector] {
		def areEqual(v: Vector, o: Any) = {
			o match {
				case (x: Number, y: Number) => x.doubleValue === v.x.doubleValue +- tolerance && y.doubleValue === v.y.doubleValue +- tolerance
				case _ => false
			}
		}
	}

	implicit val DoubleEquality = new Equality[Double] {
		def areEqual(d: Double, o: Any) = {
			o match {
				case n: Double => d === n +- tolerance
				case _ => false
			}
		}
	}
}