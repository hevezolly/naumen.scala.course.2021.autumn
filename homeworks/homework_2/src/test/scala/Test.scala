import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(1, 6) == 14)
            assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)
        }
        'primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(42) == Seq(2, 3, 7))
            assert(Exercises.primeFactor(112344320) == Seq(2,5,11,79,101))
        }
        'Vector2D - {
            val v1 = new Exercises.Vector2D(2, 0)
            val v2 = new Exercises.Vector2D(-2, 0)
            val v3 = new Exercises.Vector2D(0, 1)
            val v4 = new Exercises.Vector2D(0, -1)
            assert(math.abs(Exercises.sumScalars(v1, v2, v3, v4) + 5) < 0.0001)
            assert(math.abs(Exercises.sumScalars(v1, v3, v2, v4)) < 0.0001)
            assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) + 2) < 0.0001)
            assert(math.abs(Exercises.sumCosines(v1, v3, v2, v4)) < 0.0001)
        }
        'balls -{
            val balls1 = Map(
                "3" -> (3, 2.5),
                "1" -> (1, 2.5),
                "2" -> (2, 2.5)
            )
            val balls2 = Map(
                "3" -> (1, 10.0),
                "1" -> (1, 1.0),
                "2" -> (1, 5.0)
            )
            val balls3 = Map(
                "2" -> (3, 1.0),
                "1" -> (1, 10.0),
                "3" -> (2, 5.0)
            )
            assert(Exercises.sortByHeavyweight(balls1) == Seq("1", "2", "3"))
            assert(Exercises.sortByHeavyweight(balls2) == Seq("1", "2", "3"))
            assert(Exercises.sortByHeavyweight(balls3) == Seq("1", "2", "3"))

        }
    }
}
