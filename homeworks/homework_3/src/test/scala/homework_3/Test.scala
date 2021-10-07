package homework_3

import homework_3.Exercises.prettyBooleanFormatter1
import utest._

object Test extends TestSuite{

    def test_formatters(func : Any => String): Unit ={
        val trueStr = "правда"
        val falseStr = "ложь"
        assert(func(true) == trueStr)
        assert(func(false) == falseStr)
        assert(func("test") == "test")
        assert(func(Seq(1, 2)) == "List(1, 2)")
    }

    def test_max[T](max_func : Seq[Int] => T,
                    is_empty : (Seq[Int] => T, Seq[Int]) => Boolean,
                      get_max : T => Int): Unit = {
        val v1 = Seq()
        val v2 = Seq(1, 2, 3)
        val v3 = Seq(-1, 20, 0)
        assert(is_empty(max_func, v1))
        assert(!is_empty(max_func, v2))
        assert(get_max(max_func(v2)) == 3)
        assert(!is_empty(max_func, v3))
        assert(get_max(max_func(v3)) == 20)
    }

    def test_sum(sum_func: (Int, Int) => Int): Unit ={
        assert(sum_func(0, 0) == 0)
        assert(sum_func(-5, 5) == 0)
        assert(sum_func(10, 2) == 12)
        assert(sum_func(1254, 32334) == 33588)
    }

    val tests = Tests{
        'test_example - {
            test_formatters(Exercises.prettyBooleanFormatter1)
        }
        'test_formatter2 - {
            test_formatters(Exercises.prettyBooleanFormatter2)
        }
        'test_formatter3 - {
            test_formatters(Exercises.prettyBooleanFormatter3)
        }
        'test_max1 - {
            test_max[Int](Exercises.max1, (f, s) => {
                try {
                    f(s)
                    false
                }
                catch {case _: Exception => true}
                }, v => v)
        }
        'test_max2 - {
            test_max[Seq[Int]](Exercises.max2, (f, s) => f(s).isEmpty, v => v.head)
        }
        'test_max3 - {
            test_max[Option[Int]](Exercises.max3,
                (f, s) => f(s).isEmpty,
                v => v.get)
        }
        'test_sum1 - {
            test_sum(Exercises.sum1)
        }
        'test_sum2 - {
            test_sum(Exercises.sum2)
        }
        'test_sum3 - {
            test_sum(Exercises.sum3)
        }
    }
}
