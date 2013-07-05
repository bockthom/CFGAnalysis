package de.fosd.typechef.cfganalysis

import org.junit.Test

/**
 * Created with IntelliJ IDEA.
 * User: ckaestne
 * Date: 3/12/13
 * Time: 4:33 PM
 * To change this template use File | Settings | File Templates.
 */
class WholeProjectCFGTest {

    import de.fosd.typechef.featureexpr.FeatureExprFactory._

    val fa = createDefinedExternal("A")

    @Test def testCFG() {
        val n1 = new CFGNode(1, "declaration", null, 1, "foo", True)
        val n2 = new CFGNode(2, "statement", null, 1, "foo()", True)
        val f1 = new CFG(Set(n1, n2), Set((n2, n1, True)))

        val d1 = new CFGNode(3, "function", null, 1, "foo", True)
        val f2 = new CFG(Set(d1), Set())


        val ff = f1.link(f2)

        println(ff)
        assert(ff.edges.contains((n2, d1, True)))

        assert(ff == (f2 link f1))
    }

    @Test def testCFG2() {
        val n1 = new CFGNode(1, "declaration", null, 1, "foo", True)
        val n2 = new CFGNode(2, "statement", null, 1, "foo()", True)
        val f1 = new CFG(Set(n1, n2), Set((n2, n1, True)))

        val d1 = new CFGNode(3, "function", null, 1, "foo", fa)
        val f2 = new CFG(Set(d1), Set())


        val ff = f1.link(f2)

        println(ff)
        assert(ff.edges.contains((n2, d1, fa)))
        assert(ff.edges.contains((n2, n1, fa.not)))

        assert(ff == (f2 link f1))
    }

}
