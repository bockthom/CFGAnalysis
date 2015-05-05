package de.fosd.typechef.cfganalysis

import java.io.File

import de.fosd.typechef.featureexpr.FeatureExprFactory._
import org.junit.Test


class CFGLoadingTest {

    /**
     * Test if CFG is properly loaded if there is an implementation after a declaration (right order)
     */
    @Test def testCFGOrder() {

        val cfgFile = new File("src/test/resources/nodeOrderTest.cg")
        val cfg = new CFGLoader().loadFileCFG(cfgFile)

        // there is not declaration existent, because it is to be filtered while loading the file
        val declarations = cfg.nodes.filter(_.kind == "declaration")
        assert(declarations.isEmpty)

        // graph should still be consistent
        assert(cfg.checkConsistency)
    }

    /**
     * Test if CFG is properly loaded although there is a declaration after an implementation (wrong order)
     */
    @Test def testCFGWrongOrder() {

        val cfgFile = new File("src/test/resources/nodeWrongOrderTest.cg")
        val cfg = new CFGLoader().loadFileCFG(cfgFile)

        // there is not declaration existent, because it is to be filtered while loading the file
        val declarations = cfg.nodes.filter(_.kind == "declaration")
        assert(declarations.isEmpty)

        // graph should still be consistent
        assert(cfg.checkConsistency)
    }
}
