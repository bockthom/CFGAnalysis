import java.io.{File, FileWriter}

import de.fosd.typechef.cfganalysis.{CFGLoader, CFG}
import de.fosd.typechef.featureexpr.FeatureExprParser
import de.fosd.typechef.lexer.FeatureExprLib

object CFGValidator extends App {

  if (args.length < 3) {
    println("Usage: % CFGValidator <cfg> <fm> <out>")
  } else {

    val cfgFile = args(0)
    val fmFile = args(1)
    val outpath = args(2)

    val cfg = new CFGLoader().loadFileCFG(new File(cfgFile))

    // TODO what about .dimacs feature models?
    val parsedFmFile = new FeatureExprParser().parseFile(fmFile)
    val fm = FeatureExprLib.featureModelFactory().create(parsedFmFile)

    val satNodes = cfg.nodes.filter(_.fexpr.isSatisfiable(fm))
    println("Nodes (sat/unsat): " + satNodes.size + " vs. " + cfg.nodes.size)

    val satEdges = cfg.edges.filter(_._3.isSatisfiable(fm))
    println("Edges (sat/unsat): " + satEdges.size + " vs. " + cfg.edges.size)

    val writer = new FileWriter(outpath)

    new CFG(satNodes,satEdges).write(writer)

  }
}
