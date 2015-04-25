package de.fosd.typechef.cfganalysis

import de.fosd.typechef.options.{FrontendOptionsWithConfigFiles, FrontendOptions, OptionException}
import de.fosd.typechef.options._

class CFGValidator { 

    def validate(args: Array[String], cfg : CFG): CFG = {

        // load options
        val opt = new FrontendOptionsWithConfigFiles()
        try {
		try {			
			opt.parseOptions(args)
		} catch {
			case o: OptionException => if (!opt.isPrintVersion) throw o
		}
   	}
   	catch {
		case o: OptionException =>
		if(! o.getMessage.equals("No file specified.") 
		   && ! o.getMessage.equals("Multiple files specified. Only one supported.")) {
			println("Invocation error: " + o.getMessage)
			println("use parameter --help for more information.")
			return cfg
		}
    	}

	// get feature model
    	val fm = opt.getFullFeatureModel

	// validate nodes against the feature model
    	val satNodes = cfg.nodes.filter(_.fexpr.isSatisfiable(fm))
    	println("Nodes (sat/all): " + satNodes.size + " vs. " + cfg.nodes.size)

	// validate edges against the feature model
    	val satEdges = cfg.edges.filter(_._3.isSatisfiable(fm))
   	println("Edges (sat/all): " + satEdges.size + " vs. " + cfg.edges.size)

	// remove edges which source or target was removed in validation step above
	val unsatNodes = cfg.nodes.filterNot(_.fexpr.isSatisfiable(fm))
	val unsatNodeIds:Set[Int] = unsatNodes.map(_.id)
	val satEdgesWithoutUnsatSourceNodes = satEdges.filterNot(x => unsatNodeIds.contains(x._1.id))
	val satEdgesWithoutUnsatNodes = satEdgesWithoutUnsatSourceNodes.filterNot(x => unsatNodeIds.contains(x._2.id))
	println("Edges (sat without unsat. nodes/all): " + satEdgesWithoutUnsatNodes.size + " vs. " + satEdges.size)

	return (new CFG(satNodes,satEdgesWithoutUnsatNodes))
    }
}
