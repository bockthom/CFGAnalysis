package de.fosd.typechef.cfganalysis

import de.fosd.typechef.options.{FrontendOptionsWithConfigFiles, FrontendOptions, OptionException}

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
		println("Invocation error: " + o.getMessage)
		println("use parameter --help for more information.")
		return cfg
    	}

	// get feature model
    	val fm = opt.getFullFeatureModel

	// validate against the feature model
    	val satNodes = cfg.nodes.filter(_.fexpr.isSatisfiable(fm))
    	println("Nodes (sat/unsat): " + satNodes.size + " vs. " + cfg.nodes.size)

    	val satEdges = cfg.edges.filter(_._3.isSatisfiable(fm))
   	println("Edges (sat/unsat): " + satEdges.size + " vs. " + cfg.edges.size)

	return (new CFG(satNodes,satEdges))
    }
}
