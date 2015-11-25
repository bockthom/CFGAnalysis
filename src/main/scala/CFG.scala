package de.fosd.typechef.cfganalysis

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureExpr}
import java.io.{Writer, FileReader, BufferedReader, File}
import FeatureExprFactory._


class CFGNode(val id: Int, val kind: String, val file: File, line: Int, val name: String, val fexpr: FeatureExpr) {
    def write(writer: Writer) {
        writer.write("N;" + id + ";" + kind + ";" + (if (file != null) file.getPath else "null") + ";" + line + ";" + name + ";")
        fexpr.print(writer)
        writer.write("\n")
    }
    override def toString(): String = kind + "-" + name
    override def hashCode = id
    override def equals(that: Any) = if (that.isInstanceOf[CFGNode]) that.asInstanceOf[CFGNode].id == this.id else super.equals(that)

}

case class CFG(val nodes: Set[CFGNode], val edges: Set[(CFGNode, CFGNode, FeatureExpr)]) {
    def link(that: CFG): CFG = {

        var nodesToRemove = Set[CFGNode]()
        val thatFunctions: Map[String, Set[CFGNode]] = that.nodes.filter(_.kind == "function").groupBy(e => e.name)
        var thisReplacements: Map[CFGNode, Set[CFGNode]] = Map()
        for (node <- this.nodes) {
            if (node.kind == "declaration") {
                val functions = thatFunctions.get(node.name)
                if (functions.isDefined) {
                    thisReplacements += (node -> functions.get)
                    nodesToRemove = nodesToRemove + node
                }
            }
        }

        val newThisEdges: Set[(CFGNode, CFGNode, FeatureExpr)] = this.edges.flatMap(
            e => if (thisReplacements.contains(e._2))
                thisReplacements(e._2).map(newTarget => (e._1, newTarget, e._3 and newTarget.fexpr)) + ((e._1, e._2, thisReplacements(e._2).foldLeft(e._3)(_ andNot _.fexpr)))
            else Set(e)

        )

        val thisFunctions: Map[String, Set[CFGNode]] = this.nodes.filter(_.kind == "function").groupBy(e => e.name)
        var thatReplacements: Map[CFGNode, Set[CFGNode]] = Map()
        for (node <- that.nodes) {
            if (node.kind == "declaration") {
                val functions = thisFunctions.get(node.name)
                if (functions.isDefined) {
                    thatReplacements += (node -> functions.get)
                    nodesToRemove = nodesToRemove + node
                }
            }
        }

        val newThatEdges = that.edges.flatMap(
            e => if (thatReplacements.contains(e._2))
                thatReplacements(e._2).map(newTarget => (e._1, newTarget, e._3 and newTarget.fexpr)) + ((e._1, e._2, thatReplacements(e._2).foldLeft(e._3)(_ andNot _.fexpr)))
            else Set(e)
        )


        new CFG((this.nodes ++ that.nodes /*--  nodesToRemove [not consistent, would also remove not-already-linked declarations!]*/)
          .filter(_.fexpr.isSatisfiable()), (newThisEdges ++ newThatEdges).filter(_._3.isSatisfiable()))
    }

    def write(writer: Writer) {
        for (n <- nodes) n.write(writer)
        for ((s, t, f) <- edges) {
            writer.write("E;" + s.id + ";" + t.id + ";")
            f.print(writer)
            writer.write("\n")
        }
    }

    def writeDot(writer: Writer) {
        writer.write("digraph \"\" {\nnode [shape=record];\n")
        for (n <- nodes)
          writer.write( """"%d"[label="{{%s::%s}|%s}", color="%s", fontname="Calibri", style="filled", fillcolor="white"];"""
            .format(n.id, esc(n.file.toString), esc(n.name), esc(n.fexpr.toString()) /*"1"*/ ,
              if (n.file.toString equals "sys-unknown") "red" else if (n.kind == "function") "blue" else "black"
            ) + "\n")

        for (e <- edges)
            writer.write( """"%d" -> "%d"[label="%s"];""".format(e._1.id, e._2.id, esc(e._3.toString()) /*"1"*/) + "\n")
        writer.write("}")
    }

    private def esc(i: String) = {
        i.replace("\n", "\\l").
            replace("{", "\\{").
            replace("}", "\\}").
            replace("<", "\\<").
            replace(">", "\\>").
            replace("\"", "\\\"").
            replace("|", "\\|").
            replace(" ", "\\ ").
            replace("\\\"", "\\\\\"").
            replace("\\\\\"", "\\\\\\\"").
            replace("\\\\\\\\\"", "\\\\\\\"")
    }


    override def toString(): String = "CFG(" + nodes + ", " + edges + ")"


    def checkConsistency: Boolean =
        edges.forall(e => (nodes contains e._1) && (nodes contains e._2))
}


class CFGLoader {

    val featureExprParser = new FeatureExprParser(FeatureExprFactory.dflt)


    def loadNode(s: String, file: File, filePC: FeatureExpr, isRawFormat: Boolean): (Int, CFGNode) = {
	val fields = s.split(";")
        if (isRawFormat) {
            //(fields(1).toInt, new CFGNode(IdGen.genId(), fields(2), file, fields(3).toInt, fields(4), parseFExpr(fields(5)) and filePC))

	    // modifications for KEBA
            val path = fields(4).split("::")
            val expr = fields(5).replaceAll("IS_LINKED[(](\\(\\(\\B\\)\\))*?", "\\(")

            if (path.length == 2)
                (fields(1).toInt, new CFGNode(IdGen.genId(), fields(2), new File(path(1))/*file*/, fields(3).toInt, path(0)/*fields(4)*/, parseFExpr(expr/*fields(5)*/) and filePC))
            else
                (fields(1).toInt, new CFGNode(IdGen.genId(), fields(2), new File(path(0))/*file*/, fields(3).toInt, path(0)/*fields(4)*/, parseFExpr(expr/*fields(5)*/) and filePC))
	    // end of modifications for KEBA
        }
        else
            (fields(1).toInt, new CFGNode(IdGen.genId(), fields(2), new File(fields(3)), fields(4).toInt, fields(5), parseFExpr(fields(6)) and filePC))
    }

    private def parseFExpr(s: String): FeatureExpr = featureExprParser.parse(s.replaceAll("""True""", "1").replaceAll("""False""", "0"))

    def loadEdge(s: String): (Int, Int, FeatureExpr) = {
        val fields = s.split(";")
	//(fields(1).toInt, fields(2).toInt, parseFExpr(fields(3)))

	// modifications for KEBA
	val expr = fields(3).replaceAll("IS_LINKED[(](\\(\\(\\B\\)\\))*?", "\\(")
        (fields(1).toInt, fields(2).toInt, parseFExpr(expr/*fields(3)*/))
	// end of modifications for KEBA
    }

    //load a CFG file for one file
    def loadFileCFG(cfgFile: File, filePC: FeatureExpr = True): CFG = loadFile(cfgFile, filePC, true)

    //load a whole-project CFG file as a result of a linking process
    def loadCFG(cfgFile: File, filePC: FeatureExpr = True): CFG = loadFile(cfgFile, filePC, false)


    private def loadFile(cfgFile: File, filePC: FeatureExpr, isRawFormat: Boolean): CFG = {
        val reader = new BufferedReader(new FileReader(cfgFile))

        var nodes = Map[Int, CFGNode]()
        var edges = List[(CFGNode, CFGNode, FeatureExpr)]()

        var line = reader.readLine()
        while (line != null) {
            if (line.charAt(0) == 'N') {
                val cfgNode@(id, _) = loadNode(line, cfgFile, filePC, isRawFormat)

                // only add a node,
                // - if it was not already added OR
                // - if it was added before, but not as a function implementation (override it!)
                if (!nodes.contains(id) || !(nodes.get(id).get.kind startsWith "function"))
                    nodes = nodes + cfgNode

            }
            if (line.charAt(0) == 'E') {
                val (srcId, targetId, fexpr) = loadEdge(line)
                edges = (nodes(srcId), nodes(targetId), fexpr) :: edges
            }

            line = reader.readLine()
        }

        val n = new CFG(nodes.values.toSet, edges.toSet)
        assert(n.checkConsistency)
        n
    }

}


private object IdGen {
    private var nextId = 1;
    def genId(): Int = {
        nextId += 1
        nextId
    }
}
