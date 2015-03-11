package de.fosd.typechef.cfganalysis

import de.fosd.typechef.featureexpr.FeatureExpr
import scala.annotation.tailrec
import scala.collection.mutable
import de.fosd.typechef.featureexpr.FeatureExprFactory._

/**
 * eliminate all nodes that do not represent functions
 */
class ReduceCFG {

    /**
     * edges from a node to itself are removed
     */
    def removeSelfCycles(cfg: CFG) =
        new CFG(cfg.nodes, cfg.edges.filter(e => e._1 != e._2))


    @tailrec
    final def reduce(cfg: CFG): CFG =
        cfg.nodes.find(eliminateNode(_)) match {
            case None => cfg
            case Some(node) => reduce(removeNode(cfg, node))
        }


    def removeNode(cfg: CFG, node: CFGNode): CFG = if (!cfg.nodes.contains(node)) cfg
    else {
        print(".")
        //remove node and connect all incoming nodes with all outgoing nodes directly

        val outgoingEdges = cfg.edges.filter(_._1 eq node)
        val incomingEdges = cfg.edges.filter(_._2 eq node)


        val newEdges = for (outEdge <- outgoingEdges; inEdge <- incomingEdges) yield
            (inEdge._1, outEdge._2, inEdge._3 and outEdge._3)

        new CFG(cfg.nodes - node, cfg.edges -- incomingEdges -- outgoingEdges ++ newEdges)
    }


    private def eliminateNode(n: CFGNode): Boolean =
        (n.kind != "function") && (n.kind != "function-inline") && (n.kind != "function-static") && (n.kind != "declaration")


    /**
     * equivalent implementation of reduce optimized for performance by using mutable data structures
     * and compressing redundant edges every 100 computations
     */
    final def reduceMut(cfg: CFG, progressOutput: Boolean = false, compressRate: Int = 100, nodeFilter: CFGNode => Boolean = this.eliminateNode): CFG = {
        val nodes = new mutable.ArrayBuffer() ++ cfg.nodes
        var edges = new mutable.ArrayBuffer() ++ cfg.edges
        val nonFunctions = nodes.filter(n => nodeFilter(n))
        if (progressOutput) println(nonFunctions.size)
        var i = 0

        for (node <- nonFunctions) {
            i = i + 1
            if ((compressRate>1) && (i % compressRate == 0)) {
                if (progressOutput) print("\n" + i + ";")
                edges = new mutable.ArrayBuffer() ++ (compressRedundantEdges(new CFG(cfg.nodes, Set() ++ edges)).edges)
            }
            if (progressOutput) print(":")
            val outgoingEdges = edges.filter(_._1 eq node)
            val incomingEdges = edges.filter(_._2 eq node)


            val newEdges = for (outEdge <- outgoingEdges; inEdge <- incomingEdges;
                                if !(outEdge._2 eq node);
                                if !(inEdge._1 eq node)) yield
                (inEdge._1, outEdge._2, inEdge._3 and outEdge._3)

            if (progressOutput) print(newEdges.size)

            nodes.-=(node)
            edges.--=(incomingEdges)
            edges.--=(outgoingEdges)
            edges.++=(newEdges)

        }

        
        edges = new mutable.ArrayBuffer() ++ (compressRedundantEdges(new CFG(cfg.nodes, Set() ++ edges)).edges)
        new CFG(Set() ++ nodes, Set() ++ edges)
    }


    /**
     * multiple edges between two nodes are collapsed into a single edge
     */
    def compressRedundantEdges(cfg: CFG): CFG = {
        val uniqueEdges: Map[(CFGNode, CFGNode), Set[(CFGNode, CFGNode, FeatureExpr)]] = cfg.edges.groupBy(e => (e._1, e._2))
        val newEdges = uniqueEdges.mapValues(es => es.map(_._3).fold(False)(_ or _)).toList.map(a => (a._1._1, a._1._2, a._2))
        new CFG(cfg.nodes, newEdges.toSet)
    }


    //ignoring variability
    final def getReachableNodes(cfg: CFG, node: CFGNode, initResults: Set[CFGNode] = Set()): Set[CFGNode] = {
        val neighbors = cfg.edges.filter(_._1 == node).map(_._2)

        var result = initResults + node
        for (neighbor <- neighbors) {
            if (!(result contains neighbor))
                result = getReachableNodes(cfg, neighbor, result)
        }

        result
    }

    private def addOrMerge(result: Map[CFGNode, FeatureExpr], node: CFGNode, ctx: FeatureExpr): Map[CFGNode, FeatureExpr] =
        result + (node -> (result.getOrElse(node, False) or ctx))
    private def whenContained(result: Map[CFGNode, FeatureExpr], node: CFGNode): FeatureExpr =
        result.getOrElse(node, False)

    final def getVAReachableNodes(cfg: CFG, node: CFGNode, ctx: FeatureExpr, initResults: Map[CFGNode, FeatureExpr] = Map()): Map[CFGNode, FeatureExpr] = {
        val neighborEdges = cfg.edges.filter(_._1 == node)

        var result = addOrMerge(initResults, node, node.fexpr and ctx)
        for ((_, neighbor, edgeFExpr) <- neighborEdges) {
            if (((edgeFExpr and ctx) andNot whenContained(result, neighbor)).isSatisfiable)
                result = getVAReachableNodes(cfg, neighbor, edgeFExpr and ctx, result) // not proceeding with the minimal condition. should not make a difference
        }

        result
    }

}
