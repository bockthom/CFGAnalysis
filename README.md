This package contains an infrastructure to process the
control-flow graph dumped by TypeChef. It has no dependencies
on the internals of the Analysis or AST but works entirely
on the serialized CFG and interface files.

`CFGLinker` is an application that performs the linking for
a casestudy in the default setup. 

To use the application for several casestudies, the filenames of the .rcfg have to be adopted in src/main/scala/CFGLinker.scala - there "busybox" has to be replaced by the name of the casestudy whenever it occurs.
Maybe the path to the filelist-file has to be adopted, too (src/main/scala/CFGLinker.scala Line 20).

