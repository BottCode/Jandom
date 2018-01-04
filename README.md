# Jandom

This is Jandom, an abstract interpretation based static analyzer written
in Scala. To compile it, you need ''sbt'' (Scala Build Tools) 0.13.5 or 
later [http://www.scala-sbt.org/].

In the main directoy of Jandom, execute "sbt run" to compile and execute
Jandom. If asked about the program to run, choose "JandomGUI". It is 
possible to generate an Eclipse project from the shell with the command 
"sbt buildinfo eclipse".

If the PPL (Parma Polyhedra Library) is installed, it will be used to
extend the available numerical domains. Jandom requies PPL 1.1 or
later [http://bugseng.com/products/ppl/download/ftp/].

