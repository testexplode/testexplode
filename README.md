README by Hans-Jürgen Guth, juergen.software@freea2a.de
------ 

TestExplode -- let the set of your test cases explode !

Motivation and what the heck this module does
-----------------------------------------------

"DRY, Don't repeat yourself" is not valid, if you write testcases.
Every time you use the same sort of code, with different values
for variables or another function at one place.
Copy & paste is your friend.

That need not to be!
This module shall enable you to write the structure of whole test sets.

You have two building blocks:

1. variables that can change. Every testcase uses another combination
of variables. For defining the set of variables you can use the
list-comprehension of haskell.

2. Define the structure of your testcases as directed graphs with one
begin and one end.
Every path of the graph forms one testcase. And this testcase is
generated with all the variable-combinations, that you have defined 
with the method of 1.)

Additionally:

3. Why not combine existing testcases with new testcases ?
You can import another testgraphs. You must only cast the types of 
variables that the imported testgraph uses.

4. Defining what the testcase shall do is one thing, defining what the
expected outcome of the testcase is is the other.
To every snippet of the testcase you can add the state of the system 
under test, using the input testdata and the state of the system under
test before that snippet. The state you can use in defining
the result of the test.

The module combines texts, that you write. This texts are snippets
of the testcase. So in the text you can use whatever language you want,
the resulting testcase is in this language.

To make the testcase well commented, you must say, what the 
comment-chars of your text/programming language are. If you write
python or perl this are "# ", in haskell "-- ".

The official modules of this package generate you a sequence of
Texts. Every text is a testcase. To make files out of the text, with a
header and a footer and a nice name, the module FinalIO.hs in the 
doc/examples-directory can be used. Because the module is not so
modular, and every user wants a bit other header and footers,
it is not an official module. But it works for me.
Feel free to change it.

For the same reasons the module VizViews.hs is not an official module
and included in the directory docs/examples.
This module defines attributes for graphviz. By that we come to the 
next feature of TextExplode: Visualization of the testgraph.

The testgraph is a graph and can be printed with graphviz.
Subgraphs can be hidden in a node and with a click on the node you
come to the subgraph. This modules use graphviz and interpret the
testgraphs, so that they can be printed with graphviz.

Installation
-------------

    mkdir TestExplode
    cd TestExplode
    cabal sandbox init
    cabal install TestExplode

should do the job.

Additionally install graphviz with your package manager or on windows 
with the instructions at http://graphviz.org/


Usage
------

There are examples in the directory doc/examples.
I recommend Te_LA.hs as a starting point.
After you have understood the design of a testcase, you can go further
and understand, how testcses are iported with the example Te52.hs.

Of course the most complete docu is the haddock documentation of this
module.

The command for generating the testcases out of Te_LA.hs is:

    runghc Te_LA.hs

The resulting testcases can be found in Te_LA/*.rb.
The resulting Testgraph can be found under Te_LA/Te_LA.svg.

The subgraphs can be found under subgraphs/.


Much fun and I like getting mails about the usage of the module.
Write me, if you like!






 

