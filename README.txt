
This file contains information about how to install the Scala BIP framework
and how to run example programs.

=== Installation ===

To install Scala and the Scala framework, please follow the following guide.


--- Copying the directory to the hard drive ---

Before we can actually install Scala and the Scala BIP framework,
please ensure that the directory containing this README file has been
copied to your local hard drive. Some of the commands must be issued
from this directory.


--- Installing SBT ---

To install the framework and run examples, SBT, a Scala build tool,
is required. This build tool can be downloaded and installed from:
    
    http://www.scala-sbt.org/download.html

SBT can be installed in many different ways, which are described on
their website. Please follow the instructions proposed for your
prefered installation technique.

SBT will install the Scala compiler and libraries needed automatically
when needed.


--- Ensuring Java is installed ---

Note that SBT assumes Java to be installed on the system. If it is not
the case, SBT will issue an error when run. If that was to happen,
please follow the instructions from the Oracle website to install the Java JRE:
	
	http://www.oracle.com/technetwork/java/javase/downloads/jre7-downloads-1880261.html


=== Running the examples ===

To run the Scala examples presented in the thesis, you can issue the following
simple command (from the directory which contains this README file): 

	sbt run

The above command will ask you which example should be run.
Simply make your choice by either entering 1 or 2.
Note that the two examples will be running forever.
To kill them, simply issue a kill signal (CTRL+C in general).

Also note that the first time you run the command, SBT will have to
download most of its code, the Scala compiler and libraries.
It might take some time.

