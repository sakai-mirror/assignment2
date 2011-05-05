Assignment 2 
=======================================================

.. document notes and metadata are at the bottom

:Author: Damian Sobieralski, Steven Githens

.. contents::



Overview
========

Assignments 2 is a Sakai Module for flexible and robust dissemination of
work in the classroom, and can be used as alternative to the out of the
box Assignments module in Sakai.

Integrations
============

Assignments 2 has a number of integrations, displayed in the matrix below.

===============    =====  =====  ============
Sakai Version      2.7.x  2.8.x  2.9.x(trunk)
---------------    -----  -----  ------------
Assignment 2   
OSP Matrix
OSP Evaluations
Gradebook
Gradebook 2
Turnitin CRS

OSP Matrix Integration 
-----------------------

OSP Evaluations
---------------

Gradebook
---------

Gradebook 2
-----------

Turnitin Content Review Service
-------------------------------

Installation
============


Compatibility
-------------

Compatible with Sakai branches 2.7.x, 2.8.x and 2.9.x/trunk


Prepare Your Environment
------------------------

MySQL
`````

If you are using MySQL as your backend database for Sakai do verify 
that you have your collation set to a case sensitive one (say utf8_bin). 
Many OS distributions of MySQL default the collation to utf_general_ci.  
Renaming of assignments is a case sensitive thing and we need to make 
sure that the database is using case sensitive searching. 

Tools needed
````````````

You will need to have maven2, subversion and patch installed on your system.
Sed and cat will need to be available.  You will also need to be using the 
same local maven repository (by default $HOME/.m2/repository) that you 
used to build your Sakai branch.  Your Sakai branch needs to be built 
before you try to build/install Assignment2.

Obtaining dependencies and patching pom.xmls
````````````````````````````````````````````

For Sakai branches 2.8.x and 2.7.x you will need to download two dependencies
(taggable-2.9.x and content-review-2.9.1) and patch pom.xml files. The script 
prepare_for_sakai_env.sh script is included to accomplish this. The script 
will download into a directory in assignment2 named temp. It then compiles and 
installs into your local maven repository the dependencies.  It will patch 
the dependencies' pom.xml files as well as patch assignment2's pom.xml files 
to work w/ your branch of Sakai.  It will ONLY patch files within the 
assignment2 source tree. It does NOT touch anything outside of it so your 
base Sakai source tree is left untouched.

This assignment2 directory MUST be sitting inside your Sakai source 
tree before you try to run the script.

Compilation
```````````
After your install has been patched as above (if necessary), you can either
add Assignments 2 to your master pom file or build it with the usual

::
  
  mvn -Dmaven.tomcat.home=/your/path/to/tomcat clean install sakai:deploy


Post Install Configuration
```````````````````````````````



.. This document is written in restructured text, and at the moment I'm using the
.. lsr.css stylesheet for the html output.
.. The following is the order for header depths: = - ` : . ' " ~ ^ _ * + #
