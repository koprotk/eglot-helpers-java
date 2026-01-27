# eglot-helpers-java
My custom functions to work with eglot and java (not related to eglot-java.el project)

## Requirements
You need to have installed `jdtls` LSP server some package install it for you, not is this case. I preferd left the SO do that.

## Setup
If you work with `lombok` or an `-javaagent` url parameter you need to setuo the url of the jar file, eg:
`(setq eglot-helpers-java-lombok-jar-path "/Users/name/.m2/repository/org/projectlombok/lombok/1.18.36/lombok-1.18.36.jar")`

## Installation
The same way as any package on emacs that is not ELPA.
