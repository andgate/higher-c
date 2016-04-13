# Introduction to Hawk

Let's dive into Hawk! A good way to get a feel for the basic langauge is to write
some code. First, let's look at what a hello world in Hawk looks like.

hello.hk
```
.: HelloWorld

:. System.Print

main: Void :=
  print "Hello World"
```

This is a very simple program, but let's take it line by line. The first line
defines the current module as "HelloWorld". If this was mis