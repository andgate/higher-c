# Introduction to Hawk

Let's dive into Hawk! A good way to get a feel for the basic langauge is to write
some code. First, let's look at what a hello world in Hawk looks like. In a file
named ```hello.hk```, write the following
```
.: HelloWorld

:. System.Print

main: Void :=
  print "Hello World"
```

This is a very simple program, but let's take it line by line. The first line
defines the current module as "HelloWorld" using the ```.:``` symbol, which means
"module". Hawk is big on using symbols whenever it can. This may seem harder to
remember at first, but you will get accustom to them.