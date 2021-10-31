# Luukasa

Luukasa is a simple 2D skeletal animator.

## Background

Once, I had an (very unoriginal) idea for a game,
but I needed to create some animations for the (human) characters and I began to
think about how I'd implement it. I started to code some ad-hoc animations
in C with hard-coded joint constraints that would mimick the limits of human body,
and I promised myself I'd keep it that way and just get it over with.
But I couldn't bear the temptation so I put on my proverbial coding pants and I was
desigining a separate tool to create these animations.

My goal is to create only some simple human character animations with it and
the features reflect my personal needs directly - I'm not creating a very general
tool here. There are options for that out there already if I'm not mistaken and
I could have used one - but I thought this would be a cool program to make.

Finally, for such a simple tool, this has been quite a long project -
I originally started writing this in C few years back but the project was
halt at some point.  Now, I've been learning Haskell slowly on my free time,
and I desperately have been needing something nice and motivating to work on to
get my hands dirty and here we are again.


## Building and running
```
stack build
stack -- exec luukasa-exe
```
You will need GTK development libs installed - at least. There might be something else
too that I'm forgetting (I'll try to update this at some point).
Hopefully, if the build fails, the error messages will hint the missing prerequisites.

## Running tests
There is limited amount of tests. To run them, use
```
stack test
```