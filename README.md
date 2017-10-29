# DarbPong-GB
A very, very bad implementation of Pong in Gameboy-flavoured z80. Can be compiled and linked with wla-z80.

## Features
* Moving paddles!
* A ball that you can sometimes hit with a paddle!
* Unbeatable AI (literally)!
* No pressure gameplay, because there's no scoring mechanism!
* No boring title screen or sound effects!

## Requirements
* [WLA-DX](http://www.villehelin.com/wla.html) by Ville Helin

## Compilation instructions
    ./wla-gb -o main.s && ./wlalink Linkfile DarbPong.gb

Then run DarbPong.gb with your favourite Gameboy emulator and prepare to be amazed!

## To implement
* Enemy paddle AI - Currently perfectly follows the ball, so the player can't score
* Better collision detection
* Variable ball speed
* Scoring (!) - Ball currently bounces off the left wall
* Sound effects and music
  
## Thanks to...
* feeb and their [Gameboy hello world demo](http://pp.feeb.dog/gb_z80_helloworld.txt) 
* The Academy? No, that can't be right...