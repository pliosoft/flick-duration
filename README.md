# `flick-duration` [![Build Status](https://travis-ci.com/pliosoft/flick-duration.svg?branch=master)](https://travis-ci.com/pliosoft/flick-duration) [![Hackage](https://img.shields.io/hackage/v/flick-duration.svg?style=flat)](https://hackage.haskell.org/package/flick-duration) 

[Buy me a coffee](https://www.buymeacoffee.com/clord).

The unit of time called the [Flick](https://en.wikipedia.org/wiki/Flick_(time)) is equivalent to exactly 1/705,600,000 of a second. 

Flicks are a small unit of time that are very evenly divisible by common file format time durations; i.e., for common durations they will have no rounding.

The figure was chosen so that frequencies of 24, 25, 30, 48, 50, 60, 90, 100 and 120 Hz, as well as 1/1000 divisions of all those, can be represented with integers.
A flick is approximately 1.417 ns.

Due to the Nyquist Limit, this type should not be used for frequencies higher than ~400MHz.

In a 64bit type, the Flick can represent durations as an exact count up to ~414 years in length. This library
implements them with the Haskell Integer type, which is unbounded.

A similar unit for integer representation of temporal points was proposed in 2004 under the name TimeRef, splitting a second into 14,112,000 parts.
This makes 1 TimeRef equivalent to 50 Flicks.

First I saw of this idea was
[Christopher Horvath](https://www.facebook.com/christopher.horvath.395/posts/1157292757692660])'s description.

* 24 fps frame:     29,400,000 flicks
* 25 fps frame:     28,224,000 flicks
* 30 fps frame:     23,520,000 flicks
* 48 fps frame:     14,700,000 flicks
* 50 fps frame:     14,112,000 flicks
* 60 fps frame:     11,760,000 flicks
* 90 fps frame:      7,840,000 flicks
* 100 fps frame:     7,056,000 flicks
* 120 fps frame:     5,880,000 flicks

We can also do common audio rates with precise numbers of flicks:

* 8000 hz:      88,200 flicks
* 16000 hz:     44,100 flicks
* 22050 hz:     32,000 flicks
* 24000 hz:     29,400 flicks
* 32000 hz:     22,050 flicks
* 44100 hz:     16,000 flicks
* 48000 hz:     14,700 flicks
* 88200 hz:      8,000 flicks
* 96000 hz:      7,350 flicks
* 192000 hz:     3,675 flicks
