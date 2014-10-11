Rip the Pokémon icons (aka party sprites, box sprites, minis, ...) from
_Pokémon X_, and probably also _Pokémon Y_.  (I don't have a dump of _Y_ to
test it on.)

Requires [JuicyPixels][] (tested with 3.1.7.1).

This is a Haskell port of [pokemon-x-y-icons][], which I wrote in Python.  This
version doesn't keep the palette order — in fact, the PNGs it creates don't use
palettes at all — and doesn't add sBIT chunks.  Unfortunately, at the time of
writing, I can't fix either of these issues with JuicyPixels, and there doesn't
seem to be any better alternative.  (JuicyPixels can handle palettes, but not
with transparency.)

[JuicyPixels]: https://hackage.haskell.org/package/JuicyPixels
[pokemon-x-y-icons]: https://github.com/Zhorken/pokemon-x-y-icons
