Resistance
==========

A web app for playing The Resistance, a game designed by Don Eskridge. Written in Ur/Web, a language for web apps that catches most non-business-logic bugs at compile time.

Current version is very raw (e.g. finished games just accumulate in database) but does work! That is, all necessary player actions for the basic game work and each player receives exactly the information they need to receive in order to do those actions, so you can use this to actually play the game.

The back-end is susceptible to maliciously submitted requests and nothing is encrypted, so cheating is about as easy and just as little fun as in the physical card game.

Testing note: when playing with more than 5 players in the same browser, Ur/Web's message passing gets slowed down considerably the browser's connection limit, often in a way that breaks the application. The number 5 may vary slightly from browser to browser. I've successfully tested larger games by having half the players open in one browser and half in another.
