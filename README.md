# Twitter Streaming for Haskell

An interface ot the
[Twitter streaming API](http://dev.twitter.com/pages/streaming_api)
for Haskell. The status objects are of the same type as those in
[hs-twitter](http://hackage.haskell.org/package/hs-twitter).

The library provides an [iteratee](http://okmij.org/ftp/Streams.html) 
to extract status objects from a stream of characters as well as a 
driver to stream HTTP respones. This allows for a simple and efficient 
interface:

    import qualified Web.TwitterStream as Stream
    ...

    Stream.driver (Stream.BasicAuth "user" "pass") Stream.Sample $ do
      status <- Stream.status
      case status of
        Just status -> do something...
        Nothing -> parse fail..

Build via cabal:

    $ cabal configure && cabal build

The build the included example:

    $ ghc --make stream.hs
    $ ./stream username password
