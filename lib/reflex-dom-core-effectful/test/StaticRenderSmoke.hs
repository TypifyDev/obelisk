{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Live smoke test for 'Reflex.Effectful.DomHost.renderStatic':
-- actually runs the static renderer and asserts on the resulting
-- byte-string. Complements 'DomHostSmoke' (which is link-only for the
-- immediate-mode 'mainWidget' surface).
module Main (main) where

import qualified Reflex.Effectful.DomHost as DH
import qualified Reflex.Host.HeadlessEffectful as HH
import qualified Reflex.Class as R
import qualified Reflex.Spider.Internal as R.SI
import qualified Data.ByteString.Char8    as BS8
import qualified Data.Map                 as Map
import           System.Exit              (exitFailure)

main :: IO ()
main = do
  ((), bs1) <- DH.renderStatic $ DH.el "div" (DH.text "hello")
  check "el div (text hello)" bs1 "<div>hello</div>"

  ((), bs2) <- DH.renderStatic $ DH.elClass "div" "greeting" (DH.text "world")
  check "elClass div greeting world" bs2 "<div class=\"greeting\">world</div>"

  ((), bs3) <- DH.renderStatic $ DH.el "p" (DH.text "server-side")
  check "el p (text server-side)" bs3 "<p>server-side</p>"

  ((), bs4) <- DH.renderStatic $
    DH.el "html" $ do
      DH.el "head" (DH.text "")
      DH.el "body" (DH.text "hi")
  check "html/head/body nested" bs4 "<html><head></head><body>hi</body></html>"

  ((), bs5) <- DH.renderStatic $
    DH.elAttr "a" (Map.singleton "href" "https://example.com") (DH.text "link")
  check "elAttr a href" bs5 "<a href=\"https://example.com\">link</a>"

  ((), bs6) <- DH.renderStatic $ DH.el "br" (pure ())
  check "el br (void element)" bs6 "<br />"

  ((), bs7) <- DH.renderStatic $ DH.el "div" $ DH.text "<script>x</script>"
  check "el div (HTML-escaped text)" bs7 "<div>&lt;script&gt;x&lt;/script&gt;</div>"

  -- Test 8: a widget that uses 'prerender_' with a client body that
  -- calls JS-flavoured effects (getPostBuild + performEventE_ +
  -- liftJSM). On the STATIC render, the client body is discarded via
  -- 'UnrunnableEs t' (its interpreter is unreachable); the server body
  -- is bracketed by <!--prerender/start--> and <!--prerender/end-->
  -- comments so the client-side hydrator knows where to splice in the
  -- real client widget after switchover.
  ((), bs8) <- DH.renderStatic $ do
    DH.el "div" (DH.text "Video call app")
    DH.prerender_ @DH.StaticDomTimeline @DH.StaticDomSpace
      -- Server body: a noscript fallback the browser shows until
      -- hydration replaces it with the real client widget.
      (DH.el "noscript" (DH.text "JavaScript required"))
      -- Client body: does JS. Typechecks against 'UnrunnableEs' but
      -- never runs during static render. In real life this would set
      -- up an RTCPeerConnection or similar. Presence in the source
      -- proves the client body can carry MonadJSM + PerformEvent +
      -- PostBuild effects without breaking static rendering.
      (do ev <- HH.getPostBuildE @DH.StaticDomTimeline
          -- Callback runs in Performable (Eff es) = SpiderHostFrame Global,
          -- so 'DH.liftJSM' doesn't apply here directly — but this whole
          -- body is inside UnrunnableEs and never actually executes on the
          -- static server, so a no-op suffices to show the shape.
          HH.performEventEffE_ @'[IOE] (R.ffor ev (\_ -> pure ()))
      )
  check "el div + prerender_ (JS client body, discarded server-side)" bs8
        "<div>Video call app</div><!--prerender/start--><noscript>JavaScript required</noscript><!--prerender/end-->"

  putStrLn "renderStatic smoke test PASSED (8 cases)."
  where
    check :: String -> BS8.ByteString -> BS8.ByteString -> IO ()
    check label got want
      | got == want = pure ()
      | otherwise   = do
          putStrLn $ "FAIL: " <> label
          putStrLn $ "  got:    " <> BS8.unpack got
          putStrLn $ "  wanted: " <> BS8.unpack want
          exitFailure
