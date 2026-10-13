{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Jenga.Frontend.Auth.CompanySignup where
import Reflex.Host.HeadlessEffectful (NotReady)
import Reflex.Host.HeadlessEffectful (Adjustable)

import Jenga.Common.Auth
import Templates.Partials.Checkbox
import Jenga.Common.Errors

import qualified Jenga.Route.Frontend as RouteE (Routed, askRoute)
import qualified Jenga.Route.FrontendMTL as RouteM (Routed, askRoute)
import Reflex.Class (Reflex, Event, Dynamic, current, tag, leftmost, ffor, fanEither)
import qualified Reflex.Class as R
import Reflex.Dom.Core (HasValue(..))

import Control.Monad (forM_, join)
import Data.Proxy (Proxy)
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Dom.Core (InputElement, EventResult)
import Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Dom.Core (DomSpace)
import Reflex.Effectful.DomHost (DomBuilderSpaceE)
import Reflex.Effectful.DomHost (DomBuilderE)
import Reflex.Effectful.DomHost (DomBuilderTimelineE)
import Reflex.Dom.Core (elClass, text)
import Reflex.Host.HeadlessEffectful (PostBuild)


data SignupConfig t = SignupConfig
  { _signupConfig_errors :: Dynamic t (Maybe T.Text)
  , _signupConfig_success :: Event t ()
  }


-- | The DOM-space parameter @s@ used to be pinned to 'GhcjsDomSpace',
-- which forced every downstream consumer's effect row to satisfy
-- @DomBuilderSpaceE es ~ GhcjsDomSpace@ — a leak of the browser-JS
-- runtime out into pure-Reflex / SSR / test contexts. Parameterising
-- on @s@ lets each consumer plug in whatever 'DomSpace' its own
-- 'DomBuilderE' provides (e.g. 'DomBuilderSpaceE es' at the call
-- site), while the FRP function that reads these fields via 'value'
-- remains polymorphic in @s@.
data SignupData s t m = SignupData
  { _signup_email :: InputElement EventResult s t
  , _signup_confirmEmail :: InputElement EventResult s t
  , _signup_orgName :: InputElement EventResult s t
  , _signup_agreeToTerms :: Checkbox t
  , _signup_submit :: Event t ()
  }


-- | TODO(anyone): It would be really cool to populate forms with info they've already entered so that they can move forward faste
-- | we could do this through query params
--
-- | Widened error type: this family now accepts the full
-- 'BackendError AdminSignupError' surface. Previously it was
-- 'BackendError UserSignupError', which forced consumers to write a
-- lossy 'fmapMaybe' adapter that silently dropped the 'InvalidAdminCode'
-- branch of 'AdminSignupError'. Now every branch of
-- 'BackendError AdminSignupError' — including 'InvalidAdminCode' — is
-- rendered to the user via 'ShowUser (BackendError AdminSignupError)'.
-- | Depinned: the previous signature carried
-- @DomBuilderE t (DomBuilderSpaceE es) :> es@,
-- @DomBuilderSpaceE es ~ GhcjsDomSpace@, and @HasPrerender t es es@
-- constraints. None are needed here — the body never builds DOM (it
-- only reads values off already-constructed 'InputElement's, wires
-- 'Event'/'Dynamic' plumbing, and calls 'askRoute'). Dropping them
-- lets non-GhcjsDom callers (SSR, test) use the same FRP, and lets
-- the 'SignupData' 's' parameter float freely.
-- | Effectful variant. See 'newCompanySignup_FRP' for the MTL twin.
newCompanySignup_FRPE
  :: forall req rsp s t es.
     ( Hold t :> es, Sample t :> es
     , PostBuild t :> es
     , RouteE.Routed t (Map.Map T.Text (Maybe T.Text)) :> es
     , Reflex t

     )
  => (NewCompanyEmail -> req)
  -> (Event t req -> Eff es (Event t rsp))
  -> (rsp -> Either (BackendError AdminSignupError) T.Text)
  -> (SignupData s t (Eff es))
  -> Eff es (SignupConfig t)
newCompanySignup_FRPE mkAPI sendRequest castResponse (SignupData email' eConfirm' orgName' agree' submit) = do
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- RouteE.askRoute
  let code = join . Map.lookup "code" <$> queryParams
  let email = value email'
  let eConfirm = value eConfirm'
  let agree = value agree'
  let orgName = value orgName'
  let
    validate = (\(emailA,emailB,orgName_, agree'', code') ->
           if agree'' == False
           then Left "You must accept terms"
           else
             if emailA /= emailB
             then Left "Emails dont match"
             else
               case code' of
                 Nothing -> Left "No organization code found, please get link from lauren@aceinterviewprep.io"
                 Just code'' ->
                   case EmailValidate.validate $ T.encodeUtf8 emailA of
                     Left _ -> Left "Invalid Email"
                     Right e ->
                       Right $ NewCompanyEmail e orgName_ code''
               )
    valid = fmap validate $ (,,,,) <$> email <*> eConfirm <*> orgName <*> agree <*> code
  let (bad, good') = fanEither (tag (current valid) submit)
  let request = ffor good' mkAPI
  response <- sendRequest request
  let (errRes, goodResponse) = fanEither (fmap castResponse response) :: (Event t (BackendError AdminSignupError), Event t T.Text)
  -- 'showUser . Req . Request_ErrorAPI' renders every branch of
  -- 'BackendError AdminSignupError', including 'BUserError
  -- InvalidAdminCode' (which was previously dropped by the consumer's
  -- 'fmapMaybe' adapter).
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser . Req . Request_ErrorAPI <$> errRes
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv

  pure $ SignupConfig errors (() <$ goodResponse)

-- | MTL twin of 'newCompanySignup_FRPE'.
newCompanySignup_FRP
  :: forall req rsp s t m.
     ( RouteM.Routed t (Map.Map T.Text (Maybe T.Text)) m
     , R.MonadHold t m
     , Reflex t
     )
  => (NewCompanyEmail -> req)
  -> (Event t req -> m (Event t rsp))
  -> (rsp -> Either (BackendError AdminSignupError) T.Text)
  -> (SignupData s t m)
  -> m (SignupConfig t)
newCompanySignup_FRP mkAPI sendRequest castResponse (SignupData email' eConfirm' orgName' agree' submit) = do
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- RouteM.askRoute
  let code = join . Map.lookup "code" <$> queryParams
  let email = value email'
  let eConfirm = value eConfirm'
  let agree = value agree'
  let orgName = value orgName'
  let
    validate = (\(emailA,emailB,orgName_, agree'', code') ->
           if agree'' == False
           then Left "You must accept terms"
           else
             if emailA /= emailB
             then Left "Emails dont match"
             else
               case code' of
                 Nothing -> Left "No organization code found, please get link from lauren@aceinterviewprep.io"
                 Just code'' ->
                   case EmailValidate.validate $ T.encodeUtf8 emailA of
                     Left _ -> Left "Invalid Email"
                     Right e ->
                       Right $ NewCompanyEmail e orgName_ code''
               )
    valid = fmap validate $ (,,,,) <$> email <*> eConfirm <*> orgName <*> agree <*> code
  let (bad, good') = fanEither (tag (current valid) submit)
  let request = ffor good' mkAPI
  response <- sendRequest request
  let (errRes, goodResponse) = fanEither (fmap castResponse response) :: (Event t (BackendError AdminSignupError), Event t T.Text)
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser . Req . Request_ErrorAPI <$> errRes
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- R.holdDyn Nothing errorsEv
  pure $ SignupConfig errors (() <$ goodResponse)

-- | Render the Ace Terms of Service + Privacy Policy inside a scrollable
-- panel. Called from the signup pages so the user can inspect the terms
-- before ticking the "I agree" checkbox.
--
-- The 'Proxy t' argument pins the reflex timeline: 'elClass' has a
-- '' fundep that infers @t@ from @es@ at the call
-- site — with the 'Proxy' present, the caller supplies both, so no
-- ambiguity leaks out of this function.
--
-- Content mirrors 'Jenga.Frontend.Auth.UserSignup.seeTerms' — kept
-- duplicated (rather than re-exported) so each signup surface can
-- evolve its terms text independently if company vs. user policies
-- diverge later.
-- | Depinned: dropped @DomBuilderSpaceE es ~ GhcjsDomSpace@. The body
-- uses only 'elClass' and 'text', both of which are polymorphic in the
-- DOM space and require no JS interop, so any 'DomBuilderE' whose
-- 'DomSpace' is a 'DomSpace' will render this content correctly.
seeTerms
  :: forall t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es, NotReady t :> es, Adjustable t :> es
     , DomSpace (DomBuilderSpaceE es)
     , t ~ DomBuilderTimelineE es
     
     , Reflex t
     )
  => Proxy t -> Eff es ()
seeTerms _proxy =
  elClass "div" "bg-gray-100 flex items-center justify-center" $
    elClass "div" "bg-white p-8 rounded-3xl w-full overflow-y-auto h-[50vh]" $ do
      elClass "h1" "text-2xl font-[Sarabun] mb-4 text-gray-700" $
        text "TERMS OF SERVICE AND PRIVACY POLICY"
      elClass "p" "text-base font-[Sarabun] mb-2 text-gray-700" $
        text "Last Updated: March 23, 2023"
      forM_ paragraphsAgree $
        elClass "p" "text-base font-[Sarabun] mb-2 text-gray-700" . text
  where
    paragraphsAgree :: [T.Text]
    paragraphsAgree =
      [ "This document (\"Agreement\") is an agreement between you (\"User\" or \"You\") and Ace, Inc., (\"Company,\" \"We,\" \"Us,\" or \"Our\"), governing your use of the Ace, Inc. platform and related services (collectively, the \"Services\")."
      , "By accessing or using the Services, you acknowledge that you have read, understood, and agree to be bound by this Agreement. If you do not agree to these terms, you are not permitted to use the Services."
      , "Eligibility"
      , "To use the Services, you must be at least 13 years of age. By using the Services, you represent and warrant that you meet these age requirements and can form legally binding contracts under applicable law. If you are using the Services on behalf of a company, organization, or other legal entity, you represent and warrant that you have the authority to bind that entity to this Agreement."
      , "Account Registration"
      , "To access certain features of the Services, you may be required to register for an account. By registering, you agree to provide accurate, current, and complete information about yourself and maintain the security of your account credentials. You are responsible for all activities that occur under your account, whether or not you authorize such activities."
      , "Content"
      , "User-Generated Content"
      , "The Services allow you to create, upload, post, send, or otherwise transmit content, including videos, messages, and other materials (\"User Content\"). By submitting User Content, you represent and warrant that you own or have the necessary rights to use and distribute such content."
      , "License Grant"
      , "By submitting User Content, you grant the Company a worldwide, non-exclusive, royalty-free, sublicensable, and transferable license to use, reproduce, distribute, prepare derivative works of, display, and perform the User Content in connection with the Services and the Company's (and its successors' and affiliates') business, including for promoting and redistributing part or all of the Services."
      , "Content Restrictions"
      , "You agree not to post, upload, or transmit any User Content that is unlawful, harmful, threatening, abusive, harassing, defamatory, vulgar, obscene, libelous, invasive of another's privacy, hateful, or racially, ethnically, or otherwise objectionable."
      , "Prohibited Conduct"
      , "You agree not to engage in any of the following activities while using the Services:"
      , "Using the Services for any purpose that is unlawful or prohibited by this Agreement;"
      , "Interfering with or disrupting the Services or servers or networks connected to the Services;"
      , "Attempting to gain unauthorized access to any portion of the Services or any other user's account;"
      , "Using the Services to harass, abuse, or harm other individuals;"
      , "Creating multiple accounts or using automated systems to access the Services."
      , "Intellectual Property Rights"
      , "The Services, including all content, features, and functionality (including but not limited to all information, software, text, displays, images, video, and audio, and the design, selection, and arrangement thereof), are owned by the Company, its licensors, or other providers of such material and are protected by copyright, trademark, patent, trade secret, and other intellectual property or proprietary rights laws."
      , "Privacy Policy"
      , "Please refer to our Privacy Policy, which is incorporated into this Agreement by reference, for information on how we collect, use, and share your personal information."
      , "Termination"
      , "The Company may terminate your access to the Services at any time, with or without cause, and without prior notice. You may terminate your account by following the instructions on the Services. Upon termination, you shall cease all use of the Services and any content or materials obtained through the Services. Any provisions of this Agreement that, by their nature, should survive termination shall survive termination, including, without limitation, ownership provisions, warranty disclaimers, indemnity, and limitations of liability."
      , "Disclaimers"
      , "The Services are provided on an 'as is' and 'as available' basis, without warranty of any kind, express or implied, including, but not limited to, warranties of merchantability, fitness for a particular purpose, title, and non-infringement. The Company does not warrant that the Services will be error-free, uninterrupted, secure, or free from viruses or other harmful components."
      , "Limitation of Liability"
      , "In no event shall the Company, its affiliates, or their respective officers, directors, employees, agents, licensors, or service providers be liable for any direct, indirect, incidental, special, consequential, or punitive damages, including, without limitation, loss of profits, data, use, goodwill, or other intangible losses, resulting from your access to or use of, or inability to access or use, the Services, any conduct or content of any third party on the Services, or unauthorized access, use, or alteration of your transmissions or content, whether based on warranty, contract, tort (including negligence), or any other legal theory."
      , "Indemnification"
      , "You agree to defend, indemnify, and hold harmless the Company, its affiliates, licensors, and service providers, and its and their respective officers, directors, employees, contractors, agents, licensors, suppliers, successors, and assigns from and against any claims, liabilities, damages, judgments, awards, losses, costs, expenses, or fees (including reasonable attorneys' fees) arising out of or relating to your violation of this Agreement or your use of the Services, including, but not limited to, your User Content, any use of the Services' content, services, and products other than as expressly authorized in this Agreement, or your use of any information obtained from the Services."
      , "Governing Law and Jurisdiction"
      , "This Agreement shall be governed by and construed in accordance with the laws of Canada, without giving effect to any principles of conflicts of law. You agree that any action at law or in equity arising out of or relating to this Agreement shall be filed only in the state or federal courts located in Canada and you hereby consent and submit to the personal jurisdiction of such courts for the purposes of litigating any such action."
      , "Changes to this Agreement"
      , "We reserve the right to modify this Agreement at any time, and any such changes will be effective immediately upon posting. By continuing to use the Services after any changes, you agree to be bound by the revised Agreement. If you do not agree to the new terms, you must stop using the Services."
      , "Miscellaneous"
      , "This Agreement constitutes the entire agreement between you and the Company concerning your use of the Services and supersedes all prior or contemporaneous communications, proposals, and agreements between you and the Company with respect to the subject matter hereof. If any provision of this Agreement is found to be unlawful, void, or for any reason unenforceable, then that provision shall be deemed severable from this Agreement and shall not affect the validity and enforceability of any remaining provisions. No waiver by the Company of any term or condition set forth in this Agreement shall be deemed a further or continuing waiver of such term or condition or a waiver of any other term or condition, and any failure of the Company to assert a right or provision under this Agreement shall not constitute a waiver of such right or provision."
      ]
