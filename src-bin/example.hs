{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bool (bool)
import Control.Comonad.Cofree (Cofree ((:<)))
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Functor.Identity  (Identity)
import           Data.Functor.Misc
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Zipper       as TZ
import           Data.Time              (getCurrentTime)
import           Debug.Trace            (trace, traceShowId)
import qualified Graphics.Vty           as V
import           Reflex
import           Reflex.Network
import           Reflex.Vty
import Telomare (IExpr(..))
import qualified Telomare               as Tel
import qualified Telomare.Eval          as TE
import qualified Telomare.Parser        as TP
import Telomare.Parser (UnprocessedParsedTerm(..), UnprocessedParsedTermF(..))
import           Example.CPU

type VtyExample t m =
  ( MonadFix m
  , MonadHold t m
  , Reflex t
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocus t m
  , HasFocusReader t m
  , HasTheme t m
  )

type Manager t m =
  ( HasLayout t m
  , HasFocus t m
  )

data Example = Example_TextEditor
             | Example_Todo
             | Example_Node
             | Example_ScrollableTextDisplay
             | Example_ClickButtonsGetEmojis
             | Example_CPUStat
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

withCtrlC :: (Monad m, HasInput t m, Reflex t) => m () -> m (Event t ())
withCtrlC f = do
  inp <- input
  f
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _                               -> Nothing

darkTheme :: V.Attr
darkTheme = V.Attr {
  V.attrStyle = V.SetTo V.standout
  , V.attrForeColor = V.SetTo V.black
  , V.attrBackColor = V.Default
  , V.attrURL = V.Default
}

thisIsIt :: IO ()
thisIsIt = mainWidget $ (initManager_ :: forall t a m. (HasDisplayRegion t m, Reflex t, MonadHold t m, MonadFix m) => Layout t (Focus t m) a -> m a) $ do
  getout <- ctrlc
  tile flex $ box (pure roundedBoxStyle) $ do
    row . grout flex . text $ "Hola"
    row $ do
      rec grout flex $ text numClicksText
          buttonClicked :: Event t () <- tile flex $ textButton def "Count"
          numClicks <- count buttonClicked
          let numClicksText = current $ fmap (T.pack . show) numClicks
      pure ()
  return $ fmap (\_ -> ()) getout

nodify :: Cofree UnprocessedParsedTermF (Int, Either String IExpr) -> [Node]
nodify = fmap go . allNodes 0
  where go :: (Int, Cofree UnprocessedParsedTermF (Int, Either String IExpr)) -> Node
        go (i, x@(anno :< uptf)) = Node ( T.pack
                                        . (join (replicate i "  ") <>)
                                        . head
                                        . lines
                                        . show
                                        . TP.MultiLineShowUPT
                                        . Tel.forget
                                        $ x
                                        )
                                        ( T.pack
                                        . (join (replicate i "  ") <>)
                                        . ("-- " <>)
                                        . flattenEitherStringString
                                        . fmap show
                                        . snd
                                        $ anno
                                        )
                                        False
        allNodes :: Int -- * Indentation
                 -> Cofree UnprocessedParsedTermF (Int, Either String IExpr)
                 -> [(Int, Cofree UnprocessedParsedTermF (Int, Either String IExpr))]
        allNodes i = \case
          x@(anno :< (ITEUPF a b c)) -> (i, x) : allNodes (i + 1) a <> allNodes (i + 1) b <> allNodes (i + 1) c
          x@(anno :< (ListUPF l)) -> (i, x) : (join $ allNodes (i + 1) <$> l)
          x@(anno :< (LetUPF l a)) -> (i, x) : allNodes (i + 1) a <> (join $ allNodes (i + 1) <$> (snd <$> l))
          x@(anno :< (CaseUPF a l)) -> (i, x) : allNodes (i + 1) a <> (join $ allNodes (i + 1) <$> (snd <$> l))
          x@(anno :< (LamUPF _ a)) -> (i, x) : allNodes (i + 1) a
          x@(anno :< (AppUPF a b)) -> (i, x) : allNodes (i + 1) a <> allNodes (i + 1) b
          x@(anno :< (UnsizedRecursionUPF a b c)) -> (i, x) : allNodes (i + 1) a <> allNodes (i + 1) b <> allNodes (i + 1) c
          x@(anno :< (CheckUPF a b)) -> (i, x) : allNodes (i + 1) a <> allNodes (i + 1) b
          x@(anno :< (LeftUPF a)) -> (i, x) : allNodes (i + 1) a
          x@(anno :< (RightUPF a)) -> (i, x) : allNodes (i + 1) a
          x@(anno :< (TraceUPF a)) -> (i, x) : allNodes (i + 1) a
          x@(anno :< (HashUPF a)) -> (i, x) : allNodes (i + 1) a
          x@(anno :< (IntUPF _)) -> (i, x) : []
          x@(anno :< (VarUPF _)) -> (i, x) : []
          x@(anno :< (PairUPF a b)) -> (i, x) : allNodes (i + 1) a <> allNodes (i + 1) b

flattenEitherStringString :: Either String String -> String
flattenEitherStringString = \case
  Right str -> str
  Left str  -> str

main :: IO ()
main = mainWidget $ withCtrlC $ do
  initManager_ $ do
    tabNavigation
    let gf = grout . fixed
        t = tile flex
        buttons = col $ do
          gf 3 $ col $ do
            gf 1 $ text "Select an example."
            gf 1 $ text "Esc will bring you back here."
            gf 1 $ text "Ctrl+c to quit."
          a <- t $ textButtonStatic def "Todo List"
          f <- t $ textButtonStatic def "Nodes"
          b <- t $ textButtonStatic def "Text Editor"
          c <- t $ textButtonStatic def "Scrollable text display"
          d <- t $ textButtonStatic def "Clickable buttons"
          e <- t $ textButtonStatic def "CPU Usage"
          return $ leftmost
            [ Left Example_Todo <$ a
            , Left Example_Node <$ f
            , Left Example_TextEditor <$ b
            , Left Example_ScrollableTextDisplay <$ c
            , Left Example_ClickButtonsGetEmojis <$ d
            , Left Example_CPUStat <$ e
            ]
    let escapable w = do
          void w
          i <- input
          return $ fforMaybe i $ \case
            V.EvKey V.KEsc [] -> Just $ Right ()
            _                 -> Nothing
    rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
          Left Example_Todo -> escapable taskList
          Left Example_Node -> escapable . nodeList $ nodes0Aux
          -- Left Example_TextEditor -> escapable $ localTheme (const (constant darkTheme)) testBoxes
          Left Example_TextEditor -> escapable testBoxes
          Left Example_ScrollableTextDisplay -> escapable scrolling
          Left Example_ClickButtonsGetEmojis -> escapable easyExample
          Left Example_CPUStat -> escapable cpuStats
          Right () -> buttons
    return ()

-- * Mouse button and emojis example
easyExample :: (VtyExample t m, Manager t m, MonadHold t m) => m (Event t ())
easyExample = do
  row $ grout (fixed 39) $ col $ do
    (a1,b1,c1) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "POTATO"
      b <- tile flex $ btn "TOMATO"
      c <- tile flex $ btn "EGGPLANT"
      return (a,b,c)
    (a2,b2,c2) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "CHEESE"
      b <- tile flex $ btn "BEES"
      c <- tile flex $ btn "MY KNEES"
      return (a,b,c)
    (a3,b3,c3) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "TIME"
      b <- tile flex $ btn "RHYME"
      c <- tile flex $ btn "A BIG CRIME"
      return (a,b,c)
    tile (fixed 7) $ boxTitle (constant def) "CLICK BUTTONS TO DRAW*" $ do
      outputDyn <- foldDyn (<>) "" $ mergeWith (<>)
        [a1 $> "\129364", b1 $> "🍅", c1 $> "🍆", a2 $> "\129472", b2 $> "🐝🐝", c2 $> "💘", a3 $> "⏰", b3 $> "📜", c3 $> "💰🔪🔒"]
      text (current outputDyn)
    tile flex $ text "* Requires font support for emojis. Box may render incorrectly unless your vty is initialized with an updated char width map."
  inp <- input
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _                               -> Nothing
  where
    btn label = do
      let cfg = def { _buttonConfig_focusStyle = pure doubleBoxStyle }
      buttonClick <- textButtonStatic cfg label
      keyPress <- keyCombos $ Set.fromList
        [ (V.KEnter, [])
        , (V.KChar ' ', [])
        ]
      pure $ leftmost [() <$ buttonClick, () <$ keyPress]

-- * Task list example
taskList
  :: ( VtyExample t m
     , Manager t m
     , MonadHold t m
     , Adjustable t m
     , PostBuild t m
     )
  => m ()
taskList = col $ do
  let todos0 =
        [ Todo "Find reflex-vty" True
        , Todo "Become functional reactive" False
        , Todo "Make vty apps" False
        ]
      btn = textButtonStatic def "Add another task"
  enter <- fmap (const ()) <$> key V.KEnter
  rec grout flex . todos todos0 $ enter <> click
      click <- tile (fixed 3) btn
  pure ()

nodes0Aux =
  [ Node "PairUP" "IExpr Foo" False
  , Node "  IntUP 0" "IExpr Bar" True
  , Node "  IntUP 1" "IExpr Baz" False
  ]

data Node = Node
  { _node_label  :: Text
  , _node_eval   :: Text
  , _node_expand :: Bool
  }
  deriving (Show, Eq)

data NodeOutput t = NodeOutput
  { _nodeOutput_node    :: Dynamic t Node
  -- , _nodeOutput_expand  :: Event t ()
  , _nodeOutput_expand  :: Event t Bool
  , _nodeOutput_focusId :: FocusId
  }

node :: forall t m. ( VtyExample t m
                    , HasLayout t m
                    , HasInput t m
                    , HasImageWriter t m
                    )
     => Node
     -> m (NodeOutput t)
node n0 = do
  res <- row $ do
    (fid, _) <- tile' ( fixed
                      . pure
                      . (+1)
                      . T.length
                      . _node_label
                      $ n0
                      ) $ do
      grout flex . text . pure . _node_label $ n0
      pure ()
    value :: Dynamic t Bool <- tile (fixed 4) $ checkbox def $ _node_expand n0
    pure $ NodeOutput
      { _nodeOutput_node = Node (_node_label n0) (_node_eval n0) <$> value
      , _nodeOutput_expand = updated value
      , _nodeOutput_focusId = fid
      }
  expandTextDyn :: Dynamic t Text
    <- fmap (bool "" (_node_eval n0)) <$>
         holdDyn (_node_expand n0) (_nodeOutput_expand res)
  row $
     tile flex
   . grout flex
   -- . box (pure roundedBoxStyle)
   . text
   . current
   $ expandTextDyn
  pure res

nodes :: forall t m.
         ( MonadHold t m
         , Manager t m
         , VtyExample t m
         , Adjustable t m
         , PostBuild t m
         -- , HasInput t m
         )
      => [Node]
      -> m (Dynamic t (Map Int (NodeOutput t)))
nodes nodes0 = do
  let nodeMaps0 = Map.fromList $ zip [0..] nodes0
  rec
    listOut :: Dynamic t (Map Int (NodeOutput t))
      <- listWithKey dynMapList $ \k (dn :: Dynamic t Node) -> do
           (_, eno :: Event t (NodeOutput t))
             <- runWithReplace (grout (fixed 2) . text $ "HOLA") (updated $ grout (fixed 2) . node <$> dn)
           -- dno :: Dynamic t (NodeOutput t) <- networkHold undefined -- (pure $ Map.lookup k nodeMaps0)
           --                                                (updated $  grout (fixed 2) . node <$> dn)
           undefined
    -- listOut :: Dynamic t (Map Int (NodeOutput t))
    --   <- listHoldWithKey nodeMaps0 eventMapMaybeNodes $ \_ v -> do
    --        no <- grout (fixed 2) $ node v
    --        pure no
    -- let eventMapMaybeNodes :: Event t (Map Int (Maybe Node))
        -- eventMapMaybeNodes = coincidence $
    let -- dynMapListOut :: Dynamic t (Map Int Node)
        dynMapList :: Dynamic t (Map Int Node)
        dynMapList = joinDynThroughMap $ fmap _nodeOutput_node <$> listOut
    eventMapMaybeNodes :: Event t (Map Int (Maybe Node)) <- switchHold never $
          mergeMap . fmap (fmap Just . updated . _nodeOutput_node) <$> (updated listOut)
  pure listOut

nodeList :: ( VtyExample t m
            , Manager t m
            , MonadHold t m
            , Adjustable t m
            , PostBuild t m
            , HasInput t m
            )
         => [Node] -> m ()
nodeList nodes0 = col $ do
  grout flex $ nodes nodes0
  pure ()

evaluare :: IO ()
evaluare = mainWidget $ initManager_ $ do
  let cfg = def
        { _textInputConfig_initialValue =
          "Telomare code here"
        }
      -- textBox :: Layout t (Focus t m) a
      textBox = boxTitle (pure roundedBoxStyle) "Text Edit" $
        multilineTextInput cfg
      -- btn :: Text -> m (Event t ())
      btn label = do
        let cfg' = def { _buttonConfig_focusStyle = pure doubleBoxStyle }
        buttonClick <- textButtonStatic cfg' label
        keyPress <- keyCombos $ Set.fromList
          [ (V.KEnter, [])
          , (V.KChar ' ', [])
          ]
        pure $ leftmost [() <$ buttonClick, () <$ keyPress]
      escOrCtrlcQuit :: (Monad m, HasInput t m, Reflex t) => m (Event t ())
      escOrCtrlcQuit = do
        inp <- input
        pure $ fforMaybe inp $ \case
          V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
          V.EvKey (V.KEsc) []             -> Just ()
          _                               -> Nothing
  getout <- escOrCtrlcQuit
  tile flex $ box (pure roundedBoxStyle) $ row $ do
    rec
      runWithReplace (grout flex . col . text $
                       "Write some Telomare code and interact with the generated AST")
                     (sequence . fmap nodeList <$> telomareNodes)
      telomareNodes :: Event t (Either String [Node]) <- grout flex $ col $ do
        telomareTextInput :: TextInput t <- grout flex $ textBox
        pure . updated $ fmap ( fmap (\upt -> nodify . TE.tagUPTwithIExpr [] $ upt)
                              -- . fmap (show . TP.MultiLineShowUPT)
                              . TP.runParseLongExpr
                              . T.unpack
                              )
                              (_textInput_value telomareTextInput)
    pure ()
  pure $ fmap (\_ -> ()) getout

data Todo = Todo
  { _todo_label :: Text
  , _todo_done  :: Bool
  }
  deriving (Show, Read, Eq, Ord)

data TodoOutput t = TodoOutput
  { _todoOutput_todo    :: Dynamic t Todo
  , _todoOutput_delete  :: Event t ()
  , _todoOutput_focusId :: FocusId
  }

todos
  :: forall t m.
     ( MonadHold t m
     , Manager t m
     , VtyExample t m
     , Adjustable t m
     , PostBuild t m
     )
  => [Todo]
  -> Event t ()
  -> m (Dynamic t (Map Int (TodoOutput t)))
todos todos0 newTodo = do
  let todosMap0 :: Map Int Todo
      todosMap0 = Map.fromList $ zip [0..] todos0
  rec listOut :: Dynamic t (Map Int (TodoOutput t))
        <- listHoldWithKey todosMap0 updates $ \k t -> grout (fixed 1) $ do
          to <- todo t
          let sel :: Event t ()
              sel = select selectOnDelete $ Const2 k
          pb :: Event t () <- getPostBuild
          requestFocus $ Refocus_Id (_todoOutput_focusId to) <$ leftmost [pb, sel]
          pure to
      let delete :: Event t (Map Int (Maybe Todo))
          delete = flip Map.singleton Nothing <$> todoDelete
          todosMap :: Dynamic t (Map Int Todo)
          todosMap = joinDynThroughMap $ fmap _todoOutput_todo <$> listOut
          insert :: Event t (Map Int (Maybe Todo))
          insert = ffor (tag (current todosMap) newTodo) $ \m -> case Map.lookupMax m of
             Nothing     -> Map.singleton 0 $ Just $ Todo "" False
             Just (k, _) -> Map.singleton (k+1) $ Just $ Todo "" False
          updates :: Event t (Map Int (Maybe Todo))
          updates = leftmost [insert, delete]
          todoDelete :: Event t Int
          todoDelete = switch . current $
            leftmost . Map.elems . Map.mapWithKey (\k -> (k <$) . _todoOutput_delete) <$> listOut
          selectOnDelete :: EventSelector t (Const2 Int ())
          selectOnDelete = fanMap $ (`Map.singleton` ()) <$> attachWithMaybe
            (\m k -> let (before, after) = Map.split k m
                      in  fmap fst $ Map.lookupMax before <|> Map.lookupMin after)
            (current todosMap)
            todoDelete
  pure listOut


todo :: forall t m.
        (VtyExample t m, HasLayout t m)
     => Todo
     -> m (TodoOutput t)
todo t0 = row $ do
  let toggleKeys = Set.fromList
        [ (V.KChar ' ', [V.MCtrl])
        , (V.KChar '@', [V.MCtrl])
        ]
  anyChildFocused $ \(focused :: Dynamic t Bool) -> do
    toggleE :: Event t KeyCombo <- keyCombos toggleKeys
    filterKeys (flip Set.notMember $ Set.insert (V.KChar '\t', []) toggleKeys) $ do
      rec let cfg = def
                { _checkboxConfig_setValue = setVal
                }
          value :: Dynamic t Bool <- tile (fixed 4) $ checkbox cfg $ _todo_done t0
          let setVal :: Event t Bool
              setVal = attachWith (\v _ -> not v) (current value) $ gate (current focused) toggleE
          (fid, (ti, d)) <- tile' flex $ do
            i :: Event t VtyEvent <- input
            v :: TextInput t <- textInput $
              def { _textInputConfig_initialValue = TZ.fromText $ _todo_label t0 }
            let deleteSelf :: Event t ()
                deleteSelf = attachWithMaybe backspaceOnEmpty (current $ _textInput_value v) i
            return (v, deleteSelf)
      return $ TodoOutput
        { _todoOutput_todo = Todo <$> _textInput_value ti <*> value
        , _todoOutput_delete = d
        , _todoOutput_focusId = fid
        }
  where
    backspaceOnEmpty :: Text -> V.Event -> Maybe ()
    backspaceOnEmpty v = \case
      V.EvKey V.KBS _ | T.null v -> Just ()
      _                          -> Nothing

-- * Scrollable text example

scrolling
  :: ( VtyExample t m
     , MonadHold t m
     , Manager t m
     , PostBuild t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     )
  => m ()
scrolling = col $ do
  grout (fixed 2) $ text "Use your mouse wheel or up and down arrows to scroll:"
  (fid, out) <- tile' (fixed 5) $ boxStatic def $ scrollableText def $ "Gallia est omnis divisa in partes tres, quarum unam incolunt Belgae, aliam Aquitani, tertiam qui ipsorum lingua Celtae, nostra Galli appellantur. Hi omnes lingua, institutis, legibus inter se differunt. Gallos ab Aquitanis Garumna flumen, a Belgis Matrona et Sequana dividit. Horum omnium fortissimi sunt Belgae, propterea quod a cultu atque humanitate provinciae longissime absunt, minimeque ad eos mercatores saepe commeant atque ea quae ad effeminandos animos pertinent important, proximique sunt Germanis, qui trans Rhenum incolunt, quibuscum continenter bellum gerunt. Qua de causa Helvetii quoque reliquos Gallos virtute praecedunt, quod fere cotidianis proeliis cum Germanis contendunt, cum aut suis finibus eos prohibent aut ipsi in eorum finibus bellum gerunt. Eorum una pars, quam Gallos obtinere dictum est, initium capit a flumine Rhodano, continetur Garumna flumine, Oceano, finibus Belgarum, attingit etiam ab Sequanis et Helvetiis flumen Rhenum, vergit ad septentriones. Belgae ab extremis Galliae finibus oriuntur, pertinent ad inferiorem partem fluminis Rheni, spectant in septentrionem et orientem solem. Aquitania a Garumna flumine ad Pyrenaeos montes et eam partem Oceani quae est ad Hispaniam pertinet; spectat inter occasum solis et septentriones.\nApud Helvetios longe nobilissimus fuit et ditissimus Orgetorix. Is M. Messala, [et P.] M. Pisone consulibus regni cupiditate inductus coniurationem nobilitatis fecit et civitati persuasit ut de finibus suis cum omnibus copiis exirent: perfacile esse, cum virtute omnibus praestarent, totius Galliae imperio potiri. Id hoc facilius iis persuasit, quod undique loci natura Helvetii continentur: una ex parte flumine Rheno latissimo atque altissimo, qui agrum Helvetium a Germanis dividit; altera ex parte monte Iura altissimo, qui est inter Sequanos et Helvetios; tertia lacu Lemanno et flumine Rhodano, qui provinciam nostram ab Helvetiis dividit. His rebus fiebat ut et minus late vagarentur et minus facile finitimis bellum inferre possent; qua ex parte homines bellandi cupidi magno dolore adficiebantur. Pro multitudine autem hominum et pro gloria belli atque fortitudinis angustos se fines habere arbitrabantur, qui in longitudinem milia passuum CCXL, in latitudinem CLXXX patebant."
  pb <- getPostBuild
  requestFocus $ Refocus_Id fid <$ pb
  grout (fixed 1) $ text $ ffor (_scrollable_scrollPosition out) $ \p -> "Scrolled to " <> case p of
    ScrollPos_Top    -> "top"
    ScrollPos_Bottom -> "bottom"
    ScrollPos_Line n -> "line " <> T.pack (show n)
  e <- performEventAsync $ ffor pb $ \_ cb -> liftIO $ void $ forkIO $ forever $ do
    threadDelay 1000000
    t <- getCurrentTime
    cb $ [T.pack $ show t]
  xs <- foldDyn (flip (<>)) [] e
  grout (fixed 3) blank
  tile (fixed 10) $ col $ do
    grout (fixed 1) $ text "This one scrolls automatically as the output grows:"
    Scrollable pos total h <- tile flex $
      scrollableText (ScrollableConfig never never ScrollPos_Bottom (pure $ Just ScrollToBottom_Maintain)) $
        T.unlines <$> xs
    grout (fixed 5) $ boxStatic def $ do
      grout (fixed 1) $ row $ do
        grout (fixed 8) (text "Height:")
        grout flex $ display h
      grout (fixed 1) $ row $ do
        grout (fixed 8) $ text "Scroll:"
        grout flex $ display pos
      grout (fixed 1) $ row $ do
        grout (fixed 8) $ text "Length:"
        grout flex $ display total

--  * Text editor example with resizable boxes

testBoxes
  :: (MonadHold t m, VtyExample t m)
  => m ()
testBoxes = do
  dw <- displayWidth
  dh <- displayHeight
  let region1 = Region <$> (div' dw 6) <*> (div' dh 6) <*> (div' dw 2) <*> (div' dh 2)
      region2 = Region <$> (div' dw 4) <*> (div' dh 4) <*> (2 * div' dw 3) <*> (2 * div' dh 3)
  pane region1 (constDyn False) . boxStatic singleBoxStyle $ debugInput
  _ <- pane region2 (constDyn True) . boxStatic singleBoxStyle $
    let cfg = def
          { _textInputConfig_initialValue =
            "This box is a text input. The box below responds to mouse drag inputs. You can also drag the separator between the boxes to resize them."
          }
        textBox = boxTitle (pure roundedBoxStyle) "Text Edit" $
          multilineTextInput cfg
        dragBox = boxStatic roundedBoxStyle dragTest
    in splitVDrag (hRule doubleBoxStyle) textBox dragBox
  return ()
  where
    div' :: (Integral a, Applicative f) => f a -> f a -> f a
    div' = liftA2 div

debugFocus :: (VtyExample t m) => m ()
debugFocus = do
  f <- focus
  text $ T.pack . show <$> current f

debugInput :: (VtyExample t m, MonadHold t m) => m ()
debugInput = do
  lastEvent :: Behavior t String <- hold "No event yet" . fmap show =<< input
  text $ T.pack <$> lastEvent

dragTest :: (VtyExample t m, MonadHold t m) => m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

testStringBox :: VtyExample t m => m ()
testStringBox = boxStatic singleBoxStyle .
  text . pure . T.pack . take 500 $ cycle ('\n' : ['a'..'z'])
