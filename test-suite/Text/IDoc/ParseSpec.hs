-- | ParseSpec.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Apr 05, 2017
-- Summary: 

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Idoc.Parse where

import ClassyPrelude

import Text.IDoc.Parse
import Text.IDoc.Syntax

import qualified Data.Map as M

import Test.Hspec

filePrefix :: FilePath
filePrefix = "./test-suite/idoc/"

spec :: IO ()
spec = hspec $ do
  describe "Text.IDoc.Parse" $ do

    describe "actualTextP" $ do
      it "Turns TextT tokens into Text." $ do
        parseFileAs (filePrefix ++ "actualTextP.idoc") actualTextP `shouldReturn` (Right $ "Hello there")

    describe "textP" $ do
      it "Parses a TextT token as Text" $ do
        parseFileAs (filePrefix ++ "textP.text.idoc") textP `shouldReturn` 
          (Right $ "Alrighty then")

      it "Parses a non-TextT as Text" $ do
        parseFileAs (filePrefix ++ "textP.period.idoc") textP `shouldReturn` 
          (Right $ ".")

    describe "qTextP" $ do
      it "Parses Strong Text." $ do
        parseFileAs (filePrefix ++ "qTextP.strongText.idoc") qTextP `shouldReturn`
          (Right $ QText { _qtText = fromList [ TextC $ "hello there"
                                              , TextC $ "." 
                                              ]
                         , _qtType = Strong
                         })

    describe "attrValPairP" $ do
      it "Parses a pair with an equals between them." $ do
        parseFileAs (filePrefix ++ "attrValPairP.pair.idoc") attrValPairP `shouldReturn`
          (Right $ (AttrName "hello", Just $ AttrValue "there"))

      it "Parses a single item" $ do
        parseFileAs (filePrefix ++ "attrValPairP.single.idoc") attrValPairP `shouldReturn`
          (Right $ (AttrName "lonely", Nothing))

    describe "attrMapP" $ do
      it "Parses an AttrMap with one element." $ do
        parseFileAs (filePrefix ++ "attrMapP.single.idoc") attrMapP `shouldReturn`
          (Right $ AttrMap $ M.fromList [(AttrName "hello", Just $ AttrValue "there")])

      it "Parses an AttrMap with many elements." $ do
        parseFileAs (filePrefix ++ "attrMapP.many.idoc") attrMapP `shouldReturn`
          (Right $ AttrMap $ M.fromList [(AttrName "hello", Just $ AttrValue "there")
                                        ,(AttrName "lonely", Nothing)
                                        ,(AttrName "another", Just $ AttrValue "guy")
                                        ])

    describe "setIDP" $ do
      it "Parses a SetID." $ do
        parseFileAs (filePrefix ++ "setIDP.idoc") setIDP `shouldReturn`
          (Right $ SetID $ IDHash $ "hello")

    describe "linkP" $ do
      it "Parses an ilink without attrs and with text." $ do
        parseFileAs (filePrefix ++ "linkP.ilink.idoc") linkP `shouldReturn`
          (Right $ Link { _linkText = LinkText $ fromList [ TextC "hello" ]
                        , _linkAttrs = AttrMap $ M.empty
                        , _linkLocation = ID { _idProtocol = Nothing
                                             , _idBase = empty
                                             , _idHash = Just $ IDHash "hello"
                                             }
                        , _linkType = Internal
                        })
          
      it "Parses an olink with attrs and no text." $ do
        parseFileAs (filePrefix ++ "linkP.olink.idoc") linkP `shouldReturn`
          (Right $ Link { _linkText = LinkText empty
                        , _linkAttrs = AttrMap $ M.fromList [(AttrName "lonely", Nothing)]
                        , _linkLocation = ID { _idProtocol = Just $ Protocol "https"
                                             , _idBase = fromList [ IDBase "independentlearning.science"
                                                                  , IDBase "source.idoc"
                                                                  ]
                                             , _idHash = Just $ IDHash "hello"
                                             }
                        , _linkType = Out
                        })

      it "Parses a blink with attrs and with text." $ do
        parseFileAs (filePrefix ++ "linkP.blink.idoc") linkP `shouldReturn`
          (Right $ Link { _linkText = LinkText $ fromList [ TextC "hello" ]
                        , _linkAttrs = AttrMap $ M.fromList [(AttrName "lonely", Nothing)]
                        , _linkLocation = ID { _idProtocol = Nothing
                                             , _idBase = fromList [ IDBase "Hello"
                                                                  , IDBase "World"
                                                                  ]
                                             , _idHash = Just $ IDHash "hello there"
                                             }
                        , _linkType = Back
                        })

    describe "unorderedItemP" $ do
      it "Parses an unordered ListItem." $ do
        parseFileAs (filePrefix ++ "unorderedItemP.idoc") unorderedItemP `shouldReturn`
          (Right $ ListItem { _liAttrs = AttrMap M.empty
                            , _liLabel = Nothing
                            , _liContents = fromList [ TextC "Hello there" ]
                            , _liSetID = Nothing
                            , _liType  = Unordered
                            })

    describe "labelledItemP" $ do
      it "Parses a labelled ListItem." $ do
        parseFileAs (filePrefix ++ "labelledItemP.idoc") labelledItemP `shouldReturn`
          (Right $ ListItem { _liAttrs = AttrMap M.empty
                            , _liLabel = Just $ ListLabel $ fromList [ TextC "A label" ]
                            , _liContents = fromList [ TextC "Some content" ]
                            , _liSetID = Nothing
                            , _liType  = Labelled
                            })

    describe "listP" $ do
      it "Parses an Ordered List." $ do
        parseFileAs (filePrefix ++ "listP.ordered.idoc") listP `shouldReturn`
          (Right $ List $ fromList [ ListItem { _liAttrs = AttrMap M.empty
                                              , _liLabel = Nothing
                                              , _liContents = fromList [ TextC "some content" ]
                                              , _liSetID = Nothing
                                              , _liType  = Ordered
                                              }
                                   , ListItem { _liAttrs = AttrMap M.empty
                                              , _liLabel = Nothing
                                              , _liContents = fromList [ TextC "more content" ]
                                              , _liSetID = Nothing
                                              , _liType = Ordered
                                              }
                                   ])
      
      it "Parses a Labelled List." $ do
        parseFileAs (filePrefix ++ "listP.labelled.idoc") listP `shouldReturn`
          (Right $ List $ fromList [ ListItem { _liAttrs = AttrMap M.empty
                                              , _liLabel = Just $ ListLabel $ fromList [ TextC "hello" ]
                                              , _liContents = fromList [ TextC "some content" ]
                                              , _liSetID = Nothing
                                              , _liType  = Labelled
                                              }
                                              , ListItem { _liAttrs = AttrMap M.empty
                                              , _liLabel = Just $ ListLabel $ fromList [ TextC "hello again" ]
                                              , _liContents = fromList [ TextC "more content" ]
                                              , _liSetID = Nothing
                                              , _liType = Labelled
                                              }
                                              ])

      it "Parses a List after a Paragraph." $ do
        parseFileAs (filePrefix ++ "listP.followingParagraphP.idoc") sectionP `shouldReturn`
          (Right $ Section { _secAttrs = AttrMap M.empty
                           , _secTitle = SectionTitle $ fromList [ TextC "Hello!" ]
                           , _secSetID = Nothing
                           , _secType = TopSection
                           , _secContents = fromList [ CC $ ParagraphC $
                                                       Paragraph { _paraContents = fromList [ TextC "Hello there this is a paragraph blah blah blah", TextC "." ]
                                                                 ,_paraSetID = Nothing
                                                                 }
                                                     , CC $ ListC $ List $
                                                       fromList [ ListItem { _liAttrs = AttrMap M.empty
                                                                           , _liLabel = Nothing
                                                                           , _liType = Unordered
                                                                           , _liContents = fromList [ TextC "And a one and a", TextC ".", TextC ".", TextC "." ]
                                                                           , _liSetID = Nothing
                                                                           }
                                                                ]
                                                     ]
                           })
                                              
    describe "inlineMathP" $ do
      it "Parses InlineMath properly." $ do
        parseFileAs (filePrefix ++ "inlineMathP.idoc") inlineMathP `shouldReturn`
          (Right $ InlineMath { _imAttrs = AttrMap M.empty
                              , _imContents = fromList [ TextT "hello"
                                                       , Dash
                                                       , TextT "there"
                                                       ]
                              , _imSetID = Just $ SetID $ IDHash $ "mathStuff"
                              })

  describe "footnoteP" $ do
    it "Parses a Footnote." $ do
      parseFileAs (filePrefix ++ "footnoteP.idoc") footnoteP `shouldReturn`
        (Right $ Markup { _muType = Footnote
                        , _muAttrs = AttrMap M.empty
                        , _muContents = fromList [ TextC "A footnote" ]
                        , _muSetID = Just $ SetID $ IDHash $ "myFoot"
                        })

  describe "paragraphP" $ do
    it "Parses a Paragraph." $ do
      parseFileAs (filePrefix ++ "paragraphP.idoc") paragraphP `shouldReturn`
        (Right $ Paragraph { _paraContents = fromList [ TextC "A paragraph"
                                                      , TextC "."
                                                      ]
                           , _paraSetID = Nothing
                           })
        
  describe "simpleCoreBlockP" $ do
    it "Parses a block containing only SimpleCore." $ do
      parseFileAs (filePrefix ++ "simpleCoreBlockP.idoc") simpleCoreBlockP `shouldReturn`
        (Right $ fromList [ TextC "Hello there"
                          , TextC ","
                          , TextC " "
                          , QTextC $ QText { _qtText = fromList [ TextC "Friendo" 
                                                                , TextC "." 
                                                                ]
                                           , _qtType = Emphasis
                                           }
                          ])

  describe "coreBlockP" $ do
    it "Parses a Block containing another Block." $ do
      parseFileAs (filePrefix ++ "coreBlockP.blockception.idoc") coreBlockP `shouldReturn`
        (Right $ fromList [ CC $ BlockC $ Block { _bType = MathB $ Math $ fromList [ TextT "some math" ]
                                                , _bAttrs = AttrMap M.empty
                                                , _bTitle = Nothing
                                                , _bSetID = Nothing
                                                } ])

    it "Parses a Block containing a Labelled List." $ do
      parseFileAs (filePrefix ++ "coreBlockP.labelledList.idoc") coreBlockP `shouldReturn`
        (Right $ fromList [ CC $ ListC $ List $ fromList [ ListItem { _liAttrs = AttrMap M.empty
                                                                    , _liLabel = Just $ ListLabel $ fromList [ TextC "hello" ] 
                                                                    , _liContents = fromList [ TextC "there" ]
                                                                    , _liSetID = Nothing
                                                                    , _liType = Labelled
                                                                    } 
                                                         ] 
                          ])

    it "Parses a Block containing a Paragraph." $ do
      parseFileAs (filePrefix ++ "coreBlockP.paragraph.idoc") coreBlockP `shouldReturn`
        (Right $ fromList [ CC $ ParagraphC $ Paragraph { _paraContents = fromList [ TextC "A paragraph"
                                                                                   , TextC "."
                                                                                   ]
                                                        , _paraSetID = Nothing
                                                        }
                          ])

  describe "uninterpretedBlockP" $ do
    it "Passes through the Tokens in an uninterpreted Block." $ do 
      parseFileAs (filePrefix ++ "uninterpretedBlockP.idoc") uninterpretedBlockP `shouldReturn`
        (Right $ fromList [ TextT "hey there" ])

  describe "linkBlockP" $ do
    it "Parses the link in a linkBlock." $ do
      parseFileAs (filePrefix ++ "linkBlockP.idoc") linkBlockP `shouldReturn` 
        (Right $ Link { _linkText = LinkText empty
                      , _linkAttrs = AttrMap M.empty
                      , _linkLocation = ID { _idProtocol = Nothing
                                           , _idBase = fromList [ IDBase "Hello" ]
                                           , _idHash = Nothing
                                           }
                      , _linkType = Back
                      })

  describe "prerexItemP" $ do
    it "Parses a PrerexItem properly." $ do
      parseFileAs (filePrefix ++ "prerexItemP.idoc") prerexItemP `shouldReturn`
        (Right $ PrerexItem { _prerexItemPath = ID { _idProtocol = Nothing
                                                   , _idBase = fromList [ IDBase "Something", IDBase "SomethingElse" ]
                                                   , _idHash = Nothing
                                                   }
                            , _prerexItemDescription = fromList [ TextC "Something about something" ]
                            })

  describe "prerexP" $ do
    it "Parses a Prerex Block properly." $ do
      parseFileAs (filePrefix ++ "prerexP.idoc") prerexP `shouldReturn`
        (Right $ Prerex $ fromList [ PrerexItem { _prerexItemPath = ID { _idProtocol = Nothing
                                                                       , _idBase = fromList [ IDBase "Something", IDBase "SomethingElse" ]
                                                                       , _idHash = Nothing
                                                                       }
                                                , _prerexItemDescription = fromList [ TextC "Something about something" ]
                                                }
                                   , PrerexItem { _prerexItemPath = ID { _idProtocol = Nothing
                                                                       , _idBase = fromList [ IDBase "SomethingElse", IDBase "SomethingWeird" ]
                                                                       , _idHash = Nothing
                                                                       }
                                                , _prerexItemDescription = fromList [ TextC "Something about something else" ]
                            }
                                   ])

-- spec2 = hspec $ do
--     describe "listP" $ do
--       it "Parses an Ordered List." $ do
--         parseFileAs (filePrefix ++ "listP.ordered.idoc") listP `shouldReturn`
--           (Right $ List $ fromList [ ListItem { _liAttrs = AttrMap M.empty
--                                               , _liLabel = Nothing
--                                               , _liContents = fromList [ TextC "some content" ]
--                                               , _liSetID = Nothing
--                                               , _liType  = Ordered
--                                               }
--                                    , ListItem { _liAttrs = AttrMap M.empty
--                                               , _liLabel = Nothing
--                                               , _liContents = fromList [ TextC "more content" ]
--                                               , _liSetID = Nothing
--                                               , _liType = Ordered
--                                               }
--                                    ])
      
--       it "Parses a Labelled List." $ do
--         parseFileAs (filePrefix ++ "listP.labelled.idoc") listP `shouldReturn`
--           (Right $ List $ fromList [ ListItem { _liAttrs = AttrMap M.empty
--                                               , _liLabel = Just $ fromList [ TextC "hello" ]
--                                               , _liContents = fromList [ TextC "some content" ]
--                                               , _liSetID = Nothing
--                                               , _liType  = Labelled
--                                               }
--                                               , ListItem { _liAttrs = AttrMap M.empty
--                                               , _liLabel = Just $ fromList [ TextC "hello again" ]
--                                               , _liContents = fromList [ TextC "more content" ]
--                                               , _liSetID = Nothing
--                                               , _liType = Labelled
--                                               }
--                                               ])
                                              
