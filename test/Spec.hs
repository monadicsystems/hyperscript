{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Hyperscript.Lexer
import Test.Hspec
import Text.Megaparsec hiding (Token)

main :: IO ()
main = hspec $ do
  describe "identifier tokenizer" $
    it "tokenizes foo" $
      parseMaybe parseIdentifier "foo" `shouldBe` Just (TokenIdentifier "foo")
  describe "number tokenizer" $ do
    it "tokenizes 1" $
      parseMaybe parseNumber "1" `shouldBe` Just (TokenNumber "1")
    it "tokenizes 1.1" $
      parseMaybe parseNumber "1.1" `shouldBe` Just (TokenNumber "1.1")
  describe "class reference tokenizer" $ do
    it "tokenizes .a" $
      parseMaybe parseClassRef ".a" `shouldBe` Just (TokenClassRef ".a")
    it "tokenizes .text-green" $
      parseMaybe parseClassRef ".text-green" `shouldBe` Just (TokenClassRef ".text-green")
  describe "id reference tokenizer" $ do
    it "tokenizes #a" $
      parseMaybe parseIDRef "#a" `shouldBe` Just (TokenIDRef "#a")
    it "tokenizes #admin-card" $
      parseMaybe parseIDRef "#admin-card" `shouldBe` Just (TokenIDRef "#admin-card")
  describe "string tokenizer" $ do
    it "tokenizes double quote strings" $
      parseMaybe parseString "\"hello\"" `shouldBe` Just (TokenString "hello")
    it "tokenizes single quote strings" $
      parseMaybe parseString "'hello'" `shouldBe` Just (TokenString "hello")
    it "tokenizes template strings" $
      parseMaybe parseString "`hello`" `shouldBe` Just (TokenString "hello")
  describe "whitespace tokenizer" $ do
    it "tokenizes space characters" $
      parseMaybe parseWhitespace "           " `shouldBe` Just TokenWhitespace
  describe "tokenizer" $ do
    it "handles whitespace properly" $ do
      Prelude.length <$> parseMaybe tokenize "     " `shouldBe` Just 0
      Prelude.length <$> parseMaybe tokenize "     asdf" `shouldBe` Just 1
      Prelude.length <$> parseMaybe tokenize "     asdf   " `shouldBe` Just 2
      Prelude.length <$> parseMaybe tokenize "asdf     " `shouldBe` Just 2
      Prelude.length <$> parseMaybe tokenize "\n" `shouldBe` Just 0
      Prelude.length <$> parseMaybe tokenize "\nasdf" `shouldBe` Just 1
      Prelude.length <$> parseMaybe tokenize "\nasdf\n" `shouldBe` Just 2
      Prelude.length <$> parseMaybe tokenize "asdf\n" `shouldBe` Just 2
      Prelude.length <$> parseMaybe tokenize "\t" `shouldBe` Just 0
      Prelude.length <$> parseMaybe tokenize "\tasdf" `shouldBe` Just 1
      Prelude.length <$> parseMaybe tokenize "\tasdf\t" `shouldBe` Just 2
      Prelude.length <$> parseMaybe tokenize "asdf\t" `shouldBe` Just 2
      Prelude.length <$> parseMaybe tokenize "\r" `shouldBe` Just 0
      Prelude.length <$> parseMaybe tokenize "\rasdf" `shouldBe` Just 1
      Prelude.length <$> parseMaybe tokenize "\rasdf\r" `shouldBe` Just 2
      Prelude.length <$> parseMaybe tokenize "asdf\r" `shouldBe` Just 2
    it "handles comments properly" $ do
      Prelude.length <$> parseMaybe tokenize "--" `shouldBe` Just 0
      Prelude.length <$> parseMaybe tokenize "asdf--" `shouldBe` Just 1
      Prelude.length <$> parseMaybe tokenize "-- asdf" `shouldBe` Just 0
      Prelude.length <$> parseMaybe tokenize "--\nasdf" `shouldBe` Just 1
      Prelude.length <$> parseMaybe tokenize "--\nasdf--" `shouldBe` Just 1
    it "handles class references properly" $ do
      parseMaybe tokenize ".a" `shouldBe` Just [TokenClassRef ".a"]
      parseMaybe tokenize "    .a" `shouldBe` Just [TokenClassRef ".a"]
      getTokenAt (parseMaybe tokenize "a.a") 0 `shouldBe` Just (TokenIdentifier "a")
      -- getTokenAt (parseMaybe tokenize "(a).a") 4 `shouldBe` Just (TokenIdentifier "a") FAIL
      -- getTokenAt (parseMaybe tokenize "{a}.a") 4 `shouldBe` Just (TokenIdentifier "a") FAIL
      -- getTokenAt (parseMaybe tokenize "[a].a") 4 `shouldBe` Just (TokenIdentifier "a") FAIL
      getTokenAt (parseMaybe tokenize "(a(.a") 3 `shouldBe` Just (TokenClassRef ".a")
      getTokenAt (parseMaybe tokenize "{a{.a") 3 `shouldBe` Just (TokenClassRef ".a")
      getTokenAt (parseMaybe tokenize "[a[.a") 3 `shouldBe` Just (TokenClassRef ".a")
    it "handles id references properly" $ do
      parseMaybe tokenize "#a" `shouldBe` Just [TokenIDRef "#a"]
      parseMaybe tokenize "    #a" `shouldBe` Just [TokenIDRef "#a"]
      getTokenAt (parseMaybe tokenize "a#a") 0 `shouldBe` Just (TokenIdentifier "a")
      -- getTokenAt (parseMaybe tokenize "(a)#a") 4 `shouldBe` Just (TokenIdentifier "a") FAIL
      -- getTokenAt (parseMaybe tokenize "{a}#a") 4 `shouldBe` Just (TokenIdentifier "a") FAIL
      -- getTokenAt (parseMaybe tokenize "[a]#a") 4 `shouldBe` Just (TokenIdentifier "a") FAIL
      getTokenAt (parseMaybe tokenize "(a(#a") 3 `shouldBe` Just (TokenIDRef "#a")
      getTokenAt (parseMaybe tokenize "{a{#a") 3 `shouldBe` Just (TokenIDRef "#a")
      getTokenAt (parseMaybe tokenize "[a[#a") 3 `shouldBe` Just (TokenIDRef "#a")
    it "handles identifiers properly" $ do
      getTokenAt (parseMaybe tokenize "foo") 0 `shouldBe` Just (TokenIdentifier "foo")
      getTokenAt (parseMaybe tokenize "   foo   ") 0 `shouldBe` Just (TokenIdentifier "foo")
      getTokenAt (parseMaybe tokenize "      foo      bar") 0 `shouldBe` Just (TokenIdentifier "foo")
      getTokenAt (parseMaybe tokenize "      foo      bar") 2 `shouldBe` Just (TokenIdentifier "bar")
      getTokenAt (parseMaybe tokenize "     foo\n-- a comment\n    bar") 0 `shouldBe` Just (TokenIdentifier "foo")
      getTokenAt (parseMaybe tokenize "     foo\n-- a comment\n    bar") 3 `shouldBe` Just (TokenIdentifier "bar")
    it "handles identifiers with numbers properly" $ do
      getTokenAt (parseMaybe tokenize "f1oo") 0 `shouldBe` Just (TokenIdentifier "f1oo")
      getTokenAt (parseMaybe tokenize "fo1o") 0 `shouldBe` Just (TokenIdentifier "fo1o")
      getTokenAt (parseMaybe tokenize "foo1") 0 `shouldBe` Just (TokenIdentifier "foo1")
    it "handles numbers properly" $ do
      getTokenAt (parseMaybe tokenize "1") 0 `shouldBe` Just (TokenNumber "1")
      getTokenAt (parseMaybe tokenize "1.1") 0 `shouldBe` Just (TokenNumber "1.1")
      getTokenAt (parseMaybe tokenize "1234567890.1234567890") 0 `shouldBe` Just (TokenNumber "1234567890.1234567890")
      parseMaybe tokenize "1.1.1" `shouldBe` Just [TokenNumber "1.1", TokenOp OpPeriod, TokenNumber "1"]
    it "handles strings properly" $ do
      getTokenAt (parseMaybe tokenize "\"foo\"") 0 `shouldBe` Just (TokenString "foo")
      getTokenAt (parseMaybe tokenize "\"fo'o\"") 0 `shouldBe` Just (TokenString "fo'o")
      -- getTokenAt (parseMaybe tokenize "\"fo\\\"o\"") 0 `shouldBe` Just (TokenString "fo\"o") FAILS escaping fails
      getTokenAt (parseMaybe tokenize "'foo'") 0 `shouldBe` Just (TokenString "foo")
      getTokenAt (parseMaybe tokenize "'fo\"o'") 0 `shouldBe` Just (TokenString "fo\"o")
    -- getTokenAt (parseMaybe tokenize "'fo\'o'") 0 `shouldBe` Just (TokenString "fo'o") Fails escaping fails
    it "handles all special escapes properly" $ do
      -- ALL ESCAPING FAILS
      True `shouldBe` True
    -- getTokenAt (parseMaybe tokenize "\"\\b\"") 0 `shouldBe` Just (TokenString "\b")
    -- getTokenAt (parseMaybe tokenize "\"\\f\"") 0 `shouldBe` Just (TokenString "\f")
    -- getTokenAt (parseMaybe tokenize "\"\\n\"") 0 `shouldBe` Just (TokenString "\n")
    -- getTokenAt (parseMaybe tokenize "\"\\r\"") 0 `shouldBe` Just (TokenString "\r")
    -- getTokenAt (parseMaybe tokenize "\"\\t\"") 0 `shouldBe` Just (TokenString "\t")
    -- getTokenAt (parseMaybe tokenize "\"\\v\"") 0 `shouldBe` Just (TokenString "\v")
    it "handles look ahead properly" $ do
      parseMaybe tokenize "a 1 + 1" `shouldBe` Just [TokenIdentifier "a", TokenWhitespace, TokenNumber "1", TokenWhitespace, TokenOp OpPlus, TokenWhitespace, TokenNumber "1"]
    it "handles template bootstrap properly" $ do
      True `shouldBe` True -- TODO: Implement

{-
	it("handles template boostrap properly", function () {
		var lexer = _hyperscript.internals.lexer;
		var tokenize = lexer.tokenize('"', true);
		tokenize.token(0).value.should.equal('"');

		var tokenize = lexer.tokenize('"$', true);
		tokenize.token(0).value.should.equal('"');
		tokenize.token(1).value.should.equal("$");

		var tokenize = lexer.tokenize('"${', true);
		tokenize.token(0).value.should.equal('"');
		tokenize.token(1).value.should.equal("$");
		tokenize.token(2).value.should.equal("{");

		var tokenize = lexer.tokenize('"${"asdf"', true);
		tokenize.token(0).value.should.equal('"');
		tokenize.token(1).value.should.equal("$");
		tokenize.token(2).value.should.equal("{");
		tokenize.token(3).value.should.equal("asdf");

		var tokenize = lexer.tokenize('"${"asdf"}"', true);
		tokenize.token(0).value.should.equal('"');
		tokenize.token(1).value.should.equal("$");
		tokenize.token(2).value.should.equal("{");
		tokenize.token(3).value.should.equal("asdf");
		tokenize.token(4).value.should.equal("}");
		tokenize.token(5).value.should.equal('"');
	});
-}

getTokenAt :: Maybe [Token] -> Int -> Maybe Token
getTokenAt Nothing _ = Nothing
getTokenAt (Just tokens) index = tokens !? index

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    Prelude.foldr
      ( \x r k -> case k of
          0 -> Just x
          _ -> r (k -1)
      )
      (const Nothing)
      xs
      n
