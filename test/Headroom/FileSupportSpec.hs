{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Headroom.FileSupportSpec
  ( spec
  )
where

import           Headroom.FileSupport
import           Headroom.FileSystem                 ( loadFile )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.SourceCode                 ( LineType(..)
                                                     , SourceCode(..)
                                                     )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"

  describe "analyzeSourceCode" $ do
    it "analyzes C source code" $ do
      sample <- loadFile $ codeSamples </> "c" </> "sample1.c"
      let expected = SourceCode
            [ (Code   , "")
            , (Comment, "/*")
            , (Comment, " * This is header")
            , (Comment, " */")
            , (Code   , "")
            , (Code   , "#include <stdio.h>")
            , (Comment, "/* This is not header */")
            , (Code   , "int main() {")
            , (Comment, "   // printf() displays the string inside quotation")
            , (Code   , "   printf(\"Hello, World!\");")
            , (Code   , "   return 0;")
            , (Code   , "")
            , (Comment, "   /*")
            , (Comment, "    * block comment")
            , (Comment, "    */")
            , (Code   , "}")
            ]
      analyzeSourceCode (fileSupport C) sample `shouldBe` expected

    it "analyzes C++ source code" $ do
      sample <- loadFile $ codeSamples </> "cpp" </> "sample1.cpp"
      let expected = SourceCode
            [ (Code   , "")
            , (Comment, "/*")
            , (Comment, " * This is header")
            , (Comment, " */")
            , (Code   , "")
            , (Code   , "#include <iostream>")
            , (Code   , "")
            , (Comment, "/* This is not header */")
            , (Code   , "")
            , (Code   , "int main() {")
            , (Comment, "    // line comment")
            , (Code, "    std::cout << \"Hello World!\";")
            , (Code   , "    return 0;")
            , (Code   , "")
            , (Comment, "    /*")
            , (Comment, "     * multiline block comment")
            , (Comment, "     */")
            , (Code   , "}")
            ]
      analyzeSourceCode (fileSupport CPP) sample `shouldBe` expected

    it "analyzes CSS source code" $ do
      sample <- loadFile $ codeSamples </> "css" </> "sample1.css"
      let expected = SourceCode
            [ (Code   , "")
            , (Comment, "/*")
            , (Comment, " * This is")
            , (Comment, " * the header")
            , (Comment, " */")
            , (Code   , "")
            , (Code   , "body {")
            , (Comment, "    /* This is not header */")
            , (Code   , "    color: black")
            , (Code   , "}")
            , (Code   , "")
            , (Comment, "/* This is not header */")
            , (Code   , "")
            , (Comment, "/*")
            , (Comment, " * block comment")
            , (Comment, " */")
            ]
      analyzeSourceCode (fileSupport CSS) sample `shouldBe` expected

    it "analyzes Go source code" $ do
      sample <- loadFile $ codeSamples </> "go" </> "sample1.go"
      let expected = SourceCode
            [ (Code   , "")
            , (Comment, "// This is the")
            , (Comment, "// header")
            , (Code   , "")
            , (Code   , "package main")
            , (Code   , "")
            , (Comment, "// this is not the header")
            , (Code   , "import \"fmt\"")
            , (Code   , "func main() {")
            , (Comment, "    /* another comment */")
            , (Code, "    fmt.Println(\"hello world\")")
            , (Code   , "")
            , (Comment, "    /*")
            , (Comment, "     * block comment")
            , (Comment, "     */")
            , (Code   , "}")
            ]
      analyzeSourceCode (fileSupport Go) sample `shouldBe` expected

    it "analyzes Haskell source code" $ do
      sample <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      let expected = SourceCode
            [ (Code   , "")
            , (Comment, "{-|")
            , (Comment, "Module      : Test")
            , (Comment, "Description : Short description")
            , (Comment, "Copyright   : (c) Some Guy, 2013")
            , (Comment, "                  Someone Else, 2014")
            , (Comment, "License     : GPL-3")
            , (Comment, "Maintainer  : sample@email.com")
            , (Comment, "Stability   : experimental")
            , (Comment, "Portability : POSIX")
            , (Comment, "")
            , (Comment, "long")
            , (Comment, "description")
            , (Comment, "")
            , (Comment, "== Code sample")
            , (Comment, "@")
            , (Comment, "{-# LANGUAGE TypeApplications #-}")
            , (Comment, "")
            , (Comment, "module Data.VCS.Test where")
            , (Comment, "")
            , (Comment, "import Data.VCS.Ignore ( Git, Repo(..), listRepo )")
            , (Comment, "")
            , (Comment, "example :: IO [FilePath]")
            , (Comment, "example = do")
            , (Comment, "  repo <- scanRepo @Git \"path/to/repo\"")
            , (Comment, "  listRepo repo")
            , (Comment, "@")
            , (Comment, "-}")
            , (Code   , "")
            , (Code, "{-# LANGUAGE OverloadedStrings #-}")
            , (Code   , "module Test where")
            , (Code   , "")
            , (Comment, "{- single line block comment -}")
            , (Code   , "")
            , (Comment, "{-")
            , (Comment, "multi line block comment")
            , (Comment, "-}")
            , (Code   , "")
            , (Code   , "foo :: String")
            , (Code   , "foo = \"Hello, world!\"")
            , (Code   , "")
            , (Comment, "-- line comment")
            ]
      analyzeSourceCode (fileSupport Haskell) sample `shouldBe` expected

    it "analyzes HTML source code" $ do
      sample <- loadFile $ codeSamples </> "html" </> "sample1.html"
      let expected = SourceCode
            [ (Code   , "")
            , (Comment, "<!--")
            , (Comment, "    This is header.")
            , (Comment, "    ")
            , (Comment, "-->")
            , (Code   , "<!DOCTYPE html>")
            , (Comment, "<!-- this is not header -->")
            , (Code   , "<html>")
            , (Code   , "    <head>")
            , (Code, "        <meta charset=\"utf-8\" />")
            , (Code, "        <title>Test title</title>")
            , (Code   , "    </head>")
            , (Code   , "    <body>")
            , (Code   , "        Hello, World!")
            , (Code   , "    </body>")
            , (Comment, "    <!--")
            , (Comment, "        multi line block comment")
            , (Comment, "    -->")
            , (Code   , "</html>")
            ]
      analyzeSourceCode (fileSupport HTML) sample `shouldBe` expected

    it "analyzes Java source code" $ do
      sample <- loadFile $ codeSamples </> "java" </> "sample1.java"
      let expected = SourceCode
            [ (Comment, "/*")
            , (Comment, " * This is header.")
            , (Comment, " */")
            , (Code   , "")
            , (Code   , "package foo;")
            , (Code   , "")
            , (Comment, "/* This is not header */")
            , (Code   , "")
            , (Code   , "class Hello {")
            , (Comment, "    /* This is not header */")
            , (Code, "    public static void main(String[] args) {")
            , (Code, "        System.out.println(\"Hello, world!\");")
            , (Comment, "        // line header")
            , (Code   , "    }")
            , (Code   , "}")
            ]
      analyzeSourceCode (fileSupport Java) sample `shouldBe` expected

    it "analyzes Javascript source code" $ do
      sample <- loadFile $ codeSamples </> "js" </> "sample1.js"
      let expected = SourceCode
            [ (Comment, "/*")
            , (Comment, " * This is header")
            , (Comment, " */")
            , (Code   , "")
            , (Code   , "function answer() {")
            , (Comment, "  /* This is not header */")
            , (Code   , "  return 42;")
            , (Comment, "  // line comment")
            , (Code   , "}")
            ]
      analyzeSourceCode (fileSupport JS) sample `shouldBe` expected

    it "analyzes PureScript source code" $ do
      sample <- loadFile $ codeSamples </> "purescript" </> "full.purs"
      let expected = SourceCode
            [ (Code   , "")
            , (Comment, "-- Some module header here")
            , (Code   , "")
            , (Comment, "-- Some comment here")
            , (Code   , "")
            , (Code   , "module Test where")
            , (Code   , "")
            , (Comment, "{-")
            , (Comment, "block comment multi line")
            , (Comment, "-}")
            , (Code   , "")
            , (Code   , "foo :: String")
            , (Code   , "foo = \"Hello, world!\"")
            , (Code   , "")
            , (Comment, "{- block comment single line -}")
            ]
      analyzeSourceCode (fileSupport PureScript) sample `shouldBe` expected

    it "analyzes Rust source code" $ do
      sample <- loadFile $ codeSamples </> "rust" </> "sample1.rs"
      let expected = SourceCode
            [ (Comment, "/*")
            , (Comment, " * This is header")
            , (Comment, " */")
            , (Code   , "")
            , (Code   , " fn main() {")
            , (Comment, "    /* This is not header */")
            , (Code, "    println!(\"Hello World!\");")
            , (Comment, "    // line comment")
            , (Code   , "}")
            ]
      analyzeSourceCode (fileSupport Rust) sample `shouldBe` expected

    it "analyzes Scala source code" $ do
      sample <- loadFile $ codeSamples </> "scala" </> "sample1.scala"
      let expected = SourceCode
            [ (Comment, "/*")
            , (Comment, " * This is header")
            , (Comment, " */")
            , (Code   , "")
            , (Code   , "package foo")
            , (Code   , "")
            , (Comment, "/* This is not header */")
            , (Code   , "")
            , (Code   , "object Hello extends App {")
            , (Code   , "    println(\"Hello, world!\")")
            , (Comment, "    // line comment")
            , (Code   , "}")
            ]
      analyzeSourceCode (fileSupport Scala) sample `shouldBe` expected

    it "analyzes Shell source code" $ do
      sample <- loadFile $ codeSamples </> "shell" </> "sample1.sh"
      let expected = SourceCode
            [ (Code   , "#!/bin/bash")
            , (Code   , "")
            , (Comment, "# This is")
            , (Comment, "# header")
            , (Code   , "")
            , (Comment, "# This is not")
            , (Code   , "")
            , (Code   , "echo \"TEST\"")
            ]
      analyzeSourceCode (fileSupport Shell) sample `shouldBe` expected
