/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * See LICENSE.txt included in this distribution for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at LICENSE.txt.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */

package com.grantingersoll.opengrok.analysis.haskell;


import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.BaseTokenStreamTestCase;
import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class TestHaskellSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new HaskellSymbolTokenizer(newAttributeFactory());
        return new TokenStreamComponents(tokenizer);
      }
    };
  }

  @Override
  public void tearDown() throws Exception {
    analyzer.close();
    super.tearDown();
  }

  @Test
  public void testNumericLiterals() throws Exception {
    String input = "0xFFFF + 0o777";
    String[] output = new String[] {}; // zero output tokens
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void test() throws Exception {
    String input;
    try (InputStream stream = TestHaskellSymbolTokenizer.class.getResourceAsStream("QSemN.hs");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                                             //// ---- From http://git.haskell.org/packages/base.git/blob_plain/HEAD:/Control/Concurrent/QSemN.hs
                                                                                             ////
                                                                                             //// {-# LANGUAGE Trustworthy #-}
                                                                                             //// {-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
                                                                                             //// {-# OPTIONS_GHC -funbox-strict-fields #-}
                                                                                             ////
                                                                                             //// -----------------------------------------------------------------------------
                                                                                             //// -- |
                                                                                             //// -- Module      :  Control.Concurrent.QSemN
                                                                                             //// -- Copyright   :  (c) The University of Glasgow 2001
                                                                                             //// -- License     :  BSD-style (see the file libraries/base/LICENSE)
                                                                                             //// --
                                                                                             //// -- Maintainer  :  libraries@haskell.org
                                                                                             //// -- Stability   :  experimental
                                                                                             //// -- Portability :  non-portable (concurrency)
                                                                                             //// --
                                                                                             //// -- Quantity semaphores in which each thread may wait for an arbitrary
                                                                                             //// -- \"amount\".
                                                                                             //// --
                                                                                             //// -----------------------------------------------------------------------------
                                                                                             ////
        "Control", "Concurrent", "QSemN",                                                    //// module Control.Concurrent.QSemN
                                                                                             ////         (  -- * General Quantity Semaphores
        "QSemN",                                                                             ////           QSemN,        -- abstract
        "newQSemN",                                                                          ////           newQSemN,     -- :: Int   -> IO QSemN
        "waitQSemN",                                                                         ////           waitQSemN,    -- :: QSemN -> Int -> IO ()
        "signalQSemN",                                                                       ////           signalQSemN   -- :: QSemN -> Int -> IO ()
                                                                                             ////       ) where
                                                                                             ////
        "Control", "Concurrent", "MVar", "MVar", "newEmptyMVar", "takeMVar", "tryTakeMVar",  //// import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar, tryTakeMVar
        "putMVar", "newMVar",                                                                ////                           , putMVar, newMVar
        "tryPutMVar", "isEmptyMVar",                                                         ////                           , tryPutMVar, isEmptyMVar)
        "Data", "Typeable",                                                                  //// import Data.Typeable
        "Control", "Exception",                                                              //// import Control.Exception
        "Data", "Maybe",                                                                     //// import Data.Maybe
                                                                                             ////
                                                                                             //// -- | 'QSemN' is a quantity semaphore in which the resource is aqcuired
                                                                                             //// -- and released in units of one. It provides guaranteed FIFO ordering
                                                                                             //// -- for satisfying blocked `waitQSemN` calls.
                                                                                             //// --
                                                                                             //// -- The pattern
                                                                                             //// --
                                                                                             //// -- >   bracket_ (waitQSemN n) (signalQSemN n) (...)
                                                                                             //// --
                                                                                             //// -- is safe; it never loses any of the resource.
                                                                                             //// --
        "QSemN", "QSemN", "MVar", "Int", "Int", "MVar", "Int", "MVar",                       //// data QSemN = QSemN !(MVar (Int, [(Int, MVar ())], [(Int, MVar ())]))
        "Typeable",                                                                          ////   deriving Typeable
                                                                                             ////
                                                                                             //// -- The semaphore state (i, xs, ys):
                                                                                             //// --
                                                                                             //// --   i is the current resource value
                                                                                             //// --
                                                                                             //// --   (xs,ys) is the queue of blocked threads, where the queue is
                                                                                             //// --           given by xs ++ reverse ys.  We can enqueue new blocked threads
                                                                                             //// --           by consing onto ys, and dequeue by removing from the head of xs.
                                                                                             //// --
                                                                                             //// -- A blocked thread is represented by an empty (MVar ()).  To unblock
                                                                                             //// -- the thread, we put () into the MVar.
                                                                                             //// --
                                                                                             //// -- A thread can dequeue itself by also putting () into the MVar, which
                                                                                             //// -- it must do if it receives an exception while blocked in waitQSemN.
                                                                                             //// -- This means that when unblocking a thread in signalQSemN we must
                                                                                             //// -- first check whether the MVar is already full; the MVar lock on the
                                                                                             //// -- semaphore itself resolves race conditions between signalQSemN and a
                                                                                             //// -- thread attempting to dequeue itself.
                                                                                             ////
                                                                                             //// -- |Build a new 'QSemN' with a supplied initial quantity.
                                                                                             //// --  The initial quantity must be at least 0.
        "newQSemN", "Int", "IO", "QSemN",                                                    //// newQSemN :: Int -> IO QSemN
        "newQSemN", "initial",                                                               //// newQSemN initial
        "initial", "fail",                                                                   ////   | initial < 0 = fail "newQSemN: Initial quantity must be non-negative"
        "otherwise",                                                                         ////   | otherwise   = do
        "sem", "newMVar", "initial",                                                         ////       sem <- newMVar (initial, [], [])
        "return", "QSemN", "sem",                                                            ////       return (QSemN sem)
                                                                                             ////
                                                                                             //// -- |Wait for the specified quantity to become available
        "waitQSemN", "QSemN", "Int", "IO",                                                   //// waitQSemN :: QSemN -> Int -> IO ()
        "waitQSemN", "QSemN", "m", "sz",                                                     //// waitQSemN (QSemN m) sz =
        "mask_",                                                                             ////   mask_ $ do
        "i", "b1", "b2", "takeMVar", "m",                                                    ////     (i,b1,b2) <- takeMVar m
        "z", "i", "sz",                                                                      ////     let z = i-sz
        "z",                                                                                 ////     if z < 0
                                                                                             ////        then do
        "b", "newEmptyMVar",                                                                 ////          b <- newEmptyMVar
        "putMVar", "m", "i", "b1", "sz", "b", "b2",                                          ////          putMVar m (i, b1, (sz,b):b2)
        "wait", "b",                                                                         ////          wait b
                                                                                             ////        else do
        "putMVar", "m", "z", "b1", "b2",                                                     ////          putMVar m (z, b1, b2)
        "return",                                                                            ////          return ()
                                                                                             ////   where
        "wait", "b",                                                                         ////     wait b = do
        "takeMVar", "b", "onException",                                                      ////         takeMVar b `onException`
        "uninterruptibleMask_",                                                              ////                 (uninterruptibleMask_ $ do -- Note [signal uninterruptible]
        "i", "b1", "b2", "takeMVar", "m",                                                    ////                    (i,b1,b2) <- takeMVar m
        "r", "tryTakeMVar", "b",                                                             ////                    r <- tryTakeMVar b
        "r'", "isJust", "r",                                                                 ////                    r' <- if isJust r
        "signal", "sz", "i", "b1", "b2",                                                     ////                             then signal sz (i,b1,b2)
        "putMVar", "b", "return", "i", "b1", "b2",                                           ////                             else do putMVar b (); return (i,b1,b2)
        "putMVar", "m", "r'",                                                                ////                    putMVar m r')
                                                                                             ////
                                                                                             //// -- |Signal that a given quantity is now available from the 'QSemN'.
        "signalQSemN", "QSemN", "Int", "IO",                                                 //// signalQSemN :: QSemN -> Int -> IO ()
        "signalQSemN", "QSemN", "m", "sz", "uninterruptibleMask_",                           //// signalQSemN (QSemN m) sz = uninterruptibleMask_ $ do
        "r", "takeMVar", "m",                                                                ////   r <- takeMVar m
        "r'", "signal", "sz", "r",                                                           ////   r' <- signal sz r
        "putMVar", "m", "r'",                                                                ////   putMVar m r'
                                                                                             ////
        "signal", "Int",                                                                     //// signal :: Int
        "Int", "Int", "MVar", "Int", "MVar",                                                 ////        -> (Int,[(Int,MVar ())],[(Int,MVar ())])
        "IO", "Int", "Int", "MVar", "Int", "MVar",                                           ////        -> IO (Int,[(Int,MVar ())],[(Int,MVar ())])
                                                                                             ////
        "signal", "sz0", "i", "a1", "a2", "loop", "sz0", "i", "a1", "a2",                    //// signal sz0 (i,a1,a2) = loop (sz0 + i) a1 a2
                                                                                             ////  where
        "loop", "bs", "b2", "return", "bs", "b2",                                            ////    loop 0  bs b2 = return (0,  bs, b2)
        "loop", "sz", "return", "sz",                                                        ////    loop sz [] [] = return (sz, [], [])
        "loop", "sz", "b2", "loop", "sz", "reverse", "b2",                                   ////    loop sz [] b2 = loop sz (reverse b2) []
        "loop", "sz", "j", "b", "bs", "b2",                                                  ////    loop sz ((j,b):bs) b2
        "j", "sz",                                                                           ////      | j > sz = do
        "r", "isEmptyMVar", "b",                                                             ////        r <- isEmptyMVar b
        "r", "return", "sz", "j", "b", "bs", "b2",                                           ////        if r then return (sz, (j,b):bs, b2)
        "loop", "sz", "bs", "b2",                                                            ////             else loop sz bs b2
        "otherwise",                                                                         ////      | otherwise = do
        "r", "tryPutMVar", "b",                                                              ////        r <- tryPutMVar b ()
        "r", "loop", "sz", "j", "bs", "b2",                                                  ////        if r then loop (sz-j) bs b2
        "loop", "sz", "bs", "b2",                                                            ////             else loop sz bs b2
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    JFlexTokenizer tokenizer = new HaskellSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-haskell", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    JFlexTokenizer tokenizer = new HaskellSymbolTokenizer(newAttributeFactory());
    assertEquals("Haskell", tokenizer.getSourceCodeLanguage());
  }
}
