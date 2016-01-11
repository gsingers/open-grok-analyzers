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

package com.grantingersoll.opengrok.analysis.golang;


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

public class TestGolangSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new GolangSymbolTokenizer(newAttributeFactory());
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
  public void testHexLiteral() throws Exception {
    String input = "0xFFFF";
    String[] output = new String[] {}; // zero output tokens
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void test() throws Exception {
    String input;
    try (InputStream stream = TestGolangSymbolTokenizer.class.getResourceAsStream("stats.go");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                                                                                           //// //// From https://go.googlesource.com/go/+/master/test/bench/garbage/stats.go
                                                                                                                           ////
                                                                                                                           //// // Copyright 2010 The Go Authors.  All rights reserved.
                                                                                                                           //// // Use of this source code is governed by a BSD-style
                                                                                                                           //// // license that can be found in the LICENSE file.
        "main",                                                                                                            //// package main
                                                                                                                           //// import (
                                                                                                                           //// 	"fmt"
                                                                                                                           //// 	"runtime"
                                                                                                                           //// 	"sort"
                                                                                                                           //// 	"time"
                                                                                                                           //// )
        "gcstats", "name", "string", "n", "int", "t", "time", "Duration",                                                  //// func gcstats(name string, n int, t time.Duration) {
        "st", "new", "runtime", "MemStats",                                                                                //// 	st := new(runtime.MemStats)
        "runtime", "ReadMemStats", "st",                                                                                   //// 	runtime.ReadMemStats(st)
        "nprocs", "runtime", "GOMAXPROCS",                                                                                 //// 	nprocs := runtime.GOMAXPROCS(-1)
        "cpus",                                                                                                            //// 	cpus := ""
        "nprocs",                                                                                                          //// 	if nprocs != 1 {
        "cpus", "fmt", "Sprintf", "nprocs",                                                                                //// 		cpus = fmt.Sprintf("-%d", nprocs)
                                                                                                                           //// 	}
        "fmt", "Printf", "name", "cpus", "st", "Alloc", "st", "TotalAlloc", "st", "Sys", "st", "NextGC", "st", "Mallocs",  //// 	fmt.Printf("garbage.%sMem%s Alloc=%d/%d Heap=%d NextGC=%d Mallocs=%d\n", name, cpus, st.Alloc, st.TotalAlloc, st.Sys, st.NextGC, st.Mallocs)
        "fmt", "Printf", "name", "cpus", "n", "t", "Nanoseconds", "int64", "n",                                            //// 	fmt.Printf("garbage.%s%s %d %d ns/op\n", name, cpus, n, t.Nanoseconds()/int64(n))
        "fmt", "Printf", "name", "cpus", "st", "PauseNs", "st", "NumGC", "uint32", "len", "st", "PauseNs",                 //// 	fmt.Printf("garbage.%sLastPause%s 1 %d ns/op\n", name, cpus, st.PauseNs[(st.NumGC-1)%uint32(len(st.PauseNs))])
        "fmt", "Printf", "name", "cpus", "st", "NumGC", "int64", "st", "PauseTotalNs", "int64", "st", "NumGC",             //// 	fmt.Printf("garbage.%sPause%s %d %d ns/op\n", name, cpus, st.NumGC, int64(st.PauseTotalNs)/int64(st.NumGC))
        "nn", "int", "st", "NumGC",                                                                                        //// 	nn := int(st.NumGC)
        "nn", "len", "st", "PauseNs",                                                                                      //// 	if nn >= len(st.PauseNs) {
        "nn", "len", "st", "PauseNs",                                                                                      //// 		nn = len(st.PauseNs)
                                                                                                                           //// 	}
        "t1", "t2", "t3", "t4", "t5", "tukey5", "st", "PauseNs", "nn",                                                     //// 	t1, t2, t3, t4, t5 := tukey5(st.PauseNs[0:nn])
        "fmt", "Printf", "name", "cpus", "t1", "t2", "t3", "t4", "t5",                                                     //// 	fmt.Printf("garbage.%sPause5%s: %d %d %d %d %d\n", name, cpus, t1, t2, t3, t4, t5)
                                                                                                                           //// 	//	fmt.Printf("garbage.%sScan: %v\n", name, st.ScanDist)
                                                                                                                           //// }
        "T", "uint64",                                                                                                     //// type T []uint64
        "t", "T", "Len", "int", "len", "t",                                                                                //// func (t T) Len() int           { return len(t) }
        "t", "T", "Swap", "i", "j", "int", "t", "i", "t", "j", "t", "j", "t", "i",                                         //// func (t T) Swap(i, j int)      { t[i], t[j] = t[j], t[i] }
        "t", "T", "Less", "i", "j", "int", "bool", "t", "i", "t", "j",                                                     //// func (t T) Less(i, j int) bool { return t[i] < t[j] }
        "tukey5", "raw", "uint64", "lo", "q1", "q2", "q3", "hi", "uint64",                                                 //// func tukey5(raw []uint64) (lo, q1, q2, q3, hi uint64) {
        "x", "make", "T", "len", "raw",                                                                                    //// 	x := make(T, len(raw))
        "copy", "x", "raw",                                                                                                //// 	copy(x, raw)
        "sort", "Sort", "T", "x",                                                                                          //// 	sort.Sort(T(x))
        "lo", "x",                                                                                                         //// 	lo = x[0]
        "q1", "x", "len", "x",                                                                                             //// 	q1 = x[len(x)/4]
        "q2", "x", "len", "x",                                                                                             //// 	q2 = x[len(x)/2]
        "q3", "x", "len", "x",                                                                                             //// 	q3 = x[len(x)*3/4]
        "hi", "x", "len", "x",                                                                                             //// 	hi = x[len(x)-1]
                                                                                                                           //// 	return
                                                                                                                           //// }
    };                                                                                                                     ////
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    JFlexTokenizer tokenizer = new GolangSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-go", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    JFlexTokenizer tokenizer = new GolangSymbolTokenizer(newAttributeFactory());
    assertEquals("Golang", tokenizer.getSourceCodeLanguage());
  }
}
