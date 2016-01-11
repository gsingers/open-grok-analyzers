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

package com.grantingersoll.opengrok.analysis.erlang;


import com.grantingersoll.opengrok.analysis.SymbolTokenizer;
import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.BaseTokenStreamTestCase;
import org.apache.lucene.analysis.Tokenizer;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class TestErlangSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new ErlangSymbolTokenizer(newAttributeFactory());
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
    String input = "16#FFFF";
    String[] output = new String[] {}; // zero output tokens
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void test() throws Exception {
    String input;
    try (InputStream stream = TestErlangSymbolTokenizer.class.getResourceAsStream("pg_async.erl");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
                                                             //// %%%% From https://github.com/erlang/otp/blob/maint/erts/example/pg_async.erl
                                                             ////
                                                             //// %%
                                                             //// %% %CopyrightBegin%
                                                             //// %%
                                                             //// %% Copyright Ericsson AB 2006-2009. All Rights Reserved.
                                                             //// %%
                                                             //// %% Licensed under the Apache License, Version 2.0 (the "License");
                                                             //// %% you may not use this file except in compliance with the License.
                                                             //// %% You may obtain a copy of the License at
                                                             //// %%
                                                             //// %%     http://www.apache.org/licenses/LICENSE-2.0
                                                             //// %%
                                                             //// %% Unless required by applicable law or agreed to in writing, software
                                                             //// %% distributed under the License is distributed on an "AS IS" BASIS,
                                                             //// %% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
                                                             //// %% See the License for the specific language governing permissions and
                                                             //// %% limitations under the License.
                                                             //// %%
                                                             //// %% %CopyrightEnd%
                                                             //// %%
        "module", "pg_async",                                //// -module(pg_async).
                                                             ////
        "define", "DRV_CONNECT", "C",                        //// -define(DRV_CONNECT, $C).
        "define", "DRV_DISCONNECT", "D",                     //// -define(DRV_DISCONNECT, $D).
        "define", "DRV_SELECT", "S",                         //// -define(DRV_SELECT, $S).
                                                             ////
        "export", "connect", "disconnect", "select",         //// -export([connect/1, disconnect/1, select/2]).
                                                             ////
        "connect", "ConnectStr",                             //// connect(ConnectStr) ->
        "erl_ddll","load_driver",                            ////     case erl_ddll:load_driver(".", "pg_async") of
        "ok", "ok",                                          //// 	ok -> ok;
        "error", "already_loaded", "ok",                     //// 	{error, already_loaded} -> ok;
        "E", "exit", "E",                                    //// 	E -> exit(E)
                                                             ////     end,
        "Port", "open_port", "spawn", "MODULE", "binary",    ////     Port = open_port({spawn, ?MODULE}, [binary]),
        "port_control", "Port","DRV_CONNECT", "ConnectStr",  ////     port_control(Port, ?DRV_CONNECT, ConnectStr),
        "return_port_data", "Port",                          ////     case return_port_data(Port) of
        "ok",                                                //// 	ok ->
        "ok", "Port",                                        //// 	    {ok, Port};
        "Error",                                             //// 	Error ->
        "Error",                                             //// 	    Error
                                                             ////     end.
                                                             ////
        "disconnect", "Port",                                //// disconnect(Port) ->
        "port_control", "Port", "DRV_DISCONNECT",            ////     port_control(Port, ?DRV_DISCONNECT, ""),
        "R", "return_port_data", "Port",                     ////     R = return_port_data(Port),
        "port_close", "Port",                                ////     port_close(Port),
        "R",                                                 ////     R.
                                                             ////
        "select", "Port", "Query",                           //// select(Port, Query) ->
        "port_control", "Port", "DRV_SELECT", "Query",       ////     port_control(Port, ?DRV_SELECT, Query),
        "return_port_data", "Port",                          ////     return_port_data(Port).
                                                             ////
        "return_port_data", "Port",                          //// return_port_data(Port) ->
                                                             ////     receive
        "Port", "data", "Data",                              //// 	{Port, {data, Data}} ->
        "binary_to_term", "Data",                            //// 	    binary_to_term(Data)
                                                             ////     end.
                                                             ////
    };
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new ErlangSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-erlang", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new ErlangSymbolTokenizer(newAttributeFactory());
    assertEquals("Erlang", tokenizer.getSourceCodeLanguage());
  }
}
