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

/*
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved.
 */

package com.grantingersoll.opengrok.analysis.erlang;

import java.util.HashSet;
import java.util.Set;

/**
  * Holds static hash set containing the Erlang keywords
  *
  * Source: "Reserved Words" section at http://www.erlang.org/doc/reference_manual/introduction.html
  */
public class Consts{
    public static final Set<String> kwd = new HashSet<>() ;
    static {
        kwd.add("after");
        kwd.add("and");
        kwd.add("andalso");
        kwd.add("band");
        kwd.add("begin");
        kwd.add("bnot");
        kwd.add("bor");
        kwd.add("bsl");
        kwd.add("bsr");
        kwd.add("bxor");
        kwd.add("case");
        kwd.add("catch");
        kwd.add("cond");
        kwd.add("div");
        kwd.add("end");
        kwd.add("fun");
        kwd.add("if");
        kwd.add("let");
        kwd.add("not");
        kwd.add("of");
        kwd.add("or");
        kwd.add("orelse");
        // kwd.add("query"); // query no longer reserved as of Erlang R16B - see https://github.com/ignatov/intellij-erlang/issues/220
        kwd.add("receive");
        kwd.add("rem");
        kwd.add("try");
        kwd.add("when");
        kwd.add("xor");
    }
}
