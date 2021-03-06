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
 * Copyright 2006 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */
package com.grantingersoll.opengrok.analysis.lisp;

import java.util.HashSet;
import java.util.Set;

/**
  * Holds static hash set containing Lisp keywords
  */
public class Consts {
    public static final String SOURCE_CODE_LANGUAGE = "Lisp";
    public static final String MIME_TYPE = "application/x-lisp";

    public static final Set<String> kwd = new HashSet<>();
    static {
        kwd.add("and");
        kwd.add("assert");
        kwd.add("case");
        kwd.add("cond");
        kwd.add("define");
        kwd.add("defparameter");
        kwd.add("defstruct");
        kwd.add("defun");
        kwd.add("defvar");
        kwd.add("do");
        kwd.add("do");
        kwd.add("do*");
        kwd.add("dolist");
        kwd.add("dotimes");
        kwd.add("ecase");
        kwd.add("else");
        kwd.add("error");
        kwd.add("etypecase");
        kwd.add("flet");
        kwd.add("handler-bind");
        kwd.add("handler-case");
        kwd.add("if");
        kwd.add("labels");
        kwd.add("let");
        kwd.add("let*");
        kwd.add("loop");
        kwd.add("nil");
        kwd.add("not");
        kwd.add("or");
        kwd.add("otherwise");
        kwd.add("t");
        kwd.add("typecase");
        kwd.add("unless");
        kwd.add("when");
        kwd.add("xor");
    }
}
