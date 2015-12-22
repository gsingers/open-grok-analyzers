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
package com.grantingersoll.opengrok.analysis.clojure;

import java.util.HashSet;
import java.util.Set;

/**
  * Holds static hash set containing Clojure 1.6 keywords
  */
public class Consts {
    public static final Set<String> kwd = new HashSet<>();
    static {
        kwd.add("nil");
        kwd.add("*");
        kwd.add("*1");
        kwd.add("*2");
        kwd.add("*3");
        kwd.add("*agent*");
        kwd.add("*allow-unresolved-vars*");
        kwd.add("*assert*");
        kwd.add("*clojure-version*");
        kwd.add("*command-line-args*");
        kwd.add("*compile-files*");
        kwd.add("*compile-path*");
        kwd.add("*compiler-options*");
        kwd.add("*data-readers*");
        kwd.add("*default-data-reader-fn*");
        kwd.add("*e");
        kwd.add("*err*");
        kwd.add("*file*");
        kwd.add("*flush-on-newline*");
        kwd.add("*fn-loader*");
        kwd.add("*in*");
        kwd.add("*math-context*");
        kwd.add("*ns*");
        kwd.add("*out*");
        kwd.add("*print-dup*");
        kwd.add("*print-length*");
        kwd.add("*print-level*");
        kwd.add("*print-meta*");
        kwd.add("*print-readably*");
        kwd.add("*read-eval*");
        kwd.add("*source-path*");
        kwd.add("*unchecked-math*");
        kwd.add("*use-context-classloader*");
        kwd.add("*verbose-defrecords*");
        kwd.add("*warn-on-reflection*");
        kwd.add("+");
        kwd.add("+'");
        kwd.add("-");
        kwd.add("-'");
        kwd.add("->");
        kwd.add("->>");
        kwd.add("->ArrayChunk");
        kwd.add("->Vec");
        kwd.add("->VecNode");
        kwd.add("->VecSeq");
        kwd.add("-cache-protocol-fn");
        kwd.add("-reset-methods");
        kwd.add("..");
        kwd.add("/");
        kwd.add("<");
        kwd.add("<=");
        kwd.add("=");
        kwd.add("==");
        kwd.add(">");
        kwd.add(">=");
        kwd.add("EMPTY-NODE");
        kwd.add("accessor");
        kwd.add("aclone");
        kwd.add("add-classpath");
        kwd.add("add-watch");
        kwd.add("agent");
        kwd.add("agent-error");
        kwd.add("agent-errors");
        kwd.add("aget");
        kwd.add("alength");
        kwd.add("alias");
        kwd.add("all-ns");
        kwd.add("alter");
        kwd.add("alter-meta!");
        kwd.add("alter-var-root");
        kwd.add("amap");
        kwd.add("ancestors");
        kwd.add("and");
        kwd.add("apply");
        kwd.add("areduce");
        kwd.add("array-map");
        kwd.add("as->");
        kwd.add("aset");
        kwd.add("aset-boolean");
        kwd.add("aset-byte");
        kwd.add("aset-char");
        kwd.add("aset-double");
        kwd.add("aset-float");
        kwd.add("aset-int");
        kwd.add("aset-long");
        kwd.add("aset-short");
        kwd.add("assert");
        kwd.add("assoc!");
        kwd.add("assoc");
        kwd.add("assoc-in");
        kwd.add("associative?");
        kwd.add("atom");
        kwd.add("await");
        kwd.add("await-for");
        kwd.add("await1");
        kwd.add("bases");
        kwd.add("bean");
        kwd.add("bigdec");
        kwd.add("bigint");
        kwd.add("biginteger");
        kwd.add("binding");
        kwd.add("bit-and");
        kwd.add("bit-and-not");
        kwd.add("bit-clear");
        kwd.add("bit-flip");
        kwd.add("bit-not");
        kwd.add("bit-or");
        kwd.add("bit-set");
        kwd.add("bit-shift-left");
        kwd.add("bit-shift-right");
        kwd.add("bit-test");
        kwd.add("bit-xor");
        kwd.add("boolean");
        kwd.add("boolean-array");
        kwd.add("booleans");
        kwd.add("bound-fn");
        kwd.add("bound-fn*");
        kwd.add("bound?");
        kwd.add("butlast");
        kwd.add("byte");
        kwd.add("byte-array");
        kwd.add("bytes");
        kwd.add("case");
        kwd.add("cast");
        kwd.add("char");
        kwd.add("char-array");
        kwd.add("char-escape-string");
        kwd.add("char-name-string");
        kwd.add("char?");
        kwd.add("chars");
        kwd.add("chunk");
        kwd.add("chunk-append");
        kwd.add("chunk-buffer");
        kwd.add("chunk-cons");
        kwd.add("chunk-first");
        kwd.add("chunk-next");
        kwd.add("chunk-rest");
        kwd.add("chunked-seq?");
        kwd.add("class");
        kwd.add("class?");
        kwd.add("clear-agent-errors");
        kwd.add("clojure-version");
        kwd.add("coll?");
        kwd.add("comment");
        kwd.add("commute");
        kwd.add("comp");
        kwd.add("comparator");
        kwd.add("compare");
        kwd.add("compare-and-set!");
        kwd.add("compile");
        kwd.add("complement");
        kwd.add("concat");
        kwd.add("cond");
        kwd.add("cond->");
        kwd.add("cond->>");
        kwd.add("condp");
        kwd.add("conj!");
        kwd.add("conj");
        kwd.add("cons");
        kwd.add("constantly");
        kwd.add("construct-proxy");
        kwd.add("contains?");
        kwd.add("count");
        kwd.add("counted?");
        kwd.add("create-ns");
        kwd.add("create-struct");
        kwd.add("cycle");
        kwd.add("dec");
        kwd.add("dec'");
        kwd.add("decimal?");
        kwd.add("declare");
        kwd.add("def");
        kwd.add("default-data-readers");
        kwd.add("definline");
        kwd.add("definterface");
        kwd.add("defmacro");
        kwd.add("defmethod");
        kwd.add("defmulti");
        kwd.add("defn");
        kwd.add("defn-");
        kwd.add("defonce");
        kwd.add("defprotocol");
        kwd.add("defrecord");
        kwd.add("defstruct");
        kwd.add("deftype");
        kwd.add("delay");
        kwd.add("delay?");
        kwd.add("deliver");
        kwd.add("denominator");
        kwd.add("deref");
        kwd.add("derive");
        kwd.add("descendants");
        kwd.add("destructure");
        kwd.add("disj!");
        kwd.add("disj");
        kwd.add("dissoc!");
        kwd.add("dissoc");
        kwd.add("distinct");
        kwd.add("distinct?");
        kwd.add("do");
        kwd.add("doall");
        kwd.add("dorun");
        kwd.add("doseq");
        kwd.add("dosync");
        kwd.add("dotimes");
        kwd.add("doto");
        kwd.add("double");
        kwd.add("double-array");
        kwd.add("doubles");
        kwd.add("drop");
        kwd.add("drop-last");
        kwd.add("drop-while");
        kwd.add("empty");
        kwd.add("empty?");
        kwd.add("ensure");
        kwd.add("enumeration-seq");
        kwd.add("error-handler");
        kwd.add("error-mode");
        kwd.add("eval");
        kwd.add("even?");
        kwd.add("every-pred");
        kwd.add("every?");
        kwd.add("ex-data");
        kwd.add("ex-info");
        kwd.add("extend");
        kwd.add("extend-protocol");
        kwd.add("extend-type");
        kwd.add("extenders");
        kwd.add("extends?");
        kwd.add("false");
        kwd.add("false?");
        kwd.add("ffirst");
        kwd.add("file-seq");
        kwd.add("filter");
        kwd.add("filterv");
        kwd.add("finally");
        kwd.add("find");
        kwd.add("find-keyword");
        kwd.add("find-ns");
        kwd.add("find-protocol-impl");
        kwd.add("find-protocol-method");
        kwd.add("find-var");
        kwd.add("first");
        kwd.add("flatten");
        kwd.add("float");
        kwd.add("float-array");
        kwd.add("float?");
        kwd.add("floats");
        kwd.add("flush");
        kwd.add("fn");
        kwd.add("fn?");
        kwd.add("fnext");
        kwd.add("fnil");
        kwd.add("for");
        kwd.add("force");
        kwd.add("format");
        kwd.add("frequencies");
        kwd.add("future");
        kwd.add("future-call");
        kwd.add("future-cancel");
        kwd.add("future-cancelled?");
        kwd.add("future-done?");
        kwd.add("future?");
        kwd.add("gen-class");
        kwd.add("gen-interface");
        kwd.add("gensym");
        kwd.add("get");
        kwd.add("get-in");
        kwd.add("get-method");
        kwd.add("get-proxy-class");
        kwd.add("get-thread-bindings");
        kwd.add("get-validator");
        kwd.add("group-by");
        kwd.add("hash");
        kwd.add("hash-combine");
        kwd.add("hash-map");
        kwd.add("hash-ordered-coll");
        kwd.add("hash-set");
        kwd.add("hash-unordered-coll");
        kwd.add("identical?");
        kwd.add("identity");
        kwd.add("if");
        kwd.add("if-let");
        kwd.add("if-not");
        kwd.add("if-some");
        kwd.add("ifn?");
        kwd.add("import");
        kwd.add("in-ns");
        kwd.add("inc");
        kwd.add("inc'");
        kwd.add("init-proxy");
        kwd.add("instance?");
        kwd.add("int");
        kwd.add("int-array");
        kwd.add("integer?");
        kwd.add("interleave");
        kwd.add("intern");
        kwd.add("interpose");
        kwd.add("into");
        kwd.add("into-array");
        kwd.add("ints");
        kwd.add("io!");
        kwd.add("isa?");
        kwd.add("iterate");
        kwd.add("iterator-seq");
        kwd.add("juxt");
        kwd.add("keep");
        kwd.add("keep-indexed");
        kwd.add("key");
        kwd.add("keys");
        kwd.add("keyword");
        kwd.add("keyword?");
        kwd.add("last");
        kwd.add("lazy-cat");
        kwd.add("lazy-seq");
        kwd.add("let");
        kwd.add("letfn");
        kwd.add("line-seq");
        kwd.add("list");
        kwd.add("list*");
        kwd.add("list?");
        kwd.add("load");
        kwd.add("load-file");
        kwd.add("load-reader");
        kwd.add("load-string");
        kwd.add("loaded-libs");
        kwd.add("locking");
        kwd.add("long");
        kwd.add("long-array");
        kwd.add("longs");
        kwd.add("loop");
        kwd.add("macroexpand");
        kwd.add("macroexpand-1");
        kwd.add("make-array");
        kwd.add("make-hierarchy");
        kwd.add("map");
        kwd.add("map-indexed");
        kwd.add("map?");
        kwd.add("mapcat");
        kwd.add("mapv");
        kwd.add("max");
        kwd.add("max-key");
        kwd.add("memfn");
        kwd.add("memoize");
        kwd.add("merge");
        kwd.add("merge-with");
        kwd.add("meta");
        kwd.add("method-sig");
        kwd.add("methods");
        kwd.add("min");
        kwd.add("min-key");
        kwd.add("mix-collection-hash");
        kwd.add("mod");
        kwd.add("monitor-enter");
        kwd.add("monitor-exit");
        kwd.add("munge");
        kwd.add("name");
        kwd.add("namespace");
        kwd.add("namespace-munge");
        kwd.add("neg?");
        kwd.add("new");
        kwd.add("newline");
        kwd.add("next");
        kwd.add("nfirst");
        kwd.add("nil?");
        kwd.add("nnext");
        kwd.add("not");
        kwd.add("not-any?");
        kwd.add("not-empty");
        kwd.add("not-every?");
        kwd.add("not=");
        kwd.add("ns");
        kwd.add("ns-aliases");
        kwd.add("ns-imports");
        kwd.add("ns-interns");
        kwd.add("ns-map");
        kwd.add("ns-name");
        kwd.add("ns-publics");
        kwd.add("ns-refers");
        kwd.add("ns-resolve");
        kwd.add("ns-unalias");
        kwd.add("ns-unmap");
        kwd.add("nth");
        kwd.add("nthnext");
        kwd.add("nthrest");
        kwd.add("num");
        kwd.add("number?");
        kwd.add("numerator");
        kwd.add("object-array");
        kwd.add("odd?");
        kwd.add("or");
        kwd.add("parents");
        kwd.add("partial");
        kwd.add("partition");
        kwd.add("partition-all");
        kwd.add("partition-by");
        kwd.add("pcalls");
        kwd.add("peek");
        kwd.add("persistent!");
        kwd.add("pmap");
        kwd.add("pop!");
        kwd.add("pop");
        kwd.add("pop-thread-bindings");
        kwd.add("pos?");
        kwd.add("pr");
        kwd.add("pr-str");
        kwd.add("prefer-method");
        kwd.add("prefers");
        kwd.add("primitives-classnames");
        kwd.add("print");
        kwd.add("print-ctor");
        kwd.add("print-dup");
        kwd.add("print-method");
        kwd.add("print-simple");
        kwd.add("print-str");
        kwd.add("printf");
        kwd.add("println");
        kwd.add("println-str");
        kwd.add("prn");
        kwd.add("prn-str");
        kwd.add("promise");
        kwd.add("proxy");
        kwd.add("proxy-call-with-super");
        kwd.add("proxy-mappings");
        kwd.add("proxy-name");
        kwd.add("proxy-super");
        kwd.add("push-thread-bindings");
        kwd.add("pvalues");
        kwd.add("quot");
        kwd.add("quote");
        kwd.add("rand");
        kwd.add("rand-int");
        kwd.add("rand-nth");
        kwd.add("range");
        kwd.add("ratio?");
        kwd.add("rational?");
        kwd.add("rationalize");
        kwd.add("re-find");
        kwd.add("re-groups");
        kwd.add("re-matcher");
        kwd.add("re-matches");
        kwd.add("re-pattern");
        kwd.add("re-seq");
        kwd.add("read");
        kwd.add("read-line");
        kwd.add("read-string");
        kwd.add("realized?");
        kwd.add("record?");
        kwd.add("recur");
        kwd.add("reduce");
        kwd.add("reduce-kv");
        kwd.add("reduced");
        kwd.add("reduced?");
        kwd.add("reductions");
        kwd.add("ref");
        kwd.add("ref-history-count");
        kwd.add("ref-max-history");
        kwd.add("ref-min-history");
        kwd.add("ref-set");
        kwd.add("refer");
        kwd.add("refer-clojure");
        kwd.add("reify");
        kwd.add("release-pending-sends");
        kwd.add("rem");
        kwd.add("remove");
        kwd.add("remove-all-methods");
        kwd.add("remove-method");
        kwd.add("remove-ns");
        kwd.add("remove-watch");
        kwd.add("repeat");
        kwd.add("repeatedly");
        kwd.add("replace");
        kwd.add("replicate");
        kwd.add("require");
        kwd.add("reset!");
        kwd.add("reset-meta!");
        kwd.add("resolve");
        kwd.add("rest");
        kwd.add("restart-agent");
        kwd.add("resultset-seq");
        kwd.add("reverse");
        kwd.add("reversible?");
        kwd.add("rseq");
        kwd.add("rsubseq");
        kwd.add("satisfies?");
        kwd.add("second");
        kwd.add("select-keys");
        kwd.add("send");
        kwd.add("send-off");
        kwd.add("send-via");
        kwd.add("seq");
        kwd.add("seq?");
        kwd.add("seque");
        kwd.add("sequence");
        kwd.add("sequential?");
        kwd.add("set!");
        kwd.add("set");
        kwd.add("set-agent-send-executor!");
        kwd.add("set-agent-send-off-executor!");
        kwd.add("set-error-handler!");
        kwd.add("set-error-mode!");
        kwd.add("set-validator!");
        kwd.add("set?");
        kwd.add("short");
        kwd.add("short-array");
        kwd.add("shorts");
        kwd.add("shuffle");
        kwd.add("shutdown-agents");
        kwd.add("slurp");
        kwd.add("some");
        kwd.add("some->");
        kwd.add("some->>");
        kwd.add("some-fn");
        kwd.add("some?");
        kwd.add("sort");
        kwd.add("sort-by");
        kwd.add("sorted-map");
        kwd.add("sorted-map-by");
        kwd.add("sorted-set");
        kwd.add("sorted-set-by");
        kwd.add("sorted?");
        kwd.add("special-symbol?");
        kwd.add("spit");
        kwd.add("split-at");
        kwd.add("split-with");
        kwd.add("str");
        kwd.add("string?");
        kwd.add("struct");
        kwd.add("struct-map");
        kwd.add("subs");
        kwd.add("subseq");
        kwd.add("subvec");
        kwd.add("supers");
        kwd.add("swap!");
        kwd.add("symbol");
        kwd.add("symbol?");
        kwd.add("sync");
        kwd.add("take");
        kwd.add("take-last");
        kwd.add("take-nth");
        kwd.add("take-while");
        kwd.add("test");
        kwd.add("the-ns");
        kwd.add("thread-bound?");
        kwd.add("throw");
        kwd.add("time");
        kwd.add("to-array");
        kwd.add("to-array-2d");
        kwd.add("trampoline");
        kwd.add("transient");
        kwd.add("tree-seq");
        kwd.add("true");
        kwd.add("true?");
        kwd.add("try");
        kwd.add("type");
        kwd.add("unchecked-add");
        kwd.add("unchecked-add-int");
        kwd.add("unchecked-byte");
        kwd.add("unchecked-char");
        kwd.add("unchecked-dec");
        kwd.add("unchecked-dec-int");
        kwd.add("unchecked-divide-int");
        kwd.add("unchecked-double");
        kwd.add("unchecked-float");
        kwd.add("unchecked-inc");
        kwd.add("unchecked-inc-int");
        kwd.add("unchecked-int");
        kwd.add("unchecked-long");
        kwd.add("unchecked-multiply");
        kwd.add("unchecked-multiply-int");
        kwd.add("unchecked-negate");
        kwd.add("unchecked-negate-int");
        kwd.add("unchecked-remainder-int");
        kwd.add("unchecked-short");
        kwd.add("unchecked-subtract");
        kwd.add("unchecked-subtract-int");
        kwd.add("underive");
        kwd.add("unquote");
        kwd.add("unquote-splicing");
        kwd.add("unsigned-bit-shift-right");
        kwd.add("update-in");
        kwd.add("update-proxy");
        kwd.add("use");
        kwd.add("val");
        kwd.add("vals");
        kwd.add("var");
        kwd.add("var-get");
        kwd.add("var-set");
        kwd.add("var?");
        kwd.add("vary-meta");
        kwd.add("vec");
        kwd.add("vector");
        kwd.add("vector-of");
        kwd.add("vector?");
        kwd.add("when");
        kwd.add("when-first");
        kwd.add("when-let");
        kwd.add("when-not");
        kwd.add("when-some");
        kwd.add("while");
        kwd.add("with-bindings");
        kwd.add("with-bindings*");
        kwd.add("with-in-str");
        kwd.add("with-loading-context");
        kwd.add("with-local-vars");
        kwd.add("with-meta");
        kwd.add("with-open");
        kwd.add("with-out-str");
        kwd.add("with-precision");
        kwd.add("with-redefs");
        kwd.add("with-redefs-fn");
        kwd.add("xml-seq");
        kwd.add("zero?");
        kwd.add("zipmap");
    }
}
