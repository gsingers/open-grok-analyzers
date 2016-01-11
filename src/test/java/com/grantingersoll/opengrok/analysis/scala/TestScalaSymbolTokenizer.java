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

package com.grantingersoll.opengrok.analysis.scala;


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

public class TestScalaSymbolTokenizer extends BaseTokenStreamTestCase {
  private Analyzer analyzer;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    analyzer = new Analyzer() {
      @Override
      protected TokenStreamComponents createComponents(String fieldName) {
        Tokenizer tokenizer = new ScalaSymbolTokenizer(newAttributeFactory());
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
    try (InputStream stream = TestScalaSymbolTokenizer.class.getResourceAsStream("computeserver.scala");
         Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      input = IOUtils.toString(in);
    }
    String[] output = new String[] {
        "examples",                                                                                             //// package examples
                                                                                                                ////
        "language", "implicitConversions",                                                                      //// import language.implicitConversions
                                                                                                                ////
        "scala", "concurrent", "Channel", "ExecutionContext", "future", "Future", "promise",                    //// import scala.concurrent.{ Channel, ExecutionContext, future, Future, promise }
        "scala", "concurrent", "util", "Duration",                                                              //// import scala.concurrent.util.Duration
        "scala", "util", "Try", "Success", "Failure",                                                           //// import scala.util.{ Try, Success, Failure }
        "java", "util", "concurrent", "CountDownLatch", "Executors",                                            //// import java.util.concurrent.{ CountDownLatch, Executors }
        "java", "util", "concurrent", "atomic", "_",                                                            //// import java.util.concurrent.atomic._
                                                                                                                ////
        "computeServer",                                                                                        //// object computeServer {
                                                                                                                ////
        "Stats", "Tuple3", "Int", "Int", "Int",                                                                 ////   type Stats = Tuple3[Int,Int,Int]
                                                                                                                ////
        "ComputeServer", "n", "Int", "completer", "Try", "Stats", "Unit", "ctx", "ExecutionContext",            ////   class ComputeServer(n: Int, completer: Try[Stats] => Unit)(implicit ctx: ExecutionContext) {
                                                                                                                ////
        "Job",                                                                                                  ////     private trait Job {
        "T",                                                                                                    ////       type T
        "task", "T",                                                                                            ////       def task: T
        "complete", "x", "Try", "T", "Unit",                                                                    ////       def complete(x: Try[T]): Unit
                                                                                                                ////     }
                                                                                                                ////
        "openJobs", "Channel", "Job",                                                                           ////     private val openJobs = new Channel[Job]()
                                                                                                                ////
        "processor", "i", "Int",                                                                                ////     private def processor(i: Int) = {
        "printf", "i",                                                                                          ////       printf("processor %d starting\n", i)
                                                                                                                ////       // simulate failure in faulty #3
        "i",  "IllegalStateException", "format", "i",                                                           ////       if (i == 3) throw new IllegalStateException("processor %d: Drat!" format i)
        "good",                                                                                                 ////       var good = 0
        "bad",                                                                                                  ////       var bad = 0
        "isDone",                                                                                               ////       while (!isDone) {
        "job", "openJobs", "read",                                                                              ////         val job = openJobs.read
        "printf", "i",                                                                                          ////         printf("processor %d read a job\n", i)
        "res", "Try", "job", "task",                                                                            ////         val res = Try(job.task)
        "res", "isSuccess", "good",                                                                             ////         if (res.isSuccess) good += 1
        "bad",                                                                                                  ////         else bad += 1
        "job", "complete", "res",                                                                               ////         job complete res
                                                                                                                ////       }
        "printf", "i",                                                                                          ////       printf("processor %d terminating\n", i)
        "i", "good", "bad",                                                                                     ////       (i, good, bad)
                                                                                                                ////     }
                                                                                                                ////
        "submit", "A", "body", "A", "Future", "A",                                                              ////     def submit[A](body: => A): Future[A] = {
        "p", "promise", "A",                                                                                    ////       val p = promise[A]()
        "openJobs", "write",                                                                                    ////       openJobs.write {
        "Job",                                                                                                  ////         new Job {
        "T", "A",                                                                                               ////           type T = A
        "task", "body",                                                                                         ////           def task = body
        "complete", "x", "Try", "A", "p", "complete", "x",                                                      ////           def complete(x: Try[A]) = p complete x
                                                                                                                ////         }
                                                                                                                ////       }
        "p", "future",                                                                                          ////       p.future
                                                                                                                ////     }
                                                                                                                ////
        "done", "AtomicBoolean",                                                                                ////     val done = new AtomicBoolean
        "isDone", "done", "get",                                                                                ////     def isDone = done.get
        "finish",                                                                                               ////     def finish() {
        "done", "set",                                                                                          ////       done set true
        "nilJob",                                                                                               ////       val nilJob =
        "Job",                                                                                                  ////         new Job {
        "T", "Null",                                                                                            ////           type T = Null
        "task",                                                                                                 ////           def task = null
        "complete", "x", "Try", "Null",                                                                         ////           def complete(x: Try[Null]) { }
                                                                                                                ////         }
                                                                                                                ////       // unblock readers
        "_", "to", "n", "openJobs", "write", "nilJob",                                                          ////       for (_ <- 1 to n) { openJobs write nilJob }
                                                                                                                ////     }
                                                                                                                ////
                                                                                                                ////     // You can, too! http://www.manning.com/suereth/
        "futured", "A", "B", "f", "A", "B", "A", "Future", "B", "in", "future", "f", "in",                      ////     def futured[A,B](f: A => B): A => Future[B] = { in => future(f(in)) }
        "futureHasArrived", "f", "Future", "Stats", "f", "onComplete", "completer",                             ////     def futureHasArrived(f: Future[Stats]) = f onComplete completer
                                                                                                                ////
        "to", "n", "map", "futured", "processor", "foreach", "futureHasArrived",                                ////     1 to n map futured(processor) foreach futureHasArrived
                                                                                                                ////   }
                                                                                                                ////
        "inline", "Whiling", "latch", "CountDownLatch", "AnyVal",                                               ////   @inline implicit class Whiling(val latch: CountDownLatch) extends AnyVal {
        "awaitAwhile", "d", "Duration", "Boolean", "latch", "await", "d", "length", "d", "unit",                ////     def awaitAwhile()(implicit d: Duration): Boolean = latch.await(d.length, d.unit)
                                                                                                                ////   }
                                                                                                                ////
        "main", "args", "Array", "String",                                                                      ////   def main(args: Array[String]) {
        "usage", "msg", "String", "Nothing",                                                                    ////     def usage(msg: String = "scala examples.computeServer <n>"): Nothing = {
        "println", "msg",                                                                                       ////       println(msg)
        "sys", "exit",                                                                                          ////       sys.exit(1)
                                                                                                                ////     }
        "args", "length", "usage",                                                                              ////     if (args.length > 1) usage()
        "rt", "Runtime", "getRuntime",                                                                          ////     val rt = Runtime.getRuntime
        "rt", "availableProcessors", "avail",                                                                   ////     import rt.{ availableProcessors => avail }
        "using", "n", "Int", "println", "n",                                                                    ////     def using(n: Int) = { println(s"Using $n processors"); n }
        "numProcessors", "Try", "args", "head", "toInt", "filter", "_", "map", "_", "min", "avail", "recover",  ////     val numProcessors = (Try(args.head.toInt) filter (_ > 0) map (_ min avail) recover {
        "_", "NumberFormatException", "usage",                                                                  ////       case _: NumberFormatException => usage()
        "_", "using", "min", "avail",                                                                           ////       case _ => using(4 min avail)
        "get",                                                                                                  ////     }).get
                                                                                                                ////
        "ctx", "ExecutionContext", "fromExecutorService", "Executors", "newFixedThreadPool", "numProcessors",   ////     implicit val ctx = ExecutionContext fromExecutorService (Executors newFixedThreadPool (numProcessors))
        "doneLatch", "CountDownLatch", "numProcessors",                                                         ////     val doneLatch = new CountDownLatch(numProcessors)
        "completer", "e", "Try", "Stats",                                                                       ////     def completer(e: Try[Stats]) {
        "e",                                                                                                    ////       e match {
        "Success", "s", "println",                                                                              ////         case Success(s) => println(s"Processor ${s._1} completed ${s._2} jobs with ${s._3} errors")
        "Failure", "t", "println", "t", "getMessage",                                                           ////         case Failure(t) => println("Processor terminated in error: "+ t.getMessage)
                                                                                                                ////       }
        "doneLatch", "countDown",                                                                               ////       doneLatch.countDown()
                                                                                                                ////     }
        "server", "ComputeServer", "numProcessors", "completer", "_",                                           ////     val server = new ComputeServer(numProcessors, completer _)
                                                                                                                ////
        "numResults",                                                                                           ////     val numResults = 3
        "resultLatch", "CountDownLatch", "numResults",                                                          ////     val resultLatch = new CountDownLatch(numResults)
        "ResultCounter", "A", "future", "Future", "A",                                                          ////     class ResultCounter[A](future: Future[A]) {
        "onResult", "B", "body", "PartialFunction", "Try", "A", "B", "x", "ExecutionContext",                   ////       def onResult[B](body: PartialFunction[Try[A], B])(implicit x: ExecutionContext) =
        "future", "andThen", "body", "andThen", "_", "resultLatch", "countDown",                                ////         future andThen body andThen { case _ => resultLatch.countDown() }
                                                                                                                ////     }
        "countingFuture", "A", "f", "Future", "A", "ResultCounter", "A",  "ResultCounter", "A", "f",            ////     implicit def countingFuture[A](f: Future[A]): ResultCounter[A] = new ResultCounter[A](f)
                                                                                                                ////
        "dbz",                                                                                                  ////     def dbz = 1/0
        "k", "server", "submit", "dbz",                                                                         ////     val k = server submit dbz
        "k", "onResult",                                                                                        ////     k onResult {
        "Success", "v", "println", "v",                                                                         ////       case Success(v) => println("k returned? "+ v)
        "Failure", "e", "println", "e",                                                                         ////       case Failure(e) => println("k failed! "+ e)
                                                                                                                ////     }
                                                                                                                ////
        "f", "server", "submit",                                                                                ////     val f = server submit 42
        "g", "server", "submit",                                                                                ////     val g = server submit 38
        "h", "x", "f", "y", "g", "x", "y",                                                                      ////     val h = for (x <- f; y <- g) yield { x + y }
        "h", "onResult", "Success", "v", "println",                                                             ////     h onResult { case Success(v) => println(s"Computed $v") }
                                                                                                                ////
        "report", "PartialFunction", "Try", "_", "Unit",                                                        ////     val report: PartialFunction[Try[_], Unit] = {
        "Success", "v", "println",                                                                              ////       case Success(v) => println(s"Computed $v")
        "Failure", "e", "println",                                                                              ////       case Failure(e) => println(s"Does not compute: $e")
                                                                                                                ////     }
        "r",                                                                                                    ////     val r =
                                                                                                                ////       for {
        "x", "server", "submit",                                                                                ////         x <- server submit 17
        "y", "server", "submit", "RuntimeException",                                                            ////         y <- server submit { throw new RuntimeException("Simulated failure"); 13 }
        "x", "y",                                                                                               ////       } yield (x * y)
        "r", "onResult", "report",                                                                              ////     r onResult report
                                                                                                                ////
        "awhile", "Duration",                                                                                   ////     implicit val awhile = Duration("1 sec")
        "windDown",                                                                                             ////     def windDown() = {
        "server", "finish",                                                                                     ////       server.finish()
        "doneLatch", "awaitAwhile",                                                                             ////       doneLatch.awaitAwhile()
                                                                                                                ////     }
        "shutdown",                                                                                             ////     def shutdown() = {
        "ctx", "shutdown",                                                                                      ////       ctx.shutdown()
        "ctx", "awaitTermination", "awhile", "length", "awhile", "unit",                                        ////       ctx.awaitTermination(awhile.length, awhile.unit)
                                                                                                                ////     }
        "done", "resultLatch", "awaitAwhile", "windDown", "shutdown",                                           ////     val done = resultLatch.awaitAwhile() && windDown() && shutdown()
        "assert", "done",                                                                                       ////     assert(done, "Error shutting down.")
                                                                                                                ////   }
                                                                                                                //// }
    };                                                                                                          ////
    assertAnalyzesTo(analyzer, input, output);
  }

  @Test
  public void testMimeType() {
    SymbolTokenizer tokenizer = new ScalaSymbolTokenizer(newAttributeFactory());
    assertEquals("text/x-scala", tokenizer.getMimeType());
  }

  @Test
  public void testSourceCodeLanguage() {
    SymbolTokenizer tokenizer = new ScalaSymbolTokenizer(newAttributeFactory());
    assertEquals("Scala", tokenizer.getSourceCodeLanguage());
  }
}
