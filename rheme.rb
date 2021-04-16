#!/usr/bin/env ruby
#
# Rheme: The part of a clause that provides information about the theme
#        or a Ruby implementation of R4RS Scheme
#
# Copyright 2020 Marc Ferguson
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

$rheme_version = '0.8_2'

require 'cmath'
require 'readline'
require 'stringio'
require 'strscan'

$debug_stack = []
$trace_depth = 0
$trace_eval  = false

module Rheme
  extend self

  def assert_var(x)
    return x if x.is_a?(Symbol)
    fail RhemeError, "#{to_string(x)} is not a variable"
  end

  def simplify(x)
    (x.is_a?(Rational) && x.denominator == 1) ? x.numerator : x
  end

  def tail_call?(x)
    x.instance_of?(Array) && x.first.equal?(:'tail-call()')
  end

  #
  # Rheme Classes
  #

  class Env < Hash
    def initialize(outer = nil, bindings = [])
      @outer = outer
      bindings.each {|var, value| store(Rheme.assert_var(var), value)}
    end

    def let_var(var, value)
      store(Rheme.assert_var(var), value)
    end

    def set_var(var, value)
      return store(var, value) if key?(var)
      return @outer.set_var(var, value) if @outer
      fail RhemeError, "Unbound variable: #{Rheme.assert_var(var)}"
    end

    def [](var)
      return fetch(var) if key?(var)
      return @outer[var] if @outer
      fail RhemeError, "Unbound variable: #{Rheme.assert_var(var)}"
    end
  end

  class Lambda < Proc
    attr_reader :source

    def initialize(source)
      @source = source
    end

    def to_s
      s = self.class.to_s.sub('Rheme::', '')
      source[0].equal?(:'named-lambda') ? "#<#{s}: #{source[1][0]}>" : "#<#{s}>"
    end
  end

  class Macro      < Lambda;        end
  class Promise    < Lambda;        end
  class RVector    < Array;         end
  class RChar      < String;        end
  class RhemeError < StandardError; end

  #
  # Eval
  #

  def reval(expr, env)
    while true
      return env[expr] if expr.is_a?(Symbol)
      return expr unless expr.instance_of?(Array) && !expr.empty?
      puts ' ' * $debug_stack.size + to_string(expr) if $trace_eval

      $debug_stack.push(expr)
      fail RhemeError, "#{to_string(expr)} is not a proper list" if expr[-2].equal?(:'.')
      special_form = $special_forms[expr.first]
      if special_form
        val = special_form.call(expr, env)
      else
        # use Array.new here because map doesn't play well with callcc
        f, *args = Array.new(expr.length) {|i| reval(expr[i], env)}
        fail RhemeError, "#{to_string(f)} is not a procedure" unless f.is_a?(Proc)
        val = f.call(*args)
      end
      $debug_stack.pop

      if tail_call?(val)
        expr, env = val.drop(1)
      else
        puts ' ' * $debug_stack.size + to_string(val) if $trace_eval
        return val
      end
    end
  end

  #
  # Base Language
  #

  def callt(fun, args)
    val = fun.call(*args)
    tail_call?(val) ? reval(val[1], val[2]) : val
  end

  def rheme_append(x)
    return [] if x.empty?
    head = x[0..-2].inject(x[0].take(0), :concat)
    tail = x.last
    return tail if head.empty?
    return head.concat(tail) if tail.instance_of?(Array)
    head << :'.' if head.instance_of?(Array)
    head << tail
  end

  def rheme_callcc(x)
    require 'continuation'
    callcc do |c, stack = $debug_stack.dup, depth = $trace_depth|
      x.call(lambda {|y| $debug_stack = stack; $trace_depth = depth; c.call(y)})
    end
  end

  def rheme_eq(a, b)
    a.equal?(b) || (a == [] && b == [] && a.class == b.class)
  end

  def rheme_eqv(a, b)
    rheme_eq(a, b) || (a.is_a?(Numeric) && a == b && rheme_exact?(a) == rheme_exact?(b))
  end

  def rheme_exact?(n)
    n.is_a?(Integer) || n.is_a?(Rational)
  end

  def rheme_read(input)
    input.peek rescue return :EOF
    read_expr(input)
  rescue StopIteration
    raise RhemeError, 'Unexpected EOF'
  end

$predefined_symbols = {
  :+           => lambda {|*x|   x.inject(0, :+)},
  :*           => lambda {|*x|   simplify(x.inject(1, :*))},
  :-           => lambda {|x,*y| y.empty? ? -x : y.inject(x, :-)},
  :'/'         => lambda {|x,*y| simplify(y.empty? ? 1.quo(x) : y.inject(x, :quo))},
  :'='         => lambda {|*x|   x.each_cons(2).all? {|a, b| a == b}},
  :'<'         => lambda {|*x|   x.each_cons(2).all? {|a, b| a <  b}},
  :'>'         => lambda {|*x|   x.each_cons(2).all? {|a, b| a >  b}},
  :'<='        => lambda {|*x|   x.each_cons(2).all? {|a, b| a <= b}},
  :'>='        => lambda {|*x|   x.each_cons(2).all? {|a, b| a >= b}},
  :abs         => lambda {|x|    x.abs},
  :acos        => lambda {|x|    CMath.acos(x)},
  :angle       => lambda {|x|    x.angle},
  :append      => lambda {|*x|   rheme_append(x)},
  :apply       => lambda {|x,*y| x.call(*y[0..-2].concat(y[-1]))},
  :asin        => lambda {|x|    CMath.asin(x)},
  :assoc       => lambda {|x,y|  y.assoc(x) || false},
  :assq        => lambda {|x,y|  y.find {|k| rheme_eq(k[0], x)}  || false},
  :assv        => lambda {|x,y|  y.find {|k| rheme_eqv(k[0], x)} || false},
  :atan        => lambda {|*x|   x[1] ? CMath.atan2(*x) : CMath.atan(*x)},
  :boolean?    => lambda {|x|    x.equal?(true) || x.equal?(false)},
  :car         => lambda {|x|    x.fetch(0)},
  :caar        => lambda {|x|    x.fetch(0).fetch(0)},
  :caaar       => lambda {|x|    x.fetch(0).fetch(0).fetch(0)},
  :caaaar      => lambda {|x|    x.fetch(0).fetch(0).fetch(0).fetch(0)},
  :cadr        => lambda {|x|    x[1].equal?(:'.') ? x[2].fetch(0) : x.fetch(1)},
  :caddr       => lambda {|x|    x[2].equal?(:'.') ? x[3].fetch(0) : x.fetch(2)},
  :cadddr      => lambda {|x|    x[3].equal?(:'.') ? x[4].fetch(0) : x.fetch(3)},
  :cdr         => lambda {|x|    x[1].equal?(:'.') ? x.fetch(2) : x.drop(1)},
  :cdar        => lambda {|x|    a = x.fetch(0); a[1].equal?(:'.') ? a.fetch(2) : a.drop(1)},
  :ceiling     => lambda {|x|    x.ceil},
  :char?       => lambda {|x|    x.is_a?(RChar)},
  :complex?    => lambda {|x|    x.is_a?(Numeric)},
  :cons        => lambda {|x,y|  y.instance_of?(Array) ? [x].concat(y) : [x, :'.', y]},
  :cos         => lambda {|x|    CMath.cos(x)},
  :denominator => lambda {|x|    x.denominator},
  :display     => lambda {|x,y=$stdout| !y.write(x.is_a?(String) ? x : unread_expr(x))},
  :eq?         => lambda {|x,y|  rheme_eq(x, y)},
  :equal?      => lambda {|x,y|  x == y},
  :error       => lambda {|x,*y| fail RhemeError, [x, *y.map {|s| to_string(s)}].join(' ')},
  :eqv?        => lambda {|x,y|  rheme_eqv(x, y)},
  :eval        => lambda {|x,e=$toplevel_env| [:'tail-call()', x, e]},
  :even?       => lambda {|x|    x.even?},
  :exact?      => lambda {|x|    rheme_exact?(x)},
  :exit        => lambda {|x=1|  exit!(x)},
  :exp         => lambda {|x|    CMath.exp(x)},
  :expt        => lambda {|x,y|  simplify(x ** y)},
  :floor       => lambda {|x|    x.floor},
  :force       => lambda {|x|    x.is_a?(Promise) ? x.call : x},
  :format      => lambda {|*x|   rheme_format(*x)},
  :gcd         => lambda {|*x|   x.inject(0, :gcd)},
  :gensym      => lambda {||     :"#:G#{@@gensym ||= 0; @@gensym += 1}"},
  :inexact?    => lambda {|x|    !rheme_exact?(x)},
  :integer?    => lambda {|x|    x.is_a?(Numeric) && x == x.real.round},
  :lcm         => lambda {|*x|   x.inject(1, :lcm)},
  :length      => lambda {|x|    x.length},
  :list        => lambda {|*x|   x},
  :list?       => lambda {|x|    x.instance_of?(Array) && ! x[-2].equal?(:'.')},
  :load        => lambda {|x,v=false| File.open(x, 'r') {|f| !reval_stream(f, v)}},
  :log         => lambda {|x|    CMath.log(x)},
  :logand      => lambda {|*x|   x.inject(-1, :&)},
  :logior      => lambda {|*x|   x.inject(0, :|)},
  :lognot      => lambda {|x|    ~x},
  :logxor      => lambda {|*x|   x.inject(0, :^)},
  :magnitude   => lambda {|x|    x.magnitude},
  :map         => lambda {|p,a,*b| c = a.zip(*b); Array.new(c.size) {|i| callt(p, c[i])}},
  :max         => lambda {|*x|   x.max},
  :member      => lambda {|x,y|  a = y.drop_while {|z| x != z};           !a.empty? && a},
  :memq        => lambda {|x,y|  a = y.drop_while {|z| !rheme_eq(x, z)};  !a.empty? && a},
  :memv        => lambda {|x,y|  a = y.drop_while {|z| !rheme_eqv(x, z)}; !a.empty? && a},
  :min         => lambda {|*x|   x.min},
  :modulo      => lambda {|x,y|  x % y},
  :negative?   => lambda {|x|    x < 0},
  :newline     => lambda {|x=$stdout| x.puts; false},
  :not         => lambda {|x|    !x},
  :null?       => lambda {|x|    x.instance_of?(Array) && x.empty?},
  :number?     => lambda {|x|    x.is_a?(Numeric)},
  :numerator   => lambda {|x|    x.numerator},
  :odd?        => lambda {|x|    x.odd?},
  :pair?       => lambda {|x|    x.instance_of?(Array) && !x.empty?},
  :positive?   => lambda {|x|    x > 0},
  :procedure?  => lambda {|x|    x.is_a?(Proc)},
  :quit        => lambda {||     raise SystemExit},
  :quotient    => lambda {|x,y|  x.quo(y).truncate},
  :random      => lambda {|*x|   rand(*x)},
  :rationalize => lambda {|x,y|  simplify(x.rationalize(y))},
  :rational?   => lambda {|x|    x.is_a?(Numeric) && x == x.real},
  :read        => lambda {|x=$stdin_port| rheme_read(x)},
  :real?       => lambda {|x|    x.is_a?(Numeric) && x == x.real},
  :remainder   => lambda {|x,y|  x.remainder(y)},
  :reverse     => lambda {|x|    x.reverse},
  :round       => lambda {|x|    x.round},
  :sin         => lambda {|x|    CMath.sin(x)},
  :sort        => lambda {|x,p|  x.sort {|*a| callt(p, a) ? -1 : 1}},
  :sqrt        => lambda {|x|    CMath.sqrt(x)},
  :string      => lambda {|*x|   x.join},
  :string?     => lambda {|x|    x.instance_of?(String)},
  :substring   => lambda {|x,a,b| x[a...b]},
  :symbol?     => lambda {|x|    x.is_a?(Symbol)},
  :tan         => lambda {|x|    CMath.tan(x)},
  :truncate    => lambda {|x|    x.truncate},
  :vector      => lambda {|*x|   RVector.new(x)},
  :vector?     => lambda {|x|    x.is_a?(RVector)},
  :write       => lambda {|x,y=$stdout| y.write(unread_expr(x)); false},
  :zero?       => lambda {|x|    x == 0},
  :'call/cc'          => lambda {|x|       rheme_callcc(x)},
  :'char-alphabetic?' => lambda {|x|       x =~ /[A-Za-z]/ && true || false},
  :'char-ci=?'        => lambda {|x,y|     x.casecmp(y) == 0},
  :'char-ci<?'        => lambda {|x,y|     x.casecmp(y) <  0},
  :'char-ci>?'        => lambda {|x,y|     x.casecmp(y) >  0},
  :'char-ci<=?'       => lambda {|x,y|     x.casecmp(y) <= 0},
  :'char-ci>=?'       => lambda {|x,y|     x.casecmp(y) >= 0},
  :'char-downcase'    => lambda {|x|       RChar.new(x.downcase)},
  :'char-lower-case?' => lambda {|x|       x =~ /[a-z]/ && true || false},
  :'char-numeric?'    => lambda {|x|       x =~ /\d/    && true || false},
  :'char-upcase'      => lambda {|x|       RChar.new(x.upcase)},
  :'char-upper-case?' => lambda {|x|       x =~ /[A-Z]/ && true || false},
  :'char-whitespace?' => lambda {|x|       x =~ /\s/    && true || false},
  :'char->integer'    => lambda {|x|       x.ord},
  :'char=?'           => lambda {|x,y|     x == y},
  :'char<?'           => lambda {|x,y|     x < y},
  :'char>?'           => lambda {|x,y|     x > y},
  :'char<=?'          => lambda {|x,y|     x <= y},
  :'char>=?'          => lambda {|x,y|     x >= y},
  :'close-input-port' => lambda {|x|       !x.io.close rescue false},
  :'close-output-port' => lambda {|x|      !x.close    rescue false},
  :'current-input-port' => lambda {||      $stdin_port},
  :'current-output-port' => lambda {||     $stdout},
  :'current-time'     => lambda {||        Time.now.to_f},
  :'eof-object?'      => lambda {|x|       x == :EOF},
  :'exact->inexact'   => lambda {|x|       x.to_f},
  :'for-each'         => lambda {|p,a,*b|  a.zip(*b) {|args| callt(p, args)}; false},
  :'imag-part'        => lambda {|x|       x.imaginary},
  :'inexact->exact'   => lambda {|x|       simplify(x.rationalize)},
  :'input-port?'      => lambda {|x|       x.is_a?(InputPort)},
  :'integer->char'    => lambda {|x|       RChar.new(x.chr) rescue false},
  :'list-ref'         => lambda {|x,i|     x.fetch(i)},
  :'list->string'     => lambda {|x|       x.join},
  :'list->vector'     => lambda {|x|       RVector.new(x)},
  :'make-polar'       => lambda {|r,t|     Complex.polar(r, t)},
  :'make-rectangular' => lambda {|r,i|     Complex.rectangular(r, i)},
  :'make-string'      => lambda {|n,x=' '| x.chr.to_s * n},
  :'make-vector'      => lambda {|n,x=0|   RVector.new(n, x)},
  :'number->string'   => lambda {|x,r=[]|  x.is_a?(Integer) ? x.to_s(*r) : x.to_s},
  :'open-input-file'  => lambda {|x|       InputPort.new(File.new(x, 'r'))},
  :'open-output-file' => lambda {|x|       File.new(x, 'w')},
  :'output-port?'     => lambda {|x|       x.is_a?(IO) && x.stat.writable?},
  :'peek-char'        => lambda {|x=$stdin_port| x.getch(true)},
  :'read-char'        => lambda {|x=$stdin_port| x.getch(false)},
  :'real-part'        => lambda {|x|       x.real},
  :'rheme-version'    => lambda {||        $rheme_version},
  :'set-car!'         => lambda {|x,y|     x[0] = y},
  :'set-cdr!'         => lambda {|x,y|     x.replace(rheme_append([x.take(1), y]))},
  :'string-append'    => lambda {|*x|      x.inject('', :concat)},
  :'string-ci=?'      => lambda {|x,y|     x.casecmp(y) == 0},
  :'string-ci<?'      => lambda {|x,y|     x.casecmp(y) <  0},
  :'string-ci>?'      => lambda {|x,y|     x.casecmp(y) >  0},
  :'string-ci<=?'     => lambda {|x,y|     x.casecmp(y) <= 0},
  :'string-ci>=?'     => lambda {|x,y|     x.casecmp(y) >= 0},
  :'string-copy'      => lambda {|x|       String.new(x)},
  :'string-fill!'     => lambda {|x,y|     x.gsub!(/./, y[0])},
  :'string-length'    => lambda {|x|       x.length},
  :'string-ref'       => lambda {|x,i|     RChar.new(x[i])},
  :'string-set!'      => lambda {|v,i,x|   v[i] = x},
  :'string->list'     => lambda {|x|       x.chars.map {|c| RChar.new(c)}},
  :'string->number'   => lambda {|x,r=10|  x != '.' && string_to_num(x, r) rescue false},
  :'string->symbol'   => lambda {|x|       x.to_sym},
  :'string=?'         => lambda {|x,y|     x == y},
  :'string<?'         => lambda {|x,y|     x < y},
  :'string>?'         => lambda {|x,y|     x > y},
  :'string<=?'        => lambda {|x,y|     x <= y},
  :'string>=?'        => lambda {|x,y|     x >= y},
  :'symbol->string'   => lambda {|x|       x.to_s},
  :'trace-eval'       => lambda {||        $trace_eval = true},
  :'untrace-eval'     => lambda {||        $trace_eval = false},
  :'vector-fill!'     => lambda {|x,y|     x.fill(y)},
  :'vector-length'    => lambda {|v|       v.length},
  :'vector-ref'       => lambda {|v,i|     v.fetch(i)},
  :'vector-set!'      => lambda {|v,i,x|   v.fetch(i); v[i] = x},
  :'vector->list'     => lambda {|x|       Array.new(x)},
  :'write-char'       => lambda {|x,y=$stdout| y.write(x); false},
  :'interaction-environment'   => lambda {||    $toplevel_env},
  :'null-environment'          => lambda {|v=0| Env.new},
  :'scheme-report-environment' => lambda {|v=0| $rheme_env}
  }

  $toplevel_env = Env.new.update($predefined_symbols)

  #
  # Special Forms
  #

  def rheme_and(x, env)
    return false unless x[1..-2].all? {|expr| reval(expr, env)}
    x.length == 1 || [:'tail-call()', x.last, env]
  end

  def rheme_begin(x, env, skip = 1)
    x[skip..-2].each {|expr| reval(expr, env)}
    x.length > skip && [:'tail-call()', x.last, env]
  end

  def rheme_case(x, env)
    key = reval(x[1], env)
    clause = x.drop(2).find {|c| c[0].equal?(:else) || c[0].include?(key)}
    return false unless clause
    return rheme_begin(clause, env) unless clause[1].equal?(:'=>')
    [:'tail-call()', [clause[2], [:quote, key]], env]
  end 

  def rheme_cond(x, env)
    test = false
    clause = x.drop(1).find {|c| test = c[0].equal?(:else) || reval(c[0], env)}
    return false unless clause
    return rheme_begin(clause, env) if clause[0].equal?(:else)
    return test if clause.length == 1
    return rheme_begin(clause, env) unless clause[1].equal?(:'=>')
    [:'tail-call()', [clause[2], [:quote, test]], env]
  end

  def rheme_define(x, env)
    var, expr = parse_define_args(x)
    env.let_var(var, reval(expr, env))
  end

  def parse_define_args(x)
    # (define (fun arg1 arg2 ...) ...)
    # (define var val)
    return x[1][0], [:'named-lambda', *x.drop(1)] if x[1].instance_of?(Array)
    return x[1], x[2] if x.length == 3
    fail RhemeError, x.length > 3 ? 'Too many arguments' : 'Too few arguments'
  end

  def rheme_delay(source, env, memo = nil, forced = false)
    fail RhemeError, 'Too many arguments' if source.length > 2
    Promise.new(source) do
      unless forced
        val = reval(source[1] || false, env)
        memo = val unless forced
        forced = true
      end
      memo
    end
  end

  def rheme_do(x, outer)
    # (do ((var1 init1 step1) (var2 init2 step2) ...) (test ...) ...)
    env = Env.new(outer, x[1].map {|var, expr| [var, reval(expr, outer)]})
    until reval(x[2][0], env)
      x.drop(3).each {|expr| reval(expr, env)}
      new_vals = x[1].map {|var| reval(var[2], env) if var.length > 2}
      x[1].zip(new_vals) {|var, val| env.let_var(var[0], val) if var.length > 2}
    end
    rheme_begin(x[2], env)
  end

  def rheme_if(x, env)
    fail RhemeError, 'Too many arguments' if x.length > 4
    return [:'tail-call()', x[2], env] if reval(x[1], env)
    x.length > 3 && [:'tail-call()', x[3], env]
  end

  def rheme_lambda(source, outer)
    vars, min_args, rest_argp = parse_lambda_args(source)
    vars.each {|v| assert_var(v)}

    Lambda.new(source) do |*args|
      fail RhemeError, 'Too few arguments' if args.length < min_args
      args[vars.length-1] = args.pop([1 + args.length - vars.length, 0].max) if rest_argp
      fail RhemeError, 'Too many arguments' if args.length > vars.length
      env = Env.new(outer)
      vars.zip(args) {|var, arg| env.let_var(var, arg || false)}
      rheme_begin(source, env, 2)
    end
  end

  def parse_lambda_args(x)
    # (lambda (arg1 arg2 #!optional arg3 arg4 . rest) ...)
    # (named-lambda (name arg1 arg2 ...) ...)
    # (lambda arglist ...)
    if x[1].instance_of?(Array)
      vars = x[0].equal?(:'named-lambda') ? x[1].drop(1) : x[1].dup
      rest_argp = vars[-2].equal?(:'#!rest') || vars[-2].equal?(:'.')
      vars.delete_at(-2) if rest_argp
      min_args = vars.find_index(:'#!optional')
      vars.delete_at(min_args) if min_args
      min_args ||= vars.length - (rest_argp ? 1 : 0)
      return vars, min_args, rest_argp
    else
      return [x[1]], 0, true
    end
  end

  def rheme_let(x, outer)
    # (let ((var1 val1) (var2 val2) ...) ...)
    # (let fun ((arg1 init1) (arg2 init2) ...) ...)
    if x[1].instance_of?(Array)
      env = Env.new(outer, x[1].map {|var, expr| [var, reval(expr, outer)]})
      rheme_begin(x, env, 2)
    else
      env = Env.new(outer, x[2].map {|var, expr| [var, reval(expr, outer)]})
      form = [x[1], *x[2].map(&:first)]
      env.let_var(x[1], rheme_lambda([:'named-lambda', form, *x.drop(3)], env))
      [:'tail-call()', form, env]
    end
  end

  def rheme_letstar(x, outer)
    env = x[1].empty? ? Env.new(outer) : outer
    x[1].each do |var, expr|
      env = Env.new(env, [[var, reval(expr, env)]])
    end
    rheme_begin(x, env, 2)
  end

  def rheme_letrec(x, outer)
    env = Env.new(outer)
    x[1].each {|var, expr| env.let_var(var, reval(expr, env))}
    rheme_begin(x, env, 2)
  end

  def rheme_or(x, env)
    x[1..-2].each {|expr| val = reval(expr, env); return val if val}
    x.length > 1 && [:'tail-call()', x.last, env]
  end

  def rheme_quasiquote(x, env, depth = 1)
    return x unless x.is_a?(Array)
    val = x.class.new

    x.each_with_index do |expr, index|
      if depth == 0
        fail RhemeError, 'unquote requires exactly one argument' if index < x.length - 1
        val = rheme_append([val[0..-2], reval(expr, env)])
      elsif depth == 1 && expr.instance_of?(Array) && expr[0] == :'unquote-splicing'
        splice = reval(expr[1], env)
        unless splice.instance_of?(Array) || (index == x.length - 1 && !val.empty?)
          fail RhemeError, "unquote-splicing: #{to_string(splice)} is not a list"
        end
        val = rheme_append([val, splice])
      else
        depth += 1 if expr == :quasiquote
        depth -= 1 if expr == :unquote || expr == :'unquote-splicing'
        val << rheme_quasiquote(expr, env, depth)
      end
    end
    val
  end

  def rheme_source(x, env)
    target = $special_forms[x[1]] || reval(x[1], env)
    target.is_a?(Lambda) && target.source
  end

  $special_forms = {
    :and            => method(:rheme_and),
    :begin          => method(:rheme_begin),
    :case           => method(:rheme_case),
    :cond           => method(:rheme_cond),
    :define         => method(:rheme_define),
    :delay          => method(:rheme_delay),
    :do             => method(:rheme_do),
    :if             => method(:rheme_if),
    :lambda         => method(:rheme_lambda),
    :let            => method(:rheme_let),
    :letrec         => method(:rheme_letrec),
    :'let*'         => method(:rheme_letstar),
    :'named-lambda' => method(:rheme_lambda),
    :or             => method(:rheme_or),
    :quasiquote     => lambda {|x, env| rheme_quasiquote(x[1], env)},
    :quote          => lambda {|x, env| x[1]},
    :set!           => lambda {|x, env| env.set_var(x[1], reval(x[2], env))},
    :source         => method(:rheme_source),
  }

  #
  # Reader
  #

  def read_expr(input)
    token = input.next
    case token
    when '(', '#(', '['
      terminator = token[-1].tr('([', ')]')
      exp = (token == '#(') ? RVector.new : Array.new
      exp.push(read_expr(input)) until input.peek == terminator
      input.next
      return exp unless exp.include?(:'.')
      fail RhemeError, "Unexpected '.'" unless exp.instance_of?(Array)
      exp[-2..-1] = exp[-1] while exp[-2] == :'.' && exp[-1].instance_of?(Array)
      return exp unless [exp[0], *exp[1..-3], exp[-1]].include?(:'.')
      fail RhemeError, "Unexpected '.'" 
    when ')', ']'       then fail RhemeError, "Unexpected #{token}"
    when "'", "\u2019"  then [:quote,              read_expr(input)]
    when "`"            then [:quasiquote,         read_expr(input)]
    when ','            then [:unquote,            read_expr(input)]
    when ',@'           then [:'unquote-splicing', read_expr(input)]
    when '.'            then :'.'
    when /^[\d.+-]/     then string_to_num(token) rescue token.downcase.to_sym
    when /^"/
      fail RhemeError, "#{token} is not a string" unless token[-1] == '"'
      token[1..-2].gsub(/\\./).each {|x| x[1].tr('n', "\n")}
    when /^#/
      case token.downcase
      when '#t'         then true
      when '#f'         then false
      when '#\space'    then RChar.new(' ')
      when '#\newline'  then RChar.new("\n")
      when /^#\\.$/     then RChar.new(token[2])
      when /^#!/        then token.downcase.to_sym
      when /^#[eibodx]/
        string_to_num(token) rescue raise RhemeError, "#{token} is not a number"
      else fail RhemeError, "Unsupported reader syntax: #{token}"
      end
    else token.downcase.to_sym
    end
  end

  def string_to_num(str, radix = 10)
    tok = str.downcase
    exactness = tok.sub!(/^(#[bodx])?#[ei]/, '\1')
    if tok =~ /^#[bodx]/
      radix = {'b' => 2, 'o' => 8, 'd' => 10, 'x' => 16}.fetch(tok[1])
      tok = tok[2..-1]
    end
    tok.tr!('sfdl', 'e')
    tok.sub!(/[\d\.]#+(\.#*)?(e.*)?$/) {|z| z.tr('#', '0')}
    num = raw_string_to_num(tok, radix)
    !exactness ? num : (str =~ /#i/i) ? num.to_f : simplify(num.rationalize)
  end

  def raw_string_to_num(str, radix)
    return Integer(str, radix) if radix != 10
    Integer(str) rescue Float(str) rescue simplify(Rational(str)) rescue Complex(str)
  end    

  #
  # Unreader
  #

  def unread_expr(exp, limits = false, depth = 0)
    case exp
    when RVector then '#' << unread_expr(exp.to_a, limits, depth)
    when Array
      if exp.length == 2
        case exp[0]
        when :quote              then return "'"  << unread_expr(exp[1], limits, depth)
        when :quasiquote         then return '`'  << unread_expr(exp[1], limits, depth)
        when :unquote            then return ','  << unread_expr(exp[1], limits, depth)
        when :'unquote-splicing' then return ',@' << unread_expr(exp[1], limits, depth)
        end
      end
      if limits
        return '(...)' if depth >= limits[:depth]
        exp = exp.first(limits[:length]) << :'...' if exp.length > limits[:length]
      end
      '(' << exp.map {|x| unread_expr(x, limits, depth + 1)}.join(' ') << ')'
    when RChar  then '#\\' << (exp == ' ' ? 'space' : exp == "\n" ? 'newline' : exp)
    when String then exp.dump
    when Lambda then exp.to_s
    when Proc   then s = $predefined_symbols.key(exp); s ? "#<Proc: #{s}>" : exp.to_s
    when true   then '#t'
    when false  then '#f'
    when nil    then 'NIL' # for debugging
    else exp.to_s
    end
  end

  def to_string(exp)
    unread_expr(exp, {depth: 5, length: 8})
  end

  #
  # Input Scanner
  #

  class InputPort < Enumerator
    attr_accessor :io, :scanner, :prompt
    @@tokenizer = %r{\s+|,@|#\(|[()'`,\[\]\u2019]|"(\\"|[^"])*"|#\\.\w*|;.*|[^\s();"\[\]]+}

    def initialize(io)
      @scanner = StringScanner.new('')
      @io = io

      super() do |collect|
        while another_line?
          while (token = @scanner.scan(@@tokenizer))
            collect << token unless token =~ /\A\s+\z|\A;/
          end
        end
      end
    end

    def another_line?
      if @io == :REPL
        line = Readline.readline(@prompt, true)
        @prompt = '' unless line == ''
        line && @scanner << line << "\n"
      else
        line = @io.gets
        line && @scanner << line
      end
    end

    def getch(peek)
      return :EOF if @scanner.eos? && !another_line?
      RChar.new(peek ? @scanner.peek(1) : @scanner.getch)
    end
  end

  def reval_stream(stream, verbose = false)
    stream = StringIO.new(stream) if stream.is_a?(String)
    input = InputPort.new(stream)
    while (expr = rheme_read(input)) != :EOF
      puts(unread_expr(expr)) if verbose
      val = reval(expr, $toplevel_env)
      puts(unread_expr(val)) if verbose
    end
  rescue SystemExit
  end

  $stdin_port = InputPort.new($stdin)

  #
  # REPL
  #

  def repl(prompt = 'rheme> ')
    puts "Rheme version #$rheme_version" if prompt == 'rheme> '
    repl_port = InputPort.new(:REPL)
    loop do
      begin
        incomplete_expr = repl_port.scanner.rest.gsub(/^\s*(;.*)?|\n$/, '')
        repl_port.prompt = prompt + incomplete_expr
        expr = read_expr(repl_port)
      rescue RhemeError => err
        puts "REPL: #{err}"
        next repl_port.scanner.terminate
      end
      val = toplevel_eval(expr)
      repl_port.scanner.terminate if val.nil?
      puts to_string(val) unless val.nil?
    end
  rescue SystemExit
  end

  def toplevel_eval(expr)
    $debug_stack = [expr]
    $trace_depth = 0
    reval(expr, $toplevel_env)
  rescue SystemExit, StopIteration
    raise
  rescue RhemeError, SystemStackError, IOError, IndexError, ZeroDivisionError => err
    puts "Error while evaluating #{to_string($debug_stack.last)}", err
  rescue StandardError => err
    puts "Error while evaluating #{to_string($debug_stack.last)}", err
    puts err.backtrace.first(6).join("\n")
  rescue Exception
    puts "Error while evaluating #{to_string($debug_stack.last)}"
    raise
  end

  #
  # Unhygienic Macros
  #

  def rheme_define_macro(x, outer)
    name, source = parse_define_args(x)
    prev = $special_forms[assert_var(name)]
    if prev && ! (prev.is_a?(Macro) && prev.source == source)
      warn "Warning: Redefinition of keyword or macro: #{name}"
    end

    expander = reval(source, outer)
    $special_forms[name] = Macro.new(source) do |form, env|
      expansion = callt(expander, form.drop(1))
      expansion = [:begin, expansion] unless expansion.instance_of?(Array)
      [:'tail-call()', form.replace(expansion), env]
    end
    name
  end

  $special_forms[:'define-macro'] = method(:rheme_define_macro)

  #
  # Format Extension
  #

  def rheme_format(dest, fmt, *args)
    fmt = fmt.gsub(/~%/, "\n")
    args = fmt.scan(/~[AS]|%./i).reject {|x| x == '%%'}.zip(args)
    args.map! {|spec, x| (spec =~ /~S/i || !x.is_a?(String)) ? unread_expr(x) : x}
    output = fmt.gsub(/~[AS]/i, '%s') % args
    dest = $stdout if dest == true
    dest ? !dest.write(output) : output
  end

  #
  # Trace Extension
  #

  class Tracer < Lambda
    attr_reader :target

    def initialize(target)
      @target = target
      super(target.is_a?(Lambda) && target.source)
    end
  end

  def rheme_trace(x, env)
    x.drop(1).each do |name|
      target = env[name]
      if target.is_a?(Proc) && !target.is_a?(Tracer) && !target.is_a?(Promise)
        tracer = Tracer.new(target) do |*args|
          $trace_depth += 1
          puts ' ' * $trace_depth + Rheme.to_string([name, *args])
          val = Rheme.callt(target, args)
          puts ' ' * $trace_depth + Rheme.to_string(val)
          $trace_depth -= 1
          val
        end
        env.set_var(name, tracer)
      end
    end
  end

  def rheme_untrace(x, env)
    x.drop(1).select {|n| env[n].is_a?(Tracer)}.each do |name|
      env.set_var(name, env[name].target)
    end
  end

  $special_forms[:trace]   = method(:rheme_trace)
  $special_forms[:untrace] = method(:rheme_untrace)
end

#
# Other R4RS Essential Procedures
#

Rheme.reval_stream <<EOD
  (define (call-with-input-file name proc)
    (let* ((file (open-input-file name))
           (val (proc file)))
      (close-input-port file)
      val))

  (define (call-with-output-file name proc)
    (let* ((file (open-output-file name))
           (val (proc file)))
      (close-output-port file)
      val))

  (define (cddr   x) (cdr   (cdr   x)))
  (define (caadr  x) (car   (cadr  x)))
  (define (cadar  x) (cadr  (car   x)))
  (define (cdaar  x) (cdr   (caar  x)))
  (define (cdadr  x) (cdr   (cadr  x)))
  (define (cddar  x) (cdr   (cdar  x)))
  (define (cdddr  x) (cdr   (cddr  x)))
  (define (caaadr x) (caar  (cadr  x)))
  (define (caadar x) (caar  (cdar  x)))
  (define (caaddr x) (car   (caddr x)))
  (define (cadaar x) (cadr  (caar  x)))
  (define (cadadr x) (cadr  (cadr  x)))
  (define (caddar x) (caddr (car   x)))
  (define (cdaaar x) (cdr   (caaar x)))
  (define (cdaadr x) (cdar  (cadr  x)))
  (define (cdadar x) (cdar  (cdar  x)))
  (define (cdaddr x) (cdr   (caddr x)))
  (define (cddaar x) (cddr  (caar  x)))
  (define (cddadr x) (cddr  (cadr  x)))
  (define (cdddar x) (cddr  (cdar  x)))
  (define (cddddr x) (cddr  (cddr  x)))

  (define call-with-current-continuation call/cc)
EOD

$rheme_env = $toplevel_env.dup.freeze

#
# Command Line
#

if $0 == __FILE__
  ARGV.empty? ? Rheme.repl : File.open(ARGV[0], 'r') {|f| Rheme.reval_stream(f)}
end
