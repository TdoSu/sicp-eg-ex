; (load "./utils.ss")
; load 可以引入别的文件

; 分号开头的一行是注释, 会被 scheme 解释器忽略掉

(begin
  (display "Hello, World!")
  (newline))

(define (console-log . args)
  (if (not (null? args))
      (begin (display (car args))
             (display " ")
             (apply console-log (cdr args)))
      (newline)))

(define (symbol-console-log symbol)
  (console-log symbol "===>" (eval symbol)))

; Data Type

; booleans

(console-log #t)
; > #t
(console-log #f)
; > #f
(console-log (boolean? #t))
; > #t
(console-log (boolean? "Hello, World!"))
; > #f
(console-log (not "Hello, World!"))
; > #f
(console-log (not (not "Hello, World!")))
; > #t

(define (->boolean v) (not (not v)))

(console-log (->boolean 32))
; > #t

(console-log (->boolean 0))
; > #t

(if 0
    (console-log "0 is true")
    (console-log "0 is false"))
; > 0 is true

(if ""
    (console-log "empty string is true")
    (console-log "empty string is false"))
; > empty string is true

(if '()
    (console-log "() is true")
    (console-log "() is false"))
; > () is true

(console-log (number? 42))
; > #t
(console-log (number? #f))
; > #f
(console-log (number? 2+3i))
; > #t
(console-log (number? 2/3))
; > #t
(console-log (number? 2.3))
; > #t
(console-log (number? 2e3))
; > #t

; number? (complex? real? rational? integer?)

(console-log (number? #d15))
; > #t
(console-log (number? #b1111))
; > #t
(console-log (number? #o17))
; > #t
(console-log (number? #xA))
; > #t
(symbol-console-log '(= 15 #d15 #b1111 #o17 #xF))
; > (= 15 15 15 15 15) #t

(symbol-console-log '(eqv? 42 42))
; #t
(symbol-console-log '(eqv? 42 #f))
; #f
(symbol-console-log '(eqv? 42 42.0))
; #f

(symbol-console-log '(= 42 42.0))
; #t

; (symbol-console-log '(= 42 #f))
; Exception in =: #f is not a number
; = 过程会先判断值的类型, 如果不是 number 就会报错

; 数学比较和运算
; = < <= > >= + - * / expt max min remainder abs sin atan exp sqrt

; characters

(symbol-console-log '#\c)
; > c c

(symbol-console-log '(char? #\c))
; #t

(symbol-console-log '(char? 1))
; #f

; 一些特殊字符 #\newline #\space #\tab

(symbol-console-log '(->boolean #\space))
; > (->boolean ) #t

; 字符比较
; char=? char<? char<=? char>? char>=?
; 忽略大小写
; char-ci=? char-ci<?
; 大小写转换
; char-downcase char-upcase

(symbol-console-log '(char-upcase #\a))
; > (char-upcase a) A

; symbols

(symbol-console-log '(quote xyz))
; > 'xyz xyz

; 'something 是 (quote something) 的缩写

; 什么是 symbol 什么不是 symbol

(symbol-console-log '(symbol? 'xyz))
; #t
(symbol-console-log '(symbol? 42))
; #f

(symbol-console-log '(eqv? 'calorie 'Calorie))
; #f
; 有一些基于 R5RS 的 scheme 环境不区分大小写, 不过 chez scheme 区分

(begin (define xyz 9)
       (set! xyz #\c)
       (console-log xyz))

(console-log xyz)
; begin 参数中的 define 也会被定义到全局
; begin 也是一个正常的过程, 应用序求值

((lambda ()
  (define xyz 12)
  (console-log xyz)))
; > 12

(console-log xyz)
; > c

; simple data types: boolean number character symbol
; compound data types: string vector pair list
; compound 和 simple 的区别就是, compound 可以被拆分, 换句话说就是有选择函数

; 构造函数
(symbol-console-log '(string #\h #\e #\l #\l #\o))
; hello

; 选择函数
(symbol-console-log '(string-ref "Hello" 0))
; H

; 字面量 (字面量这东西就是构造函数调用的语法糖)
(symbol-console-log '"hello")
; > hello hello

; 字符串拼接
(symbol-console-log '(string-append "Hello" ", " "World!"))

(symbol-console-log '(eqv? "hello" "hello"))
; #f
(symbol-console-log '(eqv? (string #\h #\e #\l #\l #\o) "hello"))
; #f
(symbol-console-log '(eq? "hello" "hello"))
; #f
(symbol-console-log '(eq? (string #\h #\e #\l #\l #\o) "hello"))
; #f

; 对于组成元素的赋值操作
(define a-3-char-long-string (make-string 3))
(console-log a-3-char-long-string)
(string-set! a-3-char-long-string 0 #\w)
(string-set! a-3-char-long-string 1 #\e)
(string-set! a-3-char-long-string 2 #\i)
(console-log a-3-char-long-string)

; string? string-ref make-string string-set!
; vector? vector-ref make-vector vector-set!

(symbol-console-log '(vector 0 1 2 3 4))
; #(0 1 2 3 4)
(symbol-console-log '(vector-ref (vector 0 1 2 3 4) 2))
; 2

; string 和 vector 的区别就是一个由字符组成一个由数字组成

; dotted pairs and lists

; dotted pairs 的构造函数是 cons 选择函数是 car cdr

(symbol-console-log '(cons 1 #t))
; (1 . #t)
(symbol-console-log '(car (cons 1 #t)))
; 1
(symbol-console-log '(cdr (cons 1 #t)))
; #t

; list 和 string/vector 相比, 它的元素可以是不相同的任何类型
; (list 1 "s" #t) 等价于 (cons 1 (cons "s" (cons #t '())))

; (caar l) 等价于 (car (car l))
; (cadr l) 等价于 (car (cdr l))

; pair? list? null?

(symbol-console-log '(pair? '()))
; #f

;;; conversions between data types

(symbol-console-log '(char->integer #\d))
; 100
(symbol-console-log '(integer->char 50))
; 2
(symbol-console-log '(string->list "hello"))
; (h e l l o)
(symbol-console-log '(string->number "100"))
; 100
(symbol-console-log '(string->number "hello"))
; #f
(symbol-console-log '(number->string 12e2))
; 1200.0
(symbol-console-log '(symbol->string 'symbol))
(symbol-console-log '(string->symbol "string"))

; other data types

; procedure

(symbol-console-log '(procedure? +))
; #t

(console-log +)
; #<procedure +>

; port
; display 的第二个参数要求是 port 类型
; (current-output-port) 是默认值

; s-expressions
; 包括上面所有东东
; 42, #\c, (1 .2 ), #(a b c), "hello", (quote xyz), (string->number "16"), (newline) ...

;;; value -- data type -- s-expression
;;; s-expression 都有 value, 不过有时这个 value 是未定义的, 比如 (display "hello")
;;; 每个 value 都属于某个 data type
;;; 不同的 data type 可以做不同的操作, 这些是标准约定好的, 并且尽可能符合我们的直觉.
;;; data type 分成 simple, compound 两种,
;;; compound 是 simple 组合起来的, 所以可以拆分成 simple.
;;; 一些 data type 之间可以转换, 转换规则也尽量符合直觉.
;;; 类型和操作不符合约定时, 操作就会报错.

;;; (load "1.ss")
;;; (load "2.ss")
;;; (load "3.ss")
;;; 1.ss 2.ss 3.ss 中 define 的名字可以在当前文件中使用, 也会彼此冲突.
;;; 它们是全局的. 而不是模块化的. 也没有显示的表示哪些是导入的哪些是导出的.

;;; forms

;;; 有些 s 表达式的值是它自己, 有些不是.
;;; Scheme evaluates the form #\c to the value #\c, because #\c is self-evaluating.
;;; Not all s-expressions are self-evaluating.
;;; For instance the symbol s-expression xyz evaluates to the value held by the variable xyz.
;;; The list s-expression (string->number "16") evaluates to the number 16.

;;; 有些 s 表达式合法, 有些不合法, 不合法的会报错
;;; 比如 (1 . 2)

;;; Scheme evaluates a list form by examining the first element, or head, of the form.
;;; If the head evaluates to a procedure, the rest of the form is evaluated to get the procedure's
;;; arguments, and the procedure is applied to the arguments.

;;; If the head of the form is a special form,
;;; the evaluation proceeds in a manner idiosyncratic to that form.
;;; eg. begin, define, set!, if, cond, and, or, lambda.

;;; lambda 返回一个 procedure 值
;;; procedure? 可以判断一个 procedure 值
;;; procedure 值可以 apply
;;; (+ 1 2 3) 等价于 (apply + '(1 2 3))

; conditionals
; if, cond, when, unless, case

;;; Scheme's variables have lexical scope.
;;; scope: 同一个名字是不是指的同一东西 (读和修改会不会彼此影响)

;;; 产生名字的各种方式
;;; define, lambda, let, let*

((lambda (a)
   ((lambda (b)
      ((lambda (c)
        (console-log a b c))
       b))
    a))
 3)

(let ((a 3))
  (let ((b a))
    (let ((c b))
      (console-log a b c))))

(let* ((a 3)
       (b a)
       (c b))
  (console-log a b c))
; 3 3 3

;;; fluid-let -- 动态作用域

((lambda ()
   (define counter 0)
   (define bump-counter
     (lambda ()
       (set! counter (+ counter 1))
       counter))
   (console-log (bump-counter))
   ; 1
   (console-log (bump-counter))
   ; 2
   (console-log (bump-counter))
   ; 3
   ;;; fluid-let 可以破坏词法作用域 -- 提供动态作用域
   ;;; 先存一下 counter 然后改写它, 退出时再利用缓存重置 counter
   (fluid-let ((counter 99))
              (console-log (bump-counter))
              ; 100
              (console-log (bump-counter))
              ; 101
              (console-log (bump-counter))
              ; 102
    )
   (console-log counter)
   ;;; 3
   ))

;;; Recursion

(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

; even? odd?

(define is-even?
  (lambda (n)
    (if (= n 0)
        #t
        (is-odd? (- n 1)))))

(define is-odd?
  (lambda (n)
    (if (= n 0)
        #f
        (is-even? (- n 1)))))

; (let ((local-even? (lambda (n) (if (= n 0) #t (local-odd? (- n 1)))))
;       (local-odd? (lambda (n) (if (= n 0) #f (local-even? (- n 1))))))
;   (list (local-even? 23) (local-odd? 23)))
; Exception: variable local-odd? is not bound

(letrec ((local-even? (lambda (n) (if (= n 0) #t (local-odd? (- n 1)))))
         (local-odd? (lambda (n) (if (= n 0) #f (local-even? (- n 1))))))
  (console-log (list (local-even? 23) (local-odd? 23))))

;;; let, let*, lambda 好像都没办法声明出递归过程
;;; 只有 define 和 letrec 可以

;;; 用 letrec 模拟 for 循环 -- 不污染全局环境, 但是可以递归实现循环 (要么就得在自执行函数中 define)
(letrec ((countdown
           (lambda (i)
             (if (= i 0)
                 'listoff
                 (begin (display i)
                        (newline)
                        (countdown (- i 1)))))))
  (countdown 10))

;;; 用 命名 let 实现上面的东东
(let countdown ((i 10))
  (if (= i 0)
      'listoff
      (begin (display i)
             (newline)
             (countdown (- i 1)))))

;;; loop, iteration, tail-recursive

(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l)
          #f
          (if (eqv? (car l) o)
              i
              (loop (+ i 1) (cdr l)))))))

(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s)
          r
          (let ((d (cdr s)))
            (set-cdr! s r)
            (loop d s))))))

;;; map for-each

(console-log (map (lambda (x) (+ x 2)) '(1 2 3)))
(console-log (map + '(1 2 3) '(10 20 30)))


;;; I/O

; (define c (read-char))
; (console-log c)
; (define s (read))
; (console-log s)
 
; (display 9) 等价于 (display 9 (current-output-port))

(define i (open-input-file "hello.txt"))
(console-log (read-char i))
; h
(define j (read i))
(console-log j)
; ello

; (define o (open-output-file "greeting.txt"))
; (display "hello" o)
; (write-char #\space o)
; (display 'world o)
; (newline o)
; (close-output-port o)

; 文件结束符 eof-object?

;;; call-with-input-file call-with-output-file 会自动打开文件, 执行回调, 关闭文件
;;; 和 readFile writeFile 差不多

(call-with-input-file
  "hello.txt"
  (lambda (i)
    (let* ((a (read-char i))
           (b (read-char i))
           (c (read-char i)))
      (console-log (list a b c)))))

;;; 字符串也可以作为 port, 这样就可以用 read-char write-char read write display 改写字符串
;;; 用 get-output-string 可以从 port 中拿回字符串

((lambda ()
  (define i (open-input-string "hello world"))
  (console-log (read-char i))
  ; h
  (console-log (read i))
  ; ello
  (console-log (read i))
  ; world
  ))

((lambda ()
  (define o (open-output-string))
  (write 'hello o)
  (write-char #\, o)
  (display " " o)
  (display "world" o)
  (console-log (get-output-string o))
  ; hello, world
  ))

; loading file
; (load "utils.ss")
; (load-relative "utils.ss")


;;; Macros

; Users can create their own special forms by defining macros.

; chez scheme 没有 define-macro 作为内置过程
; 替代品是 define-syntax
; (define-syntax MACRO-NAME
;   (rsc-macro-transformer
;     (let ((xfmr (lambda MACRO-ARGS MACRO-BODY ...)))
;       (lambda (e r)
;         (apply xfmr (cdr e))))))

; Macro is a transformation of codes.
; Codes are transformed before being evaluated or complied,
; and the procedure continues as if the transformed codes are written from the beginning.

(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))

(define (f-nil! x)
  (set! x '()))

(define a 1)

(f-nil! a)
(console-log a)
; 1
; 用过程定义是没办法改变传入的 a 的, 因为 a 先被求值了, 然后赋值给 x, set! 改变的是 x 的值

(nil! a)
(console-log a)
; ()
; 宏是可以做到改变 a 的值的, 因为宏只是替换

(define (set2-! s)
  (let ((e (list 'set! s 2)))
    (eval e)))

(set2-! 'a)
; 传递一个符号进去, 然后用 eval 求值, 也可以做到类似效果
(console-log a)
; 2

;;; 这个区别和 c 语言中的, 参数是传递值 x 还是传递引用 &x 类似

(define-syntax when2
  ; scheme 有 when 这个过程
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))

(when2 (< 3 4)
  (console-log '(< 3 4)))

(when2 (> 3 4)
  (console-log '(> 3 4))
  ; 除 0 不会报错, 因为 when2 依赖 if 是一个特殊过程, 这也是一般的过程应用做不到的
  ; ps: 用 eval 和传递 quote 的方式也可以
  (/ 1 0))

(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop ()
       (when pred b1 ... (loop))))))

(let ((i 0))
  (while (< i 10)
         (display i)
         (display #\space)
         (set! i (+ i 1))))
(newline)

(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
         b1 ...
         (loop (+ 1 i)))))))

(for (i 0 10)
     (display i)
     (display #\space))
(newline)

;;; 宏能实现的目前看通过 过程定义 + quote + eval 都可以做到
;;; 但要求外面调用过程时, 传递 quote


;;; Structures

; chez scheme 中没有 define-structure 这个东东, 也没有 defstruct,
; Structures 并没有定义在 R5RS 中,
; 不过可以通过 define-syntax 实现它.
; (define-structure book title authors publisher year isbn)
; (define bazaar
;   (make-book "The Cathedral and the Bazaar"
;              "Eric S. Raymond"
;              "O'Reilly"
;              1999
;              0596001088))
; 这东西看起来有点像 c 的结构体或者 js 的对象.

;;; Alists and tables
; 可以用 defstruct 或者 define-structure 实现.


;;; System interface

(console-log (file-exists? "./hello.txt"))
; #t

(console-log (delete-file "./a.txt"))
; 文件存在 #t 文件不存在 #f

(define (rm path)
  (if (file-exists? path)
      (delete-file path)))

(console-log (system "ls"))
; lists current directory
; 0

(define fname "spot")
(system (string-append "test -f " fname))
; tests if file 'spot' exists

(system (string-append "rm -f " fname))
; removes 'spot'

(console-log (getenv "HOME"))
; /home/mr_w

(console-log (getenv "SHELL"))
; /bin/bash


;;; Objects and classes
; 需要用 define-structure 来实现


;;; Jumps -- call/cc
 
(console-log "------------")

(console-log (+ 1
                (call/cc
                  (lambda (k) (+ 2 (k 3))))))
; 4

(console-log (call/cc (lambda (k) (+ 2 (k 3)))))
; 3

(console-log (call/cc (lambda (k) (+ 2 (k 444)))))
; 444

; (console-log (call/cc (lambda (k) (+ 2 (k (/ k 82))))))
; Exception in /: #<continuation> is not a number

; call-with-current-continuation 调用它的参数
; 它的参数必须是一个过程, 而且这个过程只能有一个参数
; with a value called the 'current continuation'
; the current continuation at any point in the execution of a program
; is an abstraction of the rest of the program.

;;; 没搞明白这个 call/cc 和 continuation
;;; 以后再说吧


;;; Nodeterminism

; (console-log (amb 1 2))

;;; Engines

;;; Shell scripts

;;; CGI scripts


(exit)

