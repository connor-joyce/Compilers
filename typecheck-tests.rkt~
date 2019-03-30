#lang racket

(require rackunit
         "Ni-parser.rkt"
         "typecheck.rkt"
         (prefix-in types: "types.rkt"))

;;; checking expressions

(define typechecking-tests 
  (test-suite
   "Typechecking tests for Ni"
   ; noval
   (check-equal? (tc-str "()") (types:make-VoidType) "noval test")

   ; first, simple integer expressions of various sorts
   (check-equal? (tc-str "5") (types:make-IntType) "int literal test")
   (check-equal? (tc-str "5+3") (types:make-IntType) "simple math expr")
   (check-equal? (tc-str "5<3") (types:make-BoolType) "simple compare expr")
   (check-equal? (tc-str "5>3") (types:make-BoolType) "simple compare expr")
   (check-equal? (tc-str "5<=3") (types:make-BoolType) "simple compare expr")
   (check-equal? (tc-str "5>=3") (types:make-BoolType) "simple compare expr")
   (check-equal? (tc-str "5=3") (types:make-BoolType) "simple compare expr")
   ;(check-equal? (tc-str "5<>3") (types:make-BoolType) "simple compare expr") still not sure what this comparison is even supposed to do
   ;(check-exn exn:fail? (thunk (tc-str "peng = peng")) "cannot compare peng to peng") TODO can't compare peng to peng

   ; now test strings
   (check-equal? (tc-str "\"hello\"") (types:make-StringType) "string literal")
   (check-equal? (tc-str "\"hi\"<\"hello\"") (types:make-BoolType) "string comparison")
   (check-equal? (tc-str "\"hi\">\"hello\"") (types:make-BoolType) "string comparison")
   (check-equal? (tc-str "\"hi\"<=\"hello\"") (types:make-BoolType) "string comparison")
   (check-equal? (tc-str "\"hi\">=\"hello\"") (types:make-BoolType) "string comparison")
   ;(check-equal? (tc-str "\"hi\"<>\"hello\"") (types:make-BoolType) "string comparison")
   (check-equal? (tc-str "\"hi\"=\"hello\"") (types:make-BoolType) "string comparison")

   (check-exn exn:fail? (thunk (tc-str "5 = \"5\"")) "can't compare ints to strings")
   (check-exn exn:fail? (thunk (tc-str "5 >= \"5\"")) "can't compare ints to strings")

   ; bool types and logic expressions
   (check-equal? (tc-str "true") (types:make-BoolType) "bool literal")
   (check-equal? (tc-str "false") (types:make-BoolType) "bool literal")
   (check-equal? (tc-str "true & true") (types:make-BoolType) "simple logic tests")
   (check-equal? (tc-str "true | true & false") (types:make-BoolType) "more complicated logic tests")

   (check-exn exn:fail? (λ () (tc-str "true & 1")) "compare string to int")
   ;(check-equal? (tc-str "true & (5+3 < 6)") (types:make-BoolType) "logic and bool tests") ;;TODO need to get oop to work properly with params

   ; simple sequence expressions
   (check-equal? (tc-str "(1; 2; 3; 4)") (types:make-IntType) "simple sequence expressions")
   (check-equal? (tc-str "(1; 2; 3; \"hello\")") (types:make-StringType) "simple sequence expressions return last type (string)")
   
   ; simple let expressions
   (check-equal? (tc-str "let ni x is 5 in end") (types:make-VoidType) "simple let expressions without body")
   ; more complex, x should be stored and its type retreived 
   (check-equal? (tc-str "let ni x is 5 in x end") (types:make-IntType) "simple let expressions with body")
   (check-equal? (tc-str "let define itype kind as int
ni itype x is 5 in x end") (types:make-IntType)
                           "name type declaration and used in a variable declaration")

   (check-equal? (tc-str "peng") (types:make-PengType) "peng literal")

   ; simple records
   (check-equal? (tc-str
                  "let
  define empty kind as {}
  ni e is empty {}
in end") (types:make-VoidType)
         "simple records")

   ; record starting off null
   (check-equal? (tc-str "
let
  define empty kind as {}
  ni empty e is peng
in
end") (types:make-VoidType)
      "empty record (no fields)")

   ; record with 1 field
   (check-equal? (tc-str "
let
  define dot kind as { int x }
  ni dot p is dot { x is 5 }
in end") (types:make-VoidType)
         "record with 1 field")
   
   ; record with 2 fields
   (check-equal? (tc-str "
let
  define dot kind as { int x }
  ni dot p is dot { x is 5 }
in p.x end") (types:make-IntType)
             "record with 2 fields")
   
   ; record with 2 fields and a field access through dot notation
   (check-equal? (tc-str "
let
  define point kind as { int x, int y }
  ni point p is point { x is 5, y is 6 }
in p.x end") (types:make-IntType)
             "record with 2 fields and field access through dot notation")
   
   ; record with 2 fields, but accessing the 2nd kind
   (check-equal? (tc-str "
let
  define point kind as { int x, string y }
  ni point p is point { x is 5, y is \"hello\" }
in p.y end") (types:make-StringType)
             "record with 2 fields, but accessing the 2nd kind")

   ; record with 2 fields, but accessing the 2nd kind incorrectly
   (check-exn exn:fail? (thunk (tc-str "
let
  define point kind as { int x, string y }
  ni point p is point { x is 5, y is \"hello\" }
in p.y + 5 end"))
              "record with 2 fields, but accessing the 2nd kind incorrectly")
   
   ; record expression (dot notation) in a math expression
   (check-equal? (tc-str "
let
  define point kind as { int x, string y }
  ni point p is point { x is 5, y is \"hello\" }
in p.x + 5 end") (types:make-IntType)
                 "record expression (dot notation) in a math expression")

   ; record with itself
   (check-equal? (tc-str "
let
  define ilst kind as { int val, ilst rest }
  ni ilst l is ilst { val is 0, rest is peng }
in
  l.val
end") (types:make-IntType)
      "testing record with itself as a field")
   
   ; records in records
   (check-equal? (tc-str "
let
  define color kind as { int r, int g, int b }
  define point kind as { int x, int y, int z }
  define dot kind as { point p, color c }
  ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
in d.p.x + 5 end") (types:make-IntType)
                   "records in records")

   #;(check-equal? (tc-str "
let
  define intlist kind as { int val, intlist rest }
  ni intlist ilist is intlist { val is 0, rest is peng }
in
  ilist.val
end") (types:make-IntType)
                 "record has recursive field, assigns peng to it")

   #;(check-equal? (tc-str "
let
  define intlist kind as { int val, intlist rest }
  ni intlist ilist is intlist { val is 0, rest is intlist { val is 1, rest is peng } }
in
  ilist.rest.val
end")
                 (types:make-IntType)
                 "record has recursive field, instantiating field with record")
                          

   ; the following are a bit tricky, they should generally fail because you can't return
   ; a type that was declared in the same scope as the let (aliases can be an exception if
   ; they're pointing to a type in a higher scope
   (check-exn exn:fail? (thunk (tc-str "
let
  define e kind as { int x }
  ni e x is e { x is 7 }
in
  x
end")) "x is kind e, but no longer in scope")
   (check-exn exn:fail? (thunk (tc-str "
let
  define color kind as { int r, int g, int b }
  define point kind as { int x, int y, int z }
  define dot kind as { point p, color c }
  ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
in d.p end")) "d.p is kind point, but that's not in scope when let exits")
   (check-equal? (tc-str "
let
  define color kind as { int r, int g, int b }
  ni color col is 
    let
      define point kind as { int x, int y, int z }
    in
      let
         define dot kind as { point p, color c }
         ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
      in
        d.c
      end
    end
in
  col.g
end") (types:make-IntType))

   (check-exn exn:fail? (thunk (tc-str "
let
  define color kind as { int r, int g, int b }
in
  let
    define point kind as { int x, int y, int z }
  in
    let
       define dot kind as { point p, color c }
       ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
    in
      d.c
    end
  end
end")) "nested lets where color escapes body of outer let")

   (check-equal? (tc-str "
let
  define color kind as { int r, int g, int b }
  ni color col is 
    let
      define point kind as { int x, int y, int z }
    in
      let
         define mycol kind as color
         define dot kind as { point p, mycol c }
         ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is mycol { r is 1, g is 1, b is 1 } }
      in
        d.c
      end
    end
in
  col.g
end") (types:make-IntType))

   ;;; arrays -- begin with simple type delcarations, to make sure these are implemented
   (check-equal? (tc-str "
let
	define arrtype kind as array of int
in
end") (types:make-VoidType))
   ; now a assignments with an array type
   (check-equal? (tc-str "
let
  define arrtype kind as array of int
  ni a is arrtype[5] of 0
in
end") (types:make-VoidType))
   ; and one where we specify the type
   (check-equal? (tc-str "
let
  define arrtype kind as array of int
  ni arrtype a is arrtype[5] of 0
in
end") (types:make-VoidType))
   ; and subscript access
   (check-equal? (tc-str "
let
  define arrtype kind as array of int
  ni arrtype a is arrtype[5] of 0
in
  a[1]
end") (types:make-IntType))

   (check-equal? (tc-str "
let
  define dot kind as { int x }
  define dots kind as array of dot
  ni dots ds is dots[5] of dot { x is 0 }
in
  ds[0].x
end") (types:make-IntType)
      "array of a record type and assignment to it")


   (check-equal? (tc-str "
let
  define naught kind as {}
  define naughty kind as array of naught
  ni naughty nice is naughty[2] of naught {}
in
  now nice is peng
end") (types:make-VoidType)
      "arrays can actually be peng (in assignment)")

     (check-equal? (tc-str "
let
  define naught kind as {}
  define naughty kind as array of naught
  ni naughty nice is peng
in
  now nice is naughty[2] of naught {}
end") (types:make-VoidType)
      "arrays can actually be peng (in declaration)")
   
   ; branches
   (check-equal? (tc-str "if true then true else true end") (types:make-BoolType))
   (check-exn exn:fail? (thunk (tc-str "if true then true end"))
              "ifs without else must always return noval (void) type")
   (check-equal? (tc-str "if true then () end") (types:make-VoidType))


   ; assignments, begin with simple var assign
   (check-equal? (tc-str "let ni x is 5 in now x is 6 end") (types:make-VoidType) "simple var assignment in let")
   ; bad assignment
   (check-exn exn:fail? (thunk (tc-str "let ni x is 5 in now x is \"hi\" end")) "assigning string to wrong type (int)")
   ; record expression assign
   (check-equal? (tc-str "let define p kind as { int v } ni p x is p { v is 5 } in now x.v is 6 end") (types:make-VoidType) "record assignment to field")
   ; array assign
   (check-equal? (tc-str "let define i kind as array of int ni iarr is i[10] of 0 in now iarr[2] is 12 end") (types:make-VoidType) "array assign to index")
   ; assign peng
   (check-equal? (tc-str "let define no kind as { } ni x is no {} in now x is peng end") (types:make-VoidType) "peng assign to record")
   ; record assign
   (check-equal? (tc-str "let define p kind as { int v } ni p x is p { v is 5 } in now x is p { v is 7 } end") (types:make-VoidType) "new record assignment")


   ; while loops
   (check-equal? (tc-str "
let
  ni i is 0
in
  while (i < 10) do
    now i is i + 1
  end
end") (types:make-VoidType) "simple while loop")
   ; while bodies must return no value
   (check-exn exn:fail? (thunk (tc-str "
let
  ni i is 0
in
  while (i < 10) do
    (now i is i + 1; 5)
  end
end")) "while bodies must return no value")

   ; with loops, simple first
   (check-equal? (tc-str "with i as 0 to 10 do () end") (types:make-VoidType) "simple with loop")
   ; with loop bodies can't return values
   (check-exn exn:fail? (thunk (tc-str "with i as 0 to 10 do 5 end"))
              "loop bodies cannot return types")
   ; with loop cannot assign to declared variable from loop
   (check-exn exn:fail? (thunk (tc-str "with i as 0 to 10 do now i is 0 end"))
              "you cannot assign to the declared variable in the loop (it's read-only!)")

   ; breaks - breaks cannot live on their own
   (check-exn exn:fail? (thunk (tc-str "break")) "breaks cannot be outside of loops")
   (check-equal? (tc-str "with i as 0 to 10 do break end") (types:make-VoidType))
   ; should give you an error for assigning to break, wrong type really, you can't have void types for vars
   (check-exn exn:fail? (thunk (tc-str "let ni x is break in end"))
                               "can't assign break to something")
   ; types will work, but break is still wrong
   (check-exn exn:fail? (thunk (tc-str "let ni x is (break; 1) in end"))
              "break is not in the body of a loop")
   (check-exn exn:fail? (thunk (tc-str "let ni x is 0 in now x is (break; 1) end"))
              "break is not in the body of a loop")
   (check-equal? (tc-str "
while true do
  let
    ni x is 5
  in
    if true then break end
  end
end") (types:make-VoidType) "break can escape if/then if it's in a loop")
   ; break should work
   (check-equal? (tc-str "
while true do
  let
    ni x is 5
  in
    if true then break end
  end
 end") (types:make-VoidType) "break can escape let if it's in a loop")

   ; should return an error because break is trying to escape the body of a fun
   (check-exn exn:fail? (thunk (tc-str "
while(true) do
  let
    neewom fn() is break
  in
    fn()
  end
end")) "breaks can't escape the body of a function")

   ; should also fail
   (check-exn exn:fail? (thunk (tc-str "
let
  neewom fn() is break
in
  while true do
    fn()
  end ;
  while true do
    fn()
  end
end")) "break can't escape body of function, even though this looks ok")

   (check-equal? (tc-str "
while true do
  while if true then (break; true) else true end do () end
end") (types:make-VoidType) "break nested in an if/then/else within two loops")

   ; functions
   (check-equal? (tc-str "
let
  neewom nothing() is ()
in
end") (types:make-VoidType) "basic noarg fun declaration")
   ; simple function call
   (check-equal? (tc-str "
let
  neewom nothing() is ()
in
  nothing()
end") (types:make-VoidType) "noarg fun with funcall to it")


   ; sort of mutually recursive, at least the logic should execute
   (check-equal? (tc-str "
let
  neewom no() as bool is false and
  neewom yes() as bool is true
 in
end") (types:make-VoidType) "kind of like mutual recursion")
   ; mutually recursive with a call in the body
   (check-equal? (tc-str "
let
  neewom even(int x) as bool is
    if (odd(x)) then false else true end
  and neewom odd(int x) as bool is
    if (2 * (x / 2)) = x then false else true end 
in
  odd(5)
end") (types:make-BoolType) "almost like mutual recursion, odd may not be defined if you don't go through the fundecls")
   ; mutually recursive, for real this time

   ;;I THOUGHT THIS SHOULD BE ILLEGAL? TODO ASK JEFF
   #;(check-equal? (tc-str "
let
   neewom foo() is bar() and 
   neewom bar() is foo()
in
  foo()
end") (types:make-VoidType) "mutual recursion")
   ; mutually recursive, for real this time with args
   #;(check-equal? (tc-str "
let
   neewom foo(int x) as int is bar(x) and 
   neewom bar(int x) as int is foo(x)
in
  foo(42)
end") (types:make-IntType) "mutual recursion with args")

   ; multiple args
   (check-equal? (tc-str "
let
  define point kind as { int x, int y }
  neewom make-point(int x, int y) as int is
    let
      ni x is point { x is x, y is y }
    in
      x.y
    end
in
  make-point(1, 2)
end") (types:make-IntType) "mutual recursion with multiple args")

   ; this one might get errors saying print hasn't been defined, this is
   ; because these functions are part of the standard library and should
   ; have function headers in the value environment

   ;TODO PRINT DOESN'T EXIST YET
   #;(check-equal? (tc-str "
let
  ni N is 9

  define intArray kind as array of int

  ni row is intArray [ N ] of 0
  ni col is intArray [ N ] of 0
  ni diag1 is intArray [ N + N - 1] of 0
  ni diag2 is intArray [ N + N - 1] of 0

  neewom printboard () is 
    (with i as 0 to N - 1 do
      (with j as 0 to N - 1 do
        print(if col[i] = j then \" 0\" else \" .\" end)
       end;
       print(\"\n\"))
     end;
     print(\"\n\"))

  neewom try (int c) is
    if c = N - 1
    then printboard()
    else with r as 0 to N - 1 do
              if row[r] = 0 & diag1[r + c] = 0 & diag2[r + 7 - c] = 0 
              then (now row[r] is 1;
                    now diag1[r + c] is 1;
                    now diag2[r + 7 - c] is 1;
                    now col[c] is r;
                    try(c + 1);
                    now row[r] is 0;
                    now diag1[r + c] is 0;
                    now diag2[r + 7 - c] is 0)
              end
         end
    end
  in
    try(0)
end
") (types:make-VoidType) "the beautiful 8 queens example")


   ;;; These are from the ni samples, first with passing tests (modified to be correct now)
   ; test01.ni
   (check-equal? (tc-str "
/* array type and array variable example */
let
        define arrtype kind as array of int
        ni arrtype arr1 is arrtype[10] of 0
in
end") (types:make-VoidType) "test01.ni, array type and variable declared as that type")
   
   ; test02.ni
   (check-equal? (tc-str "

/* arr1 should be valid since expression 0 is int = myint */
let
        define myint kind as int
        define arrtype kind as array of myint

        ni arrtype arr1 is arrtype [10] of 0
in
end") (types:make-VoidType)
      "test02.ni: arr1 should be valid since expression 0 is int = myint)")

   ; test03.ni
   (check-equal? (tc-str "
/* test: record type and record variable */
let
        define rectype kind as {string name, int age}
        ni rectype rec1 is rectype { name is \"Nobody\", age is 1000}
in
        now rec1.name is \"Somebody\";
        rec1.name
end") (types:make-StringType)
   "test03.ni: record type and record variable assignment")

   ; test04.ni
   ;;TODO allow functions to be recursive
   (check-equal? (tc-str "
/* test: define a recursive function */
let
        /* calculate n! */
        neewom nfactor(int n) as int is
                if n = 0 then
                        1
                else
                        n * nfactor(n - 1)
                end
in
        nfactor(10)
end") (types:make-IntType)
      "test04.ni: define a recursive function")

   ; test05.ni
   (check-equal? (tc-str "
/* test: valid recursive types */
let
        /* define list type */
        define intlist kind as {int hd, intlist tl}

        /* define a tree */
        define tree kind as {int key, treelist children} and
        define treelist kind as {tree hd, treelist tl}

        ni intlist lis is intlist { hd is 0, tl is peng }
in
        lis.hd
end") (types:make-IntType)
      "test05.ni: valid recursive types")

   ; test06.ni
   (check-equal? (tc-str "
/* test: valid mutually recursive proceedures */
let
        neewom do_nothing1(int a, string b) is
                do_nothing2(a + 1)
        and
        neewom do_nothing2(int d) is
                do_nothing1(d, \"str\")
in
        do_nothing1(0, \"str2\")
end") (types:make-VoidType)
      "test06.ni: valid mutually recursive proceedures")

   ; test07.ni
   (check-equal? (tc-str "
/* test: more mutually recursive functions, these have return types */
let
        neewom do_nothing1(int a, string b) as int is
                (do_nothing2(a+1); 0)

        and neewom do_nothing2(int d) as string is
                (do_nothing1(d, \"str\"); \" \")
in
        do_nothing1(0, \"str2\")
end") (types:make-IntType)
      "test07.ni:  more mutually recursive functions, these have return types")

   ; test08.ni
   (check-equal? (tc-str "
/* test: correct if statement */
if (10 > 20) then 30 else 40 end") (types:make-IntType)
                                   "test08.ni: correct if statement")

   ; test12.ni
   (check-equal? (tc-str "
/* valid with and let */
let
        ni a is 0
in 
        with i as 0 to 100 do 
                (now a is a+1;
                ())
        end
end") (types:make-VoidType)
      "test12.ni: valid with and let expressions")

   ; test27.ni
   (check-equal? (tc-str "
/* locals hide globals */
let
        ni a is \"hello\"

        neewom g(int a) as int is a 
in
        g(2)
end") (types:make-IntType)
      "test27.ni: locals hide globals")

   ; test30.ni
   (check-equal? (tc-str "
/* synonyms are fine */
let 
                define a kind as array of int
                define b kind as a

                ni a arr1 is b [10] of 0
in
                arr1[2]
end") (types:make-IntType)
      "test30.ni: aliases for types work")

   ; test37.ni
   (check-equal? (tc-str "
/* redeclaration of variable; this is legal, there are two different
   variables with the same name.  The second one hides the first.  */
let
        ni a is 0
        ni a is \" \"
in
end") (types:make-VoidType)
      "test37.ni: redeclaration of variable; this is legal, there are two different variables with the same name.  The second one hides the first.")


   ; test41.ni
   (check-equal? (tc-str "
/* local types hide global */
let
        define a kind as int
in
        let
                define a kind as string
        in
        end
end") (types:make-VoidType)
      "test41.ni: local types hide higher level scope")

   ; test42.ni
   (check-equal? (tc-str "
/* correct declarations */
let 

    define arrtype1 kind as array of int
    define rectype1 kind as {string name, string address, int id, int age}
    define arrtype2 kind as array of rectype1
    define rectype2 kind as {string name, arrtype1 dates}

    define arrtype3 kind as array of string

    ni arr1 is arrtype1 [10] of 0
    ni arr2 is arrtype2 [5] of rectype1 {name is \"aname\", address is \"somewhere\", id is 0, age is 0}
    ni arrtype3 arr3 is arrtype3 [100] of \"\"

    ni rec1 is rectype1 {name is \"Kapoios\", address is \"Kapou\", id is 02432, age is 44}
    ni rec2 is rectype2 {name is \"Allos\", dates is arrtype1 [3] of 1900}

in

    now arr1[0] is 1; 
    now arr1[9] is 3;
    now arr2[3].name is \"kati\";
    now arr2[1].age is 23;
    now arr3[34] is \"sfd\";

    now rec1.name is \"sdf\";
    now rec2.dates[0] is 2323;
    now rec2.dates[2] is 2323
end") (types:make-VoidType)
      "test42.ni: many correct declarations and assignments")

   ; test44.ni
   (check-equal? (tc-str "
/* valid peng initialization and assignment */
let
        define rectype kind as {string name, int id}
        ni rectype b is peng
in
        now b is peng

end") (types:make-VoidType)
      "test44.ni: valid peng initialization and assignment")

   ; test46-a.ni
   #;(check-equal? (tc-str "
/* valid rec comparisons */
let 
        define rectype kind as {string name, int id}
        ni rectype b is peng
in
        now b is peng;
        b <> peng
end") (types:make-BoolType)
      "test46.ni: valid rec comparisons")

   ; test46-b.ni
   #;(check-equal? (tc-str "
/* valid rec comparisons */
let 
        define rectype kind as {string name, int id}
        ni rectype b is peng
in
        now b is peng;
        peng <> b
end") (types:make-BoolType)
      "test46.ni: valid rec comparisons, swapped order of comparison with peng")

   ; test47.ni
   (check-equal? (tc-str "
/* This is legal.  The second type \"a\" simply hides the first one.
   Because of the intervening variable declaration, the two \"a\" types
   are not in the same batch of mutually recursive types.
   See also test38 */
let
        define a kind as int
        ni b is 4
        define a kind as string
in
end") (types:make-VoidType)
      "test47.ni: type names can hide other type names in the same scope outside of mutual recursive definitions")
   
   ; test48.ni
   (check-equal? (tc-str "
   /* This is legal.  The second function \"g\" simply hides the first one.
   Because of the intervening variable declaration, the two \"g\" functions
   are not in the same  batch of mutually recursive functions. 
   See also test39 */
let
        neewom g(int a) as int is a
        define t kind as int
        neewom g(int a) as int is a
in
end") (types:make-VoidType)
      "test48: function names can hide other function names in the same scope outside of mutual recursive definitions")

   ; test50.ni
   (check-equal? (tc-str "
// break in the body of a loop, 
// note this also tests that break return a void type
let
        ni x is 5
in
        with i as 1 to 10 do
                (if x > 5 then
                        break end;
                print(\"hello\"))
        end
end") (types:make-VoidType)
      "test50.ni: break in the body of a loop, also tests that break returns a void type")

   ; test52.ni
   (check-equal? (tc-str "
// break nested in a couple of loops, both should pass
let
    neewom add(int x, int y) as int is
        let
            ni t is x + y
        in
            while (true) do
                (t + t + t; break; print(\"hello\"))
            end ;
            t
        end
in
    with i as 10 to 20 do
        (print(\"hello\"); break)
    end
end") (types:make-VoidType)
      "test52.ni: break nested in a couple of loops, both should pass")

   ; test55.ni
   (check-equal? (tc-str "
// mutually recursive types successfully going through array
let
    define c kind as b and
    define b kind as d and
    define d kind as array of c
in
end") (types:make-VoidType)
      "test55.ni: mutually recursive types successfully going through array, this means you can assign peng to arrays")

   ; test 56.ni
   (check-equal? (tc-str "
// succesful mutually recursive types that go through record
let
    define c kind as b and
    define b kind as { d x, c y} and
    define d kind as c
in
end") (types:make-VoidType)
      "test56.ni: succesful mutually recursive types that go through record")

   ; test57.ni
   (check-equal? (tc-str "
// a more complicated test that checks for nested record field access, should pass typechecking
let
    define vec4 kind as { int x, int y, int z, int w }
    define mat4 kind as { vec4 row1, vec4 row2, vec4 row3, vec4 row4 }

    neewom vec4add (vec4 v, vec4 v') as vec4 is
        vec4 { x is v.x + v'.x,
               y is v.y + v'.y,
               z is v.z + v'.z,
               w is v.w + v'.w }

    neewom mat4add (mat4 m1, mat4 m2) as mat4 is
        mat4 { row1 is vec4add(m1.row1, m2.row1),
               row2 is vec4add(m1.row2, m2.row2),
               row3 is vec4add(m1.row3, m2.row3),
               row4 is vec4add(m1.row4, m2.row4) }


    ni identity is mat4 { row1 is vec4 { x is 1, y is 0, z is 0, w is 0 },
                          row2 is vec4 { x is 0, y is 1, z is 0, w is 0 },
                          row3 is vec4 { x is 0, y is 0, z is 1, w is 0 },
                          row4 is vec4 { x is 0, y is 0, z is 0, w is 1 } }


in
    mat4add(identity, identity);

    // if you're resolving fields properly, this will work, if not, you'll get a type error
    identity.row1.x + identity.row2.x
end") (types:make-IntType)
      "test57.ni: a more complicated test that checks for nested record field access")

   ; test61.ni
   (check-equal? (tc-str "
/* this should pass (as opposed to test59 and test60), because we
   should return the actual types, not the aliased ones from the body
   of a let expression */

3 + let 
      define ity kind as int
      ni ity x is 5
    in
      x
    end") (types:make-IntType)
          "test61.ni: return the actual type from a let body, not the aliased type--see also test59 and test60")


   ;;; these tests should generate errors
   
   ; test09.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error: types of then and else differ */
if (5 > 4) then 13 else \" \" end"))
              "test09.ni: types differ from true and false branch of if-expression")

   ; test10.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error: body of while not void */
while (10 > 5) do 5 + 6 end"))
              "test10.ni: error: body of while not void")

   ; test11-2.ni
   (check-exn exn:fail? (thunk (tc-str "
   /* error: index variable erroneously assigned to.  */
with i as 10 to 20 do 
	now i is i - 1
end"))
              "test11-2.ni:  error: index variable erroneously assigned to")

   ; test11.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error: hi expr is not int, and index variable erroneously assigned to.  */
with i as 10 to " " do 
	print(i)
end"))
              "test11.ni: error: hi expr is not int")

   
   ; test13.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error: comparison of incompatible types */
3 > \"df\""))
              "test13.ni:  error: comparison of incompatible types")
   

   ; test14.ni
   #;(check-exn exn:fail? (thunk (tc-str "
/* error : compare rec with array */
let
	define arrtype kind as array of int
	define rectype kind as {string name, int id}

	ni rec is rectype {name is \"aname\", id is 0}
	ni arr is arrtype [3] of 0

in
	if rec <> arr then 3 else 4 end
end")) "test14.ni: error: compare record with array")

   ; test15.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error : if-then returns non void type */
if 20 then 3 end")) "test15.ni: error: if-then returns a non-void type")

   ; test16.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error: mutually recursive types thet do not pass through record or array */
let 
  define a kind as c and
  define b kind as a and
  define x kind as d and
  define d kind as a
in
 \"\"
end")) "test16.ni: error: mutually recursive types thet do not pass through record or array")


   ; test17.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error: definition of recursive types is interrupted because there's no AND */
let
	/* define a tree */
	define tree kind as {int key, treelist children}
	ni int d is 0
	define treelist kind as {tree hd, treelist tl}
in
	d
end")) "test17.ni: error: definition of recursive types is interrupted because there's no AND")
   
   ; test18.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error : definition of recursive functions is interrupted because there's no AND */
let

neewom do_nothing1(int a, string b) as int is
		(do_nothing2(a+1);0)

ni d is 0

neewom do_nothing2(int d) as string is 
		(do_nothing1(d, \"str\");\" \")

in
	do_nothing1(0, \"str2\")
end")) "test18.ni: error : definition of recursive functions is interrupted because there's no AND")

   ; test19.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error : second function uses variables local to the first one, undeclared variable */
let

neewom do_nothing1(int a, string b) as int is 
		(do_nothing2(a+1);0)

and neewom do_nothing2(int d) as string is 
		(do_nothing1(a, \"str\");\" \")

in
	do_nothing1(0, \"str2\")
end")) "test19.ni: error : second function uses variables local to the first one, undeclared variable")
   
   ; test20.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error: undeclared variable i */
while 10 > 5 do (i+1;()) end")) "test20.ni: error: undeclared variable i")

   ; test21.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error : procedure returns void value and procedure is used in an arithmetic expr */
let

/* calculate n! */
neewom nfactor(int n) is
		if  n = 0 then
		    1
		else
		    n * nfactor(n - 1)
		end

in
	nfactor(10)
end")) "test21.ni: error : procedure returns void value and procedure is used in an arithmetic expr")

   ; test22.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error : field not in record type */
let 
	define rectype kind as {string name, int id}
	ni rec1 is rectype {name is \"Name\", id is 0}
in
	now rec1.nam is \"asd\"
end")) "test22.ni: error: field not in record type")

   ; test23.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error : type mismatch */

let 
	define rectype kind as {string name, int id}
	ni rec1 is rectype {name is \"aname\", id is 0}
in
	now rec1.name is 3;
	now rec1.id is "" 
end")) "test23.ni: error: type mismatch for field in record")

   ; test24.ni
   (check-exn exn:fail? (thunk (tc-str "
/* error : variable not array */
let 
	ni d is 0
in
	d[3]
end")) "test24.ni: error: variable is not an array")

   ; test25.ni
   (check-type-error "/* error : variable not record */
let 
	ni d is 0
in
	d.f 
end" "test25.ni: error: variable not a record")

   ; test26.ni
   (check-type-error "
/* error : integer required */
let
  ni var is 5
in
  3 + \"var\"
end" "test26.ni: error: integers required")

   ; test28.ni
   (check-type-error "
/* error : different record types */

let
	define rectype1 kind as {string name, int id}
	define rectype2 kind as {string name, int id}

	ni rectype1 rec1 is rectype2 {name is \"Name\", id is 0}
in
end" "test28.ni: error: delcaration with wrong record type, even though they look the same")


   ; test29.ni
   (check-type-error "
/* error : different array types */

let
        define arrtype1 kind as array of int
        define arrtype2 kind as array of int

        ni arrtype1 arr1 is arrtype2 [10] of 0
in
        arr1
end" "test29.ni: error: can't assign different array types")

   ; test31.ni
   (check-type-error "/* error : type constraint and init value differ */
let 
	ni int a is \" \"
in
end" "test31.ni: error : type constraint and init value differ")

   ; test32.ni
   (check-type-error "/* error : initializing exp and array type differ */
let
	define arrayty kind as array of int

	ni a is arrayty [10] of \" \"
in
end" "test32.ni: error : initializing exp and array type differ")

   ; test33.ni
   (check-type-error "/* error : unknown type */
let
	ni a is rectype {}
in
end" "test33.ni: error : must declare a record type before using it")
   
   ; test34.ni
   (check-type-error "/* error : formals and actuals have different types */
let
	neewom g (int a, string b) as int is a
in
	g(\"one\", \"two\")
end" "test34.ni: error: error : formals and actuals have different types")

   ; test35.ni
   (check-type-error "/* error : formals are more then actuals */
let
	neewom g (int a, string b) as int is a
in
	g(\"one\")
end" "test35.ni: error : formals are more then actuals")
           
   ; test36.ni
   (check-type-error "/* error: formals are fewer than actuals */
let
	neewom g(int a, string b) as int is a
in
	g(3, \"one\", 5)
end" "test36.ni: error: formals are fewer than actuals")
              
   ; test38.ni
   (check-type-error "/* error: This is illegal, since there are two types with the same name
    in the same (consecutive) batch of mutually recursive types. 
    See also test47  */
let
	define a kind as int and
	define a kind as string
in
end" "test38.ni: error: This is illegal, since there are two types with the same name
    in the same (consecutive) batch of mutually recursive types. 
    See also test47")

   ; test39.ni
   (check-type-error "/* error: This is illegal, since there are two functions with the same name
    in the same (consecutive) batch of mutually recursive functions.
   See also test48 */
let
	neewom g(int a) as int is a and
	neewom g(int a) as int is a
in
	0
end" "test39.ni: error: This is illegal, since there are two functions with the same name
    in the same (consecutive) batch of mutually recursive functions.
   See also test48")
   
   ; test40.ni
   (check-type-error "/* error : procedure returns value */
let
	neewom g(int a) is a
in 
	g(2)
end" "test40: error : procedure returns value")
   
   ; test43.ni
   (check-type-error "/* error: initialize with void and causing type mismatch in addition */
let 
	ni a is ()
in
	a + 3
end" "test43.ni: error: initialize with void and causing type mismatch in addition")

   ; test45.ni
   (check-type-error "/* error: initializing nil expressions not constrained by record type */
let 
	define rectype kind as {string name, int id}

	ni a is peng
in
	a
end" "test45.ni: error: initializing nil expressions not constrained by record type ")
   
   ; test49.ni
   (check-type-error "/* error: syntax error (this should show up before you walk your AST), 
   peng should not be preceded by type-id.  */
let 
	define rectype kind as {string name, int id}

	ni a is rectype peng
in
	a
end" "test49.ni: error: syntax error (this should show up before you walk your AST), 
   peng should not be preceded by type-id.")

   ; test51.ni
   (check-type-error "// error: break is not in a loop, just a let, but not in a function def either
let
	ni x is 5
in
		(if x > 5 then
			break end;
		print(\"hello\"))
end" "test51.ni: error: break is not in a loop, just a let, but not in a function def either")
                       
   ; test53.ni
   (check-type-error "// error: (see test52.ni) one break is in a function body without a loop,
//      the other is just not in a loop, though it looks like it is
let
    neewom add(int x, int y) as int is
        let
            ni t is x + y
        in
            break; t
        end
in
    with i as 10 to 20 do
        (print(\"hello\")) end; break
end" "test53.ni: error: (see test52.ni) one break is in a function body without a loop,
       the other is just not in a loop, though it looks like it is")

   
   ; test54.ni
   (check-type-error "// error: mutually recursive types must go through a record or array
let
    define b kind as c and
    define c kind as d and
    define d kind as b
in
end" "test54.ni: error: mutually recursive types must go through a record or array")

   ; test58.ni
   (check-type-error "// error: this should fail on z because it's out of scope
let
   ni y is 4
in
  let
    neewom add(int x) as int is 6
    ni z is 3
  in
  end;
  z
end" "test58.ni: error: this should fail on z because it's out of scope ")

   
   ; test59.ni
   (check-type-error "/* this should fail, types cannot escape the let body 
and notice that rec returns with a type that was defined
in the decls (see 2.4.4) */

let
  define recty kind as {}
  ni recty rec is recty {}
in
  rec
end" "test59.ni: error: this should fail, types cannot escape the let body 
and notice that rec returns with a type that was defined
in the decls (see 2.4.4)")
   
   ; test60.ni
   (check-type-error "/* this should fail for the same reason test59.ni fails, the
   type escapes the body of the let (2.4.4) */
let
  define kind int_array as array of int
  ni arr is int_array[10] of 0
in
  arr
end" "test60.ni: error: this should fail for the same reason test59.ni fails, the
   type escapes the body of the let (2.4.4)")


   ))

  

(define (check-type-error str msg)
  (check-exn exn:fail? (thunk (tc-str str)) msg))

;;TODO FIX ALL NON-FAILING TESTS
(require rackunit/text-ui)
(run-tests typechecking-tests)
