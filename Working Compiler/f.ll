;;;;; START OF PROGRAM ;;;;;
; target data layout for Mac, change to m:w instead for Windows
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"

; target triple for Mac
target triple = "x86_64-apple-macosx10.12.0"

; STRUCTS for strings and arrays
%struct.string = type { i64, i8* }
%struct.array = type { i64, i64* }
; STANDARD LIB DECLARATIONS
declare %struct.string* @makeString(i8* %str)
declare %struct.string* @getChar()
declare i64 @ord(%struct.string* nocapture readonly %str)
declare %struct.string* @chr(i64 %i)
declare i64 @size(%struct.string* nocapture readonly %str)
declare %struct.string* @substring(%struct.string* nocapture readonly %str, i32 %first, i32 %n)
declare %struct.string* @concat(%struct.string* nocapture readonly %s1, %struct.string* nocapture readonly %s2)
declare void @Exit(i64 %i)
declare void @print(%struct.string* nocapture readonly %str)
declare void @printi(i64 %v) #0
declare %struct.string* @intToString(i64 %val)
declare i64* @stringCompare(%struct.string* nocapture readonly %str1, %struct.string* nocapture readonly %str2)
declare %struct.array* @makeArray(i64 %numElements)
declare i64* @getElementAddressAt(%struct.array* %arr, i64 %index)
; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1
; GLOBAL variables, defined in the program
@L1 = global [6 x i8] c"hello\00", align 1
@L2 = global %struct.string { i64 5, i8*
             getelementptr inbounds ([6 x i8], [6 x i8]* @L1, i32 0, i32 0) }, align 8
@L3 = global [6 x i8] c"hello\00", align 1
@L4 = global %struct.string { i64 5, i8*
             getelementptr inbounds ([6 x i8], [6 x i8]* @L3, i32 0, i32 0) }, align 8
@L8 = global [7 x i8] c"hello
\00", align 1
@L9 = global %struct.string { i64 6, i8*
             getelementptr inbounds ([7 x i8], [7 x i8]* @L8, i32 0, i32 0) }, align 8
@L10 = global [8 x i8] c"goodbye\00", align 1
@L11 = global %struct.string { i64 7, i8*
             getelementptr inbounds ([8 x i8], [8 x i8]* @L10, i32 0, i32 0) }, align 8
; FUNCTIONS
; MAIN

; Function Attrs: nounwind ssp uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
%1 = alloca i32, align 4
%2 = alloca i32, align 4
%3 = alloca i8**, align 8
store i32 0, i32* %1
store i32 %argc, i32* %2, align 4
store i8** %argv, i8*** %3, align 8

; Ni program to follow...

; if then else
%t2 = call i64 @stringCompare( %struct.string * @L2, %struct.string * @L4)
%t3 = icmp eq i64 %t2, 0
br i1 %t3, label %L5, label %L6

; true branch
L5:

; calling function: print
call void @print( %struct.string * @L9)
br label %L7

; false branch
L6:

; calling function: print
call void @print( %struct.string * @L11)
br label %L7
L7:
; ... end Ni program
ret i32 0
}
attributes #0 = { nounwind ssp uwtable 
"less-precise-fpmad"="false" "no-frame-pointer-elim"="true" 
"no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" 
"no-nans-fp-math"="false" "stack-protector-buffer-size"="8" 
"unsafe-fp-math"="false" "use-soft-float"="false" } 
;;;;; END OF PROGRAM ;;;;;

