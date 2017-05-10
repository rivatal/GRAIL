; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt9 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt10 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt11 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt12 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt13 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt14 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt15 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt16 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt17 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [7 x i8] c"String\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"print!1"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"printbool!2"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!3"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printint!4"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define void @"print!5"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"printbool!6"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!7"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printint!8"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @str, i32 0, i32 0))
  %printf1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt16, i32 0, i32 0), i1 true)
  %printf2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt16, i32 0, i32 0), i1 false)
  %printf3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt16, i32 0, i32 0), i32 1)
  ret i32 0
}
